;;; emacs-ide-health.el --- Health Monitoring & Diagnostics -*- lexical-binding: t -*-
;;; Commentary:
;;; IDE health status checking, performance monitoring, and diagnostic reporting.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (recalibration):
;;;   - FIX-DOUBLE-STARTUP-CHECK: emacs-ide-health-check-startup is called
;;;     at 1-second idle by init.el.  emacs-ide-health--setup-periodic-checks
;;;     is called from after-init-hook and starts a repeating idle timer at
;;;     emacs-ide-health--check-interval (60 seconds).  On a normal startup
;;;     both fire in quick succession: after-init-hook fires, the 60-second
;;;     timer is armed, then the 1-second idle fires emacs-ide-health-check-
;;;     startup, then 60 seconds later the periodic timer fires again.  This
;;;     means checks run twice in the first 60 seconds on every startup, with
;;;     the first run at 1s wasting work that the periodic timer would also do.
;;;
;;;     Root cause: the periodic timer first-fire delay equals its repeat
;;;     interval (60 s), so its FIRST invocation is at T+60s — that is fine.
;;;     The double-run happens because init.el ALSO schedules an explicit
;;;     1-second one-shot via (run-with-idle-timer 1 nil
;;;     #'emacs-ide-health-check-startup).  That explicit call is intentional
;;;     (dashboard refresh at 6s depends on health results being available
;;;     by 3s), so we should NOT remove it.
;;;
;;;     Fix: emacs-ide-health-check-startup now skips the run when health
;;;     results are already fresh (last-check within 5 seconds) to guard
;;;     against any future code path that schedules it more than once, and
;;;     it sets emacs-ide-health--startup-check-done so that the periodic
;;;     timer's own first fire (at T+60s) proceeds normally.  The key
;;;     invariant is: the startup one-shot (T+1s) and the periodic timer
;;;     (T+60s, T+120s, ...) are on non-overlapping schedules and do not
;;;     race.  The "fresh result" guard is belt-and-suspenders only.
;;;
;;;     Additionally, emacs-ide-health--setup-periodic-checks now uses
;;;     a FIRST-FIRE delay of emacs-ide-health--check-interval (60 s)
;;;     rather than 0, so the periodic timer never races with the startup
;;;     one-shot.  This was already the case (run-with-idle-timer with a
;;;     positive delay) but is now explicitly documented.
;;; Fixes vs 3.0.4 (retained):
;;;   - FIX-MISSING-VARS, FIX-MISSING-FN-RUN-CHECK, FIX-MISSING-FN-SYSTEM-TOOLS,
;;;     FIX-MISSING-FN-AUTO-FIX, FIX-MISSING-FN-SUMMARY, FIX-REPEAT-KEYWORD.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; STATE VARIABLES
;; Referenced by: ui-dashboard.el, emacs-ide-test.el, ui-modeline.el
;; ============================================================================

(defvar emacs-ide-health-results nil
  "Alist of (CHECK-KEY . plist) from the last full health check run.
Each plist has :status (ok/warning/error), :message (string).")

(defvar emacs-ide-health-last-check nil
  "Timestamp of last completed health check, or nil if never run.")

(defvar emacs-ide-health--last-errors 0
  "Count of error-status checks from the last full health run.")

(defvar emacs-ide-health--last-warnings 0
  "Count of warning-status checks from the last full health run.")

(defvar emacs-ide-health--check-interval 60
  "Interval (in seconds) for periodic health checks.")

(defvar emacs-ide-health--periodic-timer nil
  "Timer object for periodic health checks.")

(defvar emacs-ide-health--checks-run 0
  "Counter for total health checks run this session.")

;; FIX-DOUBLE-STARTUP-CHECK: flag set by emacs-ide-health-check-startup so
;; subsequent no-op guard checks can be applied if needed in the future.
(defvar emacs-ide-health--startup-check-done nil
  "Non-nil after the initial startup health check has completed.")

;; ============================================================================
;; CHECK REGISTRY
;; ============================================================================

(defvar emacs-ide-health--registered-checks nil
  "Alist of (KEY . fn) for registered health checks.")

(defun emacs-ide-health-register-check (key fn)
  "Register a health check FN under KEY.
FN must return a plist with :status (ok/warning/error) and :message."
  (let ((existing (assoc key emacs-ide-health--registered-checks)))
    (if existing
        (setcdr existing fn)
      (push (cons key fn) emacs-ide-health--registered-checks))))

;; ============================================================================
;; SINGLE-CHECK RUNNER
;; Referenced by emacs-ide-test.el — must accept (key fn) args.
;; ============================================================================

(defun emacs-ide-health-run-check (key fn)
  "Run a single health check FN under KEY.
Returns a cons (KEY . result-plist) without modifying global state.
The returned plist has :status (ok/warning/error) and :message.
This is the lightweight variant used by tests to avoid clobbering live state."
  (condition-case err
      (let ((result (funcall fn)))
        (cons key (if (plistp result) result
                    (list :status 'error :message (format "bad result: %S" result)))))
    (error
     (cons key (list :status 'error
                     :message (error-message-string err))))))

;; ============================================================================
;; BUILT-IN CHECKS
;; ============================================================================

(defun emacs-ide-health-check-system-tools ()
  "Check for required system tools on PATH.
Returns a plist with :status and :message suitable for emacs-ide-health-run-check."
  (let ((required '("git" "rg"))
        (missing '()))
    (dolist (tool required)
      (unless (executable-find tool)
        (push tool missing)))
    (if missing
        (list :status 'warning
              :message (format "missing: %s" (mapconcat #'identity missing ", ")))
      (list :status 'ok
            :message "all required tools found"))))

(defun emacs-ide-health-check-lsp ()
  "Check LSP server availability."
  (condition-case err
      (if (not (bound-and-true-p emacs-ide-lsp-enable))
          (list :status 'ok :message "LSP disabled in config")
        (let ((servers '("pyright" "rust-analyzer" "gopls"
                         "typescript-language-server" "clangd"))
              (found 0))
          (dolist (s servers)
            (when (executable-find s) (cl-incf found)))
          (if (> found 0)
              (list :status 'ok
                    :message (format "%d LSP server(s) available" found))
            (list :status 'warning
                  :message "no LSP servers found on PATH"))))
    (error
     (list :status 'error :message (error-message-string err)))))

(defun emacs-ide-health-check-config ()
  "Validate configuration integrity."
  (condition-case err
      (cond
       ((not (bound-and-true-p emacs-ide-config-loaded-p))
        (list :status 'error :message "config not loaded"))
       ((null emacs-ide-config-data)
        (list :status 'warning :message "config data is nil (using defaults)"))
       (t
        (list :status 'ok :message (format "env: %s"
                                           (or emacs-ide-config-environment "default")))))
    (error
     (list :status 'error :message (error-message-string err)))))

(defun emacs-ide-health-check-emacs-version ()
  "Check Emacs version meets minimum requirement."
  (if (version<= "29.1" emacs-version)
      (list :status 'ok :message (format "Emacs %s" emacs-version))
    (list :status 'error
          :message (format "Emacs %s < 29.1 required" emacs-version))))

;; Register built-in checks
(emacs-ide-health-register-check 'system-tools  #'emacs-ide-health-check-system-tools)
(emacs-ide-health-register-check 'lsp           #'emacs-ide-health-check-lsp)
(emacs-ide-health-register-check 'config        #'emacs-ide-health-check-config)
(emacs-ide-health-register-check 'emacs-version #'emacs-ide-health-check-emacs-version)

;; ============================================================================
;; FULL CHECK RUNNER
;; ============================================================================

(defun emacs-ide-health-run-checks ()
  "Run all registered health checks and update global state."
  (interactive)
  (setq emacs-ide-health--checks-run (1+ emacs-ide-health--checks-run))
  (let ((results '())
        (errors 0)
        (warnings 0))
    (dolist (entry emacs-ide-health--registered-checks)
      (let* ((key (car entry))
             (fn  (cdr entry))
             (res (condition-case err
                      (funcall fn)
                    (error
                     (list :status 'error :message (error-message-string err))))))
        (push (cons key res) results)
        (let ((status (plist-get res :status)))
          (cond ((eq status 'error)   (cl-incf errors))
                ((eq status 'warning) (cl-incf warnings))))))
    (setq emacs-ide-health-results        (nreverse results)
          emacs-ide-health-last-check     (current-time)
          emacs-ide-health--last-errors   errors
          emacs-ide-health--last-warnings warnings)))

;; ============================================================================
;; SUMMARY STRING
;; Referenced by ui-modeline.el
;; ============================================================================

(defun emacs-ide-health--summary-string ()
  "Return a short one-line health summary string for modeline display.
Returns empty string if no check has run yet."
  (if (null emacs-ide-health-last-check)
      ""
    (cond
     ((> emacs-ide-health--last-errors 0)
      (format "✗ %d error%s"
              emacs-ide-health--last-errors
              (if (= emacs-ide-health--last-errors 1) "" "s")))
     ((> emacs-ide-health--last-warnings 0)
      (format "⚠ %d warning%s"
              emacs-ide-health--last-warnings
              (if (= emacs-ide-health--last-warnings 1) "" "s")))
     (t "✓"))))

;; ============================================================================
;; AUTO-FIX
;; Referenced by emacs-ide-spot-check.el
;; ============================================================================

(defun emacs-ide-health-auto-fix ()
  "Attempt basic auto-remediation for known fixable health issues.
Currently handles: missing nerd-icons font prompt."
  (interactive)
  (let ((fixed 0))
    (dolist (entry emacs-ide-health-results)
      (let* ((key (car entry))
             (plist (cdr entry))
             (status (plist-get plist :status))
             (msg    (plist-get plist :message)))
        (when (eq status 'warning)
          (cond
           ;; Offer to install nerd-icons fonts
           ((and (eq key 'system-tools)
                 (string-match-p "nerd-icons" (or msg "")))
            (when (and (fboundp 'nerd-icons-install-fonts)
                       (y-or-n-p "Install nerd-icons fonts? "))
              (nerd-icons-install-fonts)
              (cl-incf fixed)))))))
    (if (> fixed 0)
        (message "✓ health-auto-fix: applied %d fix(es). Re-running checks..." fixed)
      (message "health-auto-fix: no automatic fixes available for current issues"))
    (when (> fixed 0)
      (run-with-idle-timer 1 nil #'emacs-ide-health-run-checks))))

;; ============================================================================
;; STARTUP CHECK
;; FIX-DOUBLE-STARTUP-CHECK: Skip when results are already fresh (< 5 seconds
;; old) to guard against the pathological case of two callers scheduling this
;; in quick succession.  In normal operation the startup one-shot fires at T+1s
;; idle and is the ONLY caller at that time; the periodic timer's first fire
;; is at T+60s (see emacs-ide-health--setup-periodic-checks below).
;; ============================================================================

(defun emacs-ide-health-check-startup ()
  "Run health checks once at startup (called from init.el via idle timer).
FIX-DOUBLE-STARTUP-CHECK: skips the run if a check completed within the last
5 seconds to guard against accidental duplicate scheduling.  The periodic
timer (60-second interval, set up by after-init-hook) fires independently
and is unaffected by this guard."
  ;; Skip if results are already fresh (another call beat us to it)
  (let ((fresh (and emacs-ide-health-last-check
                    (< (float-time
                        (time-subtract (current-time)
                                       emacs-ide-health-last-check))
                       5.0))))
    (unless fresh
      (emacs-ide-health-run-checks)
      (setq emacs-ide-health--startup-check-done t)
      (when (> emacs-ide-health--last-errors 0)
        (message "⚠️  IDE Health: %d error(s) detected. Run M-x emacs-ide-health-check-all"
                 emacs-ide-health--last-errors)))))

;; ============================================================================
;; PERIODIC HEALTH MONITORING
;; The FIRST-FIRE delay (first argument to run-with-idle-timer) equals the
;; repeat interval (60 s), so the periodic timer's first invocation is at
;; T+60s idle — well after the startup one-shot (T+1s) has run.  The two
;; timers are therefore on non-overlapping schedules.
;; ============================================================================

(defun emacs-ide-health--setup-periodic-checks ()
  "Setup periodic health check timer.
FIX-DOUBLE-STARTUP-CHECK: first-fire delay is emacs-ide-health--check-interval
(60 s), which is after the startup one-shot (1 s) has already run.
The two timers therefore never race."
  (when emacs-ide-health--periodic-timer
    (cancel-timer emacs-ide-health--periodic-timer))
  ;; Both the first-fire delay AND the repeat interval are set to
  ;; emacs-ide-health--check-interval so the first periodic check fires at
  ;; T+60s, not immediately after startup.
  (setq emacs-ide-health--periodic-timer
        (run-with-idle-timer emacs-ide-health--check-interval
                             emacs-ide-health--check-interval
                             #'emacs-ide-health-run-checks)))

(add-hook 'after-init-hook #'emacs-ide-health--setup-periodic-checks)

;; ============================================================================
;; HEALTH STATUS REPORTING
;; ============================================================================

(defun emacs-ide-health-status ()
  "Display IDE health status report."
  (interactive)
  (with-output-to-temp-buffer "*IDE Health*"
    (princ "=== EMACS IDE HEALTH STATUS ===\n\n")
    (princ (format "Checks run this session: %d\n" emacs-ide-health--checks-run))
    (when emacs-ide-health-last-check
      (princ (format "Last check: %s\n\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S"
                                         emacs-ide-health-last-check))))
    (if (null emacs-ide-health-results)
        (princ "No checks run yet.\n")
      (princ (format "Errors:   %d\n" emacs-ide-health--last-errors))
      (princ (format "Warnings: %d\n\n" emacs-ide-health--last-warnings))
      (dolist (entry emacs-ide-health-results)
        (let* ((key    (car entry))
               (plist  (cdr entry))
               (status (plist-get plist :status))
               (msg    (plist-get plist :message))
               (icon   (cond ((eq status 'ok)      "✓")
                             ((eq status 'warning)  "⚠")
                             ((eq status 'error)    "✗")
                             (t                     "?"))))
          (princ (format "  %s %-20s %s\n" icon key (or msg ""))))))))

(defun emacs-ide-health-check-all ()
  "Run all health checks and display results.
Main entry point expected by diagnostics system, dashboard, and tests."
  (interactive)
  (emacs-ide-health-run-checks)
  (emacs-ide-health-status))

(provide 'emacs-ide-health)
;;; emacs-ide-health.el ends here
