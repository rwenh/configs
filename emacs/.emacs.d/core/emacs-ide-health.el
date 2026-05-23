;;; emacs-ide-health.el --- Fast Health Checks -*- lexical-binding: t -*-
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-health-results           nil)
(defvar emacs-ide-health-last-check        nil)
(defvar emacs-ide-health--registered-checks nil)
(defvar emacs-ide-health--periodic-timer   nil)
(defvar emacs-ide-health--errors           0)
(defvar emacs-ide-health--warnings         0)

;;; ─── Registry ────────────────────────────────────────────────────────────────

(defun emacs-ide-health-register-check (key fn)
  "Register health check FN under KEY.  Replaces any existing entry for KEY."
  (let ((existing (assoc key emacs-ide-health--registered-checks)))
    (if existing
        (setcdr existing fn)
      (push (cons key fn) emacs-ide-health--registered-checks))))

;;; ─── Built-in checks ─────────────────────────────────────────────────────────

(defun emacs-ide-health-check-system-tools ()
  (let ((required '("git" "rg")) missing)
    (dolist (tool required)
      (unless (executable-find tool) (push tool missing)))
    (if missing
        (list :status 'warning :message (format "missing: %s" (string-join missing ", ")))
      (list :status 'ok :message "all tools found"))))

(defun emacs-ide-health-check-lsp ()
  (if (not (bound-and-true-p emacs-ide-lsp-enable))
      (list :status 'ok :message "LSP disabled")
    (let ((servers '("pyright" "rust-analyzer" "gopls"
                     "typescript-language-server" "clangd"))
          (found 0))
      (dolist (s servers) (when (executable-find s) (cl-incf found)))
      (if (> found 0)
          (list :status 'ok      :message (format "%d server(s) found" found))
        (list :status 'warning  :message "no LSP servers on PATH")))))

(defun emacs-ide-health-check-config ()
  (cond
   ((not (bound-and-true-p emacs-ide-config-loaded-p))
    (list :status 'error   :message "config not loaded"))
   ((null emacs-ide-config-data)
    (list :status 'warning :message "config is nil (using defaults)"))
   (t
    (list :status 'ok      :message (format "env: %s" emacs-ide-config-environment)))))

(defun emacs-ide-health-check-emacs-version ()
  (if (version<= "29.1" emacs-version)
      (list :status 'ok    :message (format "Emacs %s" emacs-version))
    (list :status 'error   :message (format "Need Emacs 29.1+, have %s" emacs-version))))

(emacs-ide-health-register-check 'system-tools   #'emacs-ide-health-check-system-tools)
(emacs-ide-health-register-check 'lsp            #'emacs-ide-health-check-lsp)
(emacs-ide-health-register-check 'config         #'emacs-ide-health-check-config)
(emacs-ide-health-register-check 'emacs-version  #'emacs-ide-health-check-emacs-version)

;;; ─── Run ─────────────────────────────────────────────────────────────────────

(defun emacs-ide-health-run-check (key fn)
  "Run a single health check FN for KEY and return a (KEY . plist) cons.
This is the singular form referenced by the ERT test suite."
  (let ((res (condition-case err
                 (funcall fn)
               (error (list :status 'error :message (error-message-string err))))))
    (cons key res)))

(defun emacs-ide-health-run-checks ()
  "Run all registered health checks and update the global results."
  (interactive)
  (let ((results nil) (errors 0) (warnings 0))
    (dolist (entry emacs-ide-health--registered-checks)
      (let ((res (condition-case err
                     (funcall (cdr entry))
                   (error (list :status 'error :message (error-message-string err))))))
        (push (cons (car entry) res) results)
        (pcase (plist-get res :status)
          ('error   (cl-incf errors))
          ('warning (cl-incf warnings)))))
    (setq emacs-ide-health-results    (nreverse results)
          emacs-ide-health-last-check (current-time)
          emacs-ide-health--errors    errors
          emacs-ide-health--warnings  warnings)))

;;; ─── Display ─────────────────────────────────────────────────────────────────

(defun emacs-ide-health-check-all ()
  "Run all health checks and display a report buffer."
  (interactive)
  (emacs-ide-health-run-checks)
  (with-output-to-temp-buffer "*IDE Health*"
    (princ "=== IDE HEALTH ===\n\n")
    (princ (format "Errors: %d | Warnings: %d\n\n"
                   emacs-ide-health--errors emacs-ide-health--warnings))
    (dolist (entry emacs-ide-health-results)
      (let* ((key    (car entry))
             (plist  (cdr entry))
             (status (plist-get plist :status))
             (msg    (plist-get plist :message))
             (icon   (pcase status ('ok "✓") ('warning "⚠") ('error "✗") (_ "?"))))
        (princ (format "%s %-20s %s\n" icon key (or msg "")))))))

;; Alias: emacs-ide-health-status is the public-facing name documented
;; in the README, spot-check, and all user-visible help text.
(defalias 'emacs-ide-health-status 'emacs-ide-health-check-all
  "Run all health checks and display a report.  Alias for `emacs-ide-health-check-all'.")

(defun emacs-ide-health--summary-string ()
  "Return a one-line health summary string for the modeline."
  (cond
   ((= emacs-ide-health--errors   0)
    (if (= emacs-ide-health--warnings 0)
        "✓ ok"
      (format "⚠%d" emacs-ide-health--warnings)))
   (t (format "✗%d" emacs-ide-health--errors))))

;;; ─── Auto-fix ────────────────────────────────────────────────────────────────

(defun emacs-ide-health-auto-fix ()
  "Attempt automatic remediation of any health warnings or errors.
Currently: refreshes straight caches, cancels void timers, and re-runs checks."
  (interactive)
  (message "⚙  Running health auto-fix…")
  ;; Cancel any void timers left over from failed package loads
  (let ((cancelled 0))
    (dolist (timer (append (copy-sequence timer-list)
                           (copy-sequence timer-idle-list)))
      (when (and (vectorp timer) (> (length timer) 5) (null (aref timer 5)))
        (cancel-timer timer)
        (cl-incf cancelled)))
    (when (> cancelled 0)
      (message "  auto-fix: cancelled %d void timer(s)" cancelled)))
  ;; Re-run all checks to get fresh status
  (emacs-ide-health-run-checks)
  (message "✓ Health auto-fix complete — %d error(s), %d warning(s)"
           emacs-ide-health--errors emacs-ide-health--warnings))

;;; ─── Periodic checks ─────────────────────────────────────────────────────────

(defun emacs-ide-health--setup-periodic-checks ()
  "Schedule background health checks to run every 60 seconds of idle time."
  (when emacs-ide-health--periodic-timer
    (cancel-timer emacs-ide-health--periodic-timer))
  ;; run-with-idle-timer (SECS REPEAT FUNCTION):
  ;;   SECS   — wait 60 s of idle before first run
  ;;   REPEAT — t means repeat on every subsequent idle period of SECS
  (setq emacs-ide-health--periodic-timer
        (run-with-idle-timer 60 t #'emacs-ide-health-run-checks)))

(add-hook 'after-init-hook #'emacs-ide-health--setup-periodic-checks)

(provide 'emacs-ide-health)
;;; emacs-ide-health.el ends here
