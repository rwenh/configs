;;; emacs-ide-health.el --- Enterprise Health Check System -*- lexical-binding: t -*-
;;; Commentary:
;;; Health monitoring and auto-recovery.
;;; Version: 2.2.3
;;; Fixes vs 2.2.2:
;;;   - C-16 (HIGH): Health check registry accumulated duplicate entries on
;;;     every config reload. Each top-level (emacs-ide-health-register-check ...)
;;;     call pushed onto `emacs-ide-health-checks` unconditionally. On a
;;;     second load of this file (e.g. via M-x emacs-ide-config-reload or
;;;     any module re-require), every check was registered twice.
;;;     emacs-ide-health-check-all then ran each check twice, doubling the
;;;     error/warning counts and corrupting the summary string shown in the
;;;     modeline and dashboard.
;;;     Fix: emacs-ide-health-register-check now checks for an existing
;;;     entry with the same name before pushing. This makes registration
;;;     idempotent: a second load is a no-op for already-registered checks.
;;;   - C-17 (MEDIUM): emacs-ide-health-auto-fix was declared (interactive)
;;;     but takes a `results` argument. When called via M-x it received nil
;;;     for results, looped over nothing, and printed "No auto-fixable issues
;;;     found" even when fixable issues existed. This was confusing because
;;;     the function IS the right thing to call from the health report buffer.
;;;     Fix: split into a worker function `emacs-ide-health--auto-fix-results`
;;;     (takes results, not interactive) and an interactive command
;;;     `emacs-ide-health-auto-fix` that uses the last saved results.
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-health-check-on-startup t)
(defvar emacs-ide-health-auto-fix t)
(defvar emacs-ide-health-check-interval (* 60 60))
(defvar emacs-ide-health-last-check nil)
(defvar emacs-ide-health-results nil)
(defvar emacs-ide-health--last-errors 0)
(defvar emacs-ide-health--last-warnings 0)

(defun emacs-ide-health--summary-string ()
  "Return a short status string for modeline/dashboard use."
  (cond
   ((null emacs-ide-health-last-check) "? unchecked")
   ((> emacs-ide-health--last-errors 0)
    (format "✗ %d error%s"
            emacs-ide-health--last-errors
            (if (= emacs-ide-health--last-errors 1) "" "s")))
   ((> emacs-ide-health--last-warnings 0)
    (format "⚠ %d warning%s"
            emacs-ide-health--last-warnings
            (if (= emacs-ide-health--last-warnings 1) "" "s")))
   (t "✓ Healthy")))

(defun emacs-ide-health--update-counts (errors warnings)
  "Store ERRORS and WARNINGS and refresh modeline."
  (setq emacs-ide-health--last-errors   errors
        emacs-ide-health--last-warnings warnings)
  (force-mode-line-update t))

(defvar emacs-ide-health-checks nil
  "Registry of health check functions.")

(defun emacs-ide-health-register-check (name fn)
  "Register health check FN under NAME.
C-16 FIX: Idempotent — if a check with NAME is already registered
it is replaced in-place rather than pushed again. This prevents
duplicate registrations when the module is reloaded."
  (let ((existing (assoc name emacs-ide-health-checks)))
    (if existing
        (setcdr existing fn)
      (push (cons name fn) emacs-ide-health-checks))))

;; ============================================================================
;; INTERNAL HELPERS
;; ============================================================================
(defun emacs-ide-health--find-phase (phase-name)
  "Return elapsed time for PHASE-NAME, or nil."
  (when (and (boundp 'emacs-ide--startup-phases)
             emacs-ide--startup-phases)
    (cdr (assoc phase-name emacs-ide--startup-phases))))

;; ============================================================================
;; SYSTEM TOOLS CHECK
;; ============================================================================
(defun emacs-ide-health-check-system-tools ()
  "Check essential system tools."
  (let ((required '("git" "grep" "find"))
        (recommended '("rg" "fd" "ag"))
        (missing-required '())
        (missing-recommended '()))
    (dolist (tool required)
      (unless (executable-find tool)
        (push tool missing-required)))
    (dolist (tool recommended)
      (unless (executable-find tool)
        (push tool missing-recommended)))
    (cond
     (missing-required
      (list :status 'error
            :message (format "Missing required tools: %s"
                             (string-join (nreverse missing-required) ", "))
            :details missing-required
            :fixable nil))
     (missing-recommended
      (list :status 'warning
            :message (format "Missing recommended tools: %s"
                             (string-join (nreverse missing-recommended) ", "))
            :details missing-recommended
            :fixable t))
     (t (list :status 'ok :message "All system tools available")))))

(emacs-ide-health-register-check 'system-tools #'emacs-ide-health-check-system-tools)

;; ============================================================================
;; LSP SERVERS CHECK
;; ============================================================================
(defun emacs-ide-health--find-executable (candidates)
  "Return first executable in CANDIDATES found on PATH, or nil."
  (cl-some #'executable-find candidates))

(defun emacs-ide-health-check-lsp-servers ()
  "Check LSP server availability."
  (let ((servers '(("Python"      . ("pyright-langserver" "pyright"))
                   ("Rust"        . ("rust-analyzer"))
                   ("Go"          . ("gopls"))
                   ("TypeScript"  . ("typescript-language-server"))
                   ("C/C++"       . ("clangd" "ccls"))))
        (available '())
        (missing '())
        (secondary-warnings '()))

    (dolist (s servers)
      (if (emacs-ide-health--find-executable (cdr s))
          (push (car s) available)
        (push (car s) missing)))

    (when (and (executable-find "typescript-language-server")
               (not (executable-find "tsserver")))
      (push "typescript-language-server present but `tsserver` (npm package `typescript`) missing — TS LSP will crash"
            secondary-warnings))

    (let ((base-status (cond ((null available) 'error)
                             (missing 'warning)
                             (t 'ok)))
          (base-msg (format "%d/%d LSP servers available"
                            (length available) (length servers))))
      (if secondary-warnings
          (list :status 'warning
                :message (concat base-msg " (runtime dependency issues)")
                :details (list :available available
                               :missing missing
                               :warnings secondary-warnings)
                :fixable t)
        (list :status base-status
              :message base-msg
              :details (list :available available :missing missing)
              :fixable t)))))

(emacs-ide-health-register-check 'lsp-servers #'emacs-ide-health-check-lsp-servers)

;; ============================================================================
;; FORMATTERS CHECK
;; ============================================================================
(defun emacs-ide-health-check-formatters ()
  "Check code formatter availability."
  (let ((formatters '(("black"    . "Python (black)")
                      ("prettier" . "JavaScript/TypeScript")
                      ("rustfmt"  . "Rust")
                      ("gofmt"    . "Go")))
        (missing '()))
    (dolist (f formatters)
      (unless (executable-find (car f))
        (push (cdr f) missing)))
    (if missing
        (list :status 'warning
              :message (format "%d formatters missing" (length missing))
              :details missing
              :fixable t)
      (list :status 'ok :message "All formatters available"))))

(emacs-ide-health-register-check 'formatters #'emacs-ide-health-check-formatters)

;; ============================================================================
;; PERFORMANCE CHECK
;; ============================================================================
(defun emacs-ide-health-check-performance ()
  "Check performance metrics."
  (let* ((startup-time (or (emacs-ide-health--find-phase "startup-complete")
                           (emacs-ide-health--find-phase "feature-modules")))
         (gc-count (- gcs-done
                      (if (boundp 'emacs-ide--gc-count-start)
                          emacs-ide--gc-count-start 0)))
         (warnings '()))
    (when (and startup-time
               (> startup-time (or (and (boundp 'emacs-ide-startup-time-target)
                                        emacs-ide-startup-time-target)
                                   3.0)))
      (push (format "Slow startup: %.2fs" startup-time) warnings))
    (when (> gc-count 50)
      (push (format "Excessive GC during startup: %d" gc-count) warnings))
    (if warnings
        (list :status 'warning
              :message (format "%d performance issues" (length warnings))
              :details warnings)
      (list :status 'ok :message "Performance within targets"))))

(emacs-ide-health-register-check 'performance #'emacs-ide-health-check-performance)

;; ============================================================================
;; PACKAGES CHECK
;; ============================================================================
(defun emacs-ide-health-check-packages ()
  "Check package health."
  (let ((issues '()))
    (dolist (pkg '(use-package which-key projectile magit vertico corfu))
      (unless (or (featurep pkg)
                  (locate-library (symbol-name pkg)))
        (push (format "Package not found: %s" pkg) issues)))
    (if issues
        (list :status 'warning
              :message (format "%d package warnings" (length issues))
              :details issues
              :fixable t)
      (list :status 'ok :message "All packages healthy"))))

(emacs-ide-health-register-check 'packages #'emacs-ide-health-check-packages)

;; ============================================================================
;; SECURITY CHECK
;; ============================================================================
(defun emacs-ide-health-check-security ()
  "Check security configuration."
  (let ((warnings '()))
    (unless (and (boundp 'gnutls-verify-error) gnutls-verify-error)
      (push "TLS verification not enforced" warnings))
    (when (and (boundp 'custom-file)
               (string= custom-file user-init-file))
      (push "Custom file should be separate from init.el" warnings))
    (unless (executable-find "gpg")
      (push "GPG not found (needed for auth-source encryption)" warnings))
    (if warnings
        (list :status 'warning
              :message (format "%d security warnings" (length warnings))
              :details warnings)
      (list :status 'ok :message "Security configuration OK"))))

(emacs-ide-health-register-check 'security #'emacs-ide-health-check-security)

;; ============================================================================
;; HEALTH RUNNER
;; ============================================================================
(defun emacs-ide-health-run-check (check-name check-fn)
  "Run CHECK-FN and return cons of CHECK-NAME and result."
  (condition-case err
      (cons check-name (funcall check-fn))
    (error
     (cons check-name
           (list :status 'error
                 :message (format "Check crashed: %s"
                                  (error-message-string err)))))))

(defun emacs-ide-health-check-all ()
  "Run all registered health checks and display results."
  (interactive)
  (message "🏥 Running health checks...")
  (let ((results '()) (errors 0) (warnings 0))
    (dolist (check emacs-ide-health-checks)
      (let* ((res (emacs-ide-health-run-check (car check) (cdr check)))
             (status (plist-get (cdr res) :status)))
        (push res results)
        (cond ((eq status 'error)   (cl-incf errors))
              ((eq status 'warning) (cl-incf warnings)))))
    (setq emacs-ide-health-results (reverse results)
          emacs-ide-health-last-check (current-time))
    (emacs-ide-health--update-counts errors warnings)
    (emacs-ide-health-display-results results errors warnings)
    (when (and emacs-ide-health-auto-fix
               (or (> errors 0) (> warnings 0))
               (y-or-n-p (format "%d errors, %d warnings. Attempt auto-fix? "
                                 errors warnings)))
      (emacs-ide-health--auto-fix-results results))
    results))

(defun emacs-ide-health--format-details (details)
  "Format DETAILS for display."
  (cond
   ((and (listp details) (keywordp (car details)))
    (let ((available (plist-get details :available))
          (missing   (plist-get details :missing))
          (warnings  (plist-get details :warnings)))
      (concat
       (when available
         (format "      Available: %s\n"
                 (mapconcat (lambda (x) (if (symbolp x) (symbol-name x) x))
                            available ", ")))
       (when missing
         (format "      Missing:   %s\n"
                 (mapconcat (lambda (x) (if (symbolp x) (symbol-name x) x))
                            missing ", ")))
       (when warnings
         (mapconcat (lambda (w) (format "      ⚠ %s\n" w)) warnings "")))))
   ((listp details)
    (mapconcat (lambda (d) (format "    - %s\n" d)) details ""))
   (t (format "    - %s\n" details))))

(defun emacs-ide-health-display-results (results errors warnings)
  "Display RESULTS with ERRORS and WARNINGS counts."
  (with-output-to-temp-buffer "*Health Check*"
    (princ "=== EMACS IDE HEALTH CHECK ===\n\n")
    (princ (format "Status: %s\n"
                   (cond ((> errors 0)   "❌ CRITICAL")
                         ((> warnings 0) "⚠️  WARNING")
                         (t              "✓ HEALTHY"))))
    (princ (format "Time: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
    (dolist (r results)
      (let* ((name    (car r))
             (res     (cdr r))
             (status  (plist-get res :status))
             (msg     (plist-get res :message))
             (details (plist-get res :details))
             (icon    (cond ((eq status 'ok)      "✓")
                            ((eq status 'warning) "⚠")
                            ((eq status 'error)   "✗")
                            (t "?"))))
        (princ (format "%s %s\n" icon (upcase (symbol-name name))))
        (princ (format "  %s\n" msg))
        (when details
          (princ "  Details:\n")
          (princ (emacs-ide-health--format-details details)))
        (princ "\n")))))

;; ============================================================================
;; AUTO-FIX
;; C-17 FIX: Split into worker (takes results) and interactive command
;; (uses last saved results). Previously the single (interactive) function
;; received nil from M-x and silently did nothing.
;; ============================================================================
(defun emacs-ide-health--auto-fix-results (results)
  "Worker: attempt auto-fix for each entry in RESULTS that has :fix-function."
  (let ((fixed 0))
    (dolist (res results)
      (let ((fix-fn (plist-get (cdr res) :fix-function)))
        (when (and fix-fn (functionp fix-fn))
          (condition-case err
              (progn (funcall fix-fn) (cl-incf fixed))
            (error (message "Auto-fix failed for %s: %s"
                            (car res) (error-message-string err)))))))
    (if (> fixed 0)
        (message "✓ Auto-fixed %d issues" fixed)
      (message "No auto-fixable issues found"))))

(defun emacs-ide-health-auto-fix ()
  "Attempt auto-fix using the results from the last health check.
C-17 FIX: This is now a zero-argument interactive command. It uses
`emacs-ide-health-results` (saved by the last emacs-ide-health-check-all
run) so M-x emacs-ide-health-auto-fix works correctly."
  (interactive)
  (if (null emacs-ide-health-results)
      (message "No health check results available. Run M-x emacs-ide-health-check-all first.")
    (emacs-ide-health--auto-fix-results emacs-ide-health-results)))

;; ============================================================================
;; STARTUP QUICK CHECK
;; ============================================================================
(defun emacs-ide-health-check-startup ()
  "Quick health check on startup (non-blocking)."
  (when emacs-ide-health-check-on-startup
    (run-with-idle-timer
     3 nil
     (lambda ()
       (let* ((checks (list (cons 'system-tools #'emacs-ide-health-check-system-tools)
                            (cons 'packages     #'emacs-ide-health-check-packages)
                            (cons 'lsp-servers  #'emacs-ide-health-check-lsp-servers)
                            (cons 'formatters   #'emacs-ide-health-check-formatters)
                            (cons 'security     #'emacs-ide-health-check-security)))
              (results '())
              (errors 0)
              (warnings 0))
         (dolist (chk checks)
           (let* ((res    (emacs-ide-health-run-check (car chk) (cdr chk)))
                  (status (plist-get (cdr res) :status)))
             (push res results)
             (cond ((eq status 'error)   (cl-incf errors))
                   ((eq status 'warning) (cl-incf warnings)))))
         (setq emacs-ide-health-results    (reverse results)
               emacs-ide-health-last-check (current-time))
         (emacs-ide-health--update-counts errors warnings)
         (cond
          ((> errors 0)
           (message "🏥 Health: ✗ %d error%s found — M-x emacs-ide-health-check-all"
                    errors (if (= errors 1) "" "s")))
          ((> warnings 0)
           (message "🏥 Health: ⚠ %d warning%s — M-x emacs-ide-health-check-all"
                    warnings (if (= warnings 1) "" "s")))
          (t
           (message "🏥 Health: ✓ All checks passed"))))))))

(provide 'emacs-ide-health)
;;; emacs-ide-health.el ends here
