;;; emacs-ide-health.el --- Enterprise Health Check System -*- lexical-binding: t -*-
;;; Commentary:
;;; Health monitoring and auto-recovery.
;;; FIX: dolist variable shadowing bug in emacs-ide-health-check-system-tools
;;;      (was using `t' as loop var, masking required-tools check entirely)
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-health-check-on-startup t
  "Run health check on startup.")

(defvar emacs-ide-health-auto-fix t
  "Automatically fix issues when possible.")

(defvar emacs-ide-health-check-interval (* 60 60)
  "Interval for periodic health checks (seconds).")

(defvar emacs-ide-health-last-check nil
  "Timestamp of last health check.")

(defvar emacs-ide-health-results nil
  "Results from last health check.")

(defvar emacs-ide-health-checks nil
  "Registry of health check functions; populated at load time.")

(defun emacs-ide-health-register-check (name fn)
  "Register health check FN under NAME."
  (push (cons name fn) emacs-ide-health-checks))

;; ============================================================================
;; SYSTEM TOOLS CHECK
;; BUG FIX: was (dolist (t required) ...) which shadowed the symbol `t'
;;           causing (executable-find t) to always return nil and
;;           missing-required to always be empty even when tools are absent.
;; ============================================================================
(defun emacs-ide-health-check-system-tools ()
  "Check essential system tools and return a plist result."
  (let ((required '("git" "grep" "find"))
        (recommended '("rg" "fd" "ag"))
        (missing-required '())
        (missing-recommended '()))
    ;; FIX: use 'tool' not 't' as loop variable
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
(defun emacs-ide-health-check-lsp-servers ()
  "Check for LSP servers available for configured languages."
  (let ((servers '(("pyright" . "Python")
                   ("rust-analyzer" . "Rust")
                   ("gopls" . "Go")
                   ("typescript-language-server" . "TypeScript")
                   ("clangd" . "C/C++")))
        (available '())
        (missing '()))
    (dolist (s servers)
      (if (executable-find (car s))
          (push (car s) available)
        (push (car s) missing)))
    (if available
        (list :status (if missing 'warning 'ok)
              :message (format "%d/%d LSP servers available"
                               (length available) (length servers))
              :details (list :available available :missing missing)
              :fixable t)
      (list :status 'error
            :message "No LSP servers found"
            :details missing
            :fixable t))))

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
  (let* ((startup-time (if (and (boundp 'emacs-ide--startup-phases)
                                emacs-ide--startup-phases)
                           (cdr (car emacs-ide--startup-phases))
                         nil))
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
      (unless (or (and (fboundp 'straight--installed-p)
                       (condition-case nil (straight--installed-p pkg) (error nil)))
                  (featurep pkg))
        (push (format "Package possibly missing: %s" pkg) issues)))
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
    (emacs-ide-health-display-results results errors warnings)
    (when (and emacs-ide-health-auto-fix
               (or (> errors 0) (> warnings 0))
               (y-or-n-p (format "%d errors, %d warnings. Attempt auto-fix? "
                                 errors warnings)))
      (emacs-ide-health-auto-fix results))
    results))

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
      (let* ((name   (car r))
             (res    (cdr r))
             (status (plist-get res :status))
             (msg    (plist-get res :message))
             (details (plist-get res :details))
             (icon   (cond ((eq status 'ok)      "✓")
                           ((eq status 'warning) "⚠")
                           ((eq status 'error)   "✗")
                           (t "?"))))
        (princ (format "%s %s\n" icon (upcase (symbol-name name))))
        (princ (format "  %s\n" msg))
        (when details
          (princ "  Details:\n")
          (dolist (d (if (listp details) details (list details)))
            (unless (keywordp d)
              (princ (format "    - %s\n" d)))))
        (princ "\n")))))

(defun emacs-ide-health-auto-fix (results)
  "Attempt auto-fix for RESULTS when possible."
  (interactive)
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

;; ============================================================================
;; STARTUP QUICK CHECK
;; ============================================================================
(defun emacs-ide-health-check-startup ()
  "Quick health check on startup (non-blocking)."
  (when emacs-ide-health-check-on-startup
    (run-with-idle-timer
     2 nil
     (lambda ()
       (let ((checks (list (cons 'system-tools #'emacs-ide-health-check-system-tools)
                           (cons 'packages     #'emacs-ide-health-check-packages))))
         (dolist (chk checks)
           (let* ((res    (emacs-ide-health-run-check (car chk) (cdr chk)))
                  (status (plist-get (cdr res) :status)))
             (when (eq status 'error)
               (warn "Health check failed: %s - %s"
                     (car res)
                     (plist-get (cdr res) :message))))))))))

(provide 'emacs-ide-health)
;;; emacs-ide-health.el ends here
