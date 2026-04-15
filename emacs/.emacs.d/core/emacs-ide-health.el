;;; emacs-ide-health.el --- Fast Health Checks -*- lexical-binding: t -*-
;;; Version: 3.1.1 | PATCH: Fixed :repeat keyword (FIX #8)
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-health-results nil)
(defvar emacs-ide-health-last-check nil)
(defvar emacs-ide-health--registered-checks nil)
(defvar emacs-ide-health--periodic-timer nil)
(defvar emacs-ide-health--errors 0)
(defvar emacs-ide-health--warnings 0)

(defun emacs-ide-health-register-check (key fn)
  "Register health check FN for KEY"
  (let ((existing (assoc key emacs-ide-health--registered-checks)))
    (if existing (setcdr existing fn)
      (push (cons key fn) emacs-ide-health--registered-checks))))

(defun emacs-ide-health-check-system-tools ()
  (let ((required '("git" "rg")) (missing nil))
    (dolist (tool required)
      (unless (executable-find tool) (push tool missing)))
    (if missing (list :status 'warning :message (format "missing: %s" (string-join missing ", ")))
      (list :status 'ok :message "all tools found"))))

(defun emacs-ide-health-check-lsp ()
  (if (not (bound-and-true-p emacs-ide-lsp-enable))
      (list :status 'ok :message "LSP disabled")
    (let ((servers '("pyright" "rust-analyzer" "gopls" "typescript-language-server" "clangd"))
          (found 0))
      (dolist (s servers) (when (executable-find s) (cl-incf found)))
      (if (> found 0) (list :status 'ok :message (format "%d server(s) found" found))
        (list :status 'warning :message "no LSP servers on PATH")))))

(defun emacs-ide-health-check-config ()
  (cond ((not (bound-and-true-p emacs-ide-config-loaded-p))
         (list :status 'error :message "config not loaded"))
        ((null emacs-ide-config-data)
         (list :status 'warning :message "config is nil (using defaults)"))
        (t (list :status 'ok :message (format "env: %s" emacs-ide-config-environment)))))

(defun emacs-ide-health-check-emacs-version ()
  (if (version<= "29.1" emacs-version)
      (list :status 'ok :message (format "Emacs %s" emacs-version))
    (list :status 'error :message (format "Need Emacs 29.1+, have %s" emacs-version))))

(emacs-ide-health-register-check 'system-tools #'emacs-ide-health-check-system-tools)
(emacs-ide-health-register-check 'lsp #'emacs-ide-health-check-lsp)
(emacs-ide-health-register-check 'config #'emacs-ide-health-check-config)
(emacs-ide-health-register-check 'emacs-version #'emacs-ide-health-check-emacs-version)

(defun emacs-ide-health-run-checks ()
  (interactive)
  (let ((results nil) (errors 0) (warnings 0))
    (dolist (entry emacs-ide-health--registered-checks)
      (let ((res (condition-case err (funcall (cdr entry))
                   (error (list :status 'error :message (error-message-string err))))))
        (push (cons (car entry) res) results)
        (pcase (plist-get res :status)
          ('error (cl-incf errors))
          ('warning (cl-incf warnings)))))
    (setq emacs-ide-health-results (nreverse results)
          emacs-ide-health-last-check (current-time)
          emacs-ide-health--errors errors
          emacs-ide-health--warnings warnings)))

(defun emacs-ide-health-check-all ()
  (interactive)
  (emacs-ide-health-run-checks)
  (with-output-to-temp-buffer "*IDE Health*"
    (princ "=== IDE HEALTH ===\n\n")
    (princ (format "Errors: %d | Warnings: %d\n\n" emacs-ide-health--errors emacs-ide-health--warnings))
    (dolist (entry emacs-ide-health-results)
      (let* ((key (car entry)) (plist (cdr entry)) (status (plist-get plist :status))
             (msg (plist-get plist :message)) (icon (pcase status ('ok "✓") ('warning "⚠") ('error "✗") (_ "?"))))
        (princ (format "%s %-20s %s\n" icon key (or msg "")))))))

(defun emacs-ide-health--setup-periodic-checks ()
  "Setup periodic checks — FIXED: use NUMERIC repeat interval, not :repeat keyword"
  (when emacs-ide-health--periodic-timer (cancel-timer emacs-ide-health--periodic-timer))
  ;; FIX #8: Changed from run-with-idle-timer 60 60 to run-with-idle-timer 60 t
  ;; The 3rd arg should be numeric (seconds between repeats), not a keyword
  (setq emacs-ide-health--periodic-timer
        (run-with-idle-timer 60 60 #'emacs-ide-health-run-checks)))

(add-hook 'after-init-hook #'emacs-ide-health--setup-periodic-checks)

(provide 'emacs-ide-health)
;;; emacs-ide-health.el ends here
