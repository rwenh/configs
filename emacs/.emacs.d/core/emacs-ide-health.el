;;; emacs-ide-health.el --- Health Monitoring & Diagnostics -*- lexical-binding: t -*-
;;; Commentary:
;;; IDE health status checking, performance monitoring, and diagnostic reporting.
;;; Version: 3.0.4
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-health-results nil)
(defvar emacs-ide-health-last-check nil)
(defvar emacs-ide-health--last-errors 0)
(defvar emacs-ide-health--last-warnings 0)
(defvar emacs-ide-health--check-interval 60)
(defvar emacs-ide-health--periodic-timer nil)
(defvar emacs-ide-health--checks-run 0)
(defvar emacs-ide-health--startup-check-done nil)

(defvar emacs-ide-health--registered-checks nil)

(defun emacs-ide-health-register-check (key fn)
  "Register a health check FN under KEY."
  (let ((existing (assoc key emacs-ide-health--registered-checks)))
    (if existing
        (setcdr existing fn)
      (push (cons key fn) emacs-ide-health--registered-checks))))

(defun emacs-ide-health-run-check (key fn)
  "Run a single health check FN under KEY.
Returns a cons (KEY . result-plist)."
  (condition-case err
      (let ((result (funcall fn)))
        (cons key (if (plistp result) result
                    (list :status 'error :message (format "bad result: %S" result)))))
    (error
     (cons key (list :status 'error
                     :message (error-message-string err))))))

(defun emacs-ide-health-check-system-tools ()
  "Check for required system tools on PATH."
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

(emacs-ide-health-register-check 'system-tools  #'emacs-ide-health-check-system-tools)
(emacs-ide-health-register-check 'lsp           #'emacs-ide-health-check-lsp)
(emacs-ide-health-register-check 'config        #'emacs-ide-health-check-config)
(emacs-ide-health-register-check 'emacs-version #'emacs-ide-health-check-emacs-version)

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

(defun emacs-ide-health--summary-string ()
  "Return a short one-line health summary string for modeline display."
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

(defun emacs-ide-health-auto-fix ()
  "Attempt basic auto-remediation for known fixable health issues."
  (interactive)
  (let ((fixed 0))
    (dolist (entry emacs-ide-health-results)
      (let* ((key (car entry))
             (plist (cdr entry))
             (status (plist-get plist :status))
             (msg    (plist-get plist :message)))
        (when (eq status 'warning)
          (cond
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

(defun emacs-ide-health-check-startup ()
  "Run health checks once at startup."
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

(defun emacs-ide-health--setup-periodic-checks ()
  "Setup periodic health check timer."
  (when emacs-ide-health--periodic-timer
    (cancel-timer emacs-ide-health--periodic-timer))
  (setq emacs-ide-health--periodic-timer
        (run-with-idle-timer emacs-ide-health--check-interval
                             emacs-ide-health--check-interval
                             #'emacs-ide-health-run-checks)))

(add-hook 'after-init-hook #'emacs-ide-health--setup-periodic-checks)

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
  "Run all health checks and display results."
  (interactive)
  (emacs-ide-health-run-checks)
  (emacs-ide-health-status))

(provide 'emacs-ide-health)
;;; emacs-ide-health.el ends here
