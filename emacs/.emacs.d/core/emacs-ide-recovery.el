;;; emacs-ide-recovery.el --- Enterprise Error Recovery System -*- lexical-binding: t -*-
;;; Commentary:
;;; Production-grade error recovery and safe mode
;;; Code:

;; ============================================================================
;; RECOVERY CONFIGURATION
;; ============================================================================
(defvar emacs-ide-recovery-log-file
  (expand-file-name "var/recovery.log" user-emacs-directory)
  "File to log recovery events.")

(defvar emacs-ide-recovery-max-log-size (* 10 1024 1024)
  "Maximum size of recovery log (10MB).")

(defvar emacs-ide-recovery-crash-threshold 3
  "Number of crashes before entering safe mode.")

(defvar emacs-ide-recovery-crash-count 0
  "Current crash count.")

(defvar emacs-ide-recovery-crash-history-file
  (expand-file-name "var/crash-history" user-emacs-directory)
  "File tracking crash history.")

(defvar emacs-ide-recovery-errors nil
  "List of errors encountered during session.")

;; ============================================================================
;; CRASH TRACKING
;; ============================================================================
(defun emacs-ide-recovery-load-crash-history ()
  "Load crash history from file."
  (when (file-exists-p emacs-ide-recovery-crash-history-file)
    (with-temp-buffer
      (insert-file-contents emacs-ide-recovery-crash-history-file)
      (setq emacs-ide-recovery-crash-count
            (string-to-number (buffer-string))))))

(defun emacs-ide-recovery-save-crash-history ()
  "Save crash history to file."
  (with-temp-file emacs-ide-recovery-crash-history-file
    (insert (number-to-string emacs-ide-recovery-crash-count))))

(defun emacs-ide-recovery-increment-crash-count ()
  "Increment crash counter."
  (setq emacs-ide-recovery-crash-count (1+ emacs-ide-recovery-crash-count))
  (emacs-ide-recovery-save-crash-history)
  (when (>= emacs-ide-recovery-crash-count
            emacs-ide-recovery-crash-threshold)
    (emacs-ide-recovery-enter-safe-mode)))

(defun emacs-ide-recovery-reset-crash-count ()
  "Reset crash counter after successful session."
  (interactive)
  (setq emacs-ide-recovery-crash-count 0)
  (emacs-ide-recovery-save-crash-history)
  (message "✓ Crash counter reset"))

;; Load crash history on startup
(emacs-ide-recovery-load-crash-history)

;; ============================================================================
;; ERROR LOGGING
;; ============================================================================
(defun emacs-ide-recovery-log (level message &rest args)
  "Log recovery event with LEVEL and MESSAGE with ARGS."
  (let ((formatted-message
         (format "[%s] [%s] %s\n"
                 (format-time-string "%Y-%m-%d %H:%M:%S")
                 (upcase (symbol-name level))
                 (apply #'format message args))))
    
    ;; Ensure directory exists
    (let ((log-dir (file-name-directory emacs-ide-recovery-log-file)))
      (unless (file-directory-p log-dir)
        (make-directory log-dir t)))
    
    ;; Rotate log if too large
    (when (and (file-exists-p emacs-ide-recovery-log-file)
               (> (nth 7 (file-attributes emacs-ide-recovery-log-file))
                  emacs-ide-recovery-max-log-size))
      (rename-file emacs-ide-recovery-log-file
                   (concat emacs-ide-recovery-log-file ".old")
                   t))
    
    ;; Append to log
    (with-temp-buffer
      (when (file-exists-p emacs-ide-recovery-log-file)
        (insert-file-contents emacs-ide-recovery-log-file))
      (goto-char (point-max))
      (insert formatted-message)
      (write-region (point-min) (point-max) emacs-ide-recovery-log-file))
    
    ;; Also display critical errors
    (when (eq level 'error)
      (display-warning 'emacs-ide formatted-message :error))))

(defun emacs-ide-recovery-log-error (context error)
  "Log ERROR in CONTEXT."
  (push (list :time (current-time)
              :context context
              :error error)
        emacs-ide-recovery-errors)
  (emacs-ide-recovery-log 'error "Error in %s: %s" context error))

;; ============================================================================
;; PACKAGE RECOVERY
;; ============================================================================
(defvar emacs-ide-recovery-package-fallbacks
  '((corfu . company)
    (lsp-mode . eglot)
    (vertico . ivy)
    (consult . helm)
    (magit . vc))
  "Fallback packages if primary fails.")

(defun emacs-ide-recovery-get-fallback (package)
  "Get fallback for PACKAGE."
  (cdr (assoc package emacs-ide-recovery-package-fallbacks)))

(defun emacs-ide-recovery-try-fallback (package error)
  "Try fallback for PACKAGE after ERROR."
  (if-let ((fallback (emacs-ide-recovery-get-fallback package)))
      (progn
        (emacs-ide-recovery-log 'warning
                               "Package %s failed, trying fallback %s"
                               package fallback)
        (condition-case fallback-error
            (progn
              (require fallback)
              (emacs-ide-recovery-log 'info
                                     "Successfully loaded fallback %s"
                                     fallback)
              t)
          (error
           (emacs-ide-recovery-log 'error
                                  "Fallback %s also failed: %s"
                                  fallback fallback-error)
           nil)))
    (emacs-ide-recovery-log 'error
                           "No fallback available for %s"
                           package)
    nil))

;; ============================================================================
;; SAFE MODE
;; ============================================================================
(defun emacs-ide-recovery-enter-safe-mode ()
  "Enter safe mode (minimal configuration)."
  (emacs-ide-recovery-log 'error
                         "Entering safe mode after %d crashes"
                         emacs-ide-recovery-crash-count)
  
  (setq emacs-ide-safe-mode t)
  
  (with-output-to-temp-buffer "*Safe Mode*"
    (princ "=== EMACS IDE SAFE MODE ===\n\n")
    (princ (format "Emacs IDE has crashed %d times recently.\n"
                   emacs-ide-recovery-crash-count))
    (princ "Booting in safe mode with minimal configuration.\n\n")
    (princ "To recover:\n")
    (princ "  1. Check *Recovery Log* for errors\n")
    (princ "  2. Fix configuration issues\n")
    (princ "  3. Run: M-x emacs-ide-recovery-reset-crash-count\n")
    (princ "  4. Restart Emacs\n\n")
    (princ "To force normal mode: emacs --eval \"(setq emacs-ide-safe-mode nil)\"\n")))

(defun emacs-ide-recovery-mode ()
  "Activate recovery mode with minimal features."
  (interactive)
  ;; Minimal UI
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  
  ;; Basic editing
  (electric-pair-mode 1)
  (show-paren-mode 1)
  (global-hl-line-mode 1)
  
  ;; Essential keybindings
  (global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal)
  (global-set-key (kbd "C-x C-f") 'find-file)
  (global-set-key (kbd "C-x C-s") 'save-buffer)
  (global-set-key (kbd "C-x b") 'switch-to-buffer)
  
  (message "🛡️  Recovery mode active. Limited functionality."))

;; ============================================================================
;; CONFIGURATION BACKUP
;; ============================================================================
(defun emacs-ide-recovery-backup-config ()
  "Create backup of current configuration."
  (interactive)
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (backup-dir (expand-file-name
                     (concat "var/backups/config-" timestamp)
                     user-emacs-directory)))
    (make-directory backup-dir t)
    
    (dolist (file '("init.el" "early-init.el"))
      (let ((source (expand-file-name file user-emacs-directory))
            (dest (expand-file-name file backup-dir)))
        (when (file-exists-p source)
          (copy-file source dest t))))
    
    ;; Backup core and modules
    (dolist (dir '("core" "modules"))
      (let ((source (expand-file-name dir user-emacs-directory))
            (dest (expand-file-name dir backup-dir)))
        (when (file-directory-p source)
          (copy-directory source dest t t))))
    
    (emacs-ide-recovery-log 'info "Configuration backed up to %s" backup-dir)
    (message "✓ Configuration backed up to %s" backup-dir)
    backup-dir))

(defun emacs-ide-recovery-restore-config (backup-dir)
  "Restore configuration from BACKUP-DIR."
  (interactive "DBackup directory: ")
  (when (y-or-n-p (format "Restore configuration from %s? " backup-dir))
    (dolist (file '("init.el" "early-init.el"))
      (let ((source (expand-file-name file backup-dir))
            (dest (expand-file-name file user-emacs-directory)))
        (when (file-exists-p source)
          (copy-file source dest t))))
    
    (dolist (dir '("core" "modules"))
      (let ((source (expand-file-name dir backup-dir))
            (dest (expand-file-name dir user-emacs-directory)))
        (when (file-directory-p source)
          (delete-directory dest t)
          (copy-directory source dest t t))))
    
    (emacs-ide-recovery-log 'info "Configuration restored from %s" backup-dir)
    (message "✓ Configuration restored. Restart Emacs.")))

;; ============================================================================
;; EMERGENCY COMMANDS
;; ============================================================================
(defun emacs-ide-recovery-disable-package (package)
  "Disable problematic PACKAGE."
  (interactive "sPackage to disable: ")
  (let ((disable-file (expand-file-name "var/disabled-packages"
                                       user-emacs-directory)))
    (with-temp-buffer
      (when (file-exists-p disable-file)
        (insert-file-contents disable-file))
      (goto-char (point-max))
      (insert (format "%s\n" package))
      (write-region (point-min) (point-max) disable-file))
    (emacs-ide-recovery-log 'warning "Disabled package: %s" package)
    (message "✓ Package %s disabled. Restart Emacs." package)))

(defun emacs-ide-recovery-view-log ()
  "View recovery log."
  (interactive)
  (if (file-exists-p emacs-ide-recovery-log-file)
      (view-file emacs-ide-recovery-log-file)
    (message "No recovery log found.")))

(defun emacs-ide-recovery-clear-log ()
  "Clear recovery log."
  (interactive)
  (when (y-or-n-p "Clear recovery log? ")
    (when (file-exists-p emacs-ide-recovery-log-file)
      (delete-file emacs-ide-recovery-log-file))
    (message "✓ Recovery log cleared")))

(defun emacs-ide-recovery-report ()
  "Generate recovery report."
  (interactive)
  (with-output-to-temp-buffer "*Recovery Report*"
    (princ "=== EMACS IDE RECOVERY REPORT ===\n\n")
    (princ (format "Crash Count: %d\n" emacs-ide-recovery-crash-count))
    (princ (format "Safe Mode: %s\n" (if emacs-ide-safe-mode "YES" "NO")))
    (princ (format "Errors This Session: %d\n\n" (length emacs-ide-recovery-errors)))
    
    (when emacs-ide-recovery-errors
      (princ "Recent Errors:\n")
      (dolist (err (reverse (last emacs-ide-recovery-errors 10)))
        (princ (format "\n[%s] %s\n"
                      (format-time-string "%H:%M:%S" (plist-get err :time))
                      (plist-get err :context)))
        (princ (format "  %s\n" (plist-get err :error)))))
    
    (princ "\n\nRecovery Actions:\n")
    (princ "  C-c r v  - View full recovery log\n")
    (princ "  C-c r b  - Backup current config\n")
    (princ "  C-c r r  - Restore from backup\n")
    (princ "  C-c r d  - Disable problematic package\n")
    (princ "  C-c r c  - Clear recovery log\n")
    (princ "  C-c r R  - Reset crash counter\n")))

;; ============================================================================
;; AUTOMATIC RECOVERY
;; ============================================================================
(defun emacs-ide-recovery-handle-error (data context)
  "Handle error DATA in CONTEXT."
  (let ((error-message (error-message-string data)))
    (emacs-ide-recovery-log-error context error-message)
    
    ;; Try to recover based on context
    (cond
     ((string-match-p "package" (symbol-name context))
      ;; Package error - try fallback
      (emacs-ide-recovery-try-fallback context data))
     
     ((string-match-p "lsp" (symbol-name context))
      ;; LSP error - continue without LSP
      (emacs-ide-recovery-log 'warning "Disabling LSP for this session")
      (setq lsp-enable-on-type-formatting nil))
     
     (t
      ;; Unknown error - log and continue
      (emacs-ide-recovery-log 'error "Unhandled error in %s" context)))))

;; ============================================================================
;; GRACEFUL DEGRADATION
;; ============================================================================
(defun emacs-ide-recovery-graceful-require (feature &optional fallback)
  "Require FEATURE with FALLBACK on failure."
  (condition-case err
      (require feature)
    (error
     (emacs-ide-recovery-log 'warning
                            "Failed to load %s: %s"
                            feature
                            (error-message-string err))
     (when fallback
       (emacs-ide-recovery-try-fallback feature err))
     nil)))

;; ============================================================================
;; SESSION MONITORING
;; ============================================================================
(defvar emacs-ide-recovery-session-start-time (current-time))

(defun emacs-ide-recovery-session-duration ()
  "Get current session duration in seconds."
  (float-time (time-subtract (current-time)
                             emacs-ide-recovery-session-start-time)))

;; Mark successful session after 5 minutes
(run-with-idle-timer
 300 nil
 (lambda ()
   (when (> (emacs-ide-recovery-session-duration) 300)
     (emacs-ide-recovery-reset-crash-count)
     (emacs-ide-recovery-log 'info "Session stable for 5+ minutes"))))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================
(define-prefix-command 'emacs-ide-recovery-map)
(global-set-key (kbd "C-c r") 'emacs-ide-recovery-map)

(define-key emacs-ide-recovery-map (kbd "r") 'emacs-ide-recovery-report)
(define-key emacs-ide-recovery-map (kbd "v") 'emacs-ide-recovery-view-log)
(define-key emacs-ide-recovery-map (kbd "c") 'emacs-ide-recovery-clear-log)
(define-key emacs-ide-recovery-map (kbd "b") 'emacs-ide-recovery-backup-config)
(define-key emacs-ide-recovery-map (kbd "R") 'emacs-ide-recovery-restore-config)
(define-key emacs-ide-recovery-map (kbd "d") 'emacs-ide-recovery-disable-package)
(define-key emacs-ide-recovery-map (kbd "C-r") 'emacs-ide-recovery-reset-crash-count)

(provide 'emacs-ide-recovery)
;;; emacs-ide-recovery.el ends here
