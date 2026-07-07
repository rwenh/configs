;;; emacs-ide-recovery.el --- Production Recovery System -*- lexical-binding: t -*-
;;; Version: 3.2.1
;;;
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-safe-mode nil
  "Non-nil when Emacs IDE is running in safe / minimal mode.")

(defvar emacs-ide-recovery-log-file
  (expand-file-name "var/recovery.log" user-emacs-directory))

(defvar emacs-ide-recovery-max-log-size (* 10 1024 1024)
  "Maximum size of the recovery log before rotation (bytes).")

(defvar emacs-ide-recovery-crash-count 0
  "Number of abnormal exits recorded in the crash history file.")

(defvar emacs-ide-recovery-crash-history-file
  (expand-file-name "var/crash-history" user-emacs-directory))

(defvar emacs-ide-recovery--disabled-packages nil
  "List of packages temporarily disabled by the recovery system.")

;;; ─── Logging ─────────────────────────────────────────────────────────────────

(defun emacs-ide-recovery-log (level fmt &rest args)
  "Write a log entry at LEVEL (a symbol) with format FMT and ARGS."
  (let ((msg (format "[%s] [%s] %s\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S")
                     (upcase (symbol-name level))
                     (apply #'format fmt args))))
    (make-directory (file-name-directory emacs-ide-recovery-log-file) t)
    (when (and (file-exists-p emacs-ide-recovery-log-file)
               (> (or (nth 7 (file-attributes emacs-ide-recovery-log-file)) 0)
                  emacs-ide-recovery-max-log-size))
      (rename-file emacs-ide-recovery-log-file
                   (concat emacs-ide-recovery-log-file
                           "." (format-time-string "%Y%m%d-%H%M%S"))
                   t))
    (write-region msg nil emacs-ide-recovery-log-file 'append 'quiet)))

;;; ─── Crash history ───────────────────────────────────────────────────────────

(defun emacs-ide-recovery-load-crash-history ()
  "Load crash count from the history file."
  (when (file-exists-p emacs-ide-recovery-crash-history-file)
    (condition-case nil
        (let ((n (string-to-number
                  (string-trim
                   (with-temp-buffer
                     (insert-file-contents emacs-ide-recovery-crash-history-file)
                     (buffer-string))))))
          (setq emacs-ide-recovery-crash-count (max 0 n)))
      (error nil))))

(defun emacs-ide-recovery-save-crash-history ()
  "Persist the current crash count to disk."
  (condition-case nil
      (with-temp-file emacs-ide-recovery-crash-history-file
        (insert (number-to-string emacs-ide-recovery-crash-count)))
    (error nil)))

(defun emacs-ide-recovery-reset-crash-count ()
  "Reset the crash counter to zero and persist."
  (interactive)
  (setq emacs-ide-recovery-crash-count 0)
  (emacs-ide-recovery-save-crash-history)
  (message "✓ Crash count reset to 0"))

;; Load on startup
(emacs-ide-recovery-load-crash-history)

(cl-incf emacs-ide-recovery-crash-count)
(emacs-ide-recovery-save-crash-history)

;; Reset counter on clean exit so repeated normal startups don't accumulate.
(add-hook 'kill-emacs-hook
          (lambda ()
            (setq emacs-ide-recovery-crash-count 0)
            (emacs-ide-recovery-save-crash-history)))

;;; ─── Safe / recovery mode ────────────────────────────────────────────────────

(defun emacs-ide-recovery-mode ()
  "Enter a minimal safe configuration, disabling non-essential features."
  (interactive)
  (setq emacs-ide-safe-mode t)
  (message "🛡  Recovery mode active — minimal configuration"))

;;; ─── Package disable ─────────────────────────────────────────────────────────

(defun emacs-ide-recovery-disable-package (package-name)
  "Mark PACKAGE-NAME as disabled for this session.
The package will not be loaded even if it appears in the feature modules list."
  (interactive "sPackage to disable: ")
  (let ((sym (if (stringp package-name) (intern package-name) package-name)))
    (cl-pushnew sym emacs-ide-recovery--disabled-packages)
    (emacs-ide-recovery-log 'warn "Disabled package: %s" sym)
    (message "⚠ Package %s disabled for this session" sym)))

(defun emacs-ide-recovery-package-disabled-p (package-name)
  "Return non-nil if PACKAGE-NAME has been disabled by the recovery system."
  (memq (if (stringp package-name) (intern package-name) package-name)
        emacs-ide-recovery--disabled-packages))

;;; ─── Config backup / restore ─────────────────────────────────────────────────

(defun emacs-ide-recovery-backup-config ()
  "Create a timestamped backup of config.yml in var/backups/."
  (interactive)
  (let* ((src     (expand-file-name "config.yml" user-emacs-directory))
         (bak-dir (expand-file-name "var/backups/" user-emacs-directory))
         (dest    (expand-file-name
                   (format "config-%s.yml" (format-time-string "%Y%m%d-%H%M%S"))
                   bak-dir)))
    (unless (file-directory-p bak-dir)
      (make-directory bak-dir t))
    (if (file-exists-p src)
        (progn
          (copy-file src dest t)
          (message "✓ Config backed up to %s" dest))
      (message "⚠ config.yml not found — nothing to back up"))))

(defun emacs-ide-recovery-restore-config ()
  "Interactively restore config.yml from a backup in var/backups/."
  (interactive)
  (let* ((bak-dir (expand-file-name "var/backups/" user-emacs-directory))
         (files   (when (file-directory-p bak-dir)
                    (directory-files bak-dir t "\\.yml$"))))
    (if (null files)
        (message "No config backups found in %s" bak-dir)
      (let* ((chosen (completing-read "Restore from: " (mapcar #'file-name-nondirectory files) nil t))
             (src    (expand-file-name chosen bak-dir))
             (dest   (expand-file-name "config.yml" user-emacs-directory)))
        (when (y-or-n-p (format "Restore %s → config.yml? " chosen))
          (copy-file src dest t)
          (message "✓ Restored %s → config.yml (reload with M-x emacs-ide-config-reload)" chosen))))))

;;; ─── View log ────────────────────────────────────────────────────────────────

(defun emacs-ide-recovery-view-log ()
  "Open the recovery log in a read-only view."
  (interactive)
  (if (file-exists-p emacs-ide-recovery-log-file)
      (view-file emacs-ide-recovery-log-file)
    (message "No recovery log found at %s" emacs-ide-recovery-log-file)))

;;; ─── Session timer ───────────────────────────────────────────────────────────

(defvar emacs-ide-recovery--session-timer nil
  "Periodic idle timer that flushes crash-history state to disk.")

(defun emacs-ide-recovery--start-session-timer ()
  "Start the periodic session-health flush timer."
  (when emacs-ide-recovery--session-timer
    (cancel-timer emacs-ide-recovery--session-timer))
  (setq emacs-ide-recovery--session-timer
        (run-with-idle-timer 300 t #'emacs-ide-recovery-save-crash-history)))

(add-hook 'after-init-hook #'emacs-ide-recovery--start-session-timer)

;;; ─── Report ──────────────────────────────────────────────────────────────────

(defun emacs-ide-recovery-report ()
  "Display a summary of the recovery system state."
  (interactive)
  (with-output-to-temp-buffer "*Recovery Report*"
    (princ "=== RECOVERY SYSTEM REPORT ===\n\n")
    (princ (format "Safe mode:       %s\n"
                   (if emacs-ide-safe-mode "ACTIVE" "off")))
    (princ (format "Crash count:     %d\n" emacs-ide-recovery-crash-count))
    (princ (format "Log file:        %s\n" emacs-ide-recovery-log-file))
    (princ (format "Log exists:      %s\n"
                   (if (file-exists-p emacs-ide-recovery-log-file) "yes" "no")))
    (princ (format "Disabled pkgs:   %s\n\n"
                   (if emacs-ide-recovery--disabled-packages
                       (mapconcat #'symbol-name emacs-ide-recovery--disabled-packages ", ")
                     "none")))
    (princ "Commands:\n")
    (princ "  M-x emacs-ide-recovery-view-log          — open log\n")
    (princ "  M-x emacs-ide-recovery-backup-config     — backup config.yml\n")
    (princ "  M-x emacs-ide-recovery-restore-config    — restore from backup\n")
    (princ "  M-x emacs-ide-recovery-disable-package   — disable a package\n")
    (princ "  M-x emacs-ide-recovery-reset-crash-count — reset counter\n")))

(provide 'emacs-ide-recovery)
;;; emacs-ide-recovery.el ends here
