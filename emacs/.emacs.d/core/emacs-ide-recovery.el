;;; emacs-ide-recovery.el --- Production Recovery System -*- lexical-binding: t -*-
;;; Version: 3.1.0 | Compact, safe-mode aware
;;; Code:

(defvar emacs-ide-safe-mode nil)
(defvar emacs-ide-recovery-log-file (expand-file-name "var/recovery.log" user-emacs-directory))
(defvar emacs-ide-recovery-max-log-size (* 10 1024 1024))
(defvar emacs-ide-recovery-crash-count 0)
(defvar emacs-ide-recovery-crash-history-file (expand-file-name "var/crash-history" user-emacs-directory))

(defun emacs-ide-recovery-log (level fmt &rest args)
  (let ((msg (format "[%s] [%s] %s\n" (format-time-string "%Y-%m-%d %H:%M:%S") (upcase (symbol-name level)) (apply #'format fmt args))))
    (make-directory (file-name-directory emacs-ide-recovery-log-file) t)
    (when (and (file-exists-p emacs-ide-recovery-log-file)
               (> (nth 7 (file-attributes emacs-ide-recovery-log-file) 0) emacs-ide-recovery-max-log-size))
      (rename-file emacs-ide-recovery-log-file (concat emacs-ide-recovery-log-file "." (format-time-string "%Y%m%d-%H%M%S")) t))
    (write-region msg nil emacs-ide-recovery-log-file 'append 'quiet)))

(defun emacs-ide-recovery-load-crash-history ()
  (when (file-exists-p emacs-ide-recovery-crash-history-file)
    (condition-case nil
        (let ((n (string-to-number (string-trim (with-temp-buffer (insert-file-contents emacs-ide-recovery-crash-history-file) (buffer-string))))))
          (setq emacs-ide-recovery-crash-count (max 0 n)))
      (error nil))))

(defun emacs-ide-recovery-save-crash-history ()
  (with-temp-file emacs-ide-recovery-crash-history-file
    (insert (number-to-string emacs-ide-recovery-crash-count))))

(emacs-ide-recovery-load-crash-history)

(defun emacs-ide-recovery-view-log ()
  (interactive)
  (if (file-exists-p emacs-ide-recovery-log-file)
      (view-file emacs-ide-recovery-log-file)
    (message "No recovery log")))

(provide 'emacs-ide-recovery)
;;; emacs-ide-recovery.el ends here
