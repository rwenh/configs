;;; emacs-ide-telemetry.el --- Usage Analytics (Local Only) -*- lexical-binding: t -*-
;;; Commentary:
;;; Privacy-respecting local usage analytics
;;; Code:

(defvar emacs-ide-telemetry-enabled t
  "Enable telemetry collection (local only).")

(defvar emacs-ide-telemetry-command-counts (make-hash-table :test 'equal)
  "Track command usage.")

(defvar emacs-ide-telemetry-session-start (current-time)
  "Session start time.")

(defun emacs-ide-telemetry-track-command ()
  "Track command execution."
  (when (and emacs-ide-telemetry-enabled this-command)
    (let ((count (gethash this-command emacs-ide-telemetry-command-counts 0)))
      (puthash this-command (1+ count) emacs-ide-telemetry-command-counts))))

(add-hook 'post-command-hook #'emacs-ide-telemetry-track-command)

(defun emacs-ide-telemetry-log-startup (elapsed gc-count pkg-count)
  "Log startup metrics: ELAPSED time, GC-COUNT, PKG-COUNT."
  (let ((log-file (expand-file-name "var/telemetry.log" user-emacs-directory)))
    (with-temp-buffer
      (when (file-exists-p log-file)
        (insert-file-contents log-file))
      (goto-char (point-max))
      (insert (format "[%s] Startup: %.2fs | GC: %d | Packages: %d\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S")
                     elapsed gc-count pkg-count))
      (write-region (point-min) (point-max) log-file))))

(defun emacs-ide-telemetry-report ()
  "Show telemetry report."
  (interactive)
  (let ((sorted (cl-sort (hash-table-to-list emacs-ide-telemetry-command-counts)
                         #'> :key #'cdr))
        (session-time (float-time (time-subtract (current-time)
                                                 emacs-ide-telemetry-session-start))))
    (with-output-to-temp-buffer "*Telemetry Report*"
      (princ "=== USAGE ANALYTICS ===\n\n")
      (princ (format "Session Duration: %.1f minutes\n\n" (/ session-time 60)))
      (princ "Top 20 Commands:\n\n")
      (cl-loop for (cmd . count) in (cl-subseq sorted 0 (min 20 (length sorted)))
               for i from 1
               do (princ (format "%2d. %-40s %d times\n" i cmd count))))))

(provide 'emacs-ide-telemetry)
;;; emacs-ide-telemetry.el ends here
