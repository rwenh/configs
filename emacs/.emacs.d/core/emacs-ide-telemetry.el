;;; emacs-ide-telemetry.el --- Usage Analytics (Local Only, CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; Privacy-respecting local usage analytics with config integration and rotation.
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-telemetry-enabled
  (if (boundp 'emacs-ide-telemetry-enabled) emacs-ide-telemetry-enabled t)
  "Enable telemetry collection (local only).")

(defvar emacs-ide-telemetry-command-counts (make-hash-table :test 'equal)
  "Track command usage counts.")

(defvar emacs-ide-telemetry-session-start (current-time)
  "Session start time.")

(defvar emacs-ide-telemetry-log-file
  (expand-file-name "var/telemetry.log" user-emacs-directory)
  "Telemetry log file path.")

(defun emacs-ide-telemetry-track-command ()
  "Track command execution while guarding for nil this-command."
  (when (and (bound-and-true-p emacs-ide-telemetry-enabled)
             (symbolp this-command))
    (let ((count (gethash this-command emacs-ide-telemetry-command-counts 0)))
      (puthash this-command (1+ count) emacs-ide-telemetry-command-counts))))

;; Add hook only if telemetry is enabled and hook not already present
(when (and (bound-and-true-p emacs-ide-telemetry-enabled)
           (not (member #'emacs-ide-telemetry-track-command post-command-hook)))
  (add-hook 'post-command-hook #'emacs-ide-telemetry-track-command))

(defun emacs-ide--ensure-log-dir ()
  "Ensure telemetry log directory exists."
  (let ((d (file-name-directory emacs-ide-telemetry-log-file)))
    (unless (file-directory-p d)
      (make-directory d t))))

(defun emacs-ide-telemetry-log-startup (elapsed gc-count pkg-count)
  "Log startup metrics: ELAPSED time, GC-COUNT, PKG-COUNT.
Writes atomically and rotates by size."
  (condition-case err
      (progn
        (emacs-ide--ensure-log-dir)
        (let ((entry (format "[%s] Startup: %.2fs | GC: %d | Packages: %d\n"
                             (format-time-string "%Y-%m-%d %H:%M:%S")
                             elapsed gc-count pkg-count))
              (tmp (concat emacs-ide-telemetry-log-file ".tmp")))
          ;; Append safely
          (with-temp-file tmp
            (when (file-exists-p emacs-ide-telemetry-log-file)
              (insert-file-contents emacs-ide-telemetry-log-file))
            (goto-char (point-max))
            (insert entry))
          (rename-file tmp emacs-ide-telemetry-log-file t)
          ;; Rotate if large
          (when (> (nth 7 (file-attributes emacs-ide-telemetry-log-file)) (* 10 1024 1024))
            (when (file-exists-p emacs-ide-telemetry-log-file)
              (rename-file emacs-ide-telemetry-log-file
                           (concat emacs-ide-telemetry-log-file ".old")
                           t))))))
    (error (warn "Telemetry log failed: %s" (error-message-string err)))))

(defun emacs-ide-telemetry-report ()
  "Show telemetry report (top commands and session duration)."
  (interactive)
  (let* ((alist (cl-loop for k being the hash-keys of emacs-ide-telemetry-command-counts
                         collect (cons k (gethash k emacs-ide-telemetry-command-counts))))
         (sorted (cl-sort alist #'> :key #'cdr))
         (session-time (float-time (time-subtract (current-time)
                                                  emacs-ide-telemetry-session-start))))
    (with-output-to-temp-buffer "*Telemetry Report*"
      (princ "=== USAGE ANALYTICS ===\n\n")
      (princ (format "Session Duration: %.1f minutes\n\n" (/ session-time 60.0)))
      (princ "Top Commands:\n\n")
      (let ((i 1))
        (dolist (entry (cl-subseq sorted 0 (min 20 (length sorted))))
          (princ (format "%2d. %-40s %d times\n" i (car entry) (cdr entry)))
          (cl-incf i))))))

(provide 'emacs-ide-telemetry)
;;; emacs-ide-telemetry.el ends here