;;; emacs-ide-telemetry.el --- Usage Analytics (Local Only) -*- lexical-binding: t -*-
;;; Commentary:
;;; Privacy-respecting local usage analytics with config integration and rotation.
;;; Version: 3.0.4
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-telemetry-enabled t)
(defvar emacs-ide-telemetry-command-counts (make-hash-table :test 'equal))
(defvar emacs-ide-telemetry-max-log-size (* 10 1024 1024))
(defconst emacs-ide-telemetry-report-top-n 20)

(defvar emacs-ide-telemetry--pending-counts (make-hash-table :test 'equal))
(defvar emacs-ide-telemetry--flush-timer nil)

(defun emacs-ide-telemetry--flush-pending ()
  "Merge pending counts into the main command-counts table."
  (maphash
   (lambda (cmd n)
     (puthash cmd
              (+ (gethash cmd emacs-ide-telemetry-command-counts 0) n)
              emacs-ide-telemetry-command-counts))
   emacs-ide-telemetry--pending-counts)
  (clrhash emacs-ide-telemetry--pending-counts))

(defun emacs-ide-telemetry-clear ()
  "Clear all telemetry counts for a fresh session."
  (interactive)
  (clrhash emacs-ide-telemetry-command-counts)
  (clrhash emacs-ide-telemetry--pending-counts)
  (message "✓ Telemetry counts cleared"))

(emacs-ide-telemetry-clear)

(defun emacs-ide-telemetry-track-command ()
  "Record command in pending table."
  (when (and (bound-and-true-p emacs-ide-telemetry-enabled)
             (symbolp this-command)
             this-command)
    (puthash this-command
             (1+ (gethash this-command emacs-ide-telemetry--pending-counts 0))
             emacs-ide-telemetry--pending-counts)))

(defun emacs-ide-telemetry--ensure-flush-timer ()
  "Start the idle flush timer if not already running."
  (unless (and emacs-ide-telemetry--flush-timer
               (timerp emacs-ide-telemetry--flush-timer))
    (setq emacs-ide-telemetry--flush-timer
          (run-with-idle-timer 0.5 t #'emacs-ide-telemetry--flush-pending))))

(defun emacs-ide-telemetry--cancel-flush-timer ()
  "Cancel the idle flush timer if running."
  (when (and emacs-ide-telemetry--flush-timer
             (timerp emacs-ide-telemetry--flush-timer))
    (cancel-timer emacs-ide-telemetry--flush-timer)
    (setq emacs-ide-telemetry--flush-timer nil)))

(defun emacs-ide-telemetry-enable ()
  "Enable telemetry tracking."
  (interactive)
  (setq emacs-ide-telemetry-enabled t)
  (unless (memq 'emacs-ide-telemetry-track-command post-command-hook)
    (add-hook 'post-command-hook #'emacs-ide-telemetry-track-command))
  (emacs-ide-telemetry--ensure-flush-timer)
  (message "✓ Telemetry enabled"))

(defun emacs-ide-telemetry-disable ()
  "Disable telemetry tracking."
  (interactive)
  (setq emacs-ide-telemetry-enabled nil)
  (remove-hook 'post-command-hook #'emacs-ide-telemetry-track-command)
  (emacs-ide-telemetry--flush-pending)
  (emacs-ide-telemetry--cancel-flush-timer)
  (message "✓ Telemetry disabled"))

(when (bound-and-true-p emacs-ide-telemetry-enabled)
  (unless (memq 'emacs-ide-telemetry-track-command post-command-hook)
    (add-hook 'post-command-hook #'emacs-ide-telemetry-track-command))
  (emacs-ide-telemetry--ensure-flush-timer))

(defvar emacs-ide-telemetry-session-start (current-time))
(defvar emacs-ide-telemetry-log-file
  (expand-file-name "var/telemetry.log" user-emacs-directory))

(defun emacs-ide--ensure-log-dir ()
  "Ensure telemetry log directory exists."
  (let ((d (file-name-directory emacs-ide-telemetry-log-file)))
    (unless (file-directory-p d)
      (make-directory d t))))

(defun emacs-ide-telemetry-log-startup (elapsed gc-count pkg-count)
  "Log startup metrics: ELAPSED time, GC-COUNT, PKG-COUNT."
  (condition-case err
      (progn
        (emacs-ide--ensure-log-dir)
        (when (> (or (nth 7 (file-attributes emacs-ide-telemetry-log-file)) 0)
                 emacs-ide-telemetry-max-log-size)
          (ignore-errors
            (rename-file emacs-ide-telemetry-log-file
                         (concat emacs-ide-telemetry-log-file
                                 "." (format-time-string "%Y%m%d-%H%M%S"))
                         t)))
        (let ((entry (format "[%s] Startup: %.2fs | GC: %d | Packages: %d\n"
                             (format-time-string "%Y-%m-%d %H:%M:%S")
                             elapsed gc-count pkg-count)))
          (write-region entry nil emacs-ide-telemetry-log-file 'append 'quiet)))
    (error (warn "Telemetry log failed: %s" (error-message-string err)))))

(defun emacs-ide-telemetry--format-duration (seconds)
  "Format SECONDS as a human-readable duration string."
  (let* ((s  (floor seconds))
         (h  (/ s 3600))
         (m  (/ (mod s 3600) 60))
         (sc (mod s 60)))
    (cond ((>= h 1)  (format "%dh %dm %ds" h m sc))
          ((>= m 1)  (format "%dm %ds" m sc))
          (t         (format "%ds" sc)))))

(defun emacs-ide-telemetry-report ()
  "Show telemetry report."
  (interactive)
  (emacs-ide-telemetry--flush-pending)
  (let* ((alist (cl-loop for k being the hash-keys of emacs-ide-telemetry-command-counts
                         collect (cons k (gethash k emacs-ide-telemetry-command-counts))))
         (sorted (cl-sort alist #'> :key #'cdr))
         (total  (cl-reduce #'+ sorted :key #'cdr :initial-value 0))
         (session-time (float-time (time-subtract (current-time)
                                                  emacs-ide-telemetry-session-start))))
    (with-output-to-temp-buffer "*Telemetry Report*"
      (princ "=== USAGE ANALYTICS ===\n\n")
      (princ (format "Session Duration: %s\n"
                     (emacs-ide-telemetry--format-duration session-time)))
      (princ (format "Total Commands:   %d\n" total))
      (princ (format "Unique Commands:  %d\n\n" (length sorted)))
      (princ (format "Top %d Commands:\n\n" emacs-ide-telemetry-report-top-n))
      (cl-loop for (cmd . count) in sorted
               for i from 1 to emacs-ide-telemetry-report-top-n
               do (princ (format "%2d. %-40s %d times\n" i cmd count))))))

(with-eval-after-load 'emacs-ide-config
  (let ((size (and (fboundp 'emacs-ide-config-get)
                   (emacs-ide-config-get 'telemetry 'max-log-size nil))))
    (when (and size (numberp size) (> size 0))
      (setq emacs-ide-telemetry-max-log-size size))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (ignore-errors (emacs-ide-telemetry--flush-pending))
            (emacs-ide-telemetry--cancel-flush-timer)))

(provide 'emacs-ide-telemetry)
;;; emacs-ide-telemetry.el ends here
