;;; emacs-ide-telemetry.el --- Usage Analytics (Local Only) -*- lexical-binding: t -*-
;;; Commentary:
;;; Privacy-respecting local usage analytics with config integration and rotation.
;;; Version: 3.3.0
;;;
;;; Code:

(require 'cl-lib)

;;;; ── State ───────────────────────────────────────────────────────────────────

(defvar emacs-ide-telemetry-enabled t
  "Non-nil when command tracking is active.")

(defvar emacs-ide-telemetry-command-counts
  (make-hash-table :test 'eq)
  "Hash table mapping command symbols to their invocation counts.")

(defvar emacs-ide-telemetry-max-log-size (* 10 1024 1024)
  "Maximum telemetry log file size in bytes before rotation.")

(defconst emacs-ide-telemetry-report-top-n 20
  "Number of commands to show in the telemetry report.")

(defvar emacs-ide-telemetry--pending-counts
  (make-hash-table :test 'eq)
  "Pending counts accumulated between flush timer firings.")

(defvar emacs-ide-telemetry--flush-timer nil
  "Idle timer that periodically merges pending counts into the main table.")

;;;; ── Flush ───────────────────────────────────────────────────────────────────

(defun emacs-ide-telemetry--flush-pending ()
  "Merge pending counts into the main command-counts table."
  (maphash
   (lambda (cmd n)
     (puthash cmd
              (+ (gethash cmd emacs-ide-telemetry-command-counts 0) n)
              emacs-ide-telemetry-command-counts))
   emacs-ide-telemetry--pending-counts)
  (clrhash emacs-ide-telemetry--pending-counts))

;;;; ── Lifecycle ───────────────────────────────────────────────────────────────

(defun emacs-ide-telemetry-clear ()
  "Clear all telemetry counts for a fresh session."
  (interactive)
  (clrhash emacs-ide-telemetry-command-counts)
  (clrhash emacs-ide-telemetry--pending-counts)
  (message "✓ Telemetry counts cleared"))

;; Clear on load so each session starts fresh
(emacs-ide-telemetry-clear)

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

;;;; ── Tracking ────────────────────────────────────────────────────────────────

(defun emacs-ide-telemetry-track-command ()
  "Record the current command in the pending count table."
  (when (and (bound-and-true-p emacs-ide-telemetry-enabled)
             (symbolp this-command)
             this-command)
    (puthash this-command
             (1+ (gethash this-command
                          emacs-ide-telemetry--pending-counts 0))
             emacs-ide-telemetry--pending-counts)))

;;;; ── Enable / disable ────────────────────────────────────────────────────────

(defun emacs-ide-telemetry-enable ()
  "Enable telemetry command tracking."
  (interactive)
  (setq emacs-ide-telemetry-enabled t)
  (unless (memq 'emacs-ide-telemetry-track-command post-command-hook)
    (add-hook 'post-command-hook #'emacs-ide-telemetry-track-command))
  (emacs-ide-telemetry--ensure-flush-timer)
  (message "✓ Telemetry enabled"))

(defun emacs-ide-telemetry-disable ()
  "Disable telemetry command tracking and flush pending counts."
  (interactive)
  (setq emacs-ide-telemetry-enabled nil)
  (remove-hook 'post-command-hook #'emacs-ide-telemetry-track-command)
  (emacs-ide-telemetry--flush-pending)
  (emacs-ide-telemetry--cancel-flush-timer)
  (message "✓ Telemetry disabled"))

;; Auto-enable if telemetry is on (checked after defvar default above)
(when (bound-and-true-p emacs-ide-telemetry-enabled)
  (unless (memq 'emacs-ide-telemetry-track-command post-command-hook)
    (add-hook 'post-command-hook #'emacs-ide-telemetry-track-command))
  (emacs-ide-telemetry--ensure-flush-timer))

;;;; ── Startup logging ─────────────────────────────────────────────────────────

(defvar emacs-ide-telemetry-session-start (current-time))

(defvar emacs-ide-telemetry-log-file
  (expand-file-name "var/telemetry.log" user-emacs-directory))

(defun emacs-ide--ensure-log-dir ()
  "Ensure the telemetry log directory exists."
  (let ((d (file-name-directory emacs-ide-telemetry-log-file)))
    (unless (file-directory-p d)
      (make-directory d t))))

(defun emacs-ide-telemetry-log-startup (elapsed gc-count pkg-count)
  "Write a startup metrics entry to the telemetry log.
ELAPSED is startup time in seconds, GC-COUNT is GC cycles, PKG-COUNT is
the number of straight.el packages."
  (condition-case err
      (progn
        (emacs-ide--ensure-log-dir)
        ;; Rotate log if it exceeds the size limit
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
    (error
     (message "Telemetry log failed (non-fatal): %s" (error-message-string err)))))

;;;; ── Report ──────────────────────────────────────────────────────────────────

(defun emacs-ide-telemetry--format-duration (seconds)
  "Format SECONDS as a human-readable duration string."
  (let* ((s  (floor seconds))
         (h  (/ s 3600))
         (m  (/ (mod s 3600) 60))
         (sc (mod s 60)))
    (cond
     ((>= h 1) (format "%dh %dm %ds" h m sc))
     ((>= m 1) (format "%dm %ds" m sc))
     (t        (format "%ds" sc)))))

(defun emacs-ide-telemetry-report ()
  "Display a report of the most-used commands this session."
  (interactive)
  (emacs-ide-telemetry--flush-pending)
  (let* ((alist  (cl-loop for k being the hash-keys
                          of emacs-ide-telemetry-command-counts
                          collect (cons k (gethash k
                                                   emacs-ide-telemetry-command-counts))))
         (sorted (cl-sort alist #'> :key #'cdr))
         (total  (cl-reduce #'+ sorted :key #'cdr :initial-value 0))
         (session-time (float-time (time-subtract (current-time)
                                                  emacs-ide-telemetry-session-start))))
    (with-output-to-temp-buffer "*Telemetry Report*"
      (princ "=== USAGE ANALYTICS (local only) ===\n\n")
      (princ (format "Session Duration: %s\n"
                     (emacs-ide-telemetry--format-duration session-time)))
      (princ (format "Total Commands:   %d\n" total))
      (princ (format "Unique Commands:  %d\n\n" (length sorted)))
      (princ (format "Top %d Commands:\n\n" emacs-ide-telemetry-report-top-n))
      (cl-loop for (cmd . count) in sorted
               for i from 1 to emacs-ide-telemetry-report-top-n
               do (princ (format "%2d. %-42s %d times\n" i cmd count))))))

;;;; ── Config integration ──────────────────────────────────────────────────────

(with-eval-after-load 'emacs-ide-config
  ;; Read max-log-size from config — accept both 'enabled' and 'enable' spellings
  (let ((size (and (fboundp 'emacs-ide-config-get)
                   (emacs-ide-config-get 'telemetry 'max-log-size nil))))
    (when (and size (numberp size) (> size 0))
      (setq emacs-ide-telemetry-max-log-size size)))
  ;; Honour telemetry.enable (or .enabled for legacy) from config.yml.
  ;; emacs-ide-config-apply (Session 1) already sets emacs-ide-telemetry-enabled;
  ;; this block handles the enable/disable side effect on the hook.
  (if emacs-ide-telemetry-enabled
      (emacs-ide-telemetry-enable)
    (emacs-ide-telemetry-disable)))

;;;; ── Kill hook ───────────────────────────────────────────────────────────────

(add-hook 'kill-emacs-hook
          (lambda ()
            (ignore-errors (emacs-ide-telemetry--flush-pending))
            (emacs-ide-telemetry--cancel-flush-timer)))

(provide 'emacs-ide-telemetry)
;;; emacs-ide-telemetry.el ends here
