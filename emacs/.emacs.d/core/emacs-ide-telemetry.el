;;; emacs-ide-telemetry.el --- Usage Analytics (Local Only) -*- lexical-binding: t -*-
;;; Commentary:
;;; Privacy-respecting local usage analytics with config integration and rotation.
;;; Version: 2.2.3
;;; Fixes:
;;;   - 2.2.3: emacs-ide-telemetry-enable/disable: adding functions to toggle
;;;     telemetry on/off at runtime now properly restarts the flush timer when
;;;     re-enabling. Previously, if telemetry was disabled then re-enabled, the
;;;     hook was re-added but the flush timer was not restarted (it was only
;;;     created at load time), so pending counts were never flushed to the main
;;;     table — all subsequent commands were silently dropped from the report.
;;;   - 2.2.2: post-command-hook tracking throttled via pending counter + idle
;;;     flush timer. Previously ran puthash on every keystroke.
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-telemetry-enabled t
  "Enable telemetry collection (local only).")

(defvar emacs-ide-telemetry-command-counts (make-hash-table :test 'equal)
  "Track command usage counts.")

;; ============================================================================
;; THROTTLED COMMAND TRACKING
;; ============================================================================
(defvar emacs-ide-telemetry--pending-counts (make-hash-table :test 'equal)
  "Transient accumulator; flushed to command-counts on idle.")

(defvar emacs-ide-telemetry--flush-timer nil
  "Idle timer for flushing pending counts.")

(defun emacs-ide-telemetry--flush-pending ()
  "Merge pending counts into the main command-counts table."
  (maphash
   (lambda (cmd n)
     (puthash cmd
              (+ (gethash cmd emacs-ide-telemetry-command-counts 0) n)
              emacs-ide-telemetry-command-counts))
   emacs-ide-telemetry--pending-counts)
  (clrhash emacs-ide-telemetry--pending-counts))

(defun emacs-ide-telemetry-track-command ()
  "Record command in pending table (flushed to main table on idle)."
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

;; ============================================================================
;; ENABLE / DISABLE AT RUNTIME
;; FIX 2.2.3: Provide explicit enable/disable functions so that toggling
;;   telemetry off then on again correctly restarts the flush timer.
;;   Previously re-enabling added the hook but left the timer cancelled,
;;   so pending counts were never flushed and the report showed no data.
;; ============================================================================
(defun emacs-ide-telemetry-enable ()
  "Enable telemetry tracking (hook + flush timer)."
  (interactive)
  (setq emacs-ide-telemetry-enabled t)
  (unless (member #'emacs-ide-telemetry-track-command post-command-hook)
    (add-hook 'post-command-hook #'emacs-ide-telemetry-track-command))
  (emacs-ide-telemetry--ensure-flush-timer)
  (message "✓ Telemetry enabled"))

(defun emacs-ide-telemetry-disable ()
  "Disable telemetry tracking (hook + flush timer)."
  (interactive)
  (setq emacs-ide-telemetry-enabled nil)
  (remove-hook 'post-command-hook #'emacs-ide-telemetry-track-command)
  (emacs-ide-telemetry--ensure-flush-timer) ; flush any pending before stopping
  (emacs-ide-telemetry--flush-pending)
  (emacs-ide-telemetry--cancel-flush-timer)
  (message "✓ Telemetry disabled"))

;; Install hook + timer at load time if telemetry is enabled
(when (bound-and-true-p emacs-ide-telemetry-enabled)
  (unless (member #'emacs-ide-telemetry-track-command post-command-hook)
    (add-hook 'post-command-hook #'emacs-ide-telemetry-track-command))
  (emacs-ide-telemetry--ensure-flush-timer))

;; ============================================================================
;; LOG HELPERS
;; ============================================================================
(defvar emacs-ide-telemetry-session-start (current-time)
  "Session start time.")

(defvar emacs-ide-telemetry-log-file
  (expand-file-name "var/telemetry.log" user-emacs-directory)
  "Telemetry log file path.")

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
                           t)))))
    (error (warn "Telemetry log failed: %s" (error-message-string err)))))

;; ============================================================================
;; REPORT
;; ============================================================================
(defun emacs-ide-telemetry-report ()
  "Show telemetry report (top commands and session duration)."
  (interactive)
  ;; Flush any pending counts before reporting
  (emacs-ide-telemetry--flush-pending)
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
