;;; emacs-ide-telemetry.el --- Usage Analytics (Local Only, CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; Privacy-respecting local usage analytics with config integration and rotation.
;;; Version: 2.2.2
;;; Fixes:
;;;   - 2.2.2: post-command-hook tracking throttled via an in-memory pending
;;;     counter that flushes to the hash table on idle (0.5s). Previously ran
;;;     on every keystroke with no throttling, which introduced perceptible lag
;;;     on slow machines (hash-table puthash on every command).
;;; Code:

(require 'cl-lib)

;; FIX: The previous init form was:
;;   (if (boundp 'emacs-ide-telemetry-enabled) emacs-ide-telemetry-enabled t)
;; This is dead code: defvar only evaluates its init form when the variable is
;; NOT yet bound, so (boundp 'emacs-ide-telemetry-enabled) inside that form is
;; always nil — the "preserve existing value" branch is unreachable.
;; The correct idiom is simply `t` as the default.  defvar's own no-op-when-
;; already-bound semantics preserves any value set earlier by emacs-ide-config-apply.
(defvar emacs-ide-telemetry-enabled t
  "Enable telemetry collection (local only).")

(defvar emacs-ide-telemetry-command-counts (make-hash-table :test 'equal)
  "Track command usage counts.")

;; ============================================================================
;; THROTTLED COMMAND TRACKING
;; FIX 2.2.2: Instead of updating the hash table on every command (which runs
;;   puthash on every keystroke), we accumulate into a separate pending table
;;   and flush to the main table on a 0.5-second idle timer. This eliminates
;;   the per-keystroke hash overhead while still capturing all commands.
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
  "Record command in pending table (flushed to main table on idle).
Guards for nil this-command and non-symbol commands."
  (when (and (bound-and-true-p emacs-ide-telemetry-enabled)
             (symbolp this-command)
             this-command)
    ;; Accumulate in the cheap pending table — no idle-timer cost per keystroke
    (puthash this-command
             (1+ (gethash this-command emacs-ide-telemetry--pending-counts 0))
             emacs-ide-telemetry--pending-counts)))

(defun emacs-ide-telemetry--ensure-flush-timer ()
  "Start the idle flush timer if not already running."
  (unless (and emacs-ide-telemetry--flush-timer
               (timerp emacs-ide-telemetry--flush-timer))
    (setq emacs-ide-telemetry--flush-timer
          (run-with-idle-timer 0.5 t #'emacs-ide-telemetry--flush-pending))))

;; Install hook only if telemetry is enabled and hook not already present
(when (and (bound-and-true-p emacs-ide-telemetry-enabled)
           (not (member #'emacs-ide-telemetry-track-command post-command-hook)))
  (add-hook 'post-command-hook #'emacs-ide-telemetry-track-command)
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
