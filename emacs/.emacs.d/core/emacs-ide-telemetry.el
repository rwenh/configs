;;; emacs-ide-telemetry.el --- Usage Analytics (Local Only) -*- lexical-binding: t -*-
;;; Commentary:
;;; Privacy-respecting local usage analytics with config integration and rotation.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.3 to 3.0.4.
;;;   - FIX-LOG-APPEND: emacs-ide-telemetry-log-startup now uses write-region
;;;     with 'append instead of reading the entire log file into a temp buffer.
;;;     The old approach was O(n) on every startup — same class of bug as
;;;     FIX-LOG-APPEND in emacs-ide-recovery.el.
;;;   - FIX-ROTATION: Log rotation now uses a timestamped suffix instead of
;;;     a fixed .old — each rotation preserves prior history. Mirrors
;;;     FIX-ROTATION in emacs-ide-recovery.el.
;;;   - FIX-ROTATION-ORDER: Rotation check now runs BEFORE writing the new
;;;     entry so fresh entries go into the new log, not into the rotated .old.
;;;     Also added (or (nth 7 ...) 0) nil-guard on file-attributes size.
;;;   - FIX-MAX-LOG-SIZE: Max log size extracted to a configurable defvar
;;;     emacs-ide-telemetry-max-log-size, read from config.yml
;;;     telemetry.max-log-size after emacs-ide-config loads.
;;;   - FIX-HOOK-MEMQ: Hook idempotency guard changed from
;;;     (member #'fn hook) to (memq 'fn hook) — member with a function
;;;     object may not match a symbol-form entry added by add-hook.
;;;   - FIX-DISABLE-TIMER: Removed spurious emacs-ide-telemetry--ensure-
;;;     flush-timer call inside emacs-ide-telemetry-disable — it was a
;;;     no-op (timer already running) and misleadingly commented as
;;;     "flush any pending before stopping". The explicit --flush-pending
;;;     call that follows is sufficient.
;;;   - FIX-CLEAR: emacs-ide-telemetry-clear command added to reset both
;;;     hash tables. Also auto-called on each module load so reloads
;;;     produce a clean session report rather than accumulated totals.
;;;   - FIX-KILL-HOOK: kill-emacs-hook added to flush pending counts and
;;;     cancel the flush timer on exit — pending data was previously lost.
;;;   - FIX-SUBSEQ: cl-subseq replaced with cl-loop :to in report.
;;;     Top-N count extracted to defconst emacs-ide-telemetry-report-top-n.
;;;   - FIX-DURATION-FMT: Session duration now formatted as Xh Ym Zs for
;;;     readability across short and long sessions.
;;; Fixes vs 2.2.3 (retained):
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

;; FIX-MAX-LOG-SIZE: Configurable defvar — read from config.yml
;; telemetry.max-log-size after emacs-ide-config loads.
(defvar emacs-ide-telemetry-max-log-size (* 10 1024 1024)
  "Maximum telemetry log size in bytes before rotation (default 10MB).")

;; FIX-SUBSEQ: Single source of truth for report top-N.
(defconst emacs-ide-telemetry-report-top-n 20
  "Number of top commands shown in the telemetry report.")

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

(defun emacs-ide-telemetry-clear ()
  "Clear all telemetry counts for a fresh session.
FIX-CLEAR: Resets both the main and pending hash tables so that
M-x emacs-ide-config-reload produces a clean report rather than
accumulating counts across multiple config reloads."
  (interactive)
  (clrhash emacs-ide-telemetry-command-counts)
  (clrhash emacs-ide-telemetry--pending-counts)
  (message "✓ Telemetry counts cleared"))

;; FIX-CLEAR: Auto-clear on each load so reloads start fresh.
(emacs-ide-telemetry-clear)

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
  ;; FIX-HOOK-MEMQ: use memq with symbol, not member with function object.
  ;; add-hook stores symbols; (member #'fn hook) may not match them.
  (unless (memq 'emacs-ide-telemetry-track-command post-command-hook)
    (add-hook 'post-command-hook #'emacs-ide-telemetry-track-command))
  (emacs-ide-telemetry--ensure-flush-timer)
  (message "✓ Telemetry enabled"))

(defun emacs-ide-telemetry-disable ()
  "Disable telemetry tracking (hook + flush timer)."
  (interactive)
  (setq emacs-ide-telemetry-enabled nil)
  (remove-hook 'post-command-hook #'emacs-ide-telemetry-track-command)
  ;; FIX-DISABLE-TIMER: removed spurious --ensure-flush-timer call here.
  ;; It was a no-op (timer already running) and its comment "flush any
  ;; pending before stopping" was misleading — flushing is done by the
  ;; explicit --flush-pending call below.
  (emacs-ide-telemetry--flush-pending)
  (emacs-ide-telemetry--cancel-flush-timer)
  (message "✓ Telemetry disabled"))

;; Install hook + timer at load time if telemetry is enabled
(when (bound-and-true-p emacs-ide-telemetry-enabled)
  ;; FIX-HOOK-MEMQ: memq with symbol matches how add-hook stores entries
  (unless (memq 'emacs-ide-telemetry-track-command post-command-hook)
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
FIX-ROTATION-ORDER: Rotation check now runs BEFORE writing the new
entry so fresh entries land in the new log, not in the rotated .old.
FIX-LOG-APPEND: Uses write-region 'append instead of reading the
entire file into memory — avoids O(n) read on every startup.
FIX-ROTATION: Uses timestamped suffix to preserve rotation history.
FIX-MAX-LOG-SIZE: Uses emacs-ide-telemetry-max-log-size defvar."
  (condition-case err
      (progn
        (emacs-ide--ensure-log-dir)
        ;; FIX-ROTATION-ORDER: rotate BEFORE writing so new entry goes
        ;; into the fresh log, not into the file about to be renamed.
        ;; FIX-MAX-LOG-SIZE: use defvar instead of hardcoded (* 10 1024 1024)
        (when (> (or (nth 7 (file-attributes emacs-ide-telemetry-log-file)) 0)
                 emacs-ide-telemetry-max-log-size)
          ;; FIX-ROTATION: timestamp suffix preserves prior rotation history
          (ignore-errors
            (rename-file emacs-ide-telemetry-log-file
                         (concat emacs-ide-telemetry-log-file
                                 "." (format-time-string "%Y%m%d-%H%M%S"))
                         t)))
        ;; FIX-LOG-APPEND: append directly — no full-file read required
        (let ((entry (format "[%s] Startup: %.2fs | GC: %d | Packages: %d\n"
                             (format-time-string "%Y-%m-%d %H:%M:%S")
                             elapsed gc-count pkg-count)))
          (write-region entry nil emacs-ide-telemetry-log-file 'append 'quiet)))
    (error (warn "Telemetry log failed: %s" (error-message-string err)))))

;; ============================================================================
;; REPORT
;; ============================================================================
(defun emacs-ide-telemetry--format-duration (seconds)
  "Format SECONDS as a human-readable duration string.
FIX-DURATION-FMT: Shows Xh Ym Zs for long sessions, Ys Zs for short."
  (let* ((s  (floor seconds))
         (h  (/ s 3600))
         (m  (/ (mod s 3600) 60))
         (sc (mod s 60)))
    (cond ((>= h 1)  (format "%dh %dm %ds" h m sc))
          ((>= m 1)  (format "%dm %ds" m sc))
          (t         (format "%ds" sc)))))

(defun emacs-ide-telemetry-report ()
  "Show telemetry report (top commands and session duration)."
  (interactive)
  ;; Flush any pending counts before reporting
  (emacs-ide-telemetry--flush-pending)
  (let* ((alist (cl-loop for k being the hash-keys of emacs-ide-telemetry-command-counts
                         collect (cons k (gethash k emacs-ide-telemetry-command-counts))))
         (sorted (cl-sort alist #'> :key #'cdr))
         (total  (cl-reduce #'+ sorted :key #'cdr :initial-value 0))
         (session-time (float-time (time-subtract (current-time)
                                                  emacs-ide-telemetry-session-start))))
    (with-output-to-temp-buffer "*Telemetry Report*"
      (princ "=== USAGE ANALYTICS ===\n\n")
      ;; FIX-DURATION-FMT: human-readable duration
      (princ (format "Session Duration: %s\n"
                     (emacs-ide-telemetry--format-duration session-time)))
      (princ (format "Total Commands:   %d\n" total))
      (princ (format "Unique Commands:  %d\n\n" (length sorted)))
      ;; FIX-SUBSEQ: cl-loop :to avoids allocating a second list
      (princ (format "Top %d Commands:\n\n" emacs-ide-telemetry-report-top-n))
      (cl-loop for (cmd . count) in sorted
               for i from 1 to emacs-ide-telemetry-report-top-n
               do (princ (format "%2d. %-40s %d times\n" i cmd count))))))

;; ============================================================================
;; CONFIG WIRING
;; FIX-MAX-LOG-SIZE: Read telemetry.max-log-size from config.yml after
;; emacs-ide-config loads and apply to the defvar.
;; ============================================================================
(with-eval-after-load 'emacs-ide-config
  (let ((size (and (fboundp 'emacs-ide-config-get)
                   (emacs-ide-config-get 'telemetry 'max-log-size nil))))
    (when (and size (numberp size) (> size 0))
      (setq emacs-ide-telemetry-max-log-size size))))

;; ============================================================================
;; CLEANUP ON EXIT
;; FIX-KILL-HOOK: Flush pending counts and cancel the flush timer cleanly
;; on Emacs exit. Without this, pending counts are lost on every exit.
;; Consistent with GC/health/recovery/profiler timer cleanup pattern.
;; ============================================================================
(add-hook 'kill-emacs-hook
          (lambda ()
            (ignore-errors (emacs-ide-telemetry--flush-pending))
            (emacs-ide-telemetry--cancel-flush-timer)))

(provide 'emacs-ide-telemetry)
;;; emacs-ide-telemetry.el ends here
