;;; emacs-ide-profiler.el --- Performance Profiling System -*- lexical-binding: t -*-
;;; Commentary:
;;; Advanced profiling and performance analysis.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Version header added (file had none).
;;;   - FIX-RUNNING-FLAG: emacs-ide-profiler-running now synced with the
;;;     actual profiler state via (profiler-running-p) rather than relying
;;;     solely on the flag. If profiler-stop is called externally the flag
;;;     can go stale; syncing on every entry point prevents confusion.
;;;   - FIX-STARTUP-FALLBACK: emacs-ide-profile-startup now falls back to
;;;     displaying emacs-ide-startup-report + emacs-ide-early-init-report
;;;     when esup is absent, giving the user useful data immediately.
;;;     The install hint now correctly references straight-use-package
;;;     instead of package-install (this config uses straight.el exclusively).
;;;   - FIX-STOP-MESSAGE: emacs-ide-profile-stop now emits a message when
;;;     called while the profiler is not running, for parity with
;;;     emacs-ide-profile-report.
;;;   - FIX-DOUBLE-START: emacs-ide-profile-start now guards against being
;;;     called while already running — previously silently discarded all
;;;     accumulated profiling data by resetting the profiler.
;;;   - FIX-REPORT-ORDER: emacs-ide-profile-report now calls profiler-stop
;;;     before profiler-report. The report reads the last snapshot which is
;;;     preserved after stop; stopping first ensures the snapshot is complete.
;;;   - FIX-KILL-HOOK: kill-emacs-hook added to stop the profiler cleanly
;;;     on Emacs exit, consistent with GC timer and health timer cleanup.
;;;   - FIX-RESET: emacs-ide-profile-reset command added to clear accumulated
;;;     profiler data and restart a fresh session.
;;; Code:

(require 'profiler)

;; ============================================================================
;; STATE
;; ============================================================================
(defvar emacs-ide-profiler-running nil
  "Whether the profiler was started via emacs-ide-profile-start.
FIX-RUNNING-FLAG: Always sync this with (profiler-running-p) on entry
to each command rather than trusting the flag alone.")

(defun emacs-ide-profiler--sync-state ()
  "Sync emacs-ide-profiler-running with the actual profiler state.
FIX-RUNNING-FLAG: Corrects stale flag if profiler-stop was called
externally or the profiler crashed."
  (unless (profiler-running-p)
    (setq emacs-ide-profiler-running nil)))

;; ============================================================================
;; COMMANDS
;; ============================================================================
(defun emacs-ide-profile-startup ()
  "Profile startup performance.
FIX-STARTUP-FALLBACK: When esup is not installed, falls back to
displaying the built-in startup and early-init reports so the user
gets useful data immediately rather than just an install hint.
FIX-STARTUP-FALLBACK: Install hint now references straight-use-package
instead of package-install — this config uses straight.el exclusively."
  (interactive)
  (if (fboundp 'esup)
      (esup)
    (message "esup not installed. Add (straight-use-package 'esup) to use it.")
    ;; Fall back to built-in reports — defined in init.el and early-init.el
    (when (fboundp 'emacs-ide-startup-report)
      (emacs-ide-startup-report))
    (when (fboundp 'emacs-ide-early-init-report)
      (emacs-ide-early-init-report))))

(defun emacs-ide-profile-start ()
  "Start CPU and memory profiler.
FIX-DOUBLE-START: Guards against starting when already running.
Previously calling this while running silently reset the profiler,
discarding all accumulated profiling data."
  (interactive)
  (emacs-ide-profiler--sync-state)
  (if emacs-ide-profiler-running
      (message "Profiler is already running. Use emacs-ide-profile-report to see results.")
    (profiler-start 'cpu+mem)
    (setq emacs-ide-profiler-running t)
    (message "✓ Profiler started (cpu+mem). Do your work, then M-x emacs-ide-profile-report")))

(defun emacs-ide-profile-stop ()
  "Stop the profiler.
FIX-STOP-MESSAGE: Now emits a message when called while not running,
for parity with emacs-ide-profile-report."
  (interactive)
  (emacs-ide-profiler--sync-state)
  (if emacs-ide-profiler-running
      (progn
        (profiler-stop)
        (setq emacs-ide-profiler-running nil)
        (message "✓ Profiler stopped"))
    (message "Profiler is not running.")))

(defun emacs-ide-profile-report ()
  "Stop the profiler and display the report.
FIX-REPORT-ORDER: Now stops the profiler BEFORE calling profiler-report.
The report reads the last snapshot which is preserved after stop;
stopping first guarantees the snapshot is complete and stable.
Previously profiler-report was called on a live profiler then stopped,
risking an incomplete snapshot in async report buffers."
  (interactive)
  (emacs-ide-profiler--sync-state)
  (if emacs-ide-profiler-running
      (progn
        ;; FIX-REPORT-ORDER: stop first, then report from the stable snapshot
        (profiler-stop)
        (setq emacs-ide-profiler-running nil)
        (profiler-report))
    (message "Profiler is not running. Start it first with M-x emacs-ide-profile-start.")))

(defun emacs-ide-profile-reset ()
  "Clear accumulated profiler data and start a fresh session.
FIX-RESET: New command. Useful when you want to discard prior samples
and begin a clean profiling window without stopping and restarting."
  (interactive)
  (emacs-ide-profiler--sync-state)
  (when emacs-ide-profiler-running
    (profiler-stop)
    (setq emacs-ide-profiler-running nil))
  (profiler-reset)
  (profiler-start 'cpu+mem)
  (setq emacs-ide-profiler-running t)
  (message "✓ Profiler reset and restarted (cpu+mem)"))

;; ============================================================================
;; CLEANUP ON EXIT
;; FIX-KILL-HOOK: Stop the profiler cleanly on Emacs exit, consistent with
;; GC timer cleanup in early-init.el and health timer cleanup in init.el.
;; ============================================================================
(add-hook 'kill-emacs-hook
          (lambda ()
            (when (and emacs-ide-profiler-running
                       (profiler-running-p))
              (profiler-stop)
              (setq emacs-ide-profiler-running nil))))

(provide 'emacs-ide-profiler)
;;; emacs-ide-profiler.el ends here
