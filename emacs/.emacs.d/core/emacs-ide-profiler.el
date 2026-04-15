;;; emacs-ide-profiler.el --- Performance Profiling System -*- lexical-binding: t -*-
;;; Commentary:
;;; Advanced profiling and performance analysis.
;;; Version: 3.0.4
;;; Code:

(require 'profiler)

(defvar emacs-ide-profiler-running nil)

(defun emacs-ide-profiler--sync-state ()
  "Sync emacs-ide-profiler-running with the actual profiler state."
  (unless (profiler-running-p)
    (setq emacs-ide-profiler-running nil)))

(defun emacs-ide-profile-startup ()
  "Profile startup performance."
  (interactive)
  (if (fboundp 'esup)
      (esup)
    (message "esup not installed. Add (straight-use-package 'esup) to use it.")
    (when (fboundp 'emacs-ide-startup-report)
      (emacs-ide-startup-report))
    (when (fboundp 'emacs-ide-early-init-report)
      (emacs-ide-early-init-report))))

(defun emacs-ide-profile-start ()
  "Start CPU and memory profiler."
  (interactive)
  (emacs-ide-profiler--sync-state)
  (if emacs-ide-profiler-running
      (message "Profiler is already running. Use emacs-ide-profile-report to see results.")
    (profiler-start 'cpu+mem)
    (setq emacs-ide-profiler-running t)
    (message "✓ Profiler started (cpu+mem). Do your work, then M-x emacs-ide-profile-report")))

(defun emacs-ide-profile-stop ()
  "Stop the profiler."
  (interactive)
  (emacs-ide-profiler--sync-state)
  (if emacs-ide-profiler-running
      (progn
        (profiler-stop)
        (setq emacs-ide-profiler-running nil)
        (message "✓ Profiler stopped"))
    (message "Profiler is not running.")))

(defun emacs-ide-profile-report ()
  "Stop the profiler and display the report."
  (interactive)
  (emacs-ide-profiler--sync-state)
  (if emacs-ide-profiler-running
      (progn
        (profiler-stop)
        (setq emacs-ide-profiler-running nil)
        (profiler-report))
    (message "Profiler is not running. Start it first with M-x emacs-ide-profile-start.")))

(defun emacs-ide-profile-reset ()
  "Clear accumulated profiler data and start a fresh session."
  (interactive)
  (emacs-ide-profiler--sync-state)
  (when emacs-ide-profiler-running
    (profiler-stop)
    (setq emacs-ide-profiler-running nil))
  (profiler-reset)
  (profiler-start 'cpu+mem)
  (setq emacs-ide-profiler-running t)
  (message "✓ Profiler reset and restarted (cpu+mem)"))

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (and emacs-ide-profiler-running
                       (profiler-running-p))
              (profiler-stop)
              (setq emacs-ide-profiler-running nil))))

(provide 'emacs-ide-profiler)
;;; emacs-ide-profiler.el ends here
