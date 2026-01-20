;;; emacs-ide-profiler.el --- Performance Profiling System -*- lexical-binding: t -*-
;;; Commentary:
;;; Advanced profiling and performance analysis
;;; Code:

(require 'profiler)

(defvar emacs-ide-profiler-running nil
  "Whether profiler is currently running.")

(defun emacs-ide-profile-startup ()
  "Profile startup performance."
  (interactive)
  (if (fboundp 'esup)
      (esup)
    (message "Install esup for detailed startup profiling: M-x package-install RET esup")))

(defun emacs-ide-profile-start ()
  "Start CPU and memory profiler."
  (interactive)
  (profiler-start 'cpu+mem)
  (setq emacs-ide-profiler-running t)
  (message "Profiler started. Do your work, then run emacs-ide-profile-report"))

(defun emacs-ide-profile-stop ()
  "Stop profiler."
  (interactive)
  (when emacs-ide-profiler-running
    (profiler-stop)
    (setq emacs-ide-profiler-running nil)
    (message "Profiler stopped")))

(defun emacs-ide-profile-report ()
  "Show profiler report."
  (interactive)
  (profiler-report)
  (emacs-ide-profile-stop))

(provide 'emacs-ide-profiler)
;;; emacs-ide-profiler.el ends here
