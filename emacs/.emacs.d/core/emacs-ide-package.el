;;; emacs-ide-package.el --- Package Management Utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Utilities for package management, profiling, and optimization
;;; Code:

(require 'cl-lib)

;; Package load time tracking
(defvar emacs-ide-package-load-times (make-hash-table :test 'equal)
  "Hash table tracking package load times.")

(defvar emacs-ide-package-slow-threshold 0.5
  "Warn if package takes longer than this (seconds).")

(defun emacs-ide-package-track-load (orig-fun &rest args)
  "Advice to track package load time around ORIG-FUN with ARGS."
  (let* ((package-name (car args))
         (start-time (current-time))
         (result (apply orig-fun args))
         (elapsed (float-time (time-subtract (current-time) start-time))))
    (puthash package-name elapsed emacs-ide-package-load-times)
    (when (> elapsed emacs-ide-package-slow-threshold)
      (warn "Package %s took %.2fs to load" package-name elapsed))
    result))

;; Install advice
(advice-add 'require :around #'emacs-ide-package-track-load)

(defun emacs-ide-package-report ()
  "Show package load time report."
  (interactive)
  (let ((sorted-packages
         (cl-sort (hash-table-to-list emacs-ide-package-load-times)
                  #'> :key #'cdr)))
    (with-output-to-temp-buffer "*Package Load Times*"
      (princ "=== PACKAGE LOAD TIMES ===\n\n")
      (princ "Top 20 Slowest Packages:\n\n")
      (cl-loop for (pkg . time) in (cl-subseq sorted-packages 0 (min 20 (length sorted-packages)))
               for i from 1
               do (princ (format "%2d. %-30s %.3fs\n" i pkg time))))))

(provide 'emacs-ide-package)
;;; emacs-ide-package.el ends here
