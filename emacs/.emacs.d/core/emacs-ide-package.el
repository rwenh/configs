;;; emacs-ide-package.el --- Package Management Utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Utilities for package load-time tracking and reporting.
;;; Version: 2.2.1
;;; Fixes:
;;;   - `hash-table-to-list` does not exist in Emacs 29 — replaced with
;;;     `cl-loop for k being the hash-keys ...` (same pattern used in telemetry)
;;; Code:

(require 'cl-lib)

;; Package load time tracking
(defvar emacs-ide-package-load-times (make-hash-table :test 'equal)
  "Hash table tracking package load times.")

(defvar emacs-ide-package-slow-threshold 0.5
  "Warn if package takes longer than this (seconds).")

(defun emacs-ide-package-track-load (orig-fun &rest args)
  "Advice to track package load time around ORIG-FUN with ARGS.
FIX: Only records timing on the FIRST load of each feature.
Previously always overwrote the hash entry, so a second
\(require 'foo) — a no-op at ~0ms — silently replaced the
real load time with near-zero, corrupting the report."
  (let* ((package-name (car args))
         ;; Snapshot loaded state BEFORE calling orig-fun
         (already-loaded (featurep package-name))
         (start-time (current-time))
         (result (apply orig-fun args))
         (elapsed (float-time (time-subtract (current-time) start-time))))
    (unless already-loaded
      (puthash package-name elapsed emacs-ide-package-load-times)
      (when (> elapsed emacs-ide-package-slow-threshold)
        (warn "Package %s took %.2fs to load" package-name elapsed)))
    result))

;; Install advice
(advice-add 'require :around #'emacs-ide-package-track-load)

(defun emacs-ide-package-report ()
  "Show package load time report.
FIX: `hash-table-to-list` does not exist in Emacs 29.
     Use `cl-loop for k being the hash-keys` instead."
  (interactive)
  (let* ((pairs (cl-loop for k being the hash-keys of emacs-ide-package-load-times
                          collect (cons k (gethash k emacs-ide-package-load-times))))
         (sorted (cl-sort pairs #'> :key #'cdr))
         (top (cl-subseq sorted 0 (min 20 (length sorted)))))
    (with-output-to-temp-buffer "*Package Load Times*"
      (princ "=== PACKAGE LOAD TIMES ===\n\n")
      (princ "Top 20 Slowest Packages:\n\n")
      (cl-loop for (pkg . time) in top
               for i from 1
               do (princ (format "%2d. %-30s %.3fs\n" i pkg time))))))

(provide 'emacs-ide-package)
;;; emacs-ide-package.el ends here
