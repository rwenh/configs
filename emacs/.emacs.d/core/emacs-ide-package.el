;;; emacs-ide-package.el --- Package Management Utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Utilities for package load-time tracking and reporting.
;;; Version: 2.2.2
;;; Fixes vs 2.2.1:
;;;   - C-18 (HIGH): (advice-add 'require :around ...) was called unconditionally
;;;     at top level. Every reload of this module (via M-x emacs-ide-config-reload
;;;     or any code that re-requires it) stacked another identical advice layer on
;;;     `require`. After N reloads, every (require ...) call traversed N+1 advice
;;;     wrappers, multiplying tracking overhead and writing N hash entries per
;;;     require call.
;;;     Fix: guard with (advice-member-p) before adding. This is the standard
;;;     pattern for idempotent advice installation in Emacs Lisp.
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-package-load-times (make-hash-table :test 'equal)
  "Hash table tracking package load times.")

(defvar emacs-ide-package-slow-threshold 0.5
  "Warn if package takes longer than this (seconds).")

(defun emacs-ide-package-track-load (orig-fun &rest args)
  "Advice to track package load time around ORIG-FUN with ARGS.
Only records timing on the FIRST load of each feature."
  (let* ((package-name (car args))
         (already-loaded (featurep package-name))
         (start-time (current-time))
         (result (apply orig-fun args))
         (elapsed (float-time (time-subtract (current-time) start-time))))
    (unless already-loaded
      (puthash package-name elapsed emacs-ide-package-load-times)
      (when (> elapsed emacs-ide-package-slow-threshold)
        (warn "Package %s took %.2fs to load" package-name elapsed)))
    result))

;; C-18 FIX: Guard advice installation so reloading this module does not
;; stack additional copies of the advice on top of the existing one.
;; (advice-member-p) returns non-nil if the function is already advised
;; by this exact symbol, making this safe to call multiple times.
(unless (advice-member-p #'emacs-ide-package-track-load 'require)
  (advice-add 'require :around #'emacs-ide-package-track-load))

(defun emacs-ide-package-report ()
  "Show package load time report."
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
