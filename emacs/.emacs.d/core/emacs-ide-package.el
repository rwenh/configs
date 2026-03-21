;;; emacs-ide-package.el --- Package Management Utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Utilities for package load-time tracking and reporting.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.2 to 3.0.4.
;;;   - FIX-NON-SYMBOL: emacs-ide-package-track-load now guards against
;;;     non-symbol package-name before calling featurep. Passing a string
;;;     to featurep throws wrong-type-argument symbolp.
;;;   - FIX-TOP-N: Top-N count (20) extracted to a constant so the subseq
;;;     limit and the report header string can never drift apart.
;;;   - FIX-CLEAR: emacs-ide-package-clear-times command added to reset the
;;;     hash table between reloads. Also called automatically on each fresh
;;;     load so reload reports never mix stale and fresh data.
;;;   - FIX-THRESHOLD: emacs-ide-package-slow-threshold lowered to 0.1s to
;;;     align with use-package-minimum-reported-time in init.el and to catch
;;;     meaningful bottlenecks in a sub-2s startup budget.
;;;   - FIX-SUBSEQ: cl-subseq replaced with cl-loop :to limit — avoids
;;;     allocating a second list for the top-N slice.
;;;   - FIX-SUMMARY: Report now includes a summary header line showing total
;;;     tracked package count and cumulative load time.
;;;   - FIX-CONFIG-WIRE: emacs-ide-package-slow-threshold is now read from
;;;     config.yml performance.slow-package-threshold at load time via
;;;     emacs-ide-config-get when the config module is available.
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

;; FIX-THRESHOLD: Lowered from 0.5s to 0.1s to align with
;; use-package-minimum-reported-time in init.el. In a sub-2s startup,
;; 0.5s was too coarse to catch meaningful bottlenecks.
;; FIX-CONFIG-WIRE: This value is overridden by config.yml
;; performance.slow-package-threshold after emacs-ide-config loads.
(defvar emacs-ide-package-slow-threshold 0.1
  "Warn if package takes longer than this many seconds to load.")

;; FIX-TOP-N: Single source of truth for the report top-N limit.
(defconst emacs-ide-package-report-top-n 20
  "Number of slowest packages shown in the load-time report.")

(defun emacs-ide-package-clear-times ()
  "Clear the package load-time table.
FIX-CLEAR: Call this before a fresh load to prevent stale data from
a prior session mixing with new timings in the report."
  (interactive)
  (clrhash emacs-ide-package-load-times)
  (message "✓ Package load-time table cleared"))

;; FIX-CLEAR: Clear automatically on each load of this module so that
;; M-x emacs-ide-config-reload always produces a clean report.
(emacs-ide-package-clear-times)

(defun emacs-ide-package-track-load (orig-fun &rest args)
  "Advice to track package load time around ORIG-FUN with ARGS.
Only records timing on the FIRST load of each feature.
FIX-NON-SYMBOL: Guards against non-symbol package-name before calling
featurep — (require) can receive a string in some edge cases and
featurep requires a symbol, throwing wrong-type-argument otherwise."
  (let* ((package-name (car args)))
    (if (not (symbolp package-name))
        ;; Non-symbol: pass through without tracking
        (apply orig-fun args)
      (let* ((already-loaded (featurep package-name))
             (start-time (current-time))
             (result (apply orig-fun args))
             (elapsed (float-time (time-subtract (current-time) start-time))))
        (unless already-loaded
          (puthash package-name elapsed emacs-ide-package-load-times)
          (when (> elapsed emacs-ide-package-slow-threshold)
            (warn "Package %s took %.2fs to load" package-name elapsed)))
        result))))

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
         ;; FIX-SUMMARY: compute totals for the header line
         (total-count (length sorted))
         (total-time  (cl-reduce #'+ sorted :key #'cdr :initial-value 0.0)))
    (with-output-to-temp-buffer "*Package Load Times*"
      (princ "=== PACKAGE LOAD TIMES ===\n\n")
      ;; FIX-SUMMARY: summary line with total count and cumulative time
      (princ (format "Tracked: %d packages | Cumulative load time: %.3fs\n"
                     total-count total-time))
      (princ (format "Threshold: %.2fs | Showing top %d\n\n"
                     emacs-ide-package-slow-threshold
                     emacs-ide-package-report-top-n))
      ;; FIX-TOP-N: use constant; FIX-SUBSEQ: cl-loop :to avoids extra allocation
      (princ (format "Top %d Slowest Packages:\n\n" emacs-ide-package-report-top-n))
      (cl-loop for (pkg . time) in sorted
               for i from 1 to emacs-ide-package-report-top-n
               do (princ (format "%2d. %-30s %.3fs%s\n"
                                 i pkg time
                                 (if (> time emacs-ide-package-slow-threshold)
                                     " ⚠" "")))))))

;; ============================================================================
;; CONFIG WIRING
;; FIX-CONFIG-WIRE: Read performance.slow-package-threshold from config.yml
;; after emacs-ide-config has loaded. Uses with-eval-after-load so this works
;; regardless of whether emacs-ide-config was loaded before or after this file.
;; ============================================================================
(with-eval-after-load 'emacs-ide-config
  (let ((threshold (and (fboundp 'emacs-ide-config-get)
                        (emacs-ide-config-get 'performance
                                              'slow-package-threshold
                                              nil))))
    (when (and threshold (numberp threshold) (> threshold 0))
      (setq emacs-ide-package-slow-threshold threshold))))

(provide 'emacs-ide-package)
;;; emacs-ide-package.el ends here
