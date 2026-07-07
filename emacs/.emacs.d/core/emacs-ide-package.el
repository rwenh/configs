;;; emacs-ide-package.el --- Package Management Utilities -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(require 'cl-lib)

;;;; ── State ───────────────────────────────────────────────────────────────────

(defvar emacs-ide-package-load-times
  (make-hash-table :test 'eq)
  "Hash table mapping package-name symbols to their load times in seconds.
Uses :test 'eq because keys are always interned symbols.")

(defvar emacs-ide-package-slow-threshold 0.1
  "Load time in seconds above which a package is considered slow.")

(defconst emacs-ide-package-report-top-n 20
  "Number of slowest packages shown in `emacs-ide-package-report'.")

(defvar emacs-ide-package-warn-slow nil
  "When non-nil, emit a `warn'-level alert for slow package loads.
Default nil — slow first-loads are expected and not actionable.
Set to t if you want *Warnings* buffer entries for slow packages.")

;;;; ── Tracking ────────────────────────────────────────────────────────────────

(defun emacs-ide-package-clear-times ()
  "Clear the package load-time table."
  (interactive)
  (clrhash emacs-ide-package-load-times)
  (message "✓ Package load-time table cleared"))

(emacs-ide-package-clear-times)

(defun emacs-ide-package-track-load (orig-fun &rest args)
  "Advice around `require' to track how long each package takes to load."
  (let ((package-name (car args)))
    (if (not (symbolp package-name))
        (apply orig-fun args)
      (let* ((already-loaded (featurep package-name))
             (start-time     (current-time))
             (result         (apply orig-fun args))
             (elapsed        (float-time
                              (time-subtract (current-time) start-time))))
        (unless already-loaded
          (puthash package-name elapsed emacs-ide-package-load-times)
          (when (> elapsed emacs-ide-package-slow-threshold)
            (if emacs-ide-package-warn-slow
                (warn "Package %s took %.2fs to load" package-name elapsed)
              (message "⏱ Package %s: %.2fs (slow)" package-name elapsed))))
        result))))

(unless (advice-member-p #'emacs-ide-package-track-load 'require)
  (advice-add 'require :around #'emacs-ide-package-track-load))

;;;; ── Report ──────────────────────────────────────────────────────────────────

(defun emacs-ide-package-report ()
  "Display a buffer showing the slowest packages by load time."
  (interactive)
  (let* ((pairs (cl-loop for k being the hash-keys of emacs-ide-package-load-times
                         collect (cons k (gethash k emacs-ide-package-load-times))))
         (sorted      (cl-sort pairs #'> :key #'cdr))
         (total-count (length sorted))
         (total-time  (cl-reduce #'+ sorted :key #'cdr :initial-value 0.0)))
    (with-output-to-temp-buffer "*Package Load Times*"
      (princ "=== PACKAGE LOAD TIMES ===\n\n")
      (princ (format "Tracked: %d packages | Cumulative load time: %.3fs\n"
                     total-count total-time))
      (princ (format "Slow threshold: %.2fs | Showing top %d\n\n"
                     emacs-ide-package-slow-threshold
                     emacs-ide-package-report-top-n))
      (if (null sorted)
          (princ "No packages tracked yet.\n")
        (princ (format "Top %d Slowest Packages:\n\n"
                       emacs-ide-package-report-top-n))
        (cl-loop for (pkg . time) in sorted
                 for i from 1 to emacs-ide-package-report-top-n
                 do (princ (format "%2d. %-32s %.3fs %s\n"
                                   i pkg time
                                   (if (> time emacs-ide-package-slow-threshold)
                                       "⚠" ""))))
        (princ (format "\nNote: first-boot loads are slow due to byte-compilation.\n"))
        (princ (format "      Subsequent starts use cache and are typically <1ms.\n"))))))

;;;; ── Config integration ──────────────────────────────────────────────────────

(with-eval-after-load 'emacs-ide-config
  (let ((threshold (and (fboundp 'emacs-ide-config-get)
                        (emacs-ide-config-get 'performance
                                              'slow-package-threshold
                                              nil))))
    (when (and threshold (numberp threshold) (> threshold 0))
      (setq emacs-ide-package-slow-threshold threshold))))

(provide 'emacs-ide-package)
;;; emacs-ide-package.el ends here
