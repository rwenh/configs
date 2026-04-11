;;; emacs-ide-health.el --- Health Monitoring & Diagnostics -*- lexical-binding: t -*-
;;; Commentary:
;;; IDE health status checking, performance monitoring, and diagnostic reporting.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.5 to 3.0.4.
;;;   - FIX-REPEAT-KEYWORD: :repeat keyword in run-with-idle-timer is invalid.
;;;     run-with-idle-timer signature is (timer idle-secs repeat function &rest args).
;;;     The repeat value is a number (seconds), not a symbol. Fixed to use numeric
;;;     repetition interval instead of :repeat keyword.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; HEALTH CHECK STATE
;; ============================================================================

(defvar emacs-ide-health--check-interval 60
  "Interval (in seconds) for periodic health checks.")

(defvar emacs-ide-health--periodic-timer nil
  "Timer object for periodic health checks.")

(defvar emacs-ide-health--checks-run 0
  "Counter for total health checks run this session.")

(defvar emacs-ide-health--last-check-time nil
  "Timestamp of last health check completion.")

;; ============================================================================
;; CORE HEALTH CHECKS
;; ============================================================================

(defun emacs-ide-health-run-checks ()
  "Run all registered health checks."
  (interactive)
  (setq emacs-ide-health--checks-run (1+ emacs-ide-health--checks-run))
  (setq emacs-ide-health--last-check-time (current-time))
  
  ;; Check LSP status
  (when (bound-and-true-p emacs-ide-lsp-enable)
    (emacs-ide-health--check-lsp))
  
  ;; Check package performance
  (when (fboundp 'emacs-ide-package-slow-check)
    (emacs-ide-package-slow-check))
  
  ;; Check configuration validity
  (emacs-ide-health--check-config))

(defun emacs-ide-health--check-lsp ()
  "Check LSP server availability and status."
  (condition-case err
      (when (fboundp 'lsp-check-servers)
        (lsp-check-servers))
    (error (message "⚠️  LSP health check failed: %s" err))))

(defun emacs-ide-health--check-config ()
  "Validate current configuration integrity."
  (condition-case err
      (progn
        (unless (boundp 'emacs-ide-config-data)
          (message "⚠️  Configuration not loaded"))
        (unless (boundp 'emacs-ide-config-environment)
          (message "⚠️  Environment not detected")))
    (error (message "⚠️  Configuration health check failed: %s" err))))

;; ============================================================================
;; PERIODIC HEALTH MONITORING
;; ============================================================================

(defun emacs-ide-health--setup-periodic-checks ()
  "Setup periodic health check timer."
  (when emacs-ide-health--periodic-timer
    (cancel-timer emacs-ide-health--periodic-timer))
  
  ;; FIX-REPEAT-KEYWORD: Changed from invalid :repeat to numeric repeat interval
  ;; run-with-idle-timer signature: (timer idle-secs repeat function &rest args)
  ;; repeat is a number (seconds) for how often to repeat, not a keyword.
  (setq emacs-ide-health--periodic-timer
        (run-with-idle-timer emacs-ide-health--check-interval
                            emacs-ide-health--check-interval  ; repeat interval
                            #'emacs-ide-health-run-checks)))

;; Initialize periodic checks after startup
(add-hook 'after-init-hook #'emacs-ide-health--setup-periodic-checks)

;; ============================================================================
;; HEALTH STATUS REPORTING
;; ============================================================================

(defun emacs-ide-health-status ()
  "Display IDE health status."
  (interactive)
  (with-output-to-temp-buffer "*IDE Health*"
    (princ "=== EMACS IDE HEALTH STATUS ===\n\n")
    
    (princ (format "Checks run this session: %d\n" emacs-ide-health--checks-run))
    (when emacs-ide-health--last-check-time
      (princ (format "Last check: %s ago\n\n"
                     (format-time-string "%H:%M:%S"
                                         (time-subtract (current-time)
                                                        emacs-ide-health--last-check-time)))))
    
    (princ "Configuration:\n")
    (princ (format "  Environment: %s\n" emacs-ide-config-environment))
    (princ (format "  Config loaded: %s\n" emacs-ide-config-loaded-p))
    
    (when (bound-and-true-p emacs-ide-lsp-enable)
      (princ "\nLSP:\n")
      (princ (format "  Enabled: YES\n"))
      (when (fboundp 'emacs-ide-lsp-check-servers)
        (emacs-ide-lsp-check-servers)))
    
    (princ "\nPerformance:\n")
    (princ (format "  GC threshold: %d bytes\n" gc-cons-threshold))
    (princ (format "  Garbage collections: %d\n" gcs-done))))

(defun emacs-ide-health-check-all ()
  "Run all health checks and display results.
This is the main entry point expected by the diagnostics system."
  (interactive)
  (emacs-ide-health-run-checks)
  (emacs-ide-health-status))

(provide 'emacs-ide-health)
;;; emacs-ide-health.el ends here