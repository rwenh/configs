;;; early-init.el --- Enterprise Emacs IDE Early Initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; Production-grade early initialization with performance monitoring
;;; Author: Enterprise Emacs Team
;;; Version: 2.0.0
;;; Code:

;; ============================================================================
;; PERFORMANCE TRACKING - START
;; ============================================================================
(defvar emacs-ide--init-start-time (current-time)
  "Time when early-init started.")

(defvar emacs-ide--early-init-benchmark-data nil
  "Benchmark data for early-init phases.")

(defun emacs-ide--benchmark-phase (phase-name body-fn)
  "Benchmark PHASE-NAME executing BODY-FN."
  (let ((start (current-time)))
    (funcall body-fn)
    (let ((elapsed (float-time (time-subtract (current-time) start))))
      (push (cons phase-name elapsed) emacs-ide--early-init-benchmark-data)
      (when (> elapsed 0.1)
        (warn "Early-init phase '%s' took %.3fs (threshold: 0.1s)" phase-name elapsed)))))

;; ============================================================================
;; GARBAGE COLLECTION - AGGRESSIVE OPTIMIZATION
;; ============================================================================
(emacs-ide--benchmark-phase "gc-setup"
  (lambda ()
    (setq gc-cons-threshold most-positive-fixnum
          gc-cons-percentage 1.0)))

(defvar emacs-ide--gc-timer nil
  "Timer for idle garbage collection.")

(defvar emacs-ide--file-name-handler-alist file-name-handler-alist
  "Backup of file-name-handler-alist.")

;; ============================================================================
;; PACKAGE.EL - DISABLE EARLY
;; ============================================================================
(emacs-ide--benchmark-phase "package-disable"
  (lambda ()
    (setq package-enable-at-startup nil
          package-quickstart nil)))

;; ============================================================================
;; FRAME OPTIMIZATION - PREVENT FLASHING
;; ============================================================================
(emacs-ide--benchmark-phase "frame-optimization"
  (lambda ()
    (setq frame-inhibit-implied-resize t
          frame-resize-pixelwise t
          window-resize-pixelwise t
          default-frame-alist
          '((min-height . 1)
            (min-width . 1)
            (vertical-scroll-bars . nil)
            (horizontal-scroll-bars . nil)
            (menu-bar-lines . 0)
            (tool-bar-lines . 0)
            (left-fringe . 8)
            (right-fringe . 8)
            (alpha-background . 100)
            (fullscreen . maximized)
            (internal-border-width . 0)
            (undecorated . nil)))))

;; ============================================================================
;; UI ELEMENTS - DISABLE BEFORE INIT
;; ============================================================================
(emacs-ide--benchmark-phase "ui-disable"
  (lambda ()
    (push '(menu-bar-lines . 0) default-frame-alist)
    (push '(tool-bar-lines . 0) default-frame-alist)
    (push '(vertical-scroll-bars) default-frame-alist)
    (push '(horizontal-scroll-bars) default-frame-alist)))

;; ============================================================================
;; WAYLAND/PGTK OPTIMIZATIONS
;; ============================================================================
(emacs-ide--benchmark-phase "wayland-optimization"
  (lambda ()
    (when (eq window-system 'pgtk)
      (push '(inhibit-double-buffering . t) default-frame-alist)
      (setq pgtk-wait-for-event-timeout 0.001
            pgtk-use-im-context-on-new-connection t))
    
    (when (getenv "WAYLAND_DISPLAY")
      (setq x-wait-for-event-timeout 0.001))))

;; ============================================================================
;; NATIVE COMPILATION - SILENT & OPTIMIZED
;; ============================================================================
(emacs-ide--benchmark-phase "native-comp-setup"
  (lambda ()
    (when (and (fboundp 'native-comp-available-p)
               (native-comp-available-p))
      (setq native-comp-async-report-warnings-errors nil
            native-comp-warning-on-missing-source nil
            native-comp-deferred-compilation t
            native-comp-speed 3
            native-comp-async-jobs-number (max 1 (/ (num-processors) 2))
            native-comp-always-compile t
            package-native-compile t)
      
      (when (fboundp 'startup-redirect-eln-cache)
        (startup-redirect-eln-cache
         (convert-standard-filename
          (expand-file-name "var/eln-cache/" user-emacs-directory)))))))

;; ============================================================================
;; FILE HANDLER OPTIMIZATION
;; ============================================================================
(emacs-ide--benchmark-phase "file-handler-disable"
  (lambda ()
    (setq file-name-handler-alist nil)))

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (emacs-ide--benchmark-phase "post-startup-restore"
              (lambda ()
                (setq file-name-handler-alist emacs-ide--file-name-handler-alist)
                
                ;; Restore GC to reasonable values
                (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                      gc-cons-percentage 0.1)
                
                ;; Setup idle GC
                (setq emacs-ide--gc-timer
                      (run-with-idle-timer 15 t #'garbage-collect))
                
                (garbage-collect))))
          100)

;; ============================================================================
;; PROCESS & I/O OPTIMIZATION
;; ============================================================================
(emacs-ide--benchmark-phase "io-optimization"
  (lambda ()
    (setq read-process-output-max (* 4 1024 1024)  ; 4MB
          process-adaptive-read-buffering nil)))

;; ============================================================================
;; REDISPLAY OPTIMIZATION
;; ============================================================================
(emacs-ide--benchmark-phase "redisplay-optimization"
  (lambda ()
    (setq inhibit-compacting-font-caches t
          fast-but-imprecise-scrolling t
          redisplay-skip-fontification-on-input t
          highlight-nonselected-windows nil
          command-line-x-option-alist nil)))

;; ============================================================================
;; SITE-LISP OPTIMIZATION
;; ============================================================================
(emacs-ide--benchmark-phase "site-lisp-disable"
  (lambda ()
    (setq site-run-file nil
          inhibit-default-init t)))

;; ============================================================================
;; INITIAL APPEARANCE - CATPPUCCIN COLORS
;; ============================================================================
(emacs-ide--benchmark-phase "initial-theme"
  (lambda ()
    (set-face-attribute 'default nil
                        :background "#1e1e2e"
                        :foreground "#cdd6f4")
    
    (set-face-attribute 'mode-line nil
                        :background "#313244"
                        :foreground "#cdd6f4")))

;; ============================================================================
;; REDUCE STARTUP NOISE
;; ============================================================================
(emacs-ide--benchmark-phase "warning-suppression"
  (lambda ()
    (setq warning-minimum-level :emergency
          byte-compile-warnings nil
          native-comp-async-report-warnings-errors nil)))

;; ============================================================================
;; TREESIT PREPARATION
;; ============================================================================
(emacs-ide--benchmark-phase "treesit-setup"
  (lambda ()
    (when (and (fboundp 'treesit-available-p)
               (treesit-available-p))
      (setq treesit-font-lock-level 4
            treesit-extra-load-path
            (list (expand-file-name "var/tree-sitter" user-emacs-directory))))))

;; ============================================================================
;; CUSTOM FILE EARLY SETUP
;; ============================================================================
(emacs-ide--benchmark-phase "custom-file-setup"
  (lambda ()
    (setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))))

;; ============================================================================
;; JIT LOCK OPTIMIZATION
;; ============================================================================
(emacs-ide--benchmark-phase "jit-lock-optimization"
  (lambda ()
    (setq jit-lock-defer-time 0
          jit-lock-stealth-time 1
          jit-lock-stealth-nice 0.1
          jit-lock-chunk-size 1000)))

;; ============================================================================
;; AUTO-SAVE & BACKUP EARLY DISABLE
;; ============================================================================
(emacs-ide--benchmark-phase "backup-disable"
  (lambda ()
    (setq auto-save-default nil
          make-backup-files nil
          create-lockfiles nil
          auto-save-list-file-prefix nil)))

;; ============================================================================
;; EARLY INIT BENCHMARK REPORT
;; ============================================================================
(defun emacs-ide-early-init-report ()
  "Display early-init benchmark report."
  (interactive)
  (let ((total-time (float-time (time-subtract (current-time)
                                               emacs-ide--init-start-time))))
    (with-output-to-temp-buffer "*Early-Init Benchmark*"
      (princ (format "=== EARLY-INIT PERFORMANCE REPORT ===\n\n"))
      (princ (format "Total Time: %.3fs\n\n" total-time))
      (princ "Phase Breakdown:\n")
      (dolist (phase (reverse emacs-ide--early-init-benchmark-data))
        (princ (format "  %-25s %.3fs (%.1f%%)\n"
                      (car phase)
                      (cdr phase)
                      (* 100 (/ (cdr phase) total-time)))))
      (princ "\n")
      (when (> total-time 0.5)
        (princ "⚠️  WARNING: Early-init took longer than expected (target: <0.5s)\n")))))

;; ============================================================================
;; ENVIRONMENT DETECTION
;; ============================================================================
(defvar emacs-ide-wayland-p (getenv "WAYLAND_DISPLAY")
  "Whether running on Wayland.")

(defvar emacs-ide-display-server
  (cond (emacs-ide-wayland-p "Wayland")
        ((getenv "DISPLAY") "X11")
        (t "TTY"))
  "Current display server.")

(defvar emacs-ide-processor-count
  (string-to-number
   (string-trim
    (or (getenv "NUMBER_OF_PROCESSORS")
        (shell-command-to-string
         "nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4"))))
  "Number of CPU cores.")

;; ============================================================================
;; SECURITY - EARLY TLS CONFIGURATION
;; ============================================================================
(emacs-ide--benchmark-phase "tls-security"
  (lambda ()
    (with-eval-after-load 'gnutls
      (setq gnutls-verify-error t
            gnutls-min-prime-bits 3072
            tls-checktrust t
            tls-program '("gnutls-cli --x509cafile %t -p %p %h")))))

;; ============================================================================
;; EMERGENCY RECOVERY MODE
;; ============================================================================
(defvar emacs-ide-safe-mode nil
  "If non-nil, boot in safe mode (minimal config).")

(when (or (getenv "EMACS_SAFE_MODE")
          (member "--safe" command-line-args))
  (setq emacs-ide-safe-mode t)
  (message "🛡️  EMACS IDE SAFE MODE ENABLED - Minimal configuration"))

(provide 'early-init)
;;; early-init.el ends here
