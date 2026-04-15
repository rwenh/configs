;;; early-init.el --- Enterprise Emacs IDE Early Initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; Production-grade early initialization with performance monitoring.
;;; Version: 3.0.4
;;; Code:

(defvar emacs-ide--early-init-start-time (current-time)
  "Time when early-init started.")

(defvar emacs-ide--early-init-benchmark-data nil
  "Benchmark data for early-init phases.")

(defun emacs-ide--benchmark-phase (phase-name body-fn)
  "Benchmark PHASE-NAME executing BODY-FN."
  (let ((start (current-time)))
    (funcall body-fn)
    (let ((elapsed (float-time (time-subtract (current-time) start))))
      (push (cons phase-name elapsed) emacs-ide--early-init-benchmark-data)
      (when (> elapsed 0.25)
        (warn "Early-init phase '%s' took %.3fs (slow)" phase-name elapsed)))))

(defun emacs-ide--get-processor-count ()
  "Get number of CPU cores safely."
  (cond
   ((fboundp 'num-processors) (num-processors))
   (t
    (let* ((env-count (getenv "NUMBER_OF_PROCESSORS"))
           (count (if env-count (condition-case nil (string-to-number env-count) (error 0)) 0)))
      (if (> count 0) count
        (condition-case nil
            (string-to-number (string-trim (shell-command-to-string "nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4")))
          (error 4)))))))

(defvar emacs-ide-processor-count (emacs-ide--get-processor-count)
  "Number of CPU cores — cached at early-init time.")

(defvar emacs-ide--saved-byte-compile-warnings nil)
(defvar emacs-ide--saved-warning-suppress-log-types nil)

(emacs-ide--benchmark-phase "gc-setup"
  (lambda ()
    (setq gc-cons-threshold most-positive-fixnum
          gc-cons-percentage 1.0)))

(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

(defvar emacs-ide--gc-timer nil)
(defvar emacs-ide--file-name-handler-alist file-name-handler-alist)

(emacs-ide--benchmark-phase "package-disable"
  (lambda ()
    (setq package-enable-at-startup nil
          package-quickstart nil)))

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
            (internal-border-width . 0)))))

(emacs-ide--benchmark-phase "ui-disable"
  (lambda ()
    (when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
    (when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))))

(emacs-ide--benchmark-phase "wayland-optimization"
  (lambda ()
    (when (eq window-system 'pgtk)
      (let ((desktop (or (getenv "XDG_CURRENT_DESKTOP") "")))
        (when (string-match-p "\\(GNOME\\|Unity\\|Budgie\\)" desktop)
          (push '(inhibit-double-buffering . t) default-frame-alist)))
      (setq pgtk-wait-for-event-timeout 0.001))
    (when (getenv "WAYLAND_DISPLAY")
      (setq x-wait-for-event-timeout 0.001))))

(emacs-ide--benchmark-phase "warning-suppression"
  (lambda ()
    (setq emacs-ide--saved-byte-compile-warnings
          (if (boundp 'byte-compile-warnings) byte-compile-warnings t))
    (setq warning-minimum-level :emergency
          byte-compile-warnings nil)
    (when (boundp 'native-comp-async-report-warnings-errors)
      (setq native-comp-async-report-warnings-errors 'silent))
    (unless (boundp 'warning-suppress-log-types)
      (defvar warning-suppress-log-types nil))
    (add-to-list 'warning-suppress-log-types '(defvaralias))))

(emacs-ide--benchmark-phase "native-comp-setup"
  (lambda ()
    (when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
      (setq native-comp-async-report-warnings-errors 'silent
            native-comp-speed 2
            native-comp-async-jobs-number (max 1 (/ (or emacs-ide-processor-count 4) 2)))
      (when (boundp 'native-comp-jit-compilation)
        (setq native-comp-jit-compilation t))
      (when (fboundp 'startup-redirect-eln-cache)
        (startup-redirect-eln-cache
         (convert-standard-filename
          (expand-file-name "var/eln-cache/" user-emacs-directory)))))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist emacs-ide--file-name-handler-alist
                  gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)
            (unless emacs-ide--gc-timer
              (setq emacs-ide--gc-timer (run-with-idle-timer 15 t #'garbage-collect))))
          100)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq warning-minimum-level :warning)
            (when (boundp 'emacs-ide--saved-byte-compile-warnings)
              (setq byte-compile-warnings emacs-ide--saved-byte-compile-warnings))
            (when (boundp 'emacs-ide--saved-warning-suppress-log-types)
              (setq warning-suppress-log-types emacs-ide--saved-warning-suppress-log-types)))
          90)

(add-hook 'kill-emacs-hook
          (lambda ()
            (when emacs-ide--gc-timer
              (cancel-timer emacs-ide--gc-timer))))

(emacs-ide--benchmark-phase "io-optimization"
  (lambda ()
    (setq read-process-output-max (* 4 1024 1024)
          process-adaptive-read-buffering nil)))

(emacs-ide--benchmark-phase "redisplay-optimization"
  (lambda ()
    (setq inhibit-compacting-font-caches t
          fast-but-imprecise-scrolling t
          redisplay-skip-fontification-on-input t
          highlight-nonselected-windows nil
          scroll-error-top-bottom t)))

(emacs-ide--benchmark-phase "site-lisp-disable"
  (lambda ()
    (setq inhibit-default-init t)))

(emacs-ide--benchmark-phase "initial-theme"
  (lambda ()
    (set-face-attribute 'default nil
                        :background "#000000"
                        :foreground "#ffffff")
    (set-face-attribute 'mode-line nil
                        :background "#1e1e1e"
                        :foreground "#ffffff")))

(emacs-ide--benchmark-phase "treesit-setup"
  (lambda ()
    (when (and (fboundp 'treesit-available-p) (treesit-available-p))
      (setq treesit-font-lock-level 4
            treesit-extra-load-path
            (list (expand-file-name "var/tree-sitter" user-emacs-directory))))))

(emacs-ide--benchmark-phase "custom-file-setup"
  (lambda ()
    (setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))))

(emacs-ide--benchmark-phase "jit-lock-optimization"
  (lambda ()
    (setq jit-lock-defer-time 0.025
          jit-lock-stealth-time 1
          jit-lock-stealth-nice 0.1
          jit-lock-chunk-size 1000)))

(emacs-ide--benchmark-phase "backup-disable"
  (lambda ()
    (setq auto-save-default nil
          make-backup-files nil
          create-lockfiles nil
          auto-save-list-file-prefix
          (expand-file-name "var/auto-save-list/.saves-" user-emacs-directory))))

(defun emacs-ide-early-init-report ()
  "Display early-init benchmark report."
  (interactive)
  (let ((total-time (float-time (time-subtract (current-time) emacs-ide--early-init-start-time))))
    (with-output-to-temp-buffer "*Early-Init Benchmark*"
      (princ (format "=== EARLY-INIT PERFORMANCE REPORT ===\n\n"))
      (princ (format "Total Time: %.3fs\n\n" total-time))
      (princ "Phase Breakdown:\n")
      (dolist (phase (reverse emacs-ide--early-init-benchmark-data))
        (princ (format "  %-25s %.3fs (%.1f%%)\n"
                       (car phase)
                       (cdr phase)
                       (if (zerop total-time) 0.0 (* 100 (/ (cdr phase) total-time)))))))))

(defvar emacs-ide-wayland-p (and (getenv "WAYLAND_DISPLAY") t))
(defvar emacs-ide-display-server
  (cond (emacs-ide-wayland-p "Wayland")
        ((getenv "DISPLAY") "X11")
        (t "TTY")))

(emacs-ide--benchmark-phase "tls-security"
  (lambda ()
    (require 'gnutls)
    (setq gnutls-verify-error t
          gnutls-min-prime-bits 3072
          tls-checktrust t
          tls-program '("gnutls-cli --x509cafile %t -p %p %h"))))

(emacs-ide--benchmark-phase "file-handler-disable"
  (lambda ()
    (setq file-name-handler-alist nil)))

(defvar emacs-ide-safe-mode nil)
(when (or (getenv "EMACS_SAFE_MODE") (member "--emacs-ide-safe" command-line-args-left))
  (setq emacs-ide-safe-mode t)
  (setq command-line-args-left (delete "--emacs-ide-safe" command-line-args-left))
  (message "🛡️  EMACS IDE SAFE MODE ENABLED - Minimal configuration"))

(provide 'early-init)
;;; early-init.el ends here
