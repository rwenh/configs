;;; early-init.el --- Maximum Performance Early Init -*- lexical-binding: t -*-
;;; Commentary:
;;; Hyper-optimized early initialization for sub-second startup
;;; Code:

;; ============================================================================
;; AGGRESSIVE GC OPTIMIZATION
;; ============================================================================
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

(defvar emacs-ide--gc-timer nil)
(defvar emacs-ide--file-name-handler-alist file-name-handler-alist)

;; ============================================================================
;; DISABLE PACKAGE.EL EARLY
;; ============================================================================
(setq package-enable-at-startup nil
      package-quickstart nil)

;; ============================================================================
;; FRAME OPTIMIZATION - PREVENT FLASHING
;; ============================================================================
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
        (alpha-background . 100)))

;; ============================================================================
;; UI ELEMENTS - DISABLE BEFORE INIT
;; ============================================================================
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
(push '(undecorated . nil) default-frame-alist)

;; Start maximized
(push '(fullscreen . maximized) default-frame-alist)

;; ============================================================================
;; WAYLAND/PGTK OPTIMIZATIONS
;; ============================================================================
(when (eq window-system 'pgtk)
  (push '(inhibit-double-buffering . t) default-frame-alist)
  (setq pgtk-wait-for-event-timeout 0.001
        pgtk-use-im-context-on-new-connection t))

(when (getenv "WAYLAND_DISPLAY")
  (setq x-wait-for-event-timeout 0.001))

;; ============================================================================
;; NATIVE COMPILATION - SILENT & FAST
;; ============================================================================
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
        native-comp-warning-on-missing-source nil
        native-comp-deferred-compilation t
        native-comp-speed 3
        native-comp-async-jobs-number 4
        native-comp-always-compile t
        package-native-compile t)
  
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))

;; ============================================================================
;; FILE HANDLER OPTIMIZATION
;; ============================================================================
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist emacs-ide--file-name-handler-alist)
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)
            (setq emacs-ide--gc-timer
                  (run-with-idle-timer 15 t #'garbage-collect))
            (garbage-collect)) 100)

;; ============================================================================
;; PROCESS & I/O OPTIMIZATION
;; ============================================================================
(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil)

;; ============================================================================
;; REDISPLAY OPTIMIZATION
;; ============================================================================
(setq inhibit-compacting-font-caches t
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      highlight-nonselected-windows nil
      command-line-x-option-alist nil)

;; ============================================================================
;; SITE-LISP OPTIMIZATION
;; ============================================================================
(setq site-run-file nil
      inhibit-default-init t)

;; ============================================================================
;; INITIAL APPEARANCE
;; ============================================================================
(set-face-attribute 'default nil
                    :background "#1e1e2e"
                    :foreground "#cdd6f4")

(set-face-attribute 'mode-line nil
                    :background "#313244"
                    :foreground "#cdd6f4")

;; ============================================================================
;; REDUCE STARTUP NOISE
;; ============================================================================
(setq warning-minimum-level :emergency
      byte-compile-warnings nil
      native-comp-async-report-warnings-errors nil)

;; ============================================================================
;; TREESIT PREPARATION
;; ============================================================================
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (setq treesit-font-lock-level 4
        treesit-extra-load-path
        (list (expand-file-name "tree-sitter" user-emacs-directory))))

;; ============================================================================
;; CUSTOM FILE EARLY SETUP
;; ============================================================================
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; ============================================================================
;; JIT LOCK OPTIMIZATION
;; ============================================================================
(setq jit-lock-defer-time 0
      jit-lock-stealth-time 1
      jit-lock-stealth-nice 0.1
      jit-lock-chunk-size 1000)

;; ============================================================================
;; AUTO-SAVE & BACKUP EARLY DISABLE
;; ============================================================================
(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles nil
      auto-save-list-file-prefix nil)

(provide 'early-init)
;;; early-init.el ends here