;;; early-init.el --- Enhanced Early Initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; This file is loaded before init.el and package.el initialization.
;;; Used for performance optimizations and GUI tweaks.
;;; Save as: ~/.emacs.d/early-init.el
;;;
;;; Code:

;; ============================================================================
;; GARBAGE COLLECTION OPTIMIZATION
;; ============================================================================
;; Maximize garbage collection threshold during startup
;; This dramatically improves startup time
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; ============================================================================
;; PACKAGE INITIALIZATION
;; ============================================================================
;; Prevent package.el from loading before init.el
;; We'll initialize it manually for better control
(setq package-enable-at-startup nil)

;; ============================================================================
;; UI OPTIMIZATION (Before Frame Creation)
;; ============================================================================
;; Prevent unwanted runtime frame resizing
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here before they've been initialized
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Start maximized (optional - remove if you prefer default size)
;; (push '(fullscreen . maximized) default-frame-alist)

;; ============================================================================
;; WAYLAND/X11 SPECIFIC OPTIMIZATIONS
;; ============================================================================
(when (getenv "WAYLAND_DISPLAY")
  ;; Wayland-specific: disable double buffering for better performance
  (push '(inhibit-double-buffering . t) default-frame-alist)
  
  ;; Set frame size for Wayland
  (push '(width . 140) default-frame-alist)
  (push '(height . 40) default-frame-alist))

(unless (getenv "WAYLAND_DISPLAY")
  ;; X11 frame size
  (push '(width . 140) default-frame-alist)
  (push '(height . 40) default-frame-alist))

;; ============================================================================
;; NATIVE COMPILATION (Emacs 28+)
;; ============================================================================
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Silence compiler warnings from native compilation
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-warning-on-missing-source nil
        native-comp-deferred-compilation t
        native-comp-speed 2)
  
  ;; Set native-comp cache directory
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))

;; ============================================================================
;; FILE NAME HANDLER OPTIMIZATION
;; ============================================================================
;; Reduce file-name-handler-alist during startup for speed
;; We'll restore it after init in init.el
(defvar emacs-ide--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist emacs-ide--file-name-handler-alist)))

;; ============================================================================
;; MISC OPTIMIZATIONS
;; ============================================================================
;; Increase amount of data Emacs reads from processes
(setq read-process-output-max (* 3 1024 1024)) ; 3MB

;; Don't compact font caches during GC
(setq inhibit-compacting-font-caches t)

;; Faster scrolling
(setq fast-but-imprecise-scrolling t)

;; Don't fontify during input for better performance
(setq redisplay-skip-fontification-on-input t)

;; ============================================================================
;; SITE-LISP OPTIMIZATION
;; ============================================================================
;; Prevent unwanted runtime compilation
(setq site-run-file nil)

;; ============================================================================
;; PGTK-SPECIFIC SETTINGS (Pure GTK - Wayland native)
;; ============================================================================
(when (eq window-system 'pgtk)
  ;; Better scaling
  (setq pgtk-use-im-context-on-new-connection t)
  
  ;; Smoother scrolling
  (setq pixel-scroll-precision-mode t))

;; ============================================================================
;; INITIAL FRAME SETTINGS
;; ============================================================================
;; Set a nice initial frame background (prevents white flash)
(set-face-attribute 'default nil
                    :background "#282c34"
                    :foreground "#bbc2cf")

;; ============================================================================
;; REDUCE STARTUP NOISE
;; ============================================================================
;; Disable warnings during early init
(setq warning-minimum-level :error)

;; ============================================================================
;; TREESIT PREPARATION
;; ============================================================================
;; Set tree-sitter grammar directory early
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (setq treesit-extra-load-path
        (list (expand-file-name "tree-sitter" user-emacs-directory))))

;; ============================================================================
;; EARLY CUSTOM FILE PREVENTION
;; ============================================================================
;; Prevent custom.el from loading during early-init
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'early-init)
;;; early-init.el ends here