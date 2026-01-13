;;; init.el --- Enhanced Modular Emacs IDE Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; A comprehensive, modular IDE configuration supporting 50+ languages
;;; with LSP, Tree-sitter, debugging, advanced code intelligence, and professional IDE features.
;;;
;;; Code:

;; ============================================================================
;; EARLY PERFORMANCE OPTIMIZATION
;; ============================================================================
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)
            (garbage-collect)
            (message "Emacs loaded in %.2fs with %d packages"
                     (float-time after-init-time)
                     (length package-activated-list))))

;; Suppress warnings
(setq byte-compile-warnings '(not obsolete docstrings constants)
      warning-suppress-types '((bytecomp) (emacs))
      warning-suppress-log-types '((bytecomp) (emacs))
      warning-minimum-level :error)

;; Native compilation settings
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-deferred-compilation t
        native-comp-speed 2))

;; Modern Emacs features
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode 1))
(when (fboundp 'global-subword-mode)
  (global-subword-mode 1))

;; ============================================================================
;; PACKAGE SYSTEM
;; ============================================================================
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t
      use-package-compute-statistics t)

;; ============================================================================
;; ESSENTIAL PACKAGES - ENHANCED
;; ============================================================================
(defvar emacs-ide-essential-packages
  '(;; UI & Themes
    modus-themes which-key breadcrumb dashboard all-the-icons
    
    ;; Completion & Editing
    company company-quickhelp yasnippet yasnippet-snippets
    multiple-cursors expand-region smartparens avy
    
    ;; Development Tools - Core
    flycheck lsp-mode lsp-ui eglot
    dap-mode projectile magit format-all vterm
    
    ;; Tree-sitter Support
    treesit-auto tree-sitter tree-sitter-langs
    
    ;; Window Management & Navigation
    ace-window undo-tree neotree
    
    ;; Code Intelligence
    dumb-jump xref consult
    
    ;; Language Support - Core
    lsp-pyright lsp-java lsp-haskell ccls
    rust-mode cargo go-mode
    
    ;; Language Support - Web
    web-mode js2-mode typescript-mode json-mode
    php-mode markdown-mode yaml-mode csv-mode
    
    ;; Language Support - Systems
    ada-mode haskell-mode cmake-mode
    
    ;; Language Support - Modern
    nim-mode zig-mode kotlin-mode scala-mode swift-mode
    elixir-mode erlang tuareg
    
    ;; Language Support - Hardware
    verilog-mode nasm-mode
    
    ;; Language Support - Scientific
    f90-interface-browser julia-mode
    
    ;; Language Support - Scripting
    lua-mode perl-mode
    
    ;; Language Support - Data
    protobuf-mode graphql-mode
    
    ;; Docker & Kubernetes
    dockerfile-mode docker-compose-mode kubernetes
    
    ;; Version Control Enhanced
    git-gutter git-timemachine diff-hl
    
    ;; Visual Enhancements
    rainbow-delimiters highlight-numbers hl-todo
    beacon dimmer highlight-indent-guides
    
    ;; Documentation & Help
    helpful eldoc-box
    
    ;; Performance Monitoring
    esup)
  "Essential packages for a complete IDE experience.")

;; Smart package installation with error handling
(defun emacs-ide-ensure-packages ()
  "Install missing essential packages with error handling."
  (let ((missing (seq-remove #'package-installed-p emacs-ide-essential-packages))
        (failed '()))
    (when missing
      (message "Installing %d missing packages..." (length missing))
      (condition-case err
          (package-refresh-contents)
        (error (message "Warning: Failed to refresh packages: %s" err)))
      
      (dolist (pkg missing)
        (condition-case err
            (progn
              (package-install pkg)
              (message "✓ Installed: %s" pkg))
          (error
           (push pkg failed)
           (message "✗ Failed: %s - %s" pkg err)))))
    
    (when failed
      (display-warning 'packages
                      (format "Failed to install: %s" failed)
                      :warning))))

;; Install packages on first run
(unless (seq-every-p #'package-installed-p emacs-ide-essential-packages)
  (emacs-ide-ensure-packages))

;; ============================================================================
;; PATH AND ENVIRONMENT
;; ============================================================================
(defun emacs-ide-setup-exec-path ()
  "Setup exec-path from shell environment."
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" ""
          (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Load PATH from shell on Linux/macOS
(when (memq window-system '(mac ns x pgtk))
  (emacs-ide-setup-exec-path))

;; ============================================================================
;; WAYLAND DETECTION & SETTINGS
;; ============================================================================
(defvar emacs-ide-wayland-p (getenv "WAYLAND_DISPLAY")
  "Non-nil if running under Wayland.")

(defvar emacs-ide-display-server
  (cond (emacs-ide-wayland-p "Wayland")
        ((getenv "DISPLAY") "X11")
        (t "TTY"))
  "Current display server type.")

;; Wayland-specific optimizations
(when emacs-ide-wayland-p
  (setq select-enable-clipboard t
        select-enable-primary t
        save-interprogram-paste-before-kill t))

;; ============================================================================
;; MODULE DIRECTORY SETUP
;; ============================================================================
(defvar emacs-ide-modules-dir
  (expand-file-name "modules" user-emacs-directory)
  "Directory containing configuration modules.")

(unless (file-directory-p emacs-ide-modules-dir)
  (make-directory emacs-ide-modules-dir t))

(add-to-list 'load-path emacs-ide-modules-dir)

;; ============================================================================
;; MODULE LOADER WITH ERROR HANDLING
;; ============================================================================
(defun emacs-ide-load-module (module-name)
  "Load configuration MODULE-NAME from modules directory."
  (let ((module-file (expand-file-name
                     (concat module-name ".el")
                     emacs-ide-modules-dir)))
    (if (file-exists-p module-file)
        (condition-case err
            (progn
              (load module-file)
              (message "✓ Loaded: %s" module-name))
          (error
           (display-warning 'init
                           (format "Failed to load %s: %s" module-name err)
                           :error)))
      (message "⚠ Module not found: %s (skipping)" module-name))))

;; ============================================================================
;; CORE SETTINGS
;; ============================================================================
(setq-default
 ;; UTF-8 everywhere
 buffer-file-coding-system 'utf-8-unix
 default-buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix
 
 ;; Sane defaults
 indent-tabs-mode nil
 tab-width 4
 fill-column 88
 require-final-newline t
 truncate-lines nil
 word-wrap t
 
 ;; No backups/autosave
 auto-save-default nil
 make-backup-files nil
 create-lockfiles nil)

;; Answer y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable startup screen
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

;; Better scrolling
(setq scroll-conservatively 101
      scroll-margin 3
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; ============================================================================
;; TREE-SITTER AUTO-SETUP
;; ============================================================================
(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

;; ============================================================================
;; LOAD ALL MODULES
;; ============================================================================
(message "Loading Enhanced Emacs IDE modules...")

(emacs-ide-load-module "ui-config")          
(emacs-ide-load-module "completion-config")  
(emacs-ide-load-module "editing-config")     
(emacs-ide-load-module "tools-config")       
(emacs-ide-load-module "lang-config")        
(emacs-ide-load-module "debug-config")       ; NEW: Debugging support
(emacs-ide-load-module "keybindings")        

;; ============================================================================
;; STARTUP MESSAGE
;; ============================================================================
(defun emacs-ide-startup-message ()
  "Display startup information."
  (message "Enhanced Emacs IDE ready! %d packages | %.2fs | %s | C-c ? for help"
           (length package-activated-list)
           (float-time after-init-time)
           emacs-ide-display-server))

(add-hook 'emacs-startup-hook #'emacs-ide-startup-message)

;; ============================================================================
;; CUSTOM FILE
;; ============================================================================
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here