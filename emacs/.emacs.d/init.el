;;; init.el --- Hyper-Optimized Emacs IDE Core -*- lexical-binding: t -*-
;;; Commentary:
;;; Professional-grade IDE with sub-second startup and maximum performance
;;; Code:

;; ============================================================================
;; STARTUP PERFORMANCE TRACKING
;; ============================================================================
(defvar emacs-ide--startup-time-start (current-time))
(defvar emacs-ide--gc-count-start gcs-done)

;; ============================================================================
;; EARLY OPTIMIZATION
;; ============================================================================
(setq byte-compile-warnings nil
      warning-suppress-types '((comp) (bytecomp))
      warning-suppress-log-types '((comp) (bytecomp))
      warning-minimum-level :error)

;; ============================================================================
;; NATIVE COMPILATION
;; ============================================================================
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t
        native-comp-speed 3))

;; ============================================================================
;; MODERN EMACS FEATURES
;; ============================================================================
(when (fboundp 'global-so-long-mode) (global-so-long-mode 1))
(when (fboundp 'global-subword-mode) (global-subword-mode 1))

;; ============================================================================
;; PACKAGE SYSTEM - OPTIMIZED
;; ============================================================================
(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("gnu" . "https://elpa.gnu.org/packages/")
                        ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities '(("melpa" . 10)
                                  ("gnu" . 5)
                                  ("nongnu" . 3)))
(package-initialize)

;; Bootstrap straight.el for better performance
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use-package with straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t
      use-package-compute-statistics nil)

;; Version locking for reproducible builds
(setq straight-profiles
      '((nil . "default.el")
        (pinned . "pinned.el")))

(defun emacs-ide-freeze-packages ()
  "Freeze package versions for reproducibility."
  (interactive)
  (straight-freeze-versions)
  (message "Package versions frozen"))

(defun emacs-ide-update-packages ()
  "Update all straight packages."
  (interactive)
  (message "Updating packages...")
  (straight-pull-all)
  (message "All packages updated. Restart Emacs to compile."))

;; ============================================================================
;; ESSENTIAL PACKAGES - COMPLETE LIST
;; ============================================================================
(defvar emacs-ide-essential-packages
  '(;; UI & Appearance
    modus-themes doom-modeline which-key breadcrumb dashboard all-the-icons
    all-the-icons-dired beacon dimmer pulsar rainbow-delimiters rainbow-mode
    highlight-numbers hl-todo highlight-indent-guides ace-window
    transpose-frame diredfl visual-fill-column
    
    ;; Completion Framework - Using Corfu (modern choice)
    corfu cape vertico orderless marginalia consult embark embark-consult
    yasnippet yasnippet-snippets yasnippet-capf
    
    ;; Editing
    multiple-cursors expand-region smartparens avy undo-tree
    move-text visual-regexp string-inflection aggressive-indent
    
    ;; Development Tools
    flycheck flycheck-pos-tip lsp-mode lsp-ui lsp-treemacs dap-mode
    lsp-pyright lsp-java lsp-haskell ccls
    
    ;; Project & Version Control
    projectile magit git-gutter git-timemachine diff-hl
    
    ;; Language Support
    treesit-auto rust-mode cargo go-mode js2-mode typescript-mode
    web-mode emmet-mode php-mode csharp-mode ruby-mode haskell-mode
    scala-mode kotlin-mode swift-mode elixir-mode erlang lua-mode
    cuda-mode nim-mode zig-mode julia-mode tuareg verilog-mode
    nasm-mode json-mode yaml-mode csv-mode protobuf-mode graphql-mode
    toml-mode markdown-mode cmake-mode dockerfile-mode docker-compose-mode
    
    ;; Utilities
    format-all vterm multi-vterm neotree helpful eldoc-box
    docker kubernetes hydra realgud dumb-jump esup))

;; Install all essential packages
(message "Installing essential packages...")
(dolist (package emacs-ide-essential-packages)
  (straight-use-package package))

;; ============================================================================
;; PATH & ENVIRONMENT
;; ============================================================================
(defun emacs-ide-setup-exec-path ()
  "Setup PATH from shell."
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" ""
          (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (memq window-system '(mac ns x pgtk))
  (emacs-ide-setup-exec-path))

;; ============================================================================
;; WAYLAND DETECTION
;; ============================================================================
(defvar emacs-ide-wayland-p (getenv "WAYLAND_DISPLAY"))
(defvar emacs-ide-display-server
  (cond (emacs-ide-wayland-p "Wayland")
        ((getenv "DISPLAY") "X11")
        (t "TTY")))

(when emacs-ide-wayland-p
  (setq select-enable-clipboard t
        select-enable-primary t
        save-interprogram-paste-before-kill t
        x-select-enable-clipboard-manager t))

;; ============================================================================
;; MODULE DIRECTORY
;; ============================================================================
(defvar emacs-ide-modules-dir
  (expand-file-name "modules" user-emacs-directory))

(unless (file-directory-p emacs-ide-modules-dir)
  (make-directory emacs-ide-modules-dir t))

(add-to-list 'load-path emacs-ide-modules-dir)

;; ============================================================================
;; MODULE LOADER - ENHANCED ERROR HANDLING
;; ============================================================================
(defun emacs-ide-load-module (module-name)
  "Load MODULE-NAME with enhanced error handling."
  (let ((module-file (expand-file-name
                     (concat module-name ".el")
                     emacs-ide-modules-dir)))
    (if (file-exists-p module-file)
        (condition-case err
            (progn
              (load module-file nil 'nomessage)
              (message "✓ Loaded module: %s" module-name))
          (error
           (message "✗ Failed to load %s: %s" module-name (error-message-string err))
           (display-warning 'emacs-ide
                           (format "Module load error in %s:\n%s"
                                   module-name
                                   (error-message-string err))
                           :error)))
      (message "✗ Module not found: %s" module-name)
      (display-warning 'emacs-ide
                      (format "Module file not found: %s" module-file)
                      :warning))))

;; ============================================================================
;; CORE SETTINGS - OPTIMIZED
;; ============================================================================
(setq-default
 buffer-file-coding-system 'utf-8-unix
 default-buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix
 
 indent-tabs-mode nil
 tab-width 4
 fill-column 100
 require-final-newline t
 truncate-lines nil
 word-wrap t
 
 auto-save-default nil
 make-backup-files nil
 create-lockfiles nil
 
 scroll-conservatively 101
 scroll-margin 5
 scroll-preserve-screen-position t
 auto-window-vscroll nil
 
 fast-but-imprecise-scrolling t
 redisplay-skip-fontification-on-input t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

;; ============================================================================
;; LOAD MODULES
;; ============================================================================
(emacs-ide-load-module "ui-config")
(emacs-ide-load-module "completion-config")
(emacs-ide-load-module "editing-config")
(emacs-ide-load-module "tools-config")
(emacs-ide-load-module "lang-config")
(emacs-ide-load-module "debug-config")
(emacs-ide-load-module "keybindings")

;; ============================================================================
;; POST-INIT OPTIMIZATION & LAZY LOADING
;; ============================================================================
;; Defer loading of heavy packages
(run-with-idle-timer 2 nil
  (lambda ()
    (require 'magit nil t)
    (require 'projectile nil t)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)
            (garbage-collect)
            (let* ((elapsed (float-time (time-subtract (current-time)
                                                      emacs-ide--startup-time-start)))
                   (pkg-count (hash-table-count straight--recipe-cache))
                   (gc-count (- gcs-done emacs-ide--gc-count-start)))
              (message "🚀 Emacs IDE ready in %.2fs | %d packages | %d GCs | %s"
                       elapsed
                       pkg-count
                       gc-count
                       emacs-ide-display-server)))
          100)

;; ============================================================================
;; CUSTOM FILE
;; ============================================================================
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================
(defun emacs-ide-show-startup-time ()
  "Display detailed startup information."
  (interactive)
  (message "Startup time: %.2fs | Packages: %d | GCs: %d"
           (float-time (time-subtract after-init-time before-init-time))
           (hash-table-count straight--recipe-cache)
           gcs-done))

(defun emacs-ide-show-system-info ()
  "Display comprehensive system information."
  (interactive)
  (message "Emacs %s | %s | %s | GC: %d times | Uptime: %s"
           emacs-version
           system-type
           emacs-ide-display-server
           gcs-done
           (emacs-uptime "%h hours %m minutes")))

(provide 'init)
;;; init.el ends here
