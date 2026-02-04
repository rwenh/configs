;;; init.el --- Enterprise Emacs IDE Core Bootstrap (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; Production-grade initialization with health checks and recovery
;;; Author: Enterprise Emacs Team
;;; Version: 2.1.0
;;; Code:

;; ============================================================================
;; ENTERPRISE METADATA
;; ============================================================================
(defconst emacs-ide-version "2.1.0"
  "Enterprise Emacs IDE version.")

(defconst emacs-ide-minimum-emacs-version "29.1"
  "Minimum required Emacs version.")

(defconst emacs-ide-build-date "2025-02-04"
  "Build date.")

;; ============================================================================
;; VERSION CHECK - FAIL FAST
;; ============================================================================
(when (version< emacs-version emacs-ide-minimum-emacs-version)
  (error (concat "Emacs IDE requires Emacs %s or higher. "
                 "You are running %s. Please upgrade.")
         emacs-ide-minimum-emacs-version emacs-version))

;; ============================================================================
;; STARTUP TRACKING
;; ============================================================================
(defvar emacs-ide--init-start-time (current-time))
(defvar emacs-ide--gc-count-start gcs-done)
(defvar emacs-ide--startup-phases nil
  "Track initialization phases with timestamps.")

(defun emacs-ide--track-phase (phase-name)
  "Track PHASE-NAME completion time."
  (push (cons phase-name (float-time (time-subtract (current-time)
                                                     emacs-ide--init-start-time)))
        emacs-ide--startup-phases))

;; ============================================================================
;; DIRECTORY STRUCTURE - ENTERPRISE ORGANIZATION
;; ============================================================================
(defvar emacs-ide-root-dir user-emacs-directory
  "Root directory of Emacs IDE.")

(defvar emacs-ide-core-dir
  (expand-file-name "core/" emacs-ide-root-dir)
  "Core system directory.")

(defvar emacs-ide-modules-dir
  (expand-file-name "modules/" emacs-ide-root-dir)
  "Feature modules directory.")

(defvar emacs-ide-lib-dir
  (expand-file-name "lib/" emacs-ide-root-dir)
  "Utility libraries directory.")

(defvar emacs-ide-var-dir
  (expand-file-name "var/" emacs-ide-root-dir)
  "Runtime data directory.")

(defvar emacs-ide-cache-dir
  (expand-file-name "cache/" emacs-ide-var-dir)
  "Cache directory.")

(defvar emacs-ide-backup-dir
  (expand-file-name "backups/" emacs-ide-var-dir)
  "Backup files directory.")

(defvar emacs-ide-snippets-dir
  (expand-file-name "snippets/" emacs-ide-root-dir)
  "YASnippet snippets directory.")

;; Create directories
(dolist (dir (list emacs-ide-core-dir
                   emacs-ide-modules-dir
                   emacs-ide-lib-dir
                   emacs-ide-var-dir
                   emacs-ide-cache-dir
                   emacs-ide-backup-dir
                   emacs-ide-snippets-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Add to load path
(add-to-list 'load-path emacs-ide-core-dir)
(add-to-list 'load-path emacs-ide-lib-dir)
(add-to-list 'load-path emacs-ide-modules-dir)

(emacs-ide--track-phase "directory-setup")

;; ============================================================================
;; LOAD CONFIGURATION FIRST (CRITICAL - Must happen before modules)
;; ============================================================================
(message "📋 Loading configuration...")
(require 'emacs-ide-config)
(emacs-ide-config-load)
(emacs-ide--track-phase "config-load")

;; ============================================================================
;; SAFE MODE CHECK
;; ============================================================================
(when (bound-and-true-p emacs-ide-safe-mode)
  (message "⚠️  SAFE MODE: Skipping most configuration")
  (setq-default inhibit-startup-screen t
                initial-scratch-message nil)
  ;; Load only essential recovery tools
  (load (expand-file-name "emacs-ide-recovery.el" emacs-ide-core-dir) nil t)
  (when (fboundp 'emacs-ide-recovery-mode)
    (emacs-ide-recovery-mode))
  ;; Stop here in safe mode
  (provide 'init)
  (kill-emacs 0))

;; ============================================================================
;; PACKAGE MANAGEMENT - STRAIGHT.EL BOOTSTRAP
;; ============================================================================
(defvar emacs-ide-package-system 'straight
  "Package system to use: 'straight or 'package.")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         emacs-ide-root-dir))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (message "📦 Bootstrapping straight.el...")
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight
(setq straight-use-package-by-default t
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      straight-profiles
      '((nil . "default.el")
        (pinned . "versions.lock")))

(emacs-ide--track-phase "package-bootstrap")

;; ============================================================================
;; USE-PACKAGE CONFIGURATION
;; ============================================================================
(straight-use-package 'use-package)

(setq use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t
      use-package-compute-statistics nil
      use-package-verbose nil)

;; Enterprise: Always measure package load time
(setq use-package-verbose (bound-and-true-p init-file-debug)
      use-package-minimum-reported-time 0.1)

(emacs-ide--track-phase "use-package-setup")

;; ============================================================================
;; CORE SYSTEM MODULES - LOAD IN ORDER
;; ============================================================================
(defvar emacs-ide-core-modules
  '("emacs-ide-health"      ; Health check system
    "emacs-ide-package"     ; Package management utilities
    "emacs-ide-profiler"    ; Performance profiling
    "emacs-ide-security"    ; Security hardening
    "emacs-ide-telemetry"   ; Usage tracking
    "emacs-ide-recovery")   ; Error recovery
  "Core system modules to load.")

(defun emacs-ide-load-core-module (module-name)
  "Load core MODULE-NAME with error handling."
  (let ((module-file (expand-file-name (concat module-name ".el")
                                       emacs-ide-core-dir)))
    (condition-case err
        (progn
          (load module-file nil 'nomessage)
          (message "✓ Core: %s" module-name)
          t)
      (error
       (warn "✗ Failed to load core module %s: %s" module-name err)
       ;; Critical modules must load
       (when (string= module-name "emacs-ide-recovery")
         (error "Critical: Recovery module failed to load! %s" err))
       nil))))

;; Load core modules
(message "🔧 Loading core system...")
(dolist (module emacs-ide-core-modules)
  (emacs-ide-load-core-module module))

(emacs-ide--track-phase "core-modules")

;; ============================================================================
;; PATH & ENVIRONMENT
;; ============================================================================
(defun emacs-ide-setup-exec-path ()
  "Setup PATH from shell environment safely."
  (let* ((shell (or (getenv "SHELL") "/bin/sh"))
         (path-from-shell
          (condition-case nil
              (replace-regexp-in-string
               "[ \t\n]*$" ""
               (shell-command-to-string (format "%s --login -c 'echo $PATH'" shell)))
            (error (getenv "PATH")))))
    (unless (string-empty-p path-from-shell)
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator)))))

(when (memq window-system '(mac ns x pgtk))
  (emacs-ide-setup-exec-path))

(emacs-ide--track-phase "environment-setup")

;; ============================================================================
;; CORE SETTINGS - PRODUCTION DEFAULTS
;; ============================================================================
(setq-default
 ;; Encoding
 buffer-file-coding-system 'utf-8-unix
 default-buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix

 ;; Indentation
 indent-tabs-mode nil
 tab-width 4
 fill-column 100
 require-final-newline t
 truncate-lines nil
 word-wrap t

 ;; Files
 auto-save-default nil
 make-backup-files t
 backup-directory-alist `(("." . ,emacs-ide-backup-dir))
 create-lockfiles nil

 ;; Scrolling
 scroll-conservatively 101
 scroll-margin 5
 scroll-preserve-screen-position t
 auto-window-vscroll nil
 fast-but-imprecise-scrolling t
 redisplay-skip-fontification-on-input t)

;; Yes/No to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Startup
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

(emacs-ide--track-phase "core-settings")

;; ============================================================================
;; FEATURE MODULES - LOAD ORDER MATTERS
;; ============================================================================
(defvar emacs-ide-feature-modules
  '("ui-core"              ; Core UI
    "ui-theme"             ; Theme system
    "ui-modeline"          ; Modeline
    "ui-dashboard"         ; Dashboard
    "completion-core"      ; Completion framework
    "completion-snippets"  ; Snippets
    "editing-core"         ; Core editing
    "editing-nav"          ; Navigation
    "tools-lsp"            ; LSP
    "tools-project"        ; Project management
    "tools-git"            ; Git integration
    "tools-terminal"       ; Terminal
    "lang-core"            ; Language support
    "debug-core"           ; Debugging
    "keybindings")         ; Keybindings (load last)
  "Feature modules to load in order.")

(defun emacs-ide-load-feature-module (module-name)
  "Load feature MODULE-NAME with fallback."
  (let ((module-file (expand-file-name (concat module-name ".el")
                                       emacs-ide-modules-dir)))
    (condition-case err
        (progn
          (load module-file nil 'nomessage)
          (message "✓ Feature: %s" module-name)
          t)
      (error
       (warn "✗ Failed to load feature module %s: %s" module-name err)
       ;; Try to continue without this module
       (when (fboundp 'emacs-ide-recovery-log-error)
         (emacs-ide-recovery-log-error module-name err))
       nil))))

;; Load feature modules
(message "🎨 Loading feature modules...")
(dolist (module emacs-ide-feature-modules)
  (emacs-ide-load-feature-module module))

(emacs-ide--track-phase "feature-modules")

;; ============================================================================
;; HEALTH CHECK ON STARTUP
;; ============================================================================
(when (and (fboundp 'emacs-ide-health-check-startup)
           (not noninteractive))
  (run-with-idle-timer 1 nil #'emacs-ide-health-check-startup))

;; ============================================================================
;; POST-INIT OPTIMIZATION
;; ============================================================================
(add-hook 'emacs-startup-hook
          (lambda ()
            (emacs-ide--track-phase "startup-complete")

            ;; Calculate statistics
            (let* ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-ide--init-start-time)))
                   (gc-count (- gcs-done emacs-ide--gc-count-start))
                   (pkg-count (if (fboundp 'straight--recipe-cache)
                                  (condition-case nil
                                      (hash-table-count straight--recipe-cache)
                                    (error 0))
                                0)))

              ;; Display startup message
              (message (concat "🚀 Emacs IDE v%s ready in %.2fs "
                              "| %d packages | %d GCs | %s")
                       emacs-ide-version
                       elapsed
                       pkg-count
                       gc-count
                       (or emacs-ide-display-server "TTY"))

              ;; Performance check (using config target)
              (let ((target (or (bound-and-true-p emacs-ide-startup-time-target) 3.0)))
                (when (> elapsed target)
                  (warn (concat "⚠️  Startup took %.2fs (target: <%.1fs). "
                               "Run M-x emacs-ide-profile-startup for details.")
                        elapsed target)))

              ;; Log to telemetry
              (when (fboundp 'emacs-ide-telemetry-log-startup)
                (emacs-ide-telemetry-log-startup elapsed gc-count pkg-count))))
          100)

;; ============================================================================
;; CUSTOM FILE
;; ============================================================================
(when (and (boundp 'custom-file)
           (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;; ============================================================================
;; ENTERPRISE UTILITY COMMANDS
;; ============================================================================
(defun emacs-ide-show-version ()
  "Display Emacs IDE version and system info."
  (interactive)
  (message (concat "Emacs IDE v%s | Emacs %s | %s | Build: %s")
           emacs-ide-version
           emacs-version
           (or emacs-ide-display-server "TTY")
           emacs-ide-build-date))

(defun emacs-ide-startup-report ()
  "Display detailed startup report."
  (interactive)
  (with-output-to-temp-buffer "*Startup Report*"
    (princ (format "=== EMACS IDE STARTUP REPORT ===\n\n"))
    (princ (format "Version: %s\n" emacs-ide-version))
    (princ (format "Emacs Version: %s\n" emacs-version))
    (princ (format "Display Server: %s\n" (or emacs-ide-display-server "TTY")))
    (princ (format "CPUs: %d\n\n" (or emacs-ide-processor-count 4)))
    (princ "Startup Phases:\n")
    (dolist (phase (reverse emacs-ide--startup-phases))
      (princ (format "  %-30s %.3fs\n" (car phase) (cdr phase))))
    (princ "\n")
    (princ (format "Total Startup Time: %.3fs\n"
                   (cdr (car emacs-ide--startup-phases))))
    (princ (format "Garbage Collections: %d\n"
                   (- gcs-done emacs-ide--gc-count-start)))))

(defun emacs-ide-reload ()
  "Reload Emacs IDE configuration."
  (interactive)
  (when (y-or-n-p "Reload entire configuration? ")
    (load-file user-init-file)
    (message "✓ Configuration reloaded")))

(defun emacs-ide-update ()
  "Update all packages and Emacs IDE."
  (interactive)
  (when (y-or-n-p "Update all packages? This may take several minutes. ")
    (message "📦 Updating packages...")
    (straight-pull-all)
    (message "✓ Update complete. Restart Emacs to apply changes.")))

(defun emacs-ide-freeze-versions ()
  "Freeze current package versions for reproducibility."
  (interactive)
  (straight-freeze-versions)
  (message "✓ Package versions frozen to versions.lock"))

(provide 'init)
;;; init.el ends here