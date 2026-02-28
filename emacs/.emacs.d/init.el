;;; init.el --- Enterprise Emacs IDE Core Bootstrap -*- lexical-binding: t -*-
;;; Commentary:
;;; Production-grade initialization with health checks and recovery.
;;; CHANGES FROM v2.1.0:
;;;   - Removed "editing-nav" from feature modules (merged into editing-core)
;;;   - Added "tools-format"  (new — fixes C-c F keybinding)
;;;   - Added "tools-org"     (new — full org setup from config.yml)
;;;   - Added "tools-spelling"(new — flyspell across prose + code comments)
;;; Code:

;; ============================================================================
;; ENTERPRISE METADATA
;; ============================================================================
(defconst emacs-ide-version "2.2.0"
  "Enterprise Emacs IDE version.")

(defconst emacs-ide-minimum-emacs-version "29.1"
  "Minimum required Emacs version.")

(defconst emacs-ide-build-date "2026-02-28"
  "Build date.")

;; ============================================================================
;; VERSION CHECK
;; ============================================================================
(when (version< emacs-version emacs-ide-minimum-emacs-version)
  (error "Emacs IDE requires Emacs %s or higher. You are running %s."
         emacs-ide-minimum-emacs-version emacs-version))

;; ============================================================================
;; STARTUP TRACKING
;; ============================================================================
(defvar emacs-ide--init-start-time (current-time))
(defvar emacs-ide--gc-count-start gcs-done)
(defvar emacs-ide--startup-phases nil)

(defun emacs-ide--track-phase (phase-name)
  "Record the elapsed time for PHASE-NAME."
  (push (cons phase-name
              (float-time (time-subtract (current-time)
                                         emacs-ide--init-start-time)))
        emacs-ide--startup-phases))

;; ============================================================================
;; DIRECTORY STRUCTURE
;; ============================================================================
(defvar emacs-ide-root-dir    user-emacs-directory)
(defvar emacs-ide-core-dir    (expand-file-name "core/"    emacs-ide-root-dir))
(defvar emacs-ide-modules-dir (expand-file-name "modules/" emacs-ide-root-dir))
(defvar emacs-ide-lib-dir     (expand-file-name "lib/"     emacs-ide-root-dir))
(defvar emacs-ide-var-dir     (expand-file-name "var/"     emacs-ide-root-dir))
(defvar emacs-ide-cache-dir   (expand-file-name "cache/"   emacs-ide-var-dir))
(defvar emacs-ide-backup-dir  (expand-file-name "backups/" emacs-ide-var-dir))
(defvar emacs-ide-snippets-dir (expand-file-name "snippets/" emacs-ide-root-dir))

(dolist (dir (list emacs-ide-core-dir emacs-ide-modules-dir emacs-ide-lib-dir
                   emacs-ide-var-dir emacs-ide-cache-dir emacs-ide-backup-dir
                   emacs-ide-snippets-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(add-to-list 'load-path emacs-ide-core-dir)
(add-to-list 'load-path emacs-ide-lib-dir)
(add-to-list 'load-path emacs-ide-modules-dir)

(emacs-ide--track-phase "directory-setup")

;; ============================================================================
;; CONFIGURATION
;; ============================================================================
(message "📋 Loading configuration...")
(require 'emacs-ide-config)
(emacs-ide-config-load)
(emacs-ide--track-phase "config-load")

;; ============================================================================
;; SAFE MODE
;; ============================================================================
(when (bound-and-true-p emacs-ide-safe-mode)
  (message "⚠️  SAFE MODE: Skipping most configuration")
  (setq-default inhibit-startup-screen t
                initial-scratch-message nil)
  (load (expand-file-name "emacs-ide-recovery.el" emacs-ide-core-dir) nil t)
  (when (fboundp 'emacs-ide-recovery-mode)
    (emacs-ide-recovery-mode))
  (provide 'init)
  (kill-emacs 0))

;; ============================================================================
;; PACKAGE MANAGEMENT — STRAIGHT.EL
;; ============================================================================
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

(setq straight-use-package-by-default   t
      straight-check-for-modifications  '(check-on-save find-when-checking)
      straight-cache-autoloads          t
      straight-vc-git-default-clone-depth 1
      straight-profiles '((nil . "default.el") (pinned . "versions.lock")))

(emacs-ide--track-phase "package-bootstrap")

;; ============================================================================
;; USE-PACKAGE
;; ============================================================================
(straight-use-package 'use-package)

(setq use-package-always-defer       t
      use-package-expand-minimally   t
      use-package-enable-imenu-support t
      use-package-verbose            (bound-and-true-p init-file-debug)
      use-package-minimum-reported-time 0.1)

(emacs-ide--track-phase "use-package-setup")

;; ============================================================================
;; CORE SYSTEM MODULES
;; ============================================================================
(defvar emacs-ide-core-modules
  '("emacs-ide-health"     ; Health check system
    "emacs-ide-package"    ; Package management utilities
    "emacs-ide-profiler"   ; Performance profiling
    "emacs-ide-security"   ; Security hardening
    "emacs-ide-telemetry"  ; Usage tracking
    "emacs-ide-recovery")  ; Error recovery
  "Core system modules loaded before features.")

(defun emacs-ide-load-core-module (module-name)
  "Load core MODULE-NAME with error handling."
  (let ((file (expand-file-name (concat module-name ".el") emacs-ide-core-dir)))
    (condition-case err
        (progn (load file nil 'nomessage)
               (message "✓ Core: %s" module-name)
               t)
      (error
       (warn "✗ Failed core module %s: %s" module-name err)
       (when (string= module-name "emacs-ide-recovery")
         (error "Critical: Recovery module failed: %s" err))
       nil))))

(message "🔧 Loading core system...")
(dolist (module emacs-ide-core-modules)
  (emacs-ide-load-core-module module))

(emacs-ide--track-phase "core-modules")

;; ============================================================================
;; PATH & ENVIRONMENT
;; ============================================================================
(defun emacs-ide-setup-exec-path ()
  "Set exec-path and PATH from login shell."
  (let* ((shell (or (getenv "SHELL") "/bin/sh"))
         (path-str
          (condition-case nil
              (replace-regexp-in-string
               "[ \t\n]*$" ""
               (shell-command-to-string
                (format "%s --login -c 'echo $PATH'" shell)))
            (error (getenv "PATH")))))
    (unless (string-empty-p path-str)
      (setenv "PATH" path-str)
      (setq exec-path (split-string path-str path-separator)))))

(when (memq window-system '(mac ns x pgtk))
  (emacs-ide-setup-exec-path))

(emacs-ide--track-phase "environment-setup")

;; ============================================================================
;; CORE SETTINGS
;; ============================================================================
(setq-default
 buffer-file-coding-system     'utf-8-unix
 default-buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system   'utf-8-unix
 indent-tabs-mode               nil
 tab-width                      4
 fill-column                    100
 require-final-newline          t
 truncate-lines                 nil
 word-wrap                      t
 auto-save-default              nil
 make-backup-files              t
 backup-directory-alist         `(("." . ,emacs-ide-backup-dir))
 create-lockfiles               nil
 scroll-conservatively          101
 scroll-margin                  5
 scroll-preserve-screen-position t
 auto-window-vscroll            nil
 fast-but-imprecise-scrolling   t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen    t
      inhibit-startup-message   t
      initial-scratch-message   nil
      initial-major-mode        'fundamental-mode)

(emacs-ide--track-phase "core-settings")

;; ============================================================================
;; FEATURE MODULES
;; CHANGES: removed "editing-nav" (merged into editing-core)
;;          added "tools-format", "tools-org", "tools-spelling"
;; ============================================================================
(defvar emacs-ide-feature-modules
  '("ui-core"            ; Core UI, theme, modeline, visual enhancements
    "ui-theme"           ; Theme toggle utility
    "ui-modeline"        ; Modeline fallback logic
    "ui-dashboard"       ; Startup dashboard
    "completion-core"    ; Vertico + Corfu + Consult + Cape + Orderless
    "completion-snippets" ; YASnippet
    "editing-core"       ; Core editing + navigation (editing-nav merged in)
    ;; "editing-nav"     ; REMOVED — merged into editing-core
    "tools-lsp"          ; LSP mode
    "tools-project"      ; Projectile + Treemacs
    "tools-git"          ; Magit + git-gutter + forge
    "tools-terminal"     ; VTerm + Eshell + Docker
    "tools-format"       ; NEW: format-all + apheleia + editorconfig
    "tools-org"          ; NEW: full org-mode + agenda + capture
    "tools-spelling"     ; NEW: flyspell across prose and code
    "lang-core"          ; Language modes + tree-sitter
    "debug-core"         ; DAP debugging
    "keybindings")       ; Keybindings — always last
  "Feature modules loaded in order.")

(defun emacs-ide-load-feature-module (module-name)
  "Load feature MODULE-NAME with fallback."
  (let ((file (expand-file-name (concat module-name ".el") emacs-ide-modules-dir)))
    (condition-case err
        (progn (load file nil 'nomessage)
               (message "✓ Feature: %s" module-name)
               t)
      (error
       (warn "✗ Failed feature module %s: %s" module-name err)
       (when (fboundp 'emacs-ide-recovery-log-error)
         (emacs-ide-recovery-log-error module-name err))
       nil))))

(message "🎨 Loading feature modules...")
(dolist (module emacs-ide-feature-modules)
  (emacs-ide-load-feature-module module))

(emacs-ide--track-phase "feature-modules")

;; ============================================================================
;; HEALTH CHECK
;; ============================================================================
(when (and (fboundp 'emacs-ide-health-check-startup)
           (not noninteractive))
  (run-with-idle-timer 1 nil #'emacs-ide-health-check-startup))

;; ============================================================================
;; POST-INIT
;; ============================================================================
(add-hook 'emacs-startup-hook
          (lambda ()
            (emacs-ide--track-phase "startup-complete")
            (let* ((elapsed  (float-time
                              (time-subtract (current-time)
                                             emacs-ide--init-start-time)))
                   (gc-count (- gcs-done emacs-ide--gc-count-start))
                   (pkg-count (if (fboundp 'straight--recipe-cache)
                                  (condition-case nil
                                      (hash-table-count straight--recipe-cache)
                                    (error 0))
                                0)))
              (message "🚀 Emacs IDE v%s ready in %.2fs | %d packages | %d GCs | %s"
                       emacs-ide-version elapsed pkg-count gc-count
                       (or (bound-and-true-p emacs-ide-display-server) "TTY"))
              (let ((target (or (bound-and-true-p emacs-ide-startup-time-target) 3.0)))
                (when (> elapsed target)
                  (warn "⚠️  Startup %.2fs exceeded target %.1fs. Run M-x emacs-ide-profile-startup."
                        elapsed target)))
              (when (fboundp 'emacs-ide-telemetry-log-startup)
                (emacs-ide-telemetry-log-startup elapsed gc-count pkg-count))))
          100)

;; ============================================================================
;; CUSTOM FILE
;; ============================================================================
(when (and (boundp 'custom-file) (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;; ============================================================================
;; UTILITY COMMANDS
;; ============================================================================
(defun emacs-ide-show-version ()
  "Display version and system info."
  (interactive)
  (message "Emacs IDE v%s | Emacs %s | %s | Build: %s"
           emacs-ide-version emacs-version
           (or (bound-and-true-p emacs-ide-display-server) "TTY")
           emacs-ide-build-date))

(defun emacs-ide-startup-report ()
  "Display detailed startup report."
  (interactive)
  (with-output-to-temp-buffer "*Startup Report*"
    (princ (format "=== EMACS IDE v%s STARTUP REPORT ===\n\n" emacs-ide-version))
    (princ (format "Emacs:   %s\n" emacs-version))
    (princ (format "Display: %s\n" (or (bound-and-true-p emacs-ide-display-server) "TTY")))
    (princ (format "CPUs:    %d\n\n" (or (bound-and-true-p emacs-ide-processor-count) 4)))
    (princ "Startup Phases:\n")
    (dolist (phase (reverse emacs-ide--startup-phases))
      (princ (format "  %-30s %.3fs\n" (car phase) (cdr phase))))))

(defun emacs-ide-reload ()
  "Reload entire configuration."
  (interactive)
  (when (y-or-n-p "Reload entire configuration? ")
    (load-file user-init-file)
    (message "✓ Configuration reloaded")))

(defun emacs-ide-update ()
  "Update all packages."
  (interactive)
  (when (y-or-n-p "Update all packages? This may take a few minutes. ")
    (straight-pull-all)
    (message "✓ Update complete. Restart Emacs to apply.")))

(defun emacs-ide-freeze-versions ()
  "Freeze package versions to versions.lock."
  (interactive)
  (straight-freeze-versions)
  (message "✓ Package versions frozen"))

(provide 'init)
;;; init.el ends here
