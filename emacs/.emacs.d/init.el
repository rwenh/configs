;;; init.el --- Enterprise Emacs IDE Core Bootstrap -*- lexical-binding: t -*-
;;; Commentary:
;;; Production-grade initialization with health checks and recovery.
;;; Version: 2.2.3
;;; Fixes:
;;;   - 2.2.3: tools-test added to feature-modules (language-aware test runner)
;;;   - 2.2.2: (unless emacs-ide-safe-mode ...) block now closes BEFORE the
;;;     utility defuns and (provide 'init). Previously the block swallowed
;;;     emacs-ide-show-version, emacs-ide-startup-report, emacs-ide-reload,
;;;     emacs-ide-update, emacs-ide-freeze-versions, the custom-file load,
;;;     and (provide 'init) — none of which were available in safe mode.
;;;   - Safe-mode: removed `kill-emacs 0` (was killing Emacs before window opens)
;;;   - `straight--installed-p` is not a real API -> replaced with `featurep`-only guard
;;;   - `straight--recipe-cache` -> `straight--build-cache` for correct pkg count
;;;   - `buffer-file-coding-system` removed from setq-default block (it's buffer-local)
;;;   - `make-backup-files t` reconciled with early-init nil; backups now live in
;;;     var/backups/ and early-init no longer redundantly sets nil (handled here)
;;;   - Added `electric-pair-mode` enable so test-basic-editing-modes passes
;;; Code:

;; ============================================================================
;; ENTERPRISE METADATA
;; ============================================================================
(defconst emacs-ide-version "2.2.2"
  "Enterprise Emacs IDE version.")

(defconst emacs-ide-minimum-emacs-version "29.1"
  "Minimum required Emacs version.")

(defconst emacs-ide-build-date "2026-03-02"
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
;; FIX 2.2.1: Removed `(kill-emacs 0)` — killed process before window opened.
;; FIX 2.2.2: The (unless emacs-ide-safe-mode ...) block now closes BEFORE the
;;      utility defuns and (provide 'init) so those are always available.
;;      Safe mode simply falls through with minimal config.
;; ============================================================================
(when (bound-and-true-p emacs-ide-safe-mode)
  (message "⚠️  SAFE MODE: Skipping full configuration — loading recovery only")
  (setq-default inhibit-startup-screen t
                initial-scratch-message nil)
  (load (expand-file-name "emacs-ide-recovery.el" emacs-ide-core-dir) nil t)
  (when (fboundp 'emacs-ide-recovery-mode)
    (emacs-ide-recovery-mode)))

;; ============================================================================
;; PACKAGE MANAGEMENT — STRAIGHT.EL
;; (skipped entirely in safe-mode)
;; ============================================================================
(unless (bound-and-true-p emacs-ide-safe-mode)

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
  ;; FIX: `buffer-file-coding-system` is buffer-local and must NOT appear in
  ;;      setq-default alongside global vars — moved to set-default-coding-systems.
  ;;      Backup policy: early-init.el sets make-backup-files nil for fast startup.
  ;;      Here we re-enable backups with a safe directory so files are protected.
  ;; ============================================================================
  (prefer-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8")

  (setq-default
   indent-tabs-mode               nil
   tab-width                      4
   fill-column                    100
   require-final-newline          t
   truncate-lines                 nil
   word-wrap                      t
   auto-save-default              nil
   scroll-conservatively          101
   scroll-margin                  5
   scroll-preserve-screen-position t
   auto-window-vscroll            nil
   fast-but-imprecise-scrolling   t)

  ;; Backup policy — kept separate for clarity
  (setq make-backup-files         t
        backup-directory-alist    `(("." . ,emacs-ide-backup-dir))
        backup-by-copying         t       ; Avoid breaking hard links
        version-control           t       ; Numbered backups
        kept-new-versions         6
        kept-old-versions         2
        delete-old-versions       t
        create-lockfiles          nil)

  (fset 'yes-or-no-p 'y-or-n-p)

  (setq inhibit-startup-screen    t
        inhibit-startup-message   t
        initial-scratch-message   nil
        initial-major-mode        'fundamental-mode)

  ;; FIX: Enable electric-pair-mode here so test-basic-editing-modes passes
  ;; without depending on a feature module loading successfully.
  (electric-pair-mode 1)

  (emacs-ide--track-phase "core-settings")

  ;; ============================================================================
  ;; FEATURE MODULES
  ;; ============================================================================
  (defvar emacs-ide-feature-modules
    '("ui-core"            ; Core UI, theme, modeline, visual enhancements
      "ui-theme"           ; Theme toggle utility
      "ui-modeline"        ; Modeline fallback logic
      "ui-dashboard"       ; Startup dashboard
      "completion-core"    ; Vertico + Corfu + Consult + Cape + Orderless
      "completion-snippets" ; YASnippet
      "editing-core"       ; Core editing + navigation (editing-nav merged in)
      "tools-lsp"          ; LSP mode
      "tools-project"      ; Projectile + Treemacs
      "tools-git"          ; Magit + diff-hl + forge
      "tools-terminal"     ; VTerm + Eshell + Docker
      "tools-format"       ; format-all + apheleia + editorconfig
      "tools-org"          ; org-mode + agenda + capture
      "tools-spelling"     ; flyspell across prose and code
      "lang-core"          ; Language modes + tree-sitter
      "tools-test"         ; Language-aware test runner
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
  ;; FIX: `straight--recipe-cache` does not exist; correct table is
  ;;      `straight--build-cache`. Wrapped in safe condition-case either way.
  ;; ============================================================================
  (add-hook 'emacs-startup-hook
            (lambda ()
              (emacs-ide--track-phase "startup-complete")
              (let* ((elapsed  (float-time
                                (time-subtract (current-time)
                                               emacs-ide--init-start-time)))
                     (gc-count (- gcs-done emacs-ide--gc-count-start))
                     (pkg-count (condition-case nil
                                    (if (and (fboundp 'straight--build-cache)
                                             (hash-table-p straight--build-cache))
                                        (hash-table-count straight--build-cache)
                                      0)
                                  (error 0))))
                (message "🚀 Emacs IDE v%s ready in %.2fs | %d packages | %d GCs | %s%s"
                         emacs-ide-version elapsed pkg-count gc-count
                         (or (bound-and-true-p emacs-ide-display-server) "TTY")
                         ;; Append health hint if previous check found issues
                         (let ((w (if (boundp 'emacs-ide-health--last-warnings)
                                      emacs-ide-health--last-warnings 0))
                               (e (if (boundp 'emacs-ide-health--last-errors)
                                      emacs-ide-health--last-errors 0)))
                           (cond ((> e 0) (format " | ✗ %d health error%s"
                                                   e (if (= e 1) "" "s")))
                                 ((> w 0) (format " | ⚠ %d health warning%s"
                                                   w (if (= w 1) "" "s")))
                                 (t ""))))
                (let ((target (or (bound-and-true-p emacs-ide-startup-time-target) 3.0)))
                  (when (> elapsed target)
                    (warn "⚠️  Startup %.2fs exceeded target %.1fs. Run M-x emacs-ide-profile-startup."
                          elapsed target)))
                (when (fboundp 'emacs-ide-telemetry-log-startup)
                  (emacs-ide-telemetry-log-startup elapsed gc-count pkg-count))))
            100)

) ;; end (unless emacs-ide-safe-mode ...)
;; FIX 2.2.2: The block above closes HERE — before custom-file, utility
;; defuns, and (provide 'init) — so all of the following are always available
;; regardless of safe-mode state.

;; ============================================================================
;; CUSTOM FILE
;; ============================================================================
(when (and (boundp 'custom-file) (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;; ============================================================================
;; UTILITY COMMANDS (always defined, even in safe mode)
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
