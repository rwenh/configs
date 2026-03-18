;;; init.el --- Enterprise Emacs IDE Core Bootstrap -*- lexical-binding: t -*-
;;; Commentary:
;;; Production-grade initialization with health checks and recovery.
;;; Version: 3.0.3
;;; Fixes vs 3.0.2:
;;;   - emacs-ide-spot-check.el added to core/ and loaded at startup alongside
;;;     emacs-ide-test.el. M-x emacs-ide-spot-check is always available.
;;;     Like emacs-ide-test.el it is on-demand only (not in core-modules loop).
;;;   - FIX-TEST-LOAD: emacs-ide-test.el load now warns visibly when the file
;;;     is missing (not just when load fails). Also logs success/failure to
;;;     *Messages* so emacs-ide-spot-check can detect why emacs-ide-run-tests
;;;     is absent. Added emacs-ide-run-tests forward-declaration so the command
;;;     exists even if test file fails — it then prompts to reload.
;;;   - FIX-1: emacs-ide-reload-config alias added (see bottom of file).
;;;     keybindings.el binds C-c R to this name; only emacs-ide-config-reload
;;;     existed — every C-c R press threw void-function.
;;;   - FIX-3: split-string in emacs-ide-setup-exec-path now passes t as the
;;;     omit-nulls argument, preventing "" from entering exec-path when PATH
;;;     ends with a trailing colon.
;;; Changes from 2.2.6:
;;;   - Version bumped to 3.0.0. Build date now computed at load time.
;;;   - emacs-ide-langs-dir added to directory structure and load-path.
;;;     modules/langs/ is created on startup if absent.
;;;   - emacs-ide-feature-modules updated:
;;;       REMOVED: "lang-core"   — replaced by lazy langs/ directory
;;;       ADDED:   "ui-workspace"              (perspective.el workspaces)
;;;                "core-dev"                  (shared lang infrastructure API)
;;;                "apheleia-langs-patch"       (50-lang formatter map)
;;;                "tools-test-runner-registry" (per-lang test dispatch)
;;;                "tools-repl"                 (unified REPL hub)
;;;                "tools-project-detect"       (project root → lang pre-warm)
;;;                "tools-hydra"                (10 Hydra menus)
;;;       ORDER:   ui-workspace after ui-dashboard; core-dev after editing-core
;;;                and before tools-lsp; apheleia-langs-patch after tools-format;
;;;                tools-test-runner-registry before tools-test;
;;;                tools-repl and tools-project-detect after debug-core;
;;;                tools-hydra before keybindings; keybindings always last.
;;;   - emacs-ide-purge-bytecode-cache now also clears modules/langs/.
;;;   - emacs-ide-startup-report notes that lang modules are lazy.
;;;   - All 2.2.6 fixes retained unchanged:
;;;       nosuffix load to avoid stale bytecode
;;;       straight-check-for-modifications = find-when-checking
;;;       BUG-01..06 from early-init unchanged
;;; Code:

;; ============================================================================
;; ENTERPRISE METADATA
;; ============================================================================
(defconst emacs-ide-version "3.0.0"
  "Enterprise Emacs IDE version.")

(defconst emacs-ide-minimum-emacs-version "29.1"
  "Minimum required Emacs version.")

(defconst emacs-ide-build-date (format-time-string "%Y-%m-%d")
  "Build date — computed at load time.")

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
(defvar emacs-ide-root-dir     user-emacs-directory)
(defvar emacs-ide-core-dir     (expand-file-name "core/"         emacs-ide-root-dir))
(defvar emacs-ide-modules-dir  (expand-file-name "modules/"      emacs-ide-root-dir))
(defvar emacs-ide-langs-dir    (expand-file-name "modules/langs/" emacs-ide-root-dir)) ; NEW
(defvar emacs-ide-lib-dir      (expand-file-name "lib/"          emacs-ide-root-dir))
(defvar emacs-ide-var-dir      (expand-file-name "var/"          emacs-ide-root-dir))
(defvar emacs-ide-cache-dir    (expand-file-name "cache/"        emacs-ide-var-dir))
(defvar emacs-ide-backup-dir   (expand-file-name "backups/"      emacs-ide-var-dir))
(defvar emacs-ide-snippets-dir (expand-file-name "snippets/"     emacs-ide-root-dir))

(dolist (dir (list emacs-ide-core-dir emacs-ide-modules-dir emacs-ide-langs-dir
                   emacs-ide-lib-dir emacs-ide-var-dir emacs-ide-cache-dir
                   emacs-ide-backup-dir emacs-ide-snippets-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(add-to-list 'load-path emacs-ide-core-dir)
(add-to-list 'load-path emacs-ide-lib-dir)
(add-to-list 'load-path emacs-ide-modules-dir)
(add-to-list 'load-path emacs-ide-langs-dir)   ; NEW: lazy lang modules

(emacs-ide--track-phase "directory-setup")

;; ============================================================================
;; CONFIGURATION
;; ============================================================================
(message "📋 Loading configuration...")
;; Use load-file not require — require skips if featurep already set,
;; which happens when native-comp loads a cached .eln on prior startups.
(load-file (expand-file-name "core/emacs-ide-config.el" emacs-ide-root-dir))
(emacs-ide-config-load)
(emacs-ide--track-phase "config-load")

;; ============================================================================
;; SAFE MODE
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
        ;; FIX 2.2.5: find-when-checking avoids startup stat of every package.
        ;; check-on-save added 10-30s on large package sets; removed permanently.
        straight-check-for-modifications  '(find-when-checking)
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
  ;; Unchanged from 2.2.6 — core/ is always loaded eagerly.
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
    "Load core MODULE-NAME with error handling.
Uses load-file instead of load to guarantee the .el source is always
evaluated regardless of:
  - featurep cache (load skips already-provided features)
  - native-comp .eln cache (load uses .eln even with nosuffix=t)
  - stale .elc bytecode from prior Emacs versions
load-file bypasses all three — it always reads and evaluates the
exact file path given, making all defun/defvar forms available."
    (let ((file (expand-file-name (concat module-name ".el") emacs-ide-core-dir)))
      (condition-case err
          (progn
            (load-file file)
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
  ;; Unchanged from 2.2.6.
  ;; ============================================================================
  (defun emacs-ide-setup-exec-path ()
    "Set exec-path and PATH from login shell, with a hard 2-second timeout."
    (with-timeout
        (2 (message "⚠️  exec-path setup timed out (>2s) — using existing PATH"))
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
          ;; FIX-3: pass t (omit-nulls) to exclude empty strings produced when
          ;; PATH ends with a trailing colon. "" in exec-path resolves to cwd.
          (setq exec-path (split-string path-str path-separator t))))))

  (when (memq window-system '(mac ns x pgtk))
    (emacs-ide-setup-exec-path))

  (emacs-ide--track-phase "environment-setup")

  ;; ============================================================================
  ;; CORE SETTINGS
  ;; Unchanged from 2.2.6.
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

  ;; Backup policy
  (setq make-backup-files         t
        backup-directory-alist    `(("." . ,emacs-ide-backup-dir))
        backup-by-copying         t
        version-control           t
        kept-new-versions         6
        kept-old-versions         2
        delete-old-versions       t
        create-lockfiles          nil)

  (fset 'yes-or-no-p 'y-or-n-p)

  (setq inhibit-startup-screen    t
        inhibit-startup-message   t
        initial-scratch-message   nil
        initial-major-mode        'fundamental-mode)

  (electric-pair-mode 1)
  (delete-selection-mode 1)

  (emacs-ide--track-phase "core-settings")

  ;; ============================================================================
  ;; FEATURE MODULES
  ;; ============================================================================
  ;; CHANGES FROM 2.2.6:
  ;;   REMOVED "lang-core"    — replaced by modules/langs/ (lazy-loaded by
  ;;                            tools-project-detect.el and use-package :mode)
  ;;   ADDED   "ui-workspace"               after "ui-dashboard"
  ;;           "core-dev"                   after "editing-core", before "tools-lsp"
  ;;           "apheleia-langs-patch"        after "tools-format"
  ;;           "tools-test-runner-registry"  before "tools-test"
  ;;           "tools-repl"                  after "debug-core"
  ;;           "tools-project-detect"        after "tools-repl"
  ;;           "tools-hydra"                 after "tools-project-detect"
  ;;   keybindings remains last.
  ;; ============================================================================
  (defvar emacs-ide-feature-modules
    '(;; ── UI ────────────────────────────────────────────────────────────────
      "ui-core"                    ; ef-themes · nerd-icons · visual enhancements
      "ui-theme"                   ; ef-themes toggle (F12)
      "ui-modeline"                ; doom-modeline + health segment fallback
      "ui-dashboard"               ; Startup dashboard (redesigned v3)
      "ui-workspace"               ; NEW: perspective.el workspaces + tab-bar
      ;; ── Completion ────────────────────────────────────────────────────────
      "completion-core"            ; vertico · consult · corfu · embark as hub
      "completion-snippets"        ; yasnippet + yasnippet-snippets
      ;; ── Editing ───────────────────────────────────────────────────────────
      "editing-core"               ; smartparens · avy · mc · undo-tree · meow?
      ;; ── Shared lang infrastructure (must load before tools-lsp) ──────────
      "core-dev"                   ; NEW: shared lang API (emacs-ide-dev-*)
      ;; ── Tools ─────────────────────────────────────────────────────────────
      "tools-lsp"                  ; lsp-mode · lsp-ui · eglot · dap-mode
      "tools-project"              ; projectile · treemacs
      "tools-git"                  ; magit · diff-hl · forge · timemachine
      "tools-terminal"             ; vterm · eshell · docker
      "tools-format"               ; apheleia base config
      "apheleia-langs-patch"       ; NEW: 50-lang complete formatter map
      "tools-org"                  ; org-mode · agenda · capture · roam
      "tools-spelling"             ; flyspell across prose and code
      "tools-notes"                ; denote / org-roam
      "tools-rest"                 ; restclient · verb
      ;; ── Test & Debug ──────────────────────────────────────────────────────
      "tools-test-runner-registry" ; NEW: per-lang test dispatch registry API
      "tools-test"                 ; auto-detection fallback + history + hydra
      "debug-core"                 ; DAP adapters + F5-F9 keys
      ;; ── REPL · Project Detection · Hydra ──────────────────────────────────
      "tools-repl"                 ; NEW: unified REPL hub (C-c x r/s/b/d/t)
      "tools-project-detect"       ; NEW: Cargo.toml/go.mod/etc → lang pre-warm
      "tools-hydra"                ; NEW: 10 Hydra menus (C-c h w/b/g/l/p/t/d/u/r/s)
      ;; ── Keybindings — ALWAYS LAST ─────────────────────────────────────────
      "keybindings")               ; All global keys; runs after all modules load
    "Feature modules loaded in order.
lang-core removed — langs/ loaded lazily by tools-project-detect.el
and use-package :mode/:hook triggers. Zero boot cost for lang modules.")

  (defun emacs-ide-load-feature-module (module-name)
    "Load feature MODULE-NAME with fallback.
Uses load-file for the same reason as emacs-ide-load-core-module —
bypasses featurep cache, native-comp .eln cache, and stale .elc files.
load-file always evaluates the exact .el source file."
    (let ((file (expand-file-name (concat module-name ".el") emacs-ide-modules-dir)))
      (condition-case err
          (progn
            (load-file file)
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
  ;; Unchanged from 2.2.6.
  ;; ============================================================================
  (when (and (fboundp 'emacs-ide-health-check-startup)
             (not noninteractive))
    (run-with-idle-timer 1 nil #'emacs-ide-health-check-startup))

  ;; ============================================================================
  ;; TIMER VOID-NIL CLEANUP (Emacs 30 — timers are vectors, slot 5 = function)
  ;; ============================================================================
  (defun emacs-ide--find-and-cancel-void-timers ()
    "Find and cancel any timer whose function slot is nil.
In Emacs 30, timers are vectors — use aref slot 5, not car/cdr."
    (let ((cancelled 0))
      (dolist (timer (append (copy-sequence timer-list)
                             (copy-sequence timer-idle-list)))
        (when (and (vectorp timer)
                   (> (length timer) 5)
                   (null (aref timer 5)))
          (message "emacs-ide: cancelling void timer: %S" timer)
          (cancel-timer timer)
          (cl-incf cancelled)))
      (when (> cancelled 0)
        (message "emacs-ide: cancelled %d void timer(s)" cancelled))))

  ;; Run at weight 99 — before our startup message (weight 100)
  ;; and before after-init-hook packages register additional timers.
  ;; Also run at weight 201 to catch any registered after weight 100.
  (add-hook 'emacs-startup-hook #'emacs-ide--find-and-cancel-void-timers 99)
  (add-hook 'emacs-startup-hook #'emacs-ide--find-and-cancel-void-timers 201)
  ;; window-setup-hook fires AFTER emacs-startup-hook fully completes.
  ;; dashboard-initialize (via dashboard-setup-startup-hook) may use a weight
  ;; higher than 201 and register a void timer after our startup cleanup.
  ;; window-setup-hook is the last chance to cancel before first idle fires.
  (add-hook 'window-setup-hook #'emacs-ide--find-and-cancel-void-timers 99)

  ;; 20s idle scan — identifies any void timer that slips through all hooks.
  (run-with-idle-timer 20 nil #'emacs-ide--find-and-cancel-void-timers)

  ;; ============================================================================
  ;; POST-INIT
  ;; Unchanged from 2.2.6 — startup message and telemetry hook.
  ;; ============================================================================
  (add-hook 'emacs-startup-hook
            (lambda ()
              (emacs-ide--track-phase "startup-complete")
              (let* ((elapsed  (float-time
                                (time-subtract (current-time)
                                               emacs-ide--init-start-time)))
                     (gc-count (- gcs-done emacs-ide--gc-count-start))
                     (pkg-count (condition-case nil
                                    (cond
                                     ;; straight v2+: use recipe cache
                                     ((and (fboundp 'straight--recipes-cached-p)
                                           (boundp 'straight--recipe-cache)
                                           (hash-table-p straight--recipe-cache))
                                      (hash-table-count straight--recipe-cache))
                                     ;; straight v1: build cache
                                     ((and (fboundp 'straight--build-cache)
                                           (hash-table-p straight--build-cache))
                                      (hash-table-count straight--build-cache))
                                     ;; fallback: count build dirs
                                     ((file-directory-p (expand-file-name
                                                         "straight/build"
                                                         user-emacs-directory))
                                      (length (directory-files
                                               (expand-file-name "straight/build"
                                                                  user-emacs-directory)
                                               nil "^[^.]")))
                                     (t 0))
                                  (error 0))))
                (message "🚀 Emacs IDE v%s ready in %.2fs | %d packages | %d GCs | %s%s"
                         emacs-ide-version elapsed pkg-count gc-count
                         (or (bound-and-true-p emacs-ide-display-server) "TTY")
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

;; ============================================================================
;; CUSTOM FILE
;; Unchanged from 2.2.6.
;; ============================================================================
(when (and (boundp 'custom-file) (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;; ============================================================================
;; UTILITY COMMANDS
;; Always defined, even in safe mode.
;; emacs-ide-startup-report: note added re lazy lang modules.
;; emacs-ide-purge-bytecode-cache: now also clears modules/langs/.
;; All other commands unchanged from 2.2.6.
;; ============================================================================

;; ============================================================================
;; EMACS-IDE-TEST — load so M-x emacs-ide-run-tests is available on-demand.
;; FIX-TEST-LOAD: Log success/failure clearly. Forward-declare emacs-ide-run-tests
;; so the command always exists — it prompts to reload if the file failed.
;; ============================================================================

;; Forward declaration: emacs-ide-run-tests always exists as a command.
;; If the test file loaded successfully this is immediately overwritten.
;; If it failed, calling this tells the user what to do.
(defun emacs-ide-run-tests ()
  "Run Enterprise Emacs IDE tests.
If this message appears, emacs-ide-test.el failed to load at startup.
Check: M-x emacs-ide-recovery-view-log  or  M-x load-file core/emacs-ide-test.el"
  (interactive)
  (let ((test-file (expand-file-name "core/emacs-ide-test.el" emacs-ide-root-dir)))
    (if (file-exists-p test-file)
        (progn
          (load-file test-file)
          (if (fboundp 'emacs-ide-run-tests)
              (call-interactively #'emacs-ide-run-tests)
            (message "✗ emacs-ide-test.el loaded but emacs-ide-run-tests not defined")))
      (message "✗ Test file not found: %s" test-file))))

(let ((test-file (expand-file-name "core/emacs-ide-test.el" emacs-ide-root-dir)))
  (if (not (file-exists-p test-file))
      (warn "⚠️  emacs-ide-test.el not found at %s — M-x emacs-ide-run-tests unavailable"
            test-file)
    (condition-case err
        (progn
          (load-file test-file)
          (message "✓ emacs-ide-test loaded"))
      (error
       (warn "⚠️  Could not load emacs-ide-test.el: %s" (error-message-string err))))))

;; ============================================================================
;; EMACS-IDE-SPOT-CHECK — load so M-x emacs-ide-spot-check is always available.
;; Lives in core/ alongside emacs-ide-test.el. On-demand only — not in the
;; core-modules loop. Provides a fast surface-level integrity check for all
;; commands, keybindings, and module features.
;; ============================================================================
(defun emacs-ide-spot-check ()
  "Spot-check all IDE commands and keybindings.
If this message appears, emacs-ide-spot-check.el failed to load.
Check: M-x emacs-ide-recovery-view-log"
  (interactive)
  (let ((f (expand-file-name "core/emacs-ide-spot-check.el" emacs-ide-root-dir)))
    (if (file-exists-p f)
        (progn (load-file f) (call-interactively #'emacs-ide-spot-check))
      (message "✗ emacs-ide-spot-check.el not found: %s" f))))

(let ((sc-file (expand-file-name "core/emacs-ide-spot-check.el" emacs-ide-root-dir)))
  (if (not (file-exists-p sc-file))
      (warn "⚠️  emacs-ide-spot-check.el not found — M-x emacs-ide-spot-check unavailable")
    (condition-case err
        (progn (load-file sc-file) (message "✓ emacs-ide-spot-check loaded"))
      (error
       (warn "⚠️  Could not load emacs-ide-spot-check.el: %s"
             (error-message-string err))))))

;; ============================================================================
;; MODULE DIAGNOSTIC — checks every module is alive after load
;; ============================================================================
(defun emacs-ide-diagnose ()
  "Check every module loaded by init.el and report what's working.
Shows: loaded? · feature provided? · key commands defined?
Run this when M-x commands seem missing."
  (interactive)
  (with-output-to-temp-buffer "*Emacs IDE Diagnostics*"
    (princ (format "=== EMACS IDE v%s DIAGNOSTICS ===\n" emacs-ide-version))
    (princ (format "Emacs %s | %s\n\n" emacs-version
                   (format-time-string "%Y-%m-%d %H:%M")))

    ;; ── Core modules ──────────────────────────────────────────────────────
    (princ "CORE MODULES:\n")
    (dolist (entry
             '((emacs-ide-config   . emacs-ide-config-load)
               (emacs-ide-health   . emacs-ide-health-check-all)
               (emacs-ide-recovery . emacs-ide-recovery-log)
               (emacs-ide-package  . emacs-ide-package-report)
               (emacs-ide-profiler . emacs-ide-profile-start)
               (emacs-ide-security . emacs-ide-security-check)
               (emacs-ide-telemetry . emacs-ide-telemetry-report)))
      (let* ((feat (car entry))
             (cmd  (cdr entry))
             (provided (featurep feat))
             (fn-ok    (fboundp cmd)))
        (princ (format "  %s %-30s  feature:%-5s  %s\n"
                       (if (and provided fn-ok) "✓" "✗")
                       feat
                       (if provided "yes" "NO")
                       (if fn-ok
                           (format "cmd %s: ok" cmd)
                         (format "cmd %s: MISSING" cmd))))))

    ;; ── Feature modules ───────────────────────────────────────────────────
    (princ "\nFEATURE MODULES:\n")
    (dolist (entry
             '(("ui-core"                    ui-core          emacs-ide-presentation-mode)
               ("ui-theme"                   ui-theme         emacs-ide-toggle-theme)
               ("ui-modeline"                ui-modeline      nil)
               ("ui-dashboard"               ui-dashboard     nil)
               ("ui-workspace"               ui-workspace     emacs-ide-workspace-status)
               ("completion-core"            completion-core  nil)
               ("completion-snippets"        completion-snippets nil)
               ("editing-core"               editing-core     nil)
               ("core-dev"                   core-dev         emacs-ide-dev-lang-enabled-p)
               ("tools-lsp"                  tools-lsp        emacs-ide-lsp-status)
               ("tools-project"              tools-project    nil)
               ("tools-project-detect"       tools-project-detect emacs-ide-detect-show-status)
               ("tools-git"                  tools-git        nil)
               ("tools-terminal"             tools-terminal   nil)
               ("tools-format"               tools-format     nil)
               ("apheleia-langs-patch"        apheleia-langs-patch nil)
               ("tools-repl"                 tools-repl       emacs-ide-repl-status)
               ("tools-org"                  tools-org        nil)
               ("tools-spelling"             tools-spelling   nil)
               ("tools-notes"                tools-notes      nil)
               ("tools-rest"                 tools-rest       nil)
               ("tools-test-runner-registry"  tools-test-runner-registry emacs-ide-test-runner-status)
               ("tools-test"                 tools-test       emacs-ide-test-run)
               ("debug-core"                 debug-core       nil)
               ("tools-hydra"                tools-hydra      nil)
               ("keybindings"                keybindings      emacs-ide-show-keybindings-help)))
      (let* ((name     (nth 0 entry))
             (feat     (nth 1 entry))
             (cmd      (nth 2 entry))
             (file     (expand-file-name (concat name ".el") emacs-ide-modules-dir))
             (exists   (file-exists-p file))
             (provided (featurep feat))
             (fn-ok    (or (null cmd) (fboundp cmd))))
        (princ (format "  %s %-38s  file:%-5s  feature:%-5s  %s\n"
                       (if (and exists provided fn-ok) "✓"
                         (if (not exists) "?" "✗"))
                       name
                       (if exists "yes" "NO")
                       (if provided "yes" "NO")
                       (cond
                        ((not exists) "FILE MISSING")
                        ((not provided) "NOT PROVIDED — load failed?")
                        ((not fn-ok) (format "cmd %s MISSING" cmd))
                        (t "ok"))))))

    ;; ── Key functions spot-check ───────────────────────────────────────────
    (princ "\nKEY FUNCTION SPOT-CHECK:\n")
    (dolist (fn '(emacs-ide-health-check-all
                  emacs-ide-run-tests
                  emacs-ide-toggle-theme
                  emacs-ide-detect-show-status
                  emacs-ide-repl-status
                  emacs-ide-test-runner-status
                  emacs-ide-workspace-status
                  emacs-ide-lsp-status
                  emacs-ide-startup-report
                  emacs-ide-show-version))
      (princ (format "  %s %s\n"
                     (if (fboundp fn) "✓" "✗")
                     fn)))

    ;; ── Modes spot-check ──────────────────────────────────────────────────
    (princ "\nMODE STATUS:\n")
    (dolist (mode '(electric-pair-mode
                    show-paren-mode
                    delete-selection-mode
                    display-line-numbers-mode
                    winner-mode))
      (princ (format "  %s %s\n"
                     (if (and (boundp mode) (symbol-value mode)) "✓" "✗")
                     mode)))

    (princ "\nRun M-x emacs-ide-run-tests for the full ERT suite.\n")
    (princ "Any ✗ line above shows exactly what failed to load.\n")))

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
      (princ (format "  %-35s %.3fs\n" (car phase) (cdr phase))))
    (princ "\n")
    (princ "Lang modules are lazy — they do not appear above.\n")
    (princ "They load only when you open a file of that type.\n")
    (princ "Run M-x emacs-ide-detect-show-status for pre-warm state.\n")))

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

(defun emacs-ide-purge-bytecode-cache ()
  "Delete all stale .elc and .eln files from core/, modules/, and modules/langs/.
Run this after upgrading Emacs (e.g. 29 -> 30) to force fresh
source loading on next startup."
  (interactive)
  (when (y-or-n-p "Delete all .elc and .eln cache files? ")
    (let ((count 0))
      (dolist (dir (list (expand-file-name "core/"         user-emacs-directory)
                         (expand-file-name "modules/"      user-emacs-directory)
                         (expand-file-name "modules/langs/" user-emacs-directory))) ; NEW
        (when (file-directory-p dir)
          (dolist (file (directory-files dir t "\\.elc$"))
            (delete-file file)
            (cl-incf count))
          (dolist (file (directory-files dir t "\\.eln$"))
            (delete-file file)
            (cl-incf count))))
      ;; Also clear the var/eln-cache
      (let ((eln-dir (expand-file-name "var/eln-cache/" user-emacs-directory)))
        (when (file-directory-p eln-dir)
          (delete-directory eln-dir t)
          (cl-incf count)))
      (message "✓ Purged %d cached bytecode files. Restart Emacs." count))))

;; ============================================================================
;; KEYBINDING ALIAS — FIX-1
;; keybindings.el binds C-c R to emacs-ide-reload-config. The canonical
;; function is emacs-ide-config-reload (defined in emacs-ide-config.el).
;; Provide the alias here so it is always available regardless of load order.
;; emacs-ide-reload (full restart) is intentionally kept separate.
;; ============================================================================
(defalias 'emacs-ide-reload-config 'emacs-ide-config-reload
  "Reload config.yml and apply settings. Alias for `emacs-ide-config-reload'.
Bound to C-c R by keybindings.el.")

(provide 'init)
;;; init.el ends here
