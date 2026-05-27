;;; init.el --- Enterprise Emacs IDE Core Bootstrap -*- lexical-binding: t -*-
;;; Version: 3.4.1
;;;
;;; Code:

(require 'cl-lib)

;;;; ── Version constants ───────────────────────────────────────────────────────

(defconst emacs-ide-version "3.4.1")
(defconst emacs-ide-minimum-emacs-version "29.1")
(defconst emacs-ide-session-date (format-time-string "%Y-%m-%d"))
(define-obsolete-variable-alias
  'emacs-ide-build-date 'emacs-ide-session-date "3.0.4")

(when (version< emacs-version emacs-ide-minimum-emacs-version)
  (error "Emacs IDE requires Emacs %s or higher.  Running: %s"
         emacs-ide-minimum-emacs-version emacs-version))

;;;; ── Startup timing ──────────────────────────────────────────────────────────

(defvar emacs-ide--init-start-time (current-time))
(defvar emacs-ide--gc-count-start  gcs-done)
(defvar emacs-ide--startup-phases  nil)

(defun emacs-ide--track-phase (phase-name)
  "Record elapsed time since init start for PHASE-NAME."
  (push (cons phase-name
              (float-time
               (time-subtract (current-time) emacs-ide--init-start-time)))
        emacs-ide--startup-phases))

;;;; ── Directory structure ─────────────────────────────────────────────────────

(defvar emacs-ide-root-dir    user-emacs-directory)
(defvar emacs-ide-core-dir    (expand-file-name "core/"          emacs-ide-root-dir))
(defvar emacs-ide-modules-dir (expand-file-name "modules/"       emacs-ide-root-dir))
(defvar emacs-ide-langs-dir   (expand-file-name "modules/langs/" emacs-ide-root-dir))
(defvar emacs-ide-lib-dir     (expand-file-name "lib/"           emacs-ide-root-dir))
(defvar emacs-ide-var-dir     (expand-file-name "var/"           emacs-ide-root-dir))

(dolist (dir (list emacs-ide-core-dir
                   emacs-ide-modules-dir
                   emacs-ide-langs-dir
                   emacs-ide-lib-dir
                   emacs-ide-var-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(add-to-list 'load-path emacs-ide-core-dir)
(add-to-list 'load-path emacs-ide-lib-dir)
(add-to-list 'load-path emacs-ide-modules-dir)
(add-to-list 'load-path emacs-ide-langs-dir)

(emacs-ide--track-phase "directory-setup")

;;;; ── Config (must be first — every module reads from it) ────────────────────

(message "📋 Loading configuration...")
(load-file (expand-file-name "core/emacs-ide-config.el" emacs-ide-root-dir))
(emacs-ide-config-load)
(emacs-ide--track-phase "config-load")

;;;; ── Safe mode short-circuit ─────────────────────────────────────────────────

(when (bound-and-true-p emacs-ide-safe-mode)
  (message "⚠️  SAFE MODE: Skipping full configuration")
  (setq-default inhibit-startup-screen t
                initial-scratch-message nil)
  (load (expand-file-name "emacs-ide-recovery.el" emacs-ide-core-dir) nil t)
  (when (fboundp 'emacs-ide-recovery-mode)
    (emacs-ide-recovery-mode)))

(unless (bound-and-true-p emacs-ide-safe-mode)

  ;;; ── straight.el ────────────────────────────────────────────────────────────

  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          emacs-ide-root-dir))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (message "📦 Bootstrapping straight.el (first run only)...")
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (setq straight-use-package-by-default     t
        straight-check-for-modifications    '(find-when-checking)
        straight-cache-autoloads            t
        straight-vc-git-default-clone-depth 1)

  (emacs-ide--track-phase "package-bootstrap")

  (straight-use-package 'use-package)
  (setq use-package-always-defer          t
        use-package-expand-minimally      t
        use-package-verbose               (bound-and-true-p init-file-debug)
        use-package-minimum-reported-time 0.1)

  (emacs-ide--track-phase "use-package-setup")

  ;;; ── Core modules ───────────────────────────────────────────────────────────

  (defvar emacs-ide-core-modules
    '("emacs-ide-health"
      "emacs-ide-diagnose"
      "emacs-ide-package"
      "emacs-ide-profiler"
      "emacs-ide-security"
      "emacs-ide-telemetry"
      "emacs-ide-recovery")
    "Core infrastructure modules loaded eagerly at startup.")

  (defun emacs-ide-load-core-module (module-name)
    "Load MODULE-NAME from `emacs-ide-core-dir'."
    (let ((file (expand-file-name (concat module-name ".el")
                                  emacs-ide-core-dir)))
      (condition-case err
          (progn (load-file file)
                 (message "✓ Core: %s" module-name)
                 t)
        (error
         (warn "✗ Failed core module %s: %s" module-name err)
         (when (string= module-name "emacs-ide-recovery")
           (error "Critical: recovery module failed: %s" err))
         nil))))

  (message "🔧 Loading core system...")
  (dolist (module emacs-ide-core-modules)
    (emacs-ide-load-core-module module))

  (unless (fboundp 'emacs-ide-diagnose)
    (defun emacs-ide-diagnose ()
      (interactive)
      (message
       "✗ emacs-ide-diagnose.el failed to load — check *Messages* for the error.")))

  (emacs-ide--track-phase "core-modules")

  ;;; ── exec-path-from-shell ─────────────────────────────────────────────────

  (use-package exec-path-from-shell
    :if (or (display-graphic-p) (daemonp))
    :demand t
    :config
    (dolist (var '("SSH_AUTH_SOCK"
                   "SSH_AGENT_PID"
                   "GPG_AGENT_INFO"
                   "GOPATH"
                   "GOROOT"
                   "PYTHONPATH"
                   "JAVA_HOME"
                   "CARGO_HOME"
                   "RUSTUP_HOME"
                   "LANG"
                   "LC_CTYPE"
                   "NVM_DIR"
                   "PYENV_ROOT"
                   "RBENV_ROOT"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)
    (message "✓ exec-path-from-shell: PATH and env vars imported from login shell"))

  (emacs-ide--track-phase "environment-setup")

  ;;; ── Core settings ───────────────────────────────────────────────────────────

  (prefer-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8")

  (setq-default
   indent-tabs-mode      nil
   tab-width             4
   fill-column           100
   require-final-newline t
   truncate-lines        nil
   word-wrap             t
   auto-save-default     nil
   scroll-conservatively 101
   scroll-margin         5)

  (global-so-long-mode 1)

  (setq-default bidi-paragraph-direction 'left-to-right
                bidi-inhibit-bpa         t)

  (setq bookmark-save-flag 1)

  (setq switch-to-buffer-obey-display-actions t
        switch-to-buffer-in-dedicated-window  'pop)

  ;;; ── Versioned backups ───────────────────────────────────────────────────────

  (setq make-backup-files    t
        backup-directory-alist
        `(("." . ,(expand-file-name "var/backups/" emacs-ide-root-dir)))
        backup-by-copying    t
        version-control      t
        kept-new-versions    6
        kept-old-versions    2
        delete-old-versions  t
        create-lockfiles     nil)

  (if (boundp 'use-short-answers)
      (setq use-short-answers t)
    (fset 'yes-or-no-p 'y-or-n-p))

  (setq inhibit-startup-screen  t
        inhibit-startup-message t
        initial-scratch-message nil
        initial-major-mode      'fundamental-mode)

  (emacs-ide--track-phase "core-settings")

  ;;; ── Feature modules ─────────────────────────────────────────────────────────

  (defvar emacs-ide-feature-modules
    '("ui-core" "ui-theme" "ui-modeline" "ui-dashboard" "ui-workspace"
      "completion-core" "completion-snippets"
      "editing-core"
      "core-dev"
      "tools-lsp" "tools-project" "tools-git" "tools-terminal"
      "tools-format" "apheleia-langs-patch"
      "tools-org" "tools-spelling" "tools-notes" "tools-rest"
      "tools-test-runner-registry" "tools-test" "debug-core"
      "tools-repl" "tools-project-detect" "tools-hydra"
      "keybindings")
    "Feature modules loaded eagerly in order.
keybindings must remain last so its global-set-key calls override any
:bind declarations in earlier modules.")

  (defun emacs-ide-load-feature-module (module-name)
    "Load MODULE-NAME searching core/, modules/, lib/, and root dirs."
    (let* ((in-modules (expand-file-name
                        (concat module-name ".el") emacs-ide-modules-dir))
           (at-root    (expand-file-name
                        (concat module-name ".el") emacs-ide-root-dir))
           (in-core    (expand-file-name
                        (concat module-name ".el") emacs-ide-core-dir))
           (in-lib     (expand-file-name
                        (concat module-name ".el") emacs-ide-lib-dir))
           (file (cond ((file-exists-p in-modules) in-modules)
                       ((file-exists-p at-root)    at-root)
                       ((file-exists-p in-core)    in-core)
                       ((file-exists-p in-lib)     in-lib))))
      (condition-case err
          (if file
              (progn (load-file file)
                     (message "✓ Feature: %s" module-name)
                     t)
            (warn "✗ Feature module not found: %s" module-name)
            nil)
        (error
         (warn "✗ Failed feature module %s: %s" module-name err)
         nil))))

  (message "🎨 Loading feature modules...")
  (dolist (module emacs-ide-feature-modules)
    (emacs-ide-load-feature-module module))

  (emacs-ide--track-phase "feature-modules")

  ;;; ── Post-load: startup health check ──────────────────────────────────────────

  (when (and (fboundp 'emacs-ide-health-run-checks)
             (not noninteractive))
    (run-with-idle-timer 1 nil #'emacs-ide-health-run-checks))

  ;;; ── Post-load: cancel broken timers ─────────────────────────────────────────

  (defun emacs-ide--find-and-cancel-void-timers ()
    "Cancel timers whose function slot is nil."
    (let ((cancelled 0))
      (dolist (timer (append (copy-sequence timer-list)
                             (copy-sequence timer-idle-list)))
        (when (and (vectorp timer)
                   (> (length timer) 6)
                   (null (aref timer 5)))
          (cancel-timer timer)
          (cl-incf cancelled)))
      (when (> cancelled 0)
        (message "emacs-ide: cancelled %d void timer(s)" cancelled))))

  (add-hook 'emacs-startup-hook #'emacs-ide--find-and-cancel-void-timers 99)
  (add-hook 'emacs-startup-hook #'emacs-ide--find-and-cancel-void-timers 201)
  (add-hook 'window-setup-hook  #'emacs-ide--find-and-cancel-void-timers 99)
  (run-with-idle-timer 20 nil #'emacs-ide--find-and-cancel-void-timers)

  ;;; ── Startup complete ──────────────────────────────────────────────────────────

  (add-hook 'emacs-startup-hook
            (lambda ()
              (emacs-ide--track-phase "startup-complete")
              (let* ((elapsed  (float-time
                                (time-subtract (current-time)
                                               emacs-ide--init-start-time)))
                     (gc-count (- gcs-done emacs-ide--gc-count-start))
                     (pkg-count
                      (if (and (boundp 'straight--recipe-cache)
                               (hash-table-p straight--recipe-cache))
                          (hash-table-count straight--recipe-cache)
                        0)))
                (message "🚀 Emacs IDE v%s ready in %.2fs | GCs: %d"
                         emacs-ide-version elapsed gc-count)
                (when (fboundp 'emacs-ide-telemetry-log-startup)
                  (emacs-ide-telemetry-log-startup elapsed gc-count pkg-count))))
            100)

  ;;; ── Reload helpers (normal-boot only) ───────────────────────────────────────

  (defun emacs-ide-reload ()
    "Deprecated.  Use `emacs-ide-reload-init' or `emacs-ide-config-reload'."
    (interactive)
    (message "emacs-ide-reload is deprecated.  \
Use C-c R (emacs-ide-config-reload) for safe config reload, \
or M-x emacs-ide-reload-init for full init reload.")
    (when (yes-or-no-p "Run emacs-ide-config-reload (safe) instead? ")
      (emacs-ide-config-reload)))

) ;; end (unless emacs-ide-safe-mode)

;;;; ── Kill hook ───────────────────────────────────────────────────────────────

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (and (boundp 'emacs-ide-health--periodic-timer)
                       emacs-ide-health--periodic-timer)
              (cancel-timer emacs-ide-health--periodic-timer))
            (when (and (boundp 'emacs-ide-recovery--session-timer)
                       emacs-ide-recovery--session-timer)
              (cancel-timer emacs-ide-recovery--session-timer))
            (when (and (boundp 'emacs-ide-theme--auto-timer)
                       emacs-ide-theme--auto-timer)
              (cancel-timer emacs-ide-theme--auto-timer)
              (setq emacs-ide-theme--auto-timer nil))))

;;;; ── Custom file ─────────────────────────────────────────────────────────────

(when (and (stringp custom-file) (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;;;; ── On-demand loader helpers ───────────────────────────────────────────────

(defun emacs-ide--ensure-test-loaded ()
  "Load emacs-ide-test.el on demand if not already loaded."
  (unless (featurep 'emacs-ide-test)
    (let ((file (expand-file-name "core/emacs-ide-test.el" emacs-ide-root-dir)))
      (if (file-exists-p file)
          (condition-case err
              (load-file file)
            (error (warn "⚠️  emacs-ide-test.el: %s" (error-message-string err))))
        (warn "⚠️  emacs-ide-test.el not found at %s" file)))))

(defun emacs-ide--ensure-spot-check-loaded ()
  "Load emacs-ide-spot-check.el on demand if not already loaded."
  (unless (featurep 'emacs-ide-spot-check)
    (let ((file (expand-file-name "core/emacs-ide-spot-check.el" emacs-ide-root-dir)))
      (if (file-exists-p file)
          (condition-case err
              (load-file file)
            (error (warn "⚠️  emacs-ide-spot-check.el: %s" (error-message-string err))))
        (warn "⚠️  emacs-ide-spot-check.el not found at %s" file)))))

(defvar emacs-ide--spot-check-run nil
  "Internal: set by emacs-ide-spot-check.el to the real spot-check function.")

;;;; ── Interactive helpers ─────────────────────────────────────────────────────

(defun emacs-ide-run-tests ()
  "Run all Emacs IDE ERT tests and display results in the *ert* browser.

Loads emacs-ide-test.el on demand if not already present.  Once loaded,
the function defined there overwrites this bootstrap wrapper."
  (interactive)
  (emacs-ide--ensure-test-loaded)
  (if (featurep 'emacs-ide-test)
      (ert "^test-emacs-ide-")
    (message "✗ emacs-ide-test failed to load — check *Messages*")))

(defun emacs-ide-spot-check ()
  "Run the IDE spot-check (commands, keybindings, module features).

Loads emacs-ide-spot-check.el on demand.  Dispatches through the private
`emacs-ide--spot-check-run' symbol set by that file, so there is no
ambiguity with the public function it also defines."
  (interactive)
  (emacs-ide--ensure-spot-check-loaded)
  (cond
   ((functionp emacs-ide--spot-check-run)
    (call-interactively emacs-ide--spot-check-run))
   ;; Fallback: if the file loaded but didn't set the trampoline (shouldn't
   ;; happen), call the public symbol directly — by now it IS the real fn.
   ((featurep 'emacs-ide-spot-check)
    (call-interactively (symbol-function 'emacs-ide-spot-check)))
   (t
    (message "✗ emacs-ide-spot-check failed to load — check *Messages*"))))

(defun emacs-ide-show-version ()
  "Display the IDE version string."
  (interactive)
  (message "Emacs IDE v%s | Emacs %s | %s | Session: %s"
           emacs-ide-version
           emacs-version
           (or (bound-and-true-p emacs-ide-display-server) "TTY")
           emacs-ide-session-date))

(defun emacs-ide-startup-report ()
  "Show a buffer with startup phase timings."
  (interactive)
  (with-output-to-temp-buffer "*Startup Report*"
    (princ (format "=== EMACS IDE v%s STARTUP REPORT ===\n\n"
                   emacs-ide-version))
    (princ (format "Emacs:   %s\n" emacs-version))
    (princ (format "Display: %s\n"
                   (or (bound-and-true-p emacs-ide-display-server) "TTY")))
    (princ (format "Session: %s\n\n" emacs-ide-session-date))
    (princ "Startup Phases:\n")
    (dolist (phase (reverse emacs-ide--startup-phases))
      (princ (format "  %-35s %.3fs\n" (car phase) (cdr phase))))
    (princ "\nLang modules are lazy — they load on first file open.\n")
    (princ "Test/spot-check modules are on-demand — not loaded at startup.\n")))

;;;; ── Reload helpers (always available) ──────────────────────────────────────

(defun emacs-ide-reload-init ()
  "Reload the ENTIRE init.el from disk.

⚠️  DESTRUCTIVE: All hooks will be added a second time.  Use only as a
last resort — prefer M-x emacs-ide-config-reload for config-only reloads."
  (interactive)
  (when (yes-or-no-p
         (concat "⚠️  DESTRUCTIVE reload of init.el?\n"
                 "All hooks will be duplicated.  "
                 "For config-only reload use emacs-ide-config-reload.  "
                 "Proceed? "))
    (load-file user-init-file)
    (message "✓ init.el reloaded (hooks may be duplicated — restart recommended)")))

;;;; ── Package management ──────────────────────────────────────────────────────

(defun emacs-ide-purge-bytecode-cache ()
  "Delete all .elc and .eln cache files, then prompt to restart Emacs."
  (interactive)
  (when (y-or-n-p "Delete all .elc and .eln cache files? ")
    (let ((count 0))
      (dolist (dir (list (expand-file-name "core/"          user-emacs-directory)
                         (expand-file-name "modules/"       user-emacs-directory)
                         (expand-file-name "modules/langs/" user-emacs-directory)))
        (when (file-directory-p dir)
          (dolist (f (directory-files-recursively dir "\\.elc$"))
            (delete-file f) (cl-incf count))
          (dolist (f (directory-files-recursively dir "\\.eln$"))
            (delete-file f) (cl-incf count))))
      (let ((eln-dir (expand-file-name "var/eln-cache/" user-emacs-directory)))
        (when (file-directory-p eln-dir)
          (delete-directory eln-dir t)
          (cl-incf count)))
      (message "✓ Purged %d cached bytecode file(s).  Restart Emacs." count))))

(defun emacs-ide-update ()
  "Update all straight.el packages to their latest versions."
  (interactive)
  (if (fboundp 'straight-pull-all)
      (progn
        (message "📦 Updating packages via straight.el…")
        (straight-pull-all)
        (straight-rebuild-all)
        (message "✓ Packages updated.  Restart Emacs to load new versions."))
    (message "⚠ straight.el not available.")))

(defun emacs-ide-freeze-versions ()
  "Freeze current package versions to straight/versions/default.el."
  (interactive)
  (if (fboundp 'straight-freeze-versions)
      (progn
        (straight-freeze-versions)
        (message "✓ Versions frozen to straight/versions/default.el"))
    (message "⚠ straight.el not available.")))

(provide 'init)
;;; init.el ends here
