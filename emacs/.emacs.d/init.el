;;; init.el --- Enterprise Emacs IDE Core Bootstrap -*- lexical-binding: t -*-
;;; Version: 3.2.0 | FIX: emacs-ide-spot-check wrapper caused infinite recursion
;;;           (it redefined the function then called itself).  Now the file is
;;;           simply loaded eagerly; no wrapper needed.
;;; Code:

(defconst emacs-ide-version "3.2.0")
(defconst emacs-ide-minimum-emacs-version "29.1")
(defconst emacs-ide-session-date (format-time-string "%Y-%m-%d"))
(define-obsolete-variable-alias 'emacs-ide-build-date 'emacs-ide-session-date "3.0.4")

(when (version< emacs-version emacs-ide-minimum-emacs-version)
  (error "Emacs IDE requires Emacs %s or higher. You are running %s."
         emacs-ide-minimum-emacs-version emacs-version))

(defvar emacs-ide--init-start-time  (current-time))
(defvar emacs-ide--gc-count-start   gcs-done)
(defvar emacs-ide--startup-phases   nil)

(defun emacs-ide--track-phase (phase-name)
  (push (cons phase-name
              (float-time (time-subtract (current-time) emacs-ide--init-start-time)))
        emacs-ide--startup-phases))

(defvar emacs-ide-root-dir    user-emacs-directory)
(defvar emacs-ide-core-dir    (expand-file-name "core/"         emacs-ide-root-dir))
(defvar emacs-ide-modules-dir (expand-file-name "modules/"      emacs-ide-root-dir))
(defvar emacs-ide-langs-dir   (expand-file-name "modules/langs/" emacs-ide-root-dir))
(defvar emacs-ide-lib-dir     (expand-file-name "lib/"          emacs-ide-root-dir))
(defvar emacs-ide-var-dir     (expand-file-name "var/"          emacs-ide-root-dir))

(dolist (dir (list emacs-ide-core-dir emacs-ide-modules-dir emacs-ide-langs-dir
                   emacs-ide-lib-dir  emacs-ide-var-dir))
  (unless (file-directory-p dir) (make-directory dir t)))

(add-to-list 'load-path emacs-ide-core-dir)
(add-to-list 'load-path emacs-ide-lib-dir)
(add-to-list 'load-path emacs-ide-modules-dir)
(add-to-list 'load-path emacs-ide-langs-dir)

(emacs-ide--track-phase "directory-setup")

;;; ─── Config (must be first — everything else reads from it) ─────────────────

(message "📋 Loading configuration...")
(load-file (expand-file-name "core/emacs-ide-config.el" emacs-ide-root-dir))
(emacs-ide-config-load)   ;; initial load — does NOT fire reload hook
(emacs-ide--track-phase "config-load")

;;; ─── Safe mode short-circuit ────────────────────────────────────────────────

(when (bound-and-true-p emacs-ide-safe-mode)
  (message "⚠️  SAFE MODE: Skipping full configuration")
  (setq-default inhibit-startup-screen t initial-scratch-message nil)
  (load (expand-file-name "emacs-ide-recovery.el" emacs-ide-core-dir) nil t)
  (when (fboundp 'emacs-ide-recovery-mode) (emacs-ide-recovery-mode)))

(unless (bound-and-true-p emacs-ide-safe-mode)

  ;;; ─── straight.el ──────────────────────────────────────────────────────────

  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" emacs-ide-root-dir))
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

  (setq straight-use-package-by-default      t
        straight-check-for-modifications     '(find-when-checking)
        straight-cache-autoloads             t
        straight-vc-git-default-clone-depth  1)

  (emacs-ide--track-phase "package-bootstrap")

  (straight-use-package 'use-package)
  (setq use-package-always-defer          t
        use-package-expand-minimally      t
        use-package-verbose               (bound-and-true-p init-file-debug)
        use-package-minimum-reported-time 0.1)

  (emacs-ide--track-phase "use-package-setup")

  ;;; ─── Core modules ─────────────────────────────────────────────────────────

  (defvar emacs-ide-core-modules
    '("emacs-ide-health" "emacs-ide-diagnose" "emacs-ide-package" "emacs-ide-profiler"
      "emacs-ide-security" "emacs-ide-telemetry" "emacs-ide-recovery"))

  (defun emacs-ide-load-core-module (module-name)
    (let ((file (expand-file-name (concat module-name ".el") emacs-ide-core-dir)))
      (condition-case err
          (progn (load-file file) (message "✓ Core: %s" module-name) t)
        (error
         (warn "✗ Failed core module %s: %s" module-name err)
         (when (string= module-name "emacs-ide-recovery")
           (error "Critical: Recovery module failed: %s" err))
         nil))))

  (message "🔧 Loading core system...")
  (dolist (module emacs-ide-core-modules)
    (emacs-ide-load-core-module module))

  (emacs-ide--track-phase "core-modules")

  ;;; ─── PATH from login shell ────────────────────────────────────────────────

  (defun emacs-ide-setup-exec-path ()
    (with-timeout (2 (message "⚠️  exec-path setup timed out"))
      (let* ((shell    (or (getenv "SHELL") "/bin/sh"))
             (path-str (condition-case nil
                           (replace-regexp-in-string
                            "[ \t\n]*$" ""
                            (shell-command-to-string
                             (format "%s --login -c 'echo $PATH'" shell)))
                         (error (getenv "PATH")))))
        (unless (string-empty-p path-str)
          (setenv "PATH" path-str)
          (setq exec-path (split-string path-str path-separator t))))))

  (when (memq window-system '(mac ns x pgtk))
    (emacs-ide-setup-exec-path))

  (emacs-ide--track-phase "environment-setup")

  ;;; ─── Core settings ────────────────────────────────────────────────────────

  (prefer-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8")

  (setq-default
   indent-tabs-mode    nil
   tab-width           4
   fill-column         100
   require-final-newline t
   truncate-lines      nil
   word-wrap           t
   auto-save-default   nil
   scroll-conservatively 101
   scroll-margin       5)

  (setq make-backup-files    t
        backup-directory-alist `(("." . ,(expand-file-name "var/backups/" emacs-ide-root-dir)))
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

  (electric-pair-mode 1)
  (delete-selection-mode 1)

  (emacs-ide--track-phase "core-settings")

  ;;; ─── Feature modules ──────────────────────────────────────────────────────

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
      "keybindings"))

  (defun emacs-ide-load-feature-module (module-name)
    (let* ((file-in-modules (expand-file-name (concat module-name ".el") emacs-ide-modules-dir))
           (file-at-root    (expand-file-name (concat module-name ".el") emacs-ide-root-dir))
           (file-in-core    (expand-file-name (concat module-name ".el") emacs-ide-core-dir))
           (file-in-lib     (expand-file-name (concat module-name ".el") emacs-ide-lib-dir))
           (file (cond ((file-exists-p file-in-modules) file-in-modules)
                       ((file-exists-p file-at-root)    file-at-root)
                       ((file-exists-p file-in-core)    file-in-core)
                       ((file-exists-p file-in-lib)     file-in-lib))))
      (condition-case err
          (if file
              (progn (load-file file) (message "✓ Feature: %s" module-name) t)
            (warn "✗ Feature module not found: %s" module-name) nil)
        (error (warn "✗ Failed feature module %s: %s" module-name err) nil))))

  (message "🎨 Loading feature modules...")
  (dolist (module emacs-ide-feature-modules)
    (emacs-ide-load-feature-module module))

  (emacs-ide--track-phase "feature-modules")

  ;;; ─── Post-load: optional startup health check ────────────────────────────

  (when (and (fboundp 'emacs-ide-health-run-checks) (not noninteractive))
    (run-with-idle-timer 1 nil #'emacs-ide-health-run-checks))

  ;;; ─── Post-load: cancel void timers left by failed package loads ──────────

  (defun emacs-ide--find-and-cancel-void-timers ()
    (let ((cancelled 0))
      (dolist (timer (append (copy-sequence timer-list)
                             (copy-sequence timer-idle-list)))
        (when (and (vectorp timer) (> (length timer) 5) (null (aref timer 5)))
          (cancel-timer timer)
          (cl-incf cancelled)))
      (when (> cancelled 0)
        (message "emacs-ide: cancelled %d void timer(s)" cancelled))))

  (add-hook 'emacs-startup-hook #'emacs-ide--find-and-cancel-void-timers 99)
  (add-hook 'emacs-startup-hook #'emacs-ide--find-and-cancel-void-timers 201)
  (add-hook 'window-setup-hook  #'emacs-ide--find-and-cancel-void-timers 99)
  (run-with-idle-timer 20 nil #'emacs-ide--find-and-cancel-void-timers)

  (add-hook 'emacs-startup-hook
            (lambda ()
              (emacs-ide--track-phase "startup-complete")
              (let* ((elapsed   (float-time (time-subtract (current-time)
                                                           emacs-ide--init-start-time)))
                     (gc-count  (- gcs-done emacs-ide--gc-count-start))
                     (pkg-count (if (and (boundp 'straight--recipe-cache)
                                         (hash-table-p straight--recipe-cache))
                                    (hash-table-count straight--recipe-cache)
                                  0)))
                (message "🚀 Emacs IDE v%s ready in %.2fs | GCs: %d"
                         emacs-ide-version elapsed gc-count)
                ;; Log startup metrics to telemetry if enabled
                (when (fboundp 'emacs-ide-telemetry-log-startup)
                  (emacs-ide-telemetry-log-startup elapsed gc-count pkg-count))))
            100))   ;; end (unless emacs-ide-safe-mode ...)

;;; ─── Kill hook ───────────────────────────────────────────────────────────────

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (and (boundp 'emacs-ide-health--periodic-timer)
                       emacs-ide-health--periodic-timer)
              (cancel-timer emacs-ide-health--periodic-timer))
            (when (and (boundp 'emacs-ide-recovery--session-timer)
                       emacs-ide-recovery--session-timer)
              (cancel-timer emacs-ide-recovery--session-timer))))

;;; ─── Custom file ─────────────────────────────────────────────────────────────

(when (and (stringp custom-file) (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;;; ─── Eagerly load test + spot-check modules ──────────────────────────────────
;; Both files are loaded once here with a simple condition-case guard.
;; No wrapper functions are defined — that was the source of infinite recursion:
;;   (defun emacs-ide-spot-check ()            ;; redefines the fn just loaded
;;     ...
;;     (call-interactively #'emacs-ide-spot-check))  ;; calls itself → stack overflow

(let ((test-file (expand-file-name "core/emacs-ide-test.el" emacs-ide-root-dir)))
  (if (file-exists-p test-file)
      (condition-case err
          (progn (load-file test-file) (message "✓ emacs-ide-test loaded"))
        (error (warn "⚠️  Could not load emacs-ide-test.el: %s" (error-message-string err))))
    (warn "⚠️  emacs-ide-test.el not found at %s" test-file)))

(let ((sc-file (expand-file-name "core/emacs-ide-spot-check.el" emacs-ide-root-dir)))
  (if (file-exists-p sc-file)
      (condition-case err
          (progn (load-file sc-file) (message "✓ emacs-ide-spot-check loaded"))
        (error (warn "⚠️  Could not load emacs-ide-spot-check.el: %s" (error-message-string err))))
    (warn "⚠️  emacs-ide-spot-check.el not found at %s" sc-file)))

;; Fallback stub only if the diagnose module itself failed to load
(unless (fboundp 'emacs-ide-diagnose)
  (defun emacs-ide-diagnose ()
    (interactive)
    (message "emacs-ide-diagnose.el failed to load — see *Messages* for errors.")))

;;; ─── Interactive helpers ─────────────────────────────────────────────────────

(defun emacs-ide-run-tests ()
  "Run all Enterprise Emacs IDE ERT tests interactively."
  (interactive)
  (if (featurep 'emacs-ide-test)
      (let ((ert-verbose t))
        (ert-run-tests-batch "^test-emacs-ide-"))
    (let ((test-file (expand-file-name "core/emacs-ide-test.el" emacs-ide-root-dir)))
      (if (file-exists-p test-file)
          (progn
            (load-file test-file)
            (let ((ert-verbose t))
              (ert-run-tests-batch "^test-emacs-ide-")))
        (message "✗ Test file not found: %s" test-file)))))

(defun emacs-ide-show-version ()
  (interactive)
  (message "Emacs IDE v%s | Emacs %s | %s | Session: %s"
           emacs-ide-version emacs-version
           (or (bound-and-true-p emacs-ide-display-server) "TTY")
           emacs-ide-session-date))

(defun emacs-ide-startup-report ()
  (interactive)
  (with-output-to-temp-buffer "*Startup Report*"
    (princ (format "=== EMACS IDE v%s STARTUP REPORT ===\n\n" emacs-ide-version))
    (princ (format "Emacs:   %s\n" emacs-version))
    (princ (format "Display: %s\n" (or (bound-and-true-p emacs-ide-display-server) "TTY")))
    (princ (format "Session: %s\n\n" emacs-ide-session-date))
    (princ "Startup Phases:\n")
    (dolist (phase (reverse emacs-ide--startup-phases))
      (princ (format "  %-35s %.3fs\n" (car phase) (cdr phase))))
    (princ "\nLang modules are lazy — they load on file open.\n")))

(defun emacs-ide-reload ()
  (interactive)
  (when (y-or-n-p "Reload init.el configuration? ")
    (load-file user-init-file)
    (message "✓ Configuration reloaded")))

(defun emacs-ide-purge-bytecode-cache ()
  (interactive)
  (when (y-or-n-p "Delete all .elc and .eln cache files? ")
    (let ((count 0))
      (dolist (dir (list (expand-file-name "core/"          user-emacs-directory)
                         (expand-file-name "modules/"       user-emacs-directory)
                         (expand-file-name "modules/langs/" user-emacs-directory)))
        (when (file-directory-p dir)
          (dolist (file (directory-files-recursively dir "\\.elc$"))
            (delete-file file) (cl-incf count))
          (dolist (file (directory-files-recursively dir "\\.eln$"))
            (delete-file file) (cl-incf count))))
      (let ((eln-dir (expand-file-name "var/eln-cache/" user-emacs-directory)))
        (when (file-directory-p eln-dir)
          (delete-directory eln-dir t) (cl-incf count)))
      (message "✓ Purged %d cached bytecode files. Restart Emacs." count))))

(defalias 'emacs-ide-reload-config 'emacs-ide-config-reload
  "Reload config.yml and apply settings.")

(defun emacs-ide-update ()
  "Update all straight.el packages to their latest versions.
Pulls from upstream for every package then rebuilds.
Restart Emacs afterwards to load the new versions."
  (interactive)
  (if (fboundp 'straight-pull-all)
      (progn
        (message "📦 Updating all packages via straight.el…")
        (straight-pull-all)
        (straight-rebuild-all)
        (message "✓ Packages updated. Restart Emacs to load new versions."))
    (message "⚠ straight.el not available — cannot update packages.")))

(defun emacs-ide-freeze-versions ()
  "Write current package versions to straight/versions/default.el.
Use this to pin a known-good state before running emacs-ide-update."
  (interactive)
  (if (fboundp 'straight-freeze-versions)
      (progn
        (straight-freeze-versions)
        (message "✓ Package versions frozen to straight/versions/default.el"))
    (message "⚠ straight.el not available — cannot freeze versions.")))

(provide 'init)
;;; init.el ends here
