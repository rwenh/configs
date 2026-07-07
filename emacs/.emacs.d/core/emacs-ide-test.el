;;; emacs-ide-test.el --- Enterprise Test Suite -*- lexical-binding: t -*-
;;;
;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar emacs-ide-test-timeout 10)
(defvar emacs-ide-startup-time-target 3.0
  "Maximum acceptable startup time in seconds.  Set by config.yml performance.startup-time-target.")
(defvar emacs-ide-test-startup-time-max
  (if (boundp 'emacs-ide-startup-time-target)
      emacs-ide-startup-time-target
    3.0))

(defun emacs-ide-test-package-installed-p (package)
  "Return non-nil if PACKAGE feature is loaded or its library is on load-path."
  (or (featurep package)
      (locate-library (symbol-name package))))

(defun emacs-ide-test-file-exists-p (file)
  "Return non-nil if FILE exists under `user-emacs-directory'."
  (file-exists-p (expand-file-name file user-emacs-directory)))

(defun emacs-ide-test--find-phase (phase-name)
  "Return elapsed time for PHASE-NAME in the startup phases list, or nil."
  (when (boundp 'emacs-ide--startup-phases)
    (cdr (assoc phase-name emacs-ide--startup-phases))))

;;; ─── Core / environment ──────────────────────────────────────────────────────

(ert-deftest test-emacs-ide-emacs-version ()
  "Emacs version is 29.1 or higher."
  (should (version<= "29.1" emacs-version)))

(ert-deftest test-emacs-ide-startup-time ()
  "Startup phases were recorded and elapsed time is within limit."
  (skip-unless (boundp 'emacs-ide--startup-phases))
  (skip-unless emacs-ide--startup-phases)
  (let ((startup-time
         (or (emacs-ide-test--find-phase "startup-complete")
             (cdr (car (last emacs-ide--startup-phases)))
             9999)))
    (should (< startup-time emacs-ide-test-startup-time-max))))

(ert-deftest test-emacs-ide-directory-structure ()
  "Required directories exist."
  (dolist (dir '("core" "modules" "modules/langs" "lib" "var"))
    (should (file-directory-p (expand-file-name dir user-emacs-directory)))))

(ert-deftest test-emacs-ide-langs-load-path ()
  "modules/langs/ directory is on load-path."
  (should (member (expand-file-name "modules/langs/" user-emacs-directory)
                  load-path)))

;;; ─── Core modules ────────────────────────────────────────────────────────────

(ert-deftest test-emacs-ide-core-modules-provided ()
  "All core modules provide their features."
  (dolist (feature '(emacs-ide-config
                     emacs-ide-health
                     emacs-ide-recovery
                     emacs-ide-package
                     emacs-ide-profiler
                     emacs-ide-security
                     emacs-ide-telemetry))
    (should (featurep feature))))

(ert-deftest test-emacs-ide-config-loaded ()
  "Configuration was successfully loaded."
  (should (bound-and-true-p emacs-ide-config-loaded-p)))

(ert-deftest test-emacs-ide-spot-check-feature ()
  "emacs-ide-spot-check feature is provided."
  (should (featurep 'emacs-ide-spot-check)))

;;; ─── Health system ───────────────────────────────────────────────────────────

(ert-deftest test-emacs-ide-health-check-system ()
  "Health check system functions exist — including auto-fix."
  (should (fboundp 'emacs-ide-health-check-all))
  (should (fboundp 'emacs-ide-health-check-system-tools))
  ;; emacs-ide-health-auto-fix is now defined in emacs-ide-health.el
  (should (fboundp 'emacs-ide-health-auto-fix)))

(ert-deftest test-emacs-ide-health-check-runs ()
  "Health check singular runner returns a valid (KEY . plist) cons."
  ;; emacs-ide-health-run-check (singular) is defined in emacs-ide-health.el
  (skip-unless (fboundp 'emacs-ide-health-run-check))
  (skip-unless (fboundp 'emacs-ide-health-check-system-tools))
  (let ((saved-results    emacs-ide-health-results)
        (saved-timestamp  emacs-ide-health-last-check))
    (unwind-protect
        (let ((res (emacs-ide-health-run-check
                    'system-tools
                    #'emacs-ide-health-check-system-tools)))
          (should (consp res))
          (should (eq (car res) 'system-tools))
          (should (listp (cdr res)))
          (should (memq (plist-get (cdr res) :status) '(ok warning error))))
      (setq emacs-ide-health-results   saved-results
            emacs-ide-health-last-check saved-timestamp))))

;;; ─── Recovery system ─────────────────────────────────────────────────────────

(ert-deftest test-emacs-ide-recovery-system ()
  "Recovery system core functions and variables are present."
  (should (fboundp 'emacs-ide-recovery-log))
  (should (fboundp 'emacs-ide-recovery-backup-config))
  (should (fboundp 'emacs-ide-recovery-mode))
  (should (fboundp 'emacs-ide-recovery-reset-crash-count))
  (should (fboundp 'emacs-ide-recovery-disable-package))
  (should (boundp  'emacs-ide-recovery-crash-count)))

;;; ─── Packages / completion ───────────────────────────────────────────────────

(ert-deftest test-emacs-ide-critical-packages-installed ()
  "Critical packages are installed or available."
  (let* ((backend (if (boundp 'emacs-ide-completion-backend)
                      emacs-ide-completion-backend
                    'corfu))
         (pkgs (append '(use-package which-key projectile magit vertico lsp-mode)
                       (list backend))))
    (dolist (pkg pkgs)
      (should (emacs-ide-test-package-installed-p pkg)))))

(ert-deftest test-emacs-ide-lsp-availability ()
  "LSP mode is available when configured."
  (when (bound-and-true-p emacs-ide-lsp-enable)
    (should (or (emacs-ide-test-package-installed-p 'lsp-mode)
                (emacs-ide-test-package-installed-p 'eglot)))))

(ert-deftest test-emacs-ide-completion-framework ()
  "Completion framework is active or available."
  (should (or (bound-and-true-p vertico-mode)
              (bound-and-true-p corfu-mode)
              (emacs-ide-test-package-installed-p 'vertico)
              (emacs-ide-test-package-installed-p 'corfu))))

;;; ─── Editing / UI ────────────────────────────────────────────────────────────

(ert-deftest test-emacs-ide-basic-editing-modes ()
  "Basic editing modes are enabled."
  (should (bound-and-true-p electric-pair-mode))
  (should (bound-and-true-p show-paren-mode))
  (should (bound-and-true-p delete-selection-mode)))

(ert-deftest test-emacs-ide-theme-or-modeline ()
  "At least a theme or modeline is active."
  (should (or (and (boundp 'custom-enabled-themes) (consp custom-enabled-themes))
              (bound-and-true-p doom-modeline-mode))))

;;; ─── Tools ───────────────────────────────────────────────────────────────────

(ert-deftest test-emacs-ide-git-executable ()
  "Git executable is found on PATH."
  (should (executable-find "git")))

(ert-deftest test-emacs-ide-required-files-exist ()
  "Essential config and core files exist."
  (dolist (file '("early-init.el"
                  "init.el"
                  "config.yml"
                  "core/emacs-ide-health.el"
                  "core/emacs-ide-recovery.el"))
    (should (emacs-ide-test-file-exists-p file))))

;;; ─── Runner ──────────────────────────────────────────────────────────────────

(defun emacs-ide-run-tests ()
  "Run all Enterprise Emacs IDE ERT tests interactively."
  (interactive)
  (let ((ert-verbose t))
    (ert-run-tests-batch "^test-emacs-ide-")))

(provide 'emacs-ide-test)
;;; emacs-ide-test.el ends here
