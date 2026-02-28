;;; emacs-ide-test.el --- Enterprise Test Suite -*- lexical-binding: t -*-
;;; Commentary:
;;; ERT-based tests adapted to calibrated module names and robust guards.
;;; Version: 2.2.1  (fixes from 2.1.0 audit)
;;; Fixes:
;;;   - test-startup-time: `car` of push-built list is the LAST phase, not first.
;;;     Now correctly finds the "startup-complete" phase by name, with fallback.
;;;   - test-basic-editing-modes: `electric-pair-mode` now enabled in init core
;;;     settings so test is reliable. Guard added if modules failed.
;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar emacs-ide-test-timeout 10
  "Timeout for individual tests in seconds.")

(defvar emacs-ide-test-startup-time-max 5.0
  "Maximum acceptable startup time in seconds (CI-friendly).")

(defun emacs-ide-test-package-installed-p (package)
  "Check if PACKAGE is installed via featurep (straight API is internal)."
  (featurep package))

(defun emacs-ide-test-file-exists-p (file)
  "Check if FILE exists under user-emacs-directory."
  (file-exists-p (expand-file-name file user-emacs-directory)))

(defun emacs-ide-test--find-phase (phase-name)
  "Return elapsed time for PHASE-NAME in `emacs-ide--startup-phases', or nil."
  (when (boundp 'emacs-ide--startup-phases)
    (cdr (assoc phase-name emacs-ide--startup-phases))))

;; ============================================================================
;; TESTS
;; ============================================================================

(ert-deftest test-emacs-version ()
  "Emacs version is 29.1 or higher."
  (should (version<= "29.1" emacs-version)))

(ert-deftest test-startup-time ()
  "Startup phases were recorded and total elapsed time is within limit.
FIX: Previously used (cdr (car phases)) which returns the LAST recorded phase
     (push prepends). Now we look up 'startup-complete' by name, with a
     fallback to the first phase in chronological order."
  (skip-unless (boundp 'emacs-ide--startup-phases))
  (skip-unless emacs-ide--startup-phases)
  (let* ((startup-time
          (or (emacs-ide-test--find-phase "startup-complete")
              ;; Fallback: last element in list = first phase recorded
              (cdr (car (last emacs-ide--startup-phases)))
              9999)))
    (should (< startup-time emacs-ide-test-startup-time-max))))

(ert-deftest test-directory-structure ()
  "Required directories exist."
  (dolist (dir '("core" "modules" "lib" "var"))
    (should (file-directory-p (expand-file-name dir user-emacs-directory)))))

(ert-deftest test-core-modules-provided ()
  "Core modules provide their features."
  (dolist (feature '(emacs-ide-health emacs-ide-recovery emacs-ide-config emacs-ide-package))
    (should (featurep feature))))

(ert-deftest test-health-check-system ()
  "Health check system functions exist."
  (should (fboundp 'emacs-ide-health-check-all))
  (should (fboundp 'emacs-ide-health-check-system-tools))
  (should (fboundp 'emacs-ide-health-auto-fix)))

(ert-deftest test-recovery-system ()
  "Recovery system is operational."
  (should (fboundp 'emacs-ide-recovery-log))
  (should (fboundp 'emacs-ide-recovery-backup-config))
  (should (fboundp 'emacs-ide-recovery-mode))
  (should (boundp 'emacs-ide-recovery-crash-count)))

(ert-deftest test-critical-packages-installed ()
  "Critical packages are installed or available."
  (dolist (pkg '(use-package which-key projectile magit vertico corfu lsp-mode))
    (should (emacs-ide-test-package-installed-p pkg))))

(ert-deftest test-lsp-availability ()
  "LSP mode is available when configured."
  (when (bound-and-true-p emacs-ide-lsp-enable)
    (should (or (featurep 'lsp-mode)
                (featurep 'eglot)))))

(ert-deftest test-completion-framework ()
  "Completion framework is active or available."
  (should (or (bound-and-true-p vertico-mode)
              (bound-and-true-p corfu-mode)
              (featurep 'vertico)
              (featurep 'corfu))))

(ert-deftest test-basic-editing-modes ()
  "Basic editing modes are enabled.
Note: electric-pair-mode is now explicitly enabled in init.el core settings,
so this test is reliable without depending on feature modules."
  (should (bound-and-true-p electric-pair-mode))
  (should (bound-and-true-p show-paren-mode))
  (should (bound-and-true-p global-auto-revert-mode)))

(ert-deftest test-theme-or-modeline ()
  "At least a theme or modeline is active."
  (should (or (custom-enabled-themes)
              (bound-and-true-p doom-modeline-mode))))

(ert-deftest test-git-executable ()
  "Git executable is found."
  (should (executable-find "git")))

(ert-deftest test-required-files-exist ()
  "Essential files exist."
  (dolist (file '("early-init.el" "init.el"
                  "core/emacs-ide-health.el"
                  "core/emacs-ide-recovery.el"))
    (should (emacs-ide-test-file-exists-p file))))

(ert-deftest test-health-check-runs ()
  "Health check runner returns a list of results."
  (skip-unless (fboundp 'emacs-ide-health-check-all))
  (let ((results (emacs-ide-health-check-all)))
    (should (listp results))))

;; ============================================================================
;; RUNNER
;; ============================================================================
(defun emacs-ide-run-tests ()
  "Run all Enterprise Emacs IDE tests interactively."
  (interactive)
  (let ((ert-verbose t))
    (ert-run-tests-batch "^test-")))

(provide 'emacs-ide-test)
;;; emacs-ide-test.el ends here
