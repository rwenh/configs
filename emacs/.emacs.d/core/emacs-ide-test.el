;;; emacs-ide-test.el --- Enterprise Test Suite (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; ERT-based tests adapted to calibrated module names and robust guards.
;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar emacs-ide-test-timeout 10
  "Timeout for individual tests in seconds.")

(defvar emacs-ide-test-startup-time-max 5.0
  "Maximum acceptable startup time in seconds (CI-friendly).")

(defun emacs-ide-test-package-installed-p (package)
  "Check if PACKAGE is installed via straight.el or featurep."
  (or (featurep package)
      (and (fboundp 'straight--installed-p)
           (condition-case nil (straight--installed-p package) (error nil)))))

(defun emacs-ide-test-file-exists-p (file)
  "Check if file exists under user-emacs-directory."
  (file-exists-p (expand-file-name file user-emacs-directory)))

;; Basic version test (guarded)
(ert-deftest test-emacs-version ()
  "Test: Emacs version is new enough."
  (should (version<= "29.1" emacs-version)))

(ert-deftest test-startup-time ()
  "Test that startup phases were recorded and within reasonable limit."
  (skip-unless (boundp 'emacs-ide--startup-phases))
  (let ((startup-time (or (and emacs-ide--startup-phases (cdr (car emacs-ide--startup-phases))) 9999)))
    (should (< startup-time emacs-ide-test-startup-time-max))))

(ert-deftest test-directory-structure ()
  "Test: Required directories exist."
  (dolist (dir '("core" "modules" "lib" "var"))
    (should (file-directory-p (expand-file-name dir user-emacs-directory)))))

(ert-deftest test-core-modules-provided ()
  "Test: Core modules provide their features."
  (dolist (feature '(emacs-ide-health emacs-ide-recovery emacs-ide-config emacs-ide-package))
    (should (featurep feature))))

(ert-deftest test-health-check-system ()
  "Test: Health check system functions exist."
  (should (fboundp 'emacs-ide-health-check-all))
  (should (fboundp 'emacs-ide-health-check-system-tools))
  (should (fboundp 'emacs-ide-health-auto-fix)))

(ert-deftest test-recovery-system ()
  "Test: Recovery system is operational."
  (should (fboundp 'emacs-ide-recovery-log))
  (should (fboundp 'emacs-ide-recovery-backup-config))
  (should (fboundp 'emacs-ide-recovery-mode))
  (should (boundp 'emacs-ide-recovery-crash-count)))

(ert-deftest test-critical-packages-installed ()
  "Test: Critical packages are installed or available."
  (dolist (pkg '(use-package which-key projectile magit vertico corfu lsp-mode))
    (should (emacs-ide-test-package-installed-p pkg))))

(ert-deftest test-lsp-availability ()
  "Test: LSP mode is available when configured."
  (when (bound-and-true-p emacs-ide-lsp-enable)
    (should (or (featurep 'lsp-mode) (emacs-ide-test-package-installed-p 'lsp-mode)))))

(ert-deftest test-completion-framework ()
  "Test: Completion framework is active or available."
  (should (or (bound-and-true-p vertico-mode)
              (bound-and-true-p corfu-mode)
              (emacs-ide-test-package-installed-p 'vertico)
              (emacs-ide-test-package-installed-p 'corfu))))

(ert-deftest test-basic-editing-modes ()
  "Test: Basic editing modes are enabled."
  (should (bound-and-true-p electric-pair-mode))
  (should (bound-and-true-p show-paren-mode))
  (should (bound-and-true-p global-auto-revert-mode)))

(ert-deftest test-theme-or-modeline ()
  "Test: At least a theme or modeline is active."
  (should (or (custom-enabled-themes) (bound-and-true-p doom-modeline-mode))))

(ert-deftest test-git-executable ()
  "Test: Git executable is found."
  (should (executable-find "git")))

(ert-deftest test-required-files-exist ()
  "Test: Essential files exist."
  (dolist (file '("early-init.el" "init.el" "core/emacs-ide-health.el" "core/emacs-ide-recovery.el"))
    (should (emacs-ide-test-file-exists-p file))))

;; Lightweight integration test for health check
(ert-deftest test-health-check-runs ()
  "Test: Health check runner returns list of results."
  (skip-unless (fboundp 'emacs-ide-health-check-all))
  (let ((results (emacs-ide-health-check-all)))
    (should (listp results))))

;; Run tests helper
(defun emacs-ide-run-tests ()
  "Run all Enterprise Emacs IDE tests interactively."
  (interactive)
  (let ((ert-verbose t))
    (ert-run-tests-batch "^test-")))

(provide 'emacs-ide-test)
;;; emacs-ide-test.el ends here