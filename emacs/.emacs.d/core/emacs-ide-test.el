;;; emacs-ide-test.el --- Enterprise Test Suite -*- lexical-binding: t -*-
;;; Commentary:
;;; ERT-based tests adapted to calibrated module names and robust guards.
;;; Version: 2.2.2
;;; Fixes:
;;;   - 2.2.2: test-critical-packages-installed: lsp-mode is loaded deferred
;;;     (:defer t via use-package), so featurep 'lsp-mode is nil until a
;;;     source file triggers it. Replaced with (locate-library ...) which
;;;     checks whether the package is *installed* without requiring it to be
;;;     active. Consistent with how emacs-ide-health-check-packages works.
;;;   - 2.2.2: test-basic-editing-modes: removed global-auto-revert-mode
;;;     assertion. It is not enabled in init.el core settings — only feature
;;;     modules (ui-core, tools-project) may enable it. The test was
;;;     non-deterministic: it passed if ui-core loaded successfully and failed
;;;     in safe mode or when that module errored. Replaced with
;;;     delete-selection-mode which IS enabled by init.el directly (via
;;;     (delete-selection-mode 1) in core settings — see init.el 2.2.3+).
;;;     NOTE: init.el must add `(delete-selection-mode 1)` to core-settings
;;;     for this test to be reliable (see init.el fix note below).
;;;   - 2.2.1: test-startup-time: phase lookup corrected to use assoc by name.
;;;   - 2.2.1: test-basic-editing-modes: electric-pair-mode guard added.
;;;
;;; INIT.EL FIX REQUIRED:
;;;   Add `(delete-selection-mode 1)` to the core-settings block in init.el
;;;   (alongside the existing `(electric-pair-mode 1)`) so that
;;;   test-basic-editing-modes is reliable without depending on feature modules.
;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar emacs-ide-test-timeout 10
  "Timeout for individual tests in seconds.")

(defvar emacs-ide-test-startup-time-max 5.0
  "Maximum acceptable startup time in seconds (CI-friendly).")

(defun emacs-ide-test-package-installed-p (package)
  "Check if PACKAGE is installed.
FIX: Use (locate-library) instead of (featurep) so deferred packages
that haven't been require'd yet (e.g. lsp-mode with :defer t) still
report as installed when present on disk."
  (or (featurep package)
      (locate-library (symbol-name package))))

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
  "Startup phases were recorded and total elapsed time is within limit."
  (skip-unless (boundp 'emacs-ide--startup-phases))
  (skip-unless emacs-ide--startup-phases)
  (let* ((startup-time
          (or (emacs-ide-test--find-phase "startup-complete")
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
  "Critical packages are installed or available.
FIX: Uses emacs-ide-test-package-installed-p (locate-library + featurep)
so deferred packages like lsp-mode — loaded lazily on first source file
open — are not incorrectly reported as missing."
  (dolist (pkg '(use-package which-key projectile magit vertico corfu lsp-mode))
    (should (emacs-ide-test-package-installed-p pkg))))

(ert-deftest test-lsp-availability ()
  "LSP mode is available when configured."
  (when (bound-and-true-p emacs-ide-lsp-enable)
    (should (or (emacs-ide-test-package-installed-p 'lsp-mode)
                (emacs-ide-test-package-installed-p 'eglot)))))

(ert-deftest test-completion-framework ()
  "Completion framework is active or available."
  (should (or (bound-and-true-p vertico-mode)
              (bound-and-true-p corfu-mode)
              (emacs-ide-test-package-installed-p 'vertico)
              (emacs-ide-test-package-installed-p 'corfu))))

(ert-deftest test-basic-editing-modes ()
  "Basic editing modes are enabled.
electric-pair-mode: enabled in init.el core settings (reliable).
show-paren-mode: always enabled in Emacs 29+ by default.
delete-selection-mode: must be enabled in init.el core settings.
  See INIT.EL FIX REQUIRED in this file's Commentary.
FIX: Removed global-auto-revert-mode assertion — it is not in init.el core
  settings; only feature modules enable it, making the assertion
  non-deterministic (passes in full mode, fails in safe mode)."
  (should (bound-and-true-p electric-pair-mode))
  (should (bound-and-true-p show-paren-mode))
  (should (bound-and-true-p delete-selection-mode)))

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
