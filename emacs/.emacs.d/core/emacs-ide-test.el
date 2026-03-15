;;; emacs-ide-test.el --- Enterprise Test Suite -*- lexical-binding: t -*-
;;; Commentary:
;;; ERT-based tests adapted to calibrated module names and robust guards.
;;; Version: 2.2.3
;;; Fixes vs 2.2.2:
;;;   - C-19 (MEDIUM): test-health-check-runs called emacs-ide-health-check-all
;;;     which is the full interactive runner. It has side-effects:
;;;       • sets emacs-ide-health-results (clobbers live results)
;;;       • sets emacs-ide-health-last-check (resets the timestamp)
;;;       • calls force-mode-line-update (causes a modeline flash mid-edit)
;;;       • prompts y-or-n-p for auto-fix when issues exist (hangs batch runs)
;;;     In batch --script mode the prompt hangs the process. In an interactive
;;;     session running M-x emacs-ide-run-tests mid-edit, the live health state
;;;     is silently reset and the modeline flickers.
;;;     Fix: call emacs-ide-health-run-check on a single lightweight check
;;;     (system-tools) with save/restore of the global state variables. This
;;;     verifies that the runner machinery works without clobbering live state.
;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar emacs-ide-test-timeout 10)
(defvar emacs-ide-test-startup-time-max 5.0)

(defun emacs-ide-test-package-installed-p (package)
  "Check if PACKAGE is installed (locate-library + featurep)."
  (or (featurep package)
      (locate-library (symbol-name package))))

(defun emacs-ide-test-file-exists-p (file)
  "Check if FILE exists under user-emacs-directory."
  (file-exists-p (expand-file-name file user-emacs-directory)))

(defun emacs-ide-test--find-phase (phase-name)
  "Return elapsed time for PHASE-NAME, or nil."
  (when (boundp 'emacs-ide--startup-phases)
    (cdr (assoc phase-name emacs-ide--startup-phases))))

;; ============================================================================
;; TESTS
;; ============================================================================

(ert-deftest test-emacs-version ()
  "Emacs version is 29.1 or higher."
  (should (version<= "29.1" emacs-version)))

(ert-deftest test-startup-time ()
  "Startup phases were recorded and elapsed time is within limit."
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
  "Critical packages are installed or available."
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
  "Basic editing modes are enabled."
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
  "Health check runner returns a valid result without clobbering live state.
C-19 FIX: Previously called emacs-ide-health-check-all (the full interactive
runner) which: resets emacs-ide-health-results, resets the last-check
timestamp, calls force-mode-line-update, and in batch mode hangs on the
auto-fix y-or-n-p prompt.
Now runs emacs-ide-health-run-check on one lightweight check (system-tools)
and saves/restores the two global state variables, leaving the live health
state completely untouched."
  (skip-unless (fboundp 'emacs-ide-health-run-check))
  (skip-unless (fboundp 'emacs-ide-health-check-system-tools))
  ;; Save live state
  (let ((saved-results   emacs-ide-health-results)
        (saved-timestamp emacs-ide-health-last-check))
    (unwind-protect
        (let* ((res (emacs-ide-health-run-check
                     'system-tools
                     #'emacs-ide-health-check-system-tools)))
          ;; Runner must return a cons of (name . plist)
          (should (consp res))
          (should (eq (car res) 'system-tools))
          (should (plistp (cdr res)))
          (should (memq (plist-get (cdr res) :status) '(ok warning error))))
      ;; Restore live state unconditionally (unwind-protect guarantee)
      (setq emacs-ide-health-results   saved-results
            emacs-ide-health-last-check saved-timestamp))))

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
