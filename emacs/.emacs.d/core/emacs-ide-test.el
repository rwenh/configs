;;; emacs-ide-test.el --- Enterprise Test Suite -*- lexical-binding: t -*-
;;; Commentary:
;;; ERT-based tests adapted to calibrated module names and robust guards.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.4 to 3.0.4.
;;;   - FIX-PKG-BACKEND: test-critical-packages-installed now reads
;;;     emacs-ide-completion-backend to check the configured backend
;;;     instead of always checking corfu, matching FIX-PKG-BACKEND in
;;;     emacs-ide-health.el.
;;;   - FIX-REQUIRED-FILES: config.yml added to test-required-files-exist.
;;;     Missing config causes silent fallback to defaults with no error.
;;;   - FIX-CORE-MODULES: test-core-modules-provided now checks all 7
;;;     core modules: emacs-ide-profiler, emacs-ide-security, and
;;;     emacs-ide-telemetry were previously missing.
;;;   - FIX-LANGS-DIR: modules/langs/ added to test-directory-structure
;;;     and a new test-langs-load-path test verifies the lazy lang dir
;;;     is on load-path. Missing entry silently breaks all lang modules.
;;;   - FIX-TEST-SELECTOR: emacs-ide-run-tests now uses "^test-emacs-ide-"
;;;     prefix to avoid running third-party ERT tests (magit, projectile,
;;;     etc.) that share the "^test-" namespace.
;;;   - FIX-STARTUP-TARGET: test-startup-time now uses
;;;     emacs-ide-startup-time-target from config (default 3.0s) rather
;;;     than the hardcoded 5.0s which was too generous.
;;;   - FIX-CONFIG-LOADED: New test-config-loaded verifies
;;;     emacs-ide-config-loaded-p is t — catches the entire class of
;;;     config-load failures that otherwise go silently undetected.
;;;   - FIX-SPOT-CHECK: New test-spot-check-feature verifies
;;;     emacs-ide-spot-check is provided.
;;;   - FIX-ERT-NAMES: All ert-deftest names prefixed with "test-emacs-ide-"
;;;     to match the new selector and avoid collisions with third-party tests.
;;; Fixes vs 2.2.4 (retained):
;;;   - FIX-EMACS30: custom-available-themes was removed in Emacs 30.
;;;     test-theme-or-modeline called (custom-available-themes) which throws
;;;     void-function in Emacs 30, preventing the entire test file from loading
;;;     and causing (provide 'emacs-ide-test) to never run. This made
;;;     emacs-ide-run-tests silently unavailable on Emacs 30+.
;;;     Fix: use (custom-enabled-themes) — the variable that lists currently
;;;     active themes — which exists in both Emacs 29 and 30.
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
;; FIX-STARTUP-TARGET: use config target as the ceiling, not an arbitrary 5.0s.
;; Falls back to emacs-ide-startup-time-target (default 3.0) from config.
(defvar emacs-ide-test-startup-time-max
  (if (boundp 'emacs-ide-startup-time-target)
      emacs-ide-startup-time-target
    3.0)
  "Maximum acceptable startup time for the startup test.")

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
;; FIX-ERT-NAMES: All test names use "test-emacs-ide-" prefix to match
;; the new emacs-ide-run-tests selector and avoid collisions with
;; third-party ERT tests (magit, projectile, etc.) that share "^test-".
;; ============================================================================

(ert-deftest test-emacs-ide-emacs-version ()
  "Emacs version is 29.1 or higher."
  (should (version<= "29.1" emacs-version)))

(ert-deftest test-emacs-ide-startup-time ()
  "Startup phases were recorded and elapsed time is within limit.
FIX-STARTUP-TARGET: uses emacs-ide-test-startup-time-max which reads
from emacs-ide-startup-time-target (config value) not a hardcoded 5.0."
  (skip-unless (boundp 'emacs-ide--startup-phases))
  (skip-unless emacs-ide--startup-phases)
  (let* ((startup-time
          (or (emacs-ide-test--find-phase "startup-complete")
              (cdr (car (last emacs-ide--startup-phases)))
              9999)))
    (should (< startup-time emacs-ide-test-startup-time-max))))

(ert-deftest test-emacs-ide-directory-structure ()
  "Required directories exist including modules/langs/.
FIX-LANGS-DIR: modules/langs/ added — missing dir silently breaks
all lazy language module loading."
  (dolist (dir '("core" "modules" "modules/langs" "lib" "var"))
    (should (file-directory-p (expand-file-name dir user-emacs-directory)))))

(ert-deftest test-emacs-ide-langs-load-path ()
  "modules/langs/ directory is on load-path.
FIX-LANGS-DIR: If missing, all lang modules silently fail to load
when their file type is opened."
  (should (member (expand-file-name "modules/langs/" user-emacs-directory)
                  load-path)))

(ert-deftest test-emacs-ide-core-modules-provided ()
  "All 7 core modules provide their features.
FIX-CORE-MODULES: emacs-ide-profiler, emacs-ide-security, and
emacs-ide-telemetry were previously missing from this check."
  (dolist (feature '(emacs-ide-config
                     emacs-ide-health
                     emacs-ide-recovery
                     emacs-ide-package
                     emacs-ide-profiler
                     emacs-ide-security
                     emacs-ide-telemetry))
    (should (featurep feature))))

(ert-deftest test-emacs-ide-config-loaded ()
  "Configuration was successfully loaded.
FIX-CONFIG-LOADED: emacs-ide-config-loaded-p catches the entire class
of config-load failures that otherwise go silently undetected."
  (should (bound-and-true-p emacs-ide-config-loaded-p)))

(ert-deftest test-emacs-ide-spot-check-feature ()
  "emacs-ide-spot-check feature is provided.
FIX-SPOT-CHECK: Verifies the spot-check module loaded successfully."
  (should (featurep 'emacs-ide-spot-check)))

(ert-deftest test-emacs-ide-health-check-system ()
  "Health check system functions exist."
  (should (fboundp 'emacs-ide-health-check-all))
  (should (fboundp 'emacs-ide-health-check-system-tools))
  (should (fboundp 'emacs-ide-health-auto-fix)))

(ert-deftest test-emacs-ide-recovery-system ()
  "Recovery system is operational."
  (should (fboundp 'emacs-ide-recovery-log))
  (should (fboundp 'emacs-ide-recovery-backup-config))
  (should (fboundp 'emacs-ide-recovery-mode))
  (should (boundp 'emacs-ide-recovery-crash-count)))

(ert-deftest test-emacs-ide-critical-packages-installed ()
  "Critical packages are installed or available.
FIX-PKG-BACKEND: checks the configured completion backend
(emacs-ide-completion-backend) instead of hardcoding corfu,
so switching to company in config.yml does not cause false failures."
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

(ert-deftest test-emacs-ide-basic-editing-modes ()
  "Basic editing modes are enabled."
  (should (bound-and-true-p electric-pair-mode))
  (should (bound-and-true-p show-paren-mode))
  (should (bound-and-true-p delete-selection-mode)))

(ert-deftest test-emacs-ide-theme-or-modeline ()
  "At least a theme or modeline is active."
  ;; FIX-EMACS30: custom-available-themes was removed in Emacs 30.
  ;; custom-enabled-themes is a variable listing active themes.
  ;; It exists in both Emacs 29 and 30.
  (should (or (and (boundp 'custom-enabled-themes)
                   (consp custom-enabled-themes))
              (bound-and-true-p doom-modeline-mode))))

(ert-deftest test-emacs-ide-git-executable ()
  "Git executable is found."
  (should (executable-find "git")))

(ert-deftest test-emacs-ide-required-files-exist ()
  "Essential files exist including config.yml.
FIX-REQUIRED-FILES: config.yml added — missing config causes silent
fallback to defaults with no user-visible error."
  (dolist (file '("early-init.el" "init.el"
                  "config.yml"
                  "core/emacs-ide-health.el"
                  "core/emacs-ide-recovery.el"))
    (should (emacs-ide-test-file-exists-p file))))

(ert-deftest test-emacs-ide-health-check-runs ()
  "Health check runner returns a valid result without clobbering live state.
C-19 FIX: Runs emacs-ide-health-run-check on one lightweight check
and saves/restores global state with unwind-protect."
  (skip-unless (fboundp 'emacs-ide-health-run-check))
  (skip-unless (fboundp 'emacs-ide-health-check-system-tools))
  (let ((saved-results   emacs-ide-health-results)
        (saved-timestamp emacs-ide-health-last-check))
    (unwind-protect
        (let* ((res (emacs-ide-health-run-check
                     'system-tools
                     #'emacs-ide-health-check-system-tools)))
          (should (consp res))
          (should (eq (car res) 'system-tools))
          ;; plistp available since Emacs 29.1 — safe given test-emacs-version
          (should (plistp (cdr res)))
          (should (memq (plist-get (cdr res) :status) '(ok warning error))))
      (setq emacs-ide-health-results   saved-results
            emacs-ide-health-last-check saved-timestamp))))

;; ============================================================================
;; RUNNER
;; ============================================================================
(defun emacs-ide-run-tests ()
  "Run all Enterprise Emacs IDE tests interactively.
FIX-TEST-SELECTOR: Uses \"^test-emacs-ide-\" prefix instead of \"^test-\"
to avoid running third-party ERT tests (magit, projectile, etc.)
that share the generic test- namespace."
  (interactive)
  (let ((ert-verbose t))
    (ert-run-tests-batch "^test-emacs-ide-")))

(provide 'emacs-ide-test)
;;; emacs-ide-test.el ends here
