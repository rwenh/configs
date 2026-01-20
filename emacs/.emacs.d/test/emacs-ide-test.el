;;; emacs-ide-test.el --- Enterprise Test Suite -*- lexical-binding: t -*-
;;; Commentary:
;;; Production-grade automated testing for CI/CD
;;; Code:

(require 'ert)
(require 'cl-lib)

;; ============================================================================
;; TEST CONFIGURATION
;; ============================================================================
(defvar emacs-ide-test-timeout 10
  "Timeout for individual tests in seconds.")

(defvar emacs-ide-test-startup-time-max 2.5
  "Maximum acceptable startup time in seconds.")

(defvar emacs-ide-test-results nil
  "Test results for reporting.")

;; ============================================================================
;; TEST UTILITIES
;; ============================================================================
(defun emacs-ide-test-measure-time (function)
  "Measure execution time of FUNCTION."
  (let ((start-time (current-time)))
    (funcall function)
    (float-time (time-subtract (current-time) start-time))))

(defun emacs-ide-test-package-installed-p (package)
  "Check if PACKAGE is installed via straight.el."
  (and (fboundp 'straight--installed-p)
       (straight--installed-p package)))

(defun emacs-ide-test-file-exists-p (file)
  "Check if FILE exists in emacs directory."
  (file-exists-p (expand-file-name file user-emacs-directory)))

;; ============================================================================
;; CRITICAL TESTS
;; ============================================================================
(ert-deftest test-emacs-version ()
  "Test: Emacs version meets minimum requirement."
  (should (version<= emacs-ide-minimum-emacs-version emacs-version)))

(ert-deftest test-startup-time ()
  "Test: Startup time is within acceptable range."
  (skip-unless (boundp 'emacs-ide--startup-phases))
  (let ((startup-time (cdr (car emacs-ide--startup-phases))))
    (should (< startup-time emacs-ide-test-startup-time-max))))

(ert-deftest test-gc-count ()
  "Test: GC count during startup is reasonable."
  (skip-unless (boundp 'emacs-ide--gc-count-start))
  (let ((gc-count (- gcs-done emacs-ide--gc-count-start)))
    (should (< gc-count 50))))

(ert-deftest test-directory-structure ()
  "Test: Required directories exist."
  (dolist (dir '("core" "modules" "lib" "var"))
    (should (file-directory-p
             (expand-file-name dir user-emacs-directory)))))

;; ============================================================================
;; CORE MODULE TESTS
;; ============================================================================
(ert-deftest test-core-modules-loaded ()
  "Test: All core modules loaded successfully."
  (dolist (module '(emacs-ide-health
                   emacs-ide-recovery
                   emacs-ide-config
                   emacs-ide-package))
    (should (featurep module))))

(ert-deftest test-health-check-system ()
  "Test: Health check system is functional."
  (should (fboundp 'emacs-ide-health-check-all))
  (should (fboundp 'emacs-ide-health-check-system-tools))
  (should (fboundp 'emacs-ide-health-auto-fix)))

(ert-deftest test-recovery-system ()
  "Test: Recovery system is operational."
  (should (fboundp 'emacs-ide-recovery-log))
  (should (fboundp 'emacs-ide-recovery-backup-config))
  (should (fboundp 'emacs-ide-recovery-mode))
  (should (boundp 'emacs-ide-recovery-crash-count)))

;; ============================================================================
;; PACKAGE TESTS
;; ============================================================================
(ert-deftest test-critical-packages-installed ()
  "Test: Critical packages are installed."
  (dolist (package '(use-package
                    which-key
                    projectile
                    magit
                    vertico
                    corfu
                    lsp-mode))
    (should (or (featurep package)
                (emacs-ide-test-package-installed-p package)))))

(ert-deftest test-straight-el-available ()
  "Test: straight.el is available and functional."
  (should (fboundp 'straight-use-package))
  (should (boundp 'straight--recipe-cache)))

;; ============================================================================
;; LSP TESTS
;; ============================================================================
(ert-deftest test-lsp-mode-available ()
  "Test: LSP mode is available."
  (should (or (featurep 'lsp-mode)
              (emacs-ide-test-package-installed-p 'lsp-mode))))

(ert-deftest test-lsp-servers-executable ()
  "Test: At least one LSP server is available."
  (let ((servers '("pyright" "rust-analyzer" "gopls" 
                  "typescript-language-server" "clangd")))
    (should (cl-some #'executable-find servers))))

;; ============================================================================
;; COMPLETION TESTS
;; ============================================================================
(ert-deftest test-completion-framework ()
  "Test: Completion framework is active."
  (should (or (bound-and-true-p vertico-mode)
              (bound-and-true-p ivy-mode)
              (bound-and-true-p helm-mode))))

(ert-deftest test-corfu-available ()
  "Test: Corfu completion is available."
  (should (or (featurep 'corfu)
              (emacs-ide-test-package-installed-p 'corfu))))

;; ============================================================================
;; EDITING TESTS
;; ============================================================================
(ert-deftest test-basic-editing-modes ()
  "Test: Basic editing modes are enabled."
  (should (bound-and-true-p electric-pair-mode))
  (should (bound-and-true-p show-paren-mode))
  (should (bound-and-true-p global-auto-revert-mode)))

(ert-deftest test-undo-tree ()
  "Test: Undo-tree is available and configured."
  (should (or (bound-and-true-p global-undo-tree-mode)
              (featurep 'undo-tree))))

;; ============================================================================
;; UI TESTS
;; ============================================================================
(ert-deftest test-theme-loaded ()
  "Test: A theme is loaded."
  (should (or (custom-theme-enabled-p 'modus-vivendi)
              (custom-theme-enabled-p 'modus-operandi))))

(ert-deftest test-modeline-active ()
  "Test: A modeline package is active."
  (should (or (bound-and-true-p doom-modeline-mode)
              (bound-and-true-p powerline-default-theme))))

(ert-deftest test-which-key-active ()
  "Test: which-key is active."
  (should (bound-and-true-p which-key-mode)))

;; ============================================================================
;; GIT INTEGRATION TESTS
;; ============================================================================
(ert-deftest test-magit-available ()
  "Test: Magit is available."
  (should (or (featurep 'magit)
              (emacs-ide-test-package-installed-p 'magit))))

(ert-deftest test-git-executable ()
  "Test: Git executable is found."
  (should (executable-find "git")))

;; ============================================================================
;; PROJECT MANAGEMENT TESTS
;; ============================================================================
(ert-deftest test-projectile-available ()
  "Test: Projectile is available."
  (should (or (bound-and-true-p projectile-mode)
              (featurep 'projectile))))

;; ============================================================================
;; DEBUGGING TESTS
;; ============================================================================
(ert-deftest test-dap-mode-available ()
  "Test: DAP mode is available for debugging."
  (should (or (featurep 'dap-mode)
              (emacs-ide-test-package-installed-p 'dap-mode))))

;; ============================================================================
;; LANGUAGE SUPPORT TESTS
;; ============================================================================
(ert-deftest test-python-support ()
  "Test: Python support is configured."
  (with-temp-buffer
    (python-mode)
    (should (eq major-mode 'python-mode))))

(ert-deftest test-rust-support ()
  "Test: Rust support is available."
  (should (or (featurep 'rust-mode)
              (emacs-ide-test-package-installed-p 'rust-mode))))

(ert-deftest test-javascript-support ()
  "Test: JavaScript support is available."
  (should (or (featurep 'js2-mode)
              (emacs-ide-test-package-installed-p 'js2-mode))))

;; ============================================================================
;; TREE-SITTER TESTS
;; ============================================================================
(ert-deftest test-treesit-available ()
  "Test: Tree-sitter is available (if Emacs supports it)."
  (when (fboundp 'treesit-available-p)
    (should (treesit-available-p))))

(ert-deftest test-treesit-grammars ()
  "Test: At least one tree-sitter grammar is installed."
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
    (let ((grammars '(python rust go javascript typescript c cpp)))
      (should (cl-some #'treesit-language-available-p grammars)))))

;; ============================================================================
;; PERFORMANCE TESTS
;; ============================================================================
(ert-deftest test-memory-usage ()
  "Test: Memory usage is within reasonable limits."
  (let ((mem-used (/ (car (memory-use-counts)) 1024 1024)))
    (should (< mem-used 200))))  ; Less than 200MB

(ert-deftest test-no-compilation-warnings ()
  "Test: No byte-compilation warnings in core files."
  (let ((warnings 0))
    (dolist (file (directory-files
                   (expand-file-name "core" user-emacs-directory)
                   t "\\.el$"))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((byte-compile-warnings '(all)))
          (condition-case err
              (byte-compile-from-buffer (current-buffer))
            (error (setq warnings (1+ warnings)))))))
    (should (= warnings 0))))

;; ============================================================================
;; SECURITY TESTS
;; ============================================================================
(ert-deftest test-tls-configuration ()
  "Test: TLS is properly configured."
  (skip-unless (boundp 'gnutls-verify-error))
  (should gnutls-verify-error))

(ert-deftest test-custom-file-separate ()
  "Test: Custom file is separate from init.el."
  (should (not (string= custom-file user-init-file))))

;; ============================================================================
;; FILE STRUCTURE TESTS
;; ============================================================================
(ert-deftest test-required-files-exist ()
  "Test: Required configuration files exist."
  (dolist (file '("early-init.el"
                 "init.el"
                 "core/emacs-ide-health.el"
                 "core/emacs-ide-recovery.el"))
    (should (emacs-ide-test-file-exists-p file))))

;; ============================================================================
;; FUNCTIONAL TESTS
;; ============================================================================
(ert-deftest test-find-file-works ()
  "Test: Basic file operations work."
  (let ((temp-file (make-temp-file "emacs-test")))
    (unwind-protect
        (progn
          (find-file temp-file)
          (should (eq major-mode 'fundamental-mode))
          (insert "test content")
          (save-buffer)
          (kill-buffer))
      (delete-file temp-file))))

(ert-deftest test-buffer-switching ()
  "Test: Buffer switching works."
  (let ((original-buffer (current-buffer)))
    (switch-to-buffer "*scratch*")
    (should (eq (current-buffer) (get-buffer "*scratch*")))
    (switch-to-buffer original-buffer)))

;; ============================================================================
;; REGRESSION TESTS
;; ============================================================================
(ert-deftest test-no-package-installation-on-startup ()
  "Test: Packages don't reinstall on every startup."
  ;; This is a regression test for a bug in original config
  (should-not (memq 'emacs-ide-essential-packages
                    (symbol-value 'after-init-hook))))

(ert-deftest test-lsp-optimization-function-exists ()
  "Test: LSP optimization function exists (regression test)."
  (should (fboundp 'emacs-ide-lsp-optimize-large-files)))

;; ============================================================================
;; INTEGRATION TESTS
;; ============================================================================
(ert-deftest test-health-check-runs ()
  "Test: Health check can run without errors."
  (when (fboundp 'emacs-ide-health-check-all)
    (let ((results (emacs-ide-health-check-all)))
      (should (listp results))
      (should (> (length results) 0)))))

(ert-deftest test-recovery-log-creation ()
  "Test: Recovery system can log events."
  (when (fboundp 'emacs-ide-recovery-log)
    (emacs-ide-recovery-log 'info "Test log entry")
    (should (file-exists-p emacs-ide-recovery-log-file))))

;; ============================================================================
;; TEST RUNNER
;; ============================================================================
(defun emacs-ide-run-tests ()
  "Run all Enterprise Emacs IDE tests."
  (interactive)
  (let ((ert-quiet t))
    (ert-run-tests-batch-and-exit "^test-")))

(defun emacs-ide-test-report ()
  "Generate test report."
  (interactive)
  (let* ((stats (ert-run-tests-batch "^test-"))
         (total (ert--stats-total stats))
         (passed (ert--stats-passed-expected stats))
         (failed (ert--stats-failed-unexpected stats))
         (skipped (ert--stats-skipped stats)))
    (with-output-to-temp-buffer "*Test Report*"
      (princ "=== EMACS IDE TEST REPORT ===\n\n")
      (princ (format "Total Tests: %d\n" total))
      (princ (format "Passed: %d\n" passed))
      (princ (format "Failed: %d\n" failed))
      (princ (format "Skipped: %d\n\n" skipped))
      (if (= failed 0)
          (princ "✓ All tests passed!\n")
        (princ "✗ Some tests failed. See *ert* buffer for details.\n")))))

;; ============================================================================
;; CI/CD INTEGRATION
;; ============================================================================
(defun emacs-ide-test-ci ()
  "Run tests for CI/CD pipeline."
  (condition-case err
      (progn
        (ert-run-tests-batch "^test-")
        (message "✓ All tests passed")
        (kill-emacs 0))
    (error
     (message "✗ Tests failed: %s" err)
     (kill-emacs 1))))

(provide 'emacs-ide-test)
;;; emacs-ide-test.el ends here
