;;; tools-test.el --- Language-Aware Test Runner -*- lexical-binding: t -*-
;;; Commentary:
;;; v3.0.0: Delegates to tools-test-runner-registry.el when a lang module
;;; has registered a runner. Falls back to existing auto-detection logic
;;; for any language not yet registered. Zero regressions.
;;; All fixes from 1.0.5 retained.
;;; Version: 3.0.1
;;; Fixes vs 3.0.0:
;;;   - FIX-2: C-c x r → C-c x R (test report). C-c x r is owned by
;;;     tools-repl.el (emacs-ide-repl-launch). C-c x l → C-c X l.
;;; Code:

(require 'cl-lib)
(require 'compile)

;; ============================================================================
;; STATE (unchanged)
;; ============================================================================
(defvar emacs-ide-test--last-command   nil)
(defvar emacs-ide-test--last-directory nil)
(defvar emacs-ide-test--history        nil)
(defvar emacs-ide-test--history-max    50)
(defvar emacs-ide-test--buffer-name    "*Test Runner*")

;; ============================================================================
;; DISPATCH — registry first, detection fallback
;; ============================================================================

(defun emacs-ide-test-run (&optional arg)
  "Smart test runner.
Delegates to tools-test-runner-registry.el if a runner is registered
for current major-mode. Falls back to auto-detection otherwise.
  No prefix:   run file tests
  C-u:         run project tests
  C-u C-u:     run test at point"
  (interactive "P")
  (cond
   ;; Registry dispatch (lang modules register themselves)
   ((and (fboundp 'emacs-ide-test--runner-for)
         (equal arg '(16))
         (emacs-ide-test--runner-for major-mode :point-fn))
    (funcall (emacs-ide-test--runner-for major-mode :point-fn)))
   ((and (fboundp 'emacs-ide-test--runner-for)
         (equal arg '(4))
         (emacs-ide-test--runner-for major-mode :project-fn))
    (funcall (emacs-ide-test--runner-for major-mode :project-fn)))
   ((and (fboundp 'emacs-ide-test--runner-for)
         (emacs-ide-test--runner-for major-mode :file-fn))
    (funcall (emacs-ide-test--runner-for major-mode :file-fn)))
   ;; Fallback to existing auto-detection
   (t (emacs-ide-test--auto-detect-and-run arg))))

(defun emacs-ide-test-run-all ()
  "Run full project test suite, ignoring file context."
  (interactive)
  (let ((buffer-file-name nil))
    (emacs-ide-test--auto-detect-and-run nil)))

;; ============================================================================
;; AUTO-DETECTION (existing logic from 1.0.5, unchanged)
;; ============================================================================

(defun emacs-ide-test--project-root ()
  (or (and (fboundp 'projectile-project-root) (projectile-project-root))
      default-directory))

(defun emacs-ide-test--run-compile (cmd &optional dir)
  "Run CMD in DIR (or project root), track in history."
  (let* ((directory (or dir (emacs-ide-test--project-root)))
         (default-directory directory)
         (start-time (float-time)))
    (setq emacs-ide-test--last-command   cmd
          emacs-ide-test--last-directory directory)
    (let ((buf (compile cmd)))
      (when buf
        (with-current-buffer buf
          (add-hook 'compilation-finish-functions
                    (lambda (buf status)
                      (emacs-ide-test--record-history
                       cmd directory status start-time))
                    nil t))))))

(defun emacs-ide-test--record-history (cmd dir status start-time)
  (let ((entry (list :time      (format-time-string "%H:%M:%S")
                     :command   cmd
                     :directory dir
                     :status    (if (string-match "finished" status) 'pass 'fail)
                     :duration  (- (float-time) start-time))))
    (push entry emacs-ide-test--history)
    (when (> (length emacs-ide-test--history) emacs-ide-test--history-max)
      (setq emacs-ide-test--history
            (seq-take emacs-ide-test--history emacs-ide-test--history-max)))))

(defun emacs-ide-test--auto-detect-and-run (arg)
  "Auto-detect language and run appropriate test command."
  (let ((cmd (emacs-ide-test--detect-command)))
    (if cmd
        (emacs-ide-test--run-compile cmd)
      (message "test-runner: no test framework detected. Use C-c h t for test hydra."))))

(defun emacs-ide-test--detect-command ()
  "Detect test command from mode and project markers."
  (let ((root (emacs-ide-test--project-root)))
    (cond
     ;; Mode-based detection
     ((derived-mode-p 'python-mode 'python-ts-mode)
      (cond ((executable-find "pytest")   "pytest -v")
            ((executable-find "python3")  "python3 -m unittest discover")
            (t nil)))
     ((derived-mode-p 'rust-mode 'rust-ts-mode)
      (when (executable-find "cargo") "cargo test"))
     ((derived-mode-p 'go-mode 'go-ts-mode)
      (when (executable-find "go") "go test ./..."))
     ((derived-mode-p 'js2-mode 'typescript-mode 'js-ts-mode 'typescript-ts-mode)
      (cond ((executable-find "jest")   "jest")
            ((executable-find "vitest") "vitest run")
            ((executable-find "npm")    "npm test")
            (t nil)))
     ((derived-mode-p 'java-mode 'java-ts-mode)
      (cond ((file-exists-p (expand-file-name "pom.xml"         root)) "mvn test -q")
            ((file-exists-p (expand-file-name "build.gradle"    root)) "gradle test")
            ((file-exists-p (expand-file-name "build.gradle.kts" root)) "gradle test")
            (t nil)))
     ((derived-mode-p 'elixir-mode)
      (when (executable-find "mix") "mix test"))
     ((derived-mode-p 'ruby-mode 'ruby-ts-mode)
      (cond ((executable-find "rspec")    "bundle exec rspec")
            ((executable-find "ruby")     "ruby -Itest")
            (t nil)))
     ((derived-mode-p 'haskell-mode)
      (cond ((executable-find "cabal") "cabal test")
            ((executable-find "stack") "stack test")
            (t nil)))
     ((derived-mode-p 'sh-mode 'bash-ts-mode)
      (when (executable-find "bats") (format "bats %s" (shell-quote-argument (buffer-file-name)))))
     ((derived-mode-p 'emacs-lisp-mode)
      (when (fboundp 'ert-run-tests-interactively)
        (ert-run-tests-interactively t) nil))
     ((derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
      (cond ((file-exists-p (expand-file-name "CMakeLists.txt" root)) "ctest --test-dir build")
            ((emacs-ide-test--makefile-has-test-target root) "make test")
            (t nil)))
     ((derived-mode-p 'kotlin-mode)
      (when (executable-find "gradle") "gradle test"))
     ((derived-mode-p 'scala-mode)
      (when (executable-find "sbt") "sbt test"))
     ((derived-mode-p 'clojure-mode 'clojurescript-mode)
      (when (executable-find "clj") "clj -M:test"))
     ;; Project-marker fallback
     (t (emacs-ide-test--detect-from-markers root)))))

(defun emacs-ide-test--detect-from-markers (root)
  (cond
   ((file-exists-p (expand-file-name "Cargo.toml"     root)) "cargo test")
   ((file-exists-p (expand-file-name "go.mod"         root)) "go test ./...")
   ((file-exists-p (expand-file-name "pyproject.toml" root)) "pytest -v")
   ((file-exists-p (expand-file-name "package.json"   root)) "npm test")
   ((file-exists-p (expand-file-name "pom.xml"        root)) "mvn test -q")
   ((file-exists-p (expand-file-name "build.gradle"   root)) "gradle test")
   ((file-exists-p (expand-file-name "mix.exs"        root)) "mix test")
   ((file-exists-p (expand-file-name "Gemfile"        root)) "bundle exec rspec")
   (t nil)))

(defun emacs-ide-test--makefile-has-test-target (root)
  "Return non-nil if Makefile in ROOT has a test target. (1.0.5 fix: in-process grep)"
  (let ((makefile (expand-file-name "Makefile" root)))
    (and (file-exists-p makefile)
         (with-temp-buffer
           (insert-file-contents makefile)
           (re-search-forward "^test[[:space:]]*:" nil t)))))

;; ============================================================================
;; REPEAT LAST / REPORT (unchanged)
;; ============================================================================

(defun emacs-ide-test-run-last ()
  "Repeat the last test command."
  (interactive)
  (if emacs-ide-test--last-command
      (let ((default-directory (or emacs-ide-test--last-directory default-directory)))
        (compile emacs-ide-test--last-command))
    (message "test-runner: no previous test run")))

(defun emacs-ide-test-report ()
  "Show test history report."
  (interactive)
  (with-output-to-temp-buffer "*Test History*"
    (princ "=== TEST HISTORY ===\n\n")
    (if (null emacs-ide-test--history)
        (princ "No tests run yet.\n")
      (dolist (entry emacs-ide-test--history)
        (princ (format "[%s] %s  %s  (%.1fs)\n"
                       (plist-get entry :time)
                       (if (eq (plist-get entry :status) 'pass) "✓" "✗")
                       (plist-get entry :command)
                       (or (plist-get entry :duration) 0)))))))

;; ============================================================================
;; KEYBINDINGS (supplement tools-test-runner-registry.el and keybindings.el)
;; C-c x r is owned by tools-repl.el → emacs-ide-repl-launch. Do NOT use.
;; C-c X (uppercase) is the test prefix from tools-test-runner-registry.el.
;; ============================================================================
(global-set-key (kbd "C-c C-t")   #'emacs-ide-test-run)
(global-set-key (kbd "C-c C-T")   #'emacs-ide-test-run-all)
(global-set-key (kbd "C-c X l")   #'emacs-ide-test-run-last)  ; was C-c x l
(global-set-key (kbd "C-c x R")   #'emacs-ide-test-report)    ; was C-c x r (collision fixed)

(provide 'tools-test)
;;; tools-test.el ends here
