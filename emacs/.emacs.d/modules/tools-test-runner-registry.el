;;; tools-test-runner-registry.el --- Per-lang test runner registry -*- lexical-binding: t -*-
;;; Commentary:
;;; Extends tools-test.el with a registry API so each lang-*.el can
;;; register its own test runner without modifying tools-test.el.
;;; tools-test.el dispatches through this registry.
;;;
;;; Add "tools-test-runner-registry" to feature-modules BEFORE "tools-test".
;;;
;;; Version: 1.0.1
;;; Fixes vs 1.0.0:
;;;   - FIX-1: Removed duplicate emacs-ide-test-run definition. tools-test.el
;;;     is the canonical owner; having both caused the last-loaded version to
;;;     silently win depending on load order.
;;;   - FIX-2: Removed stray global-set-key for C-c x r from this file.
;;;     tools-repl.el and keybindings.el both own C-c x r → emacs-ide-repl-launch;
;;;     the binding here clobbered that with a test function.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; REGISTRY
;; ============================================================================

(defvar emacs-ide-test--runner-registry nil
  "Alist of (MAJOR-MODE . plist).
plist keys:
  :file-fn    (fn) run tests in current file
  :project-fn (fn) run all project tests
  :point-fn   (fn) run test at point
  :watch-fn   (fn) watch mode (optional)")

(defun emacs-ide-test-register-runner (mode &rest plist)
  "Register test runner fns for MAJOR-MODE.
Call from lang-*.el after-load blocks.
  (emacs-ide-test-register-runner 'python-mode
    :file-fn    #'emacs-ide-python-test-file
    :project-fn #'emacs-ide-python-test-project
    :point-fn   #'emacs-ide-python-test-at-point)"
  (setf (alist-get mode emacs-ide-test--runner-registry) plist))

(defun emacs-ide-test--runner-for (mode key)
  "Return the runner fn for MODE and KEY (:file-fn/:project-fn/:point-fn)."
  (let ((info (alist-get mode emacs-ide-test--runner-registry)))
    (and info (plist-get info key))))

;; ============================================================================
;; DISPATCH COMMANDS — these replace the monolithic tools-test.el switch
;; ============================================================================

(defun emacs-ide-test-run-file ()
  "Run tests for the current file using the registered runner for this mode."
  (interactive)
  (let ((fn (emacs-ide-test--runner-for major-mode :file-fn)))
    (if fn (funcall fn)
      (message "test-runner: no file runner for %s. Register with emacs-ide-test-register-runner."
               major-mode))))

(defun emacs-ide-test-run-project ()
  "Run all project tests using the registered runner for this mode."
  (interactive)
  (let ((fn (emacs-ide-test--runner-for major-mode :project-fn)))
    (if fn (funcall fn)
      (message "test-runner: no project runner for %s." major-mode))))

(defun emacs-ide-test-run-at-point ()
  "Run the test at point using the registered runner for this mode."
  (interactive)
  (let ((fn (emacs-ide-test--runner-for major-mode :point-fn)))
    (if fn (funcall fn)
      (message "test-runner: no point runner for %s." major-mode))))

(defun emacs-ide-test-watch ()
  "Start test watch mode if available for this language."
  (interactive)
  (let ((fn (emacs-ide-test--runner-for major-mode :watch-fn)))
    (if fn (funcall fn)
      (message "test-runner: no watch mode for %s." major-mode))))

;; ============================================================================
;; FALLBACK RUNNERS — used when no lang module has registered
;; Detects test framework from project root markers
;; ============================================================================

(defun emacs-ide-test--detect-and-run ()
  "Best-effort test runner based on project markers."
  (interactive)
  (let ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                  default-directory)))
    (cond
     ((file-exists-p (expand-file-name "pytest.ini"     root)) (compile "pytest -v"))
     ((file-exists-p (expand-file-name "pyproject.toml" root)) (compile "pytest -v"))
     ((file-exists-p (expand-file-name "Cargo.toml"     root)) (compile "cargo test"))
     ((file-exists-p (expand-file-name "go.mod"         root)) (compile "go test ./..."))
     ((file-exists-p (expand-file-name "package.json"   root)) (compile "npm test"))
     ((file-exists-p (expand-file-name "pom.xml"        root)) (compile "mvn test -q"))
     ((file-exists-p (expand-file-name "build.gradle"   root)) (compile "gradle test"))
     ((file-exists-p (expand-file-name "mix.exs"        root)) (compile "mix test"))
     ((file-exists-p (expand-file-name "Gemfile"        root)) (compile "bundle exec rspec"))
     (t (call-interactively #'compile)))))

;; ============================================================================
;; STATUS
;; ============================================================================

(defun emacs-ide-test-runner-status ()
  "Show all registered test runners."
  (interactive)
  (with-output-to-temp-buffer "*Test Runner Registry*"
    (princ "=== TEST RUNNER REGISTRY ===\n\n")
    (if (null emacs-ide-test--runner-registry)
        (princ "No runners registered yet (lang modules load lazily on file open).\n")
      (dolist (entry emacs-ide-test--runner-registry)
        (let* ((mode (car entry))
               (info (cdr entry)))
          (princ (format "%-24s  file:%-30s  project:%-30s  point:%s\n"
                         mode
                         (or (plist-get info :file-fn) "—")
                         (or (plist-get info :project-fn) "—")
                         (or (plist-get info :point-fn) "—"))))))))

;; ============================================================================
;; KEYBINDINGS
;; C-c t = vterm (tools-terminal.el) — do not use as prefix.
;; C-c T = vterm-other-window — do not use.
;; Test dispatch uses C-c X (unoccupied prefix).
;; C-c x r is owned by tools-repl.el (emacs-ide-repl-launch) — do NOT bind here.
;; ============================================================================

(define-prefix-command 'emacs-ide-test-map)
(global-set-key (kbd "C-c X")   'emacs-ide-test-map)
(global-set-key (kbd "C-c X f") #'emacs-ide-test-run-file)
(global-set-key (kbd "C-c X p") #'emacs-ide-test-run-project)
(global-set-key (kbd "C-c X .") #'emacs-ide-test-run-at-point)
(global-set-key (kbd "C-c X w") #'emacs-ide-test-watch)
(global-set-key (kbd "C-c X s") #'emacs-ide-test-runner-status)

(provide 'tools-test-runner-registry)
;;; tools-test-runner-registry.el ends here
