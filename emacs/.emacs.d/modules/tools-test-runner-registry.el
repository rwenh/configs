;;; tools-test-runner-registry.el --- Per-lang test runner registry -*- lexical-binding: t -*-
;;; Commentary:
;;; Extends tools-test.el with a registry API so each lang-*.el can
;;; register its own test runner without modifying tools-test.el.
;;; tools-test.el dispatches through this registry.
;;;
;;; Add "tools-test-runner-registry" to feature-modules BEFORE "tools-test".
;;;
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.1 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.1 to 3.0.4.
;;;   - FIX-REGISTRY-RELOAD: emacs-ide-test-register-runner used
;;;     (setf (alist-get mode ...) plist) which appends a new cons on every
;;;     call rather than updating the existing entry — duplicate entries
;;;     accumulated on M-x emacs-ide-config-reload. Fixed to update in-place
;;;     (same pattern as emacs-ide-health-register-check and tools-repl.el).
;;;   - FIX-RUNNER-FOR-PLIST: emacs-ide-test--runner-for used alist-get
;;;     which returns the raw cdr. After the reload fix the registry uses
;;;     standard cons cells — changed to assoc + cdr for clarity and
;;;     consistency with the new register implementation.
;;;   - FIX-DETECT-AND-RUN-ROOT: emacs-ide-test--detect-and-run called
;;;     projectile-project-root without ignore-errors — crashes when not in
;;;     a project. Wrapped with ignore-errors.
;;;   - FIX-DETECT-AND-RUN-UNUSED: emacs-ide-test--detect-and-run was never
;;;     called from any dispatch command or keybinding — it duplicated logic
;;;     in tools-test.el and was dead code. Retained as an interactive
;;;     standalone command with a clear docstring explaining its purpose.
;;;   - FIX-STATUS-FN-FORMAT: emacs-ide-test-runner-status formatted function
;;;     values with bare %s — lambdas printed as unreadable forms. Now uses
;;;     a helper that prints the symbol name for symbols and "#<lambda>" for
;;;     anonymous functions.
;;;   - FIX-WATCH-MESSAGE: Unregistered-mode messages now consistently include
;;;     the "Register with emacs-ide-test-register-runner" hint across all
;;;     four dispatch commands.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-1: Removed duplicate emacs-ide-test-run definition.
;;;   - FIX-2: Removed stray C-c x r binding.
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
    :point-fn   #'emacs-ide-python-test-at-point)
FIX-REGISTRY-RELOAD: idempotent — updates existing entry in-place rather
than pushing a duplicate on every config reload."
  (let ((existing (assoc mode emacs-ide-test--runner-registry)))
    (if existing
        (setcdr existing plist)
      (push (cons mode plist) emacs-ide-test--runner-registry))))

(defun emacs-ide-test--runner-for (mode key)
  "Return the runner fn for MODE and KEY (:file-fn/:project-fn/:point-fn/:watch-fn).
FIX-RUNNER-FOR-PLIST: uses assoc + cdr consistent with the updated registry."
  (let ((entry (assoc mode emacs-ide-test--runner-registry)))
    (and entry (plist-get (cdr entry) key))))

;; ============================================================================
;; DISPATCH HELPERS
;; FIX-WATCH-MESSAGE: all unregistered-mode messages now include the hint.
;; ============================================================================
(defun emacs-ide-test--no-runner-message (key)
  "Emit a consistent 'no runner' message for KEY in current major-mode."
  (message "test-runner: no %s runner for %s. Register with emacs-ide-test-register-runner."
           (substring (symbol-name key) 1)  ; strip leading colon
           major-mode))

;; ============================================================================
;; DISPATCH COMMANDS
;; ============================================================================
(defun emacs-ide-test-run-file ()
  "Run tests for the current file using the registered runner for this mode."
  (interactive)
  (let ((fn (emacs-ide-test--runner-for major-mode :file-fn)))
    (if fn (funcall fn)
      (emacs-ide-test--no-runner-message :file-fn))))

(defun emacs-ide-test-run-project ()
  "Run all project tests using the registered runner for this mode."
  (interactive)
  (let ((fn (emacs-ide-test--runner-for major-mode :project-fn)))
    (if fn (funcall fn)
      (emacs-ide-test--no-runner-message :project-fn))))

(defun emacs-ide-test-run-at-point ()
  "Run the test at point using the registered runner for this mode."
  (interactive)
  (let ((fn (emacs-ide-test--runner-for major-mode :point-fn)))
    (if fn (funcall fn)
      (emacs-ide-test--no-runner-message :point-fn))))

(defun emacs-ide-test-watch ()
  "Start test watch mode if available for this language."
  (interactive)
  (let ((fn (emacs-ide-test--runner-for major-mode :watch-fn)))
    (if fn (funcall fn)
      (emacs-ide-test--no-runner-message :watch-fn))))

;; ============================================================================
;; FALLBACK RUNNER
;; FIX-DETECT-AND-RUN-UNUSED: retained as an explicit interactive standalone
;; command for use when no lang module is registered AND the user wants a
;; quick marker-based dispatch without going through the full test hydra.
;; FIX-DETECT-AND-RUN-ROOT: wrapped with ignore-errors.
;; ============================================================================
(defun emacs-ide-test--detect-and-run ()
  "Best-effort test runner based on project root markers.
This is a standalone interactive fallback — it is not called by the
normal dispatch chain (which goes through tools-test.el auto-detection).
Use when no lang module runner is registered and you want a quick run."
  (interactive)
  (let ((root (or (and (fboundp 'projectile-project-root)
                       (ignore-errors (projectile-project-root)))
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
;; FIX-STATUS-FN-FORMAT: helper formats function symbols by name and lambdas
;; as "#<lambda>" rather than printing the raw unreadable lambda form.
;; ============================================================================
(defun emacs-ide-test--format-fn (fn)
  "Return a readable string for FN (symbol name or \"#<lambda>\")."
  (cond
   ((null fn)      "—")
   ((symbolp fn)   (symbol-name fn))
   (t              "#<lambda>")))

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
          (princ (format "%-24s  file:%-30s  project:%-30s  point:%-24s  watch:%s\n"
                         mode
                         (emacs-ide-test--format-fn (plist-get info :file-fn))
                         (emacs-ide-test--format-fn (plist-get info :project-fn))
                         (emacs-ide-test--format-fn (plist-get info :point-fn))
                         (emacs-ide-test--format-fn (plist-get info :watch-fn)))))))))

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
