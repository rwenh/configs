;;; tools-test-runner-registry.el --- Per-lang test runner registry -*- lexical-binding: t -*-
;;; Version: 3.2.1 | FIX: Removed duplicate global-set-key calls and dead define-prefix-command.
;;;           Keybindings are set canonically in keybindings.el.
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-test--runner-registry nil
  "Alist of (MAJOR-MODE . plist) for test runners.")

(defun emacs-ide-test-register-runner (mode &rest plist)
  (let ((existing (assoc mode emacs-ide-test--runner-registry)))
    (if existing
        (setcdr existing plist)
      (push (cons mode plist) emacs-ide-test--runner-registry))))

(defun emacs-ide-test--runner-for (mode key)
  (let ((entry (assoc mode emacs-ide-test--runner-registry)))
    (and entry (plist-get (cdr entry) key))))

(defun emacs-ide-test--no-runner-message (key)
  (message "test-runner: no %s runner for %s. Register with emacs-ide-test-register-runner."
           (substring (symbol-name key) 1)
           major-mode))

(defun emacs-ide-test-run-file ()
  (interactive)
  (let ((fn (emacs-ide-test--runner-for major-mode :file-fn)))
    (if fn (funcall fn)
      (emacs-ide-test--no-runner-message :file-fn))))

(defun emacs-ide-test-run-project ()
  (interactive)
  (let ((fn (emacs-ide-test--runner-for major-mode :project-fn)))
    (if fn (funcall fn)
      (emacs-ide-test--no-runner-message :project-fn))))

(defun emacs-ide-test-run-at-point ()
  (interactive)
  (let ((fn (emacs-ide-test--runner-for major-mode :point-fn)))
    (if fn (funcall fn)
      (emacs-ide-test--no-runner-message :point-fn))))

(defun emacs-ide-test-watch ()
  (interactive)
  (let ((fn (emacs-ide-test--runner-for major-mode :watch-fn)))
    (if fn (funcall fn)
      (emacs-ide-test--no-runner-message :watch-fn))))

(defun emacs-ide-test--detect-and-run ()
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

(defun emacs-ide-test--format-fn (fn)
  (cond
   ((null fn)      "—")
   ((symbolp fn)   (symbol-name fn))
   (t              "#<lambda>")))

(defun emacs-ide-test-runner-status ()
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

(provide 'tools-test-runner-registry)
;;; tools-test-runner-registry.el ends here
