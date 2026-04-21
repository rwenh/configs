;;; tools-test.el --- Language-Aware Test Runner -*- lexical-binding: t -*-
;;; Version: 3.2.1 | FIX: Removed duplicate global-set-key calls (all 4 are set
;;;           canonically in keybindings.el which loads last).
;;; Code:

(require 'cl-lib)
(require 'compile)

(defvar emacs-ide-test--last-command   nil)
(defvar emacs-ide-test--last-directory nil)
(defvar emacs-ide-test--history        nil)
(defvar emacs-ide-test--history-max    50)

(defun emacs-ide-test-run (&optional arg)
  (interactive "P")
  (cond
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
   (t (emacs-ide-test--auto-detect-and-run arg))))

(defun emacs-ide-test-run-all ()
  (interactive)
  (let ((buffer-file-name nil))
    (emacs-ide-test--auto-detect-and-run nil)))

(defun emacs-ide-test--project-root ()
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      default-directory))

(defun emacs-ide-test--run-compile (cmd &optional dir)
  (let* ((directory  (or dir (emacs-ide-test--project-root)))
         (default-directory directory)
         (start-time (float-time)))
    (setq emacs-ide-test--last-command   cmd
          emacs-ide-test--last-directory directory)
    (let ((buf (compile cmd)))
      (when buf
        (with-current-buffer buf
          (setq-local compilation-finish-functions nil)
          (add-hook 'compilation-finish-functions
                    (lambda (_buf status)
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

(defun emacs-ide-test--auto-detect-and-run (_arg)
  (let ((cmd (emacs-ide-test--detect-command)))
    (cond
     ((null cmd)
      (message "test-runner: no test framework detected. Use C-c h t for test hydra."))
     ((eq cmd :ert)
      (if (fboundp 'ert-run-tests-interactively)
          (ert-run-tests-interactively t)
        (message "test-runner: ert not available")))
     (t
      (emacs-ide-test--run-compile cmd)))))

(defun emacs-ide-test--detect-command ()
  (let ((root (emacs-ide-test--project-root)))
    (cond
     ((derived-mode-p 'python-mode 'python-ts-mode)
      (cond ((executable-find "pytest")  "pytest -v")
            ((executable-find "python3") "python3 -m unittest discover")
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
      (cond ((file-exists-p (expand-file-name "pom.xml"          root)) "mvn test -q")
            ((file-exists-p (expand-file-name "build.gradle"     root)) "gradle test")
            ((file-exists-p (expand-file-name "build.gradle.kts" root)) "gradle test")
            (t nil)))
     ((derived-mode-p 'elixir-mode)
      (when (executable-find "mix") "mix test"))
     ((derived-mode-p 'ruby-mode 'ruby-ts-mode)
      (cond ((executable-find "rspec") "bundle exec rspec")
            ((executable-find "ruby")  "ruby -Itest")
            (t nil)))
     ((derived-mode-p 'haskell-mode)
      (cond ((executable-find "cabal") "cabal test")
            ((executable-find "stack") "stack test")
            (t nil)))
     ((derived-mode-p 'sh-mode 'bash-ts-mode)
      (when (and (executable-find "bats") (buffer-file-name))
        (format "bats %s" (shell-quote-argument (buffer-file-name)))))
     ((derived-mode-p 'emacs-lisp-mode)
      (when (fboundp 'ert-run-tests-interactively) :ert))
     ((derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
      (cond ((file-exists-p (expand-file-name "CMakeLists.txt" root)) "ctest --test-dir build")
            ((emacs-ide-test--makefile-has-test-target root)          "make test")
            (t nil)))
     ((derived-mode-p 'kotlin-mode)
      (when (executable-find "gradle") "gradle test"))
     ((derived-mode-p 'scala-mode)
      (when (executable-find "sbt") "sbt test"))
     ((derived-mode-p 'clojure-mode 'clojurescript-mode)
      (when (executable-find "clj") "clj -M:test"))
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
  (let ((makefile (expand-file-name "Makefile" root)))
    (and (file-exists-p makefile)
         (with-temp-buffer
           (insert-file-contents makefile)
           (re-search-forward "^test[[:space:]]*:" nil t)))))

(defun emacs-ide-test-run-last ()
  (interactive)
  (if emacs-ide-test--last-command
      (let ((default-directory
              (or emacs-ide-test--last-directory default-directory)))
        (compile emacs-ide-test--last-command))
    (message "test-runner: no previous test run")))

(defun emacs-ide-test-report ()
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

(provide 'tools-test)
;;; tools-test.el ends here
