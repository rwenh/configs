;;; tools-project.el --- Project Management with Projectile -*- lexical-binding: t -*-
;;; Version: 3.4.0
;;; Code:

(require 'cl-lib)

(when (bound-and-true-p emacs-ide-project-enable)

(defun emacs-ide-project--apply-search-paths ()
  (when (fboundp 'projectile-mode)
    (setq projectile-project-search-path
          (or (and (boundp 'emacs-ide-config-data)
                   emacs-ide-config-data
                   (when-let ((project-config
                               (cdr (assoc 'project emacs-ide-config-data))))
                     (when-let ((paths
                                 (cdr (assoc 'search-paths project-config))))
                       (when (and paths (listp paths))
                         (mapcar (lambda (p) (cons (expand-file-name p) 2))
                                 paths)))))
              '(("~/projects" . 2)
                ("~/work"     . 2)
                ("~/code"     . 2))))))

(when (boundp 'emacs-ide-config-reload-hook)
  (add-hook 'emacs-ide-config-reload-hook
            (lambda ()
              (emacs-ide-project--apply-search-paths)
              (when (fboundp 'projectile-invalidate-cache)
                (projectile-invalidate-cache nil)))))

(defconst emacs-ide-project--default-ignored-dirs
  '(".git" ".svn" ".hg" "node_modules" "__pycache__"
    ".pytest_cache" ".mypy_cache" "target" "build" "dist"
    ".venv" "venv" "vendor" ".next" ".nuxt" "out"
    "coverage" ".cache" ".idea" ".vscode" "*.egg-info"
    ".tox" "htmlcov" ".elixir_ls" ".eunit" "_build")
  "Baseline ignored directories always excluded from projectile.")

(defun emacs-ide-project--ignored-dirs ()
  (let ((cfg-dirs (and (boundp 'emacs-ide-config-data)
                       (when-let ((pc (cdr (assoc 'project emacs-ide-config-data))))
                         (cdr (assoc 'globally-ignored-directories pc))))))
    (if (and cfg-dirs (listp cfg-dirs))
        (delete-dups (append emacs-ide-project--default-ignored-dirs cfg-dirs))
      emacs-ide-project--default-ignored-dirs)))

(use-package projectile
  :demand t
  :init
  (setq projectile-completion-system  'default
        projectile-enable-caching     t
        projectile-indexing-method    'alien
        projectile-sort-order         'recentf
        projectile-globally-ignored-directories
        (emacs-ide-project--ignored-dirs)
        projectile-globally-ignored-files
        '("*.pyc" "*.o" "*.so" "*.dll" "*.exe" "*.class"
          "*.elc" "*.log" ".DS_Store" "Thumbs.db" "*.jar"
          "*.war" "*.beam" "TAGS")
        projectile-auto-discover                    t
        projectile-switch-project-action            #'projectile-find-file
        projectile-require-project-root             nil
        projectile-track-known-projects-automatically t
        projectile-use-git-grep
        (eq (bound-and-true-p emacs-ide-project-search) 'git-grep)
        projectile-generic-command
        (cond
         ((and (not (eq (bound-and-true-p emacs-ide-project-search) 'grep))
               (executable-find "rg"))
          "rg --files --hidden --follow --glob '!.git'")
         ((executable-find "fd") "fd . -0 -H -E .git")
         (t "find . -type f -print0")))

  :config
  (projectile-mode +1)
  (emacs-ide-project--apply-search-paths)

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :bind (:map projectile-mode-map
              ("C-c p f"   . projectile-find-file)
              ("C-c p d"   . projectile-find-dir)
              ("C-c p s r" . projectile-ripgrep)
              ("C-c p s g" . projectile-grep)
              ("C-c p b"   . projectile-switch-to-buffer)
              ("C-c p p"   . projectile-switch-project)
              ("C-c p c"   . projectile-compile-project)
              ("C-c p t"   . projectile-test-project)
              ("C-c p r"   . projectile-run-project)
              ("C-c p k"   . projectile-kill-buffers)
              ("C-c p D"   . projectile-dired)
              ("C-c p e"   . projectile-recentf)
              ("C-c p i"   . projectile-invalidate-cache)
              ("C-c p R"   . projectile-regenerate-tags)
              ("C-c p !"   . projectile-run-shell-command-in-root)
              ("C-c p &"   . projectile-run-async-shell-command-in-root)))

(use-package consult-projectile
  :after (projectile consult)
  :bind (("C-c p h" . consult-projectile)))

;;; ─── ibuffer-project ─────────────────────────────────────────────────────────

(use-package ibuffer-project
  :after projectile
  :config
  (defun emacs-ide-ibuffer-project-setup ()
    "Configure ibuffer to group buffers by project and sort by file path."
    (setq ibuffer-filter-groups
          (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))

  (add-hook 'ibuffer-hook #'emacs-ide-ibuffer-project-setup)
  (keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)
  (setq ibuffer-movement-cycle nil
        ibuffer-old-time       24))

;;; ─── treemacs — strictly deferred ───────────────────────────────────────────

(use-package treemacs
  :commands (treemacs
             treemacs-select-window
             treemacs-find-file
             treemacs-add-project-to-workspace
             treemacs-remove-project-from-workspace)
  :init
  (setq treemacs-width                    35
        treemacs-collapse-dirs            3
        treemacs-show-hidden-files        t
        treemacs-is-never-other-window    t
        treemacs-sorting                  'alphabetic-case-insensitive-asc
        treemacs-fringe-indicator-mode    'always
        ;; Avoid pulling in lsp-treemacs just by opening treemacs
        treemacs-python-executable        (executable-find "python3"))
  :config
  (when (fboundp 'treemacs-follow-mode)    (treemacs-follow-mode 1))
  (when (fboundp 'treemacs-filewatch-mode) (treemacs-filewatch-mode 1))
  (when (fboundp 'treemacs-git-mode)       (treemacs-git-mode 'deferred))
  :bind (("<f9>" . treemacs)))

(use-package treemacs-projectile
  :commands (treemacs-projectile)
  :after (treemacs projectile))

(use-package treemacs-magit
  :commands (treemacs-magit--schedule-update)
  :after (treemacs magit))

;;; ─── Project helpers ─────────────────────────────────────────────────────────

(defun emacs-ide-project-root ()
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      default-directory))

(defun emacs-ide-project-find-file-at-point ()
  (interactive)
  (let ((file (thing-at-point 'filename)))
    (when file
      (if (fboundp 'projectile-find-file-in-known-projects)
          (projectile-find-file-in-known-projects file)
        (message "⚠️  Projectile not available")))))

(defun emacs-ide-project-info ()
  (interactive)
  (if-let ((project-root (emacs-ide-project-root)))
      (if (fboundp 'projectile-project-name)
          (message "Project: %s | Files: %d"
                   (projectile-project-name)
                   (length (or (and (fboundp 'projectile-current-project-files)
                                    (projectile-current-project-files))
                               '())))
        (message "Project: %s" project-root))
    (message "Not in a project")))

(defun emacs-ide-project-compile ()
  (interactive)
  (let ((default-directory (emacs-ide-project-root)))
    (cond
     ((file-exists-p "Makefile")       (compile "make"))
     ((file-exists-p "CMakeLists.txt") (compile "cmake --build build"))
     ((file-exists-p "Cargo.toml")     (compile "cargo build"))
     ((file-exists-p "package.json")
      (when (executable-find "npm")    (compile "npm run build")))
     ((file-exists-p "pom.xml")
      (when (executable-find "mvn")    (compile "mvn compile")))
     ((file-exists-p "build.gradle")
      (when (executable-find "gradle") (compile "gradle build")))
     (t (call-interactively 'compile)))))

(defun emacs-ide-project-run ()
  (interactive)
  (let ((default-directory (emacs-ide-project-root)))
    (cond
     ((file-exists-p "Cargo.toml")     (compile "cargo run"))
     ((file-exists-p "package.json")
      (when (executable-find "npm")    (compile "npm start")))
     ((file-exists-p "manage.py")
      (when (executable-find "python3") (compile "python3 manage.py runserver")))
     ((file-exists-p "main.go")
      (when (executable-find "go")     (compile "go run .")))
     ((fboundp 'projectile-run-project) (projectile-run-project))
     (t (message "⚠️  No run command detected for this project")))))

(defun emacs-ide-project-test ()
  (interactive)
  (let ((default-directory (emacs-ide-project-root)))
    (cond
     ((file-exists-p "Cargo.toml")     (compile "cargo test"))
     ((file-exists-p "package.json")
      (when (executable-find "npm")    (compile "npm test")))
     ((file-exists-p "pytest.ini")
      (when (executable-find "pytest") (compile "pytest")))
     ((file-exists-p "go.mod")
      (when (executable-find "go")     (compile "go test ./...")))
     ((fboundp 'projectile-test-project) (projectile-test-project))
     (t (message "⚠️  No test command detected for this project")))))

(defun emacs-ide-project-create-python ()
  (interactive)
  (let ((name (read-string "Project name: ")))
    (unless (string-empty-p name)
      (make-directory name)
      (make-directory (concat name "/tests"))
      (with-temp-file (concat name "/README.md")
        (insert (format "# %s\n\n" name)))
      (with-temp-file (concat name "/requirements.txt") (insert ""))
      (with-temp-file (concat name "/.gitignore")
        (insert "__pycache__/\n*.pyc\n.venv/\n"))
      (message "✓ Created Python project: %s" name))))

(defun emacs-ide-project-create-rust ()
  (interactive)
  (if (executable-find "cargo")
      (let ((name (read-string "Project name: ")))
        (unless (string-empty-p name)
          (shell-command (format "cargo new %s" (shell-quote-argument name)))
          (message "✓ Created Rust project: %s" name)))
    (message "⚠️  Cargo not found.")))

(defun emacs-ide-project-create-go ()
  (interactive)
  (let ((name (read-string "Project name: ")))
    (unless (string-empty-p name)
      (make-directory name)
      (with-temp-file (concat name "/main.go")
        (insert "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello, World!\")\n}\n"))
      (with-temp-file (concat name "/go.mod")
        (insert (format "module %s\n\ngo 1.21\n" name)))
      (message "✓ Created Go project: %s" name))))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map    (kbd "C-c p I") #'emacs-ide-project-info)
  (define-key projectile-command-map (kbd "F")       #'treemacs-find-file)
  (define-key projectile-command-map (kbd "W")       #'treemacs-select-window))

) ;; end (when emacs-ide-project-enable)

(provide 'tools-project)
;;; tools-project.el ends here
