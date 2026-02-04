;;; tools-project.el --- Project Management with Projectile (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; Professional project management and navigation with config integration
;;; Code:

;; ============================================================================
;; PROJECTILE - PROJECT MANAGEMENT
;; ============================================================================
(use-package projectile
  :demand t
  :init
  (setq projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'hybrid
        projectile-sort-order 'recentf
        
        ;; Project search paths - use config if available
        projectile-project-search-path
        (or (and (boundp 'emacs-ide-config-data)
                (let ((project-cfg (cdr (assoc 'project emacs-ide-config-data))))
                  (when project-cfg
                    (let ((paths (cdr (assoc 'search-paths project-cfg))))
                      (when (listp paths)
                        (mapcar (lambda (p) (cons p 2)) paths))))))
            '(("~/projects" . 2)
              ("~/work" . 2)
              ("~/code" . 2)))
        
        ;; Ignored directories
        projectile-globally-ignored-directories
        '(".git" ".svn" ".hg" "node_modules" "__pycache__"
          ".pytest_cache" ".mypy_cache" "target" "build" "dist"
          ".venv" "venv" "vendor" ".next" ".nuxt" "out"
          "coverage" ".cache" ".idea" ".vscode" "*.egg-info"
          ".tox" "htmlcov" ".elixir_ls" ".eunit" "_build")
        
        ;; Ignored files
        projectile-globally-ignored-files
        '("*.pyc" "*.o" "*.so" "*.dll" "*.exe" "*.class"
          "*.elc" "*.log" ".DS_Store" "Thumbs.db" "*.jar"
          "*.war" "*.beam" "TAGS")
        
        ;; Features
        projectile-auto-discover t
        projectile-switch-project-action #'projectile-dired
        projectile-require-project-root nil
        projectile-track-known-projects-automatically t
        
        ;; Use ripgrep if available, fallback to git grep
        projectile-use-git-grep (not (executable-find "rg"))
        projectile-generic-command
        (cond
         ((executable-find "rg") "rg --files --hidden --follow --glob '!.git'")
         ((executable-find "fd") "fd . -0 -H -E .git")
         (t "find . -type f -print0")))
  
  :config
  (projectile-mode +1)
  
  ;; Load additional config-based project paths if available
  (when (and (boundp 'emacs-ide-config-data)
             emacs-ide-config-data)
    (when-let ((project-config (cdr (assoc 'project emacs-ide-config-data))))
      (when-let ((paths (cdr (assoc 'search-paths project-config))))
        (when (listp paths)
          (setq projectile-project-search-path
                (mapcar (lambda (p) (cons (expand-file-name p) 2)) paths))))))
  
  :bind-keymap
  ("C-c p" . projectile-command-map)
  
  :bind (:map projectile-mode-map
              ("C-c p f" . projectile-find-file)
              ("C-c p d" . projectile-find-dir)
              ("C-c p s r" . projectile-ripgrep)
              ("C-c p s g" . projectile-grep)
              ("C-c p b" . projectile-switch-to-buffer)
              ("C-c p p" . projectile-switch-project)
              ("C-c p c" . projectile-compile-project)
              ("C-c p t" . projectile-test-project)
              ("C-c p r" . projectile-run-project)
              ("C-c p k" . projectile-kill-buffers)
              ("C-c p D" . projectile-dired)
              ("C-c p e" . projectile-recentf)
              ("C-c p i" . projectile-invalidate-cache)
              ("C-c p R" . projectile-regenerate-tags)
              ("C-c p !" . projectile-run-shell-command-in-root)
              ("C-c p &" . projectile-run-async-shell-command-in-root)))

;; ============================================================================
;; CONSULT-PROJECTILE INTEGRATION
;; ============================================================================
(use-package consult-projectile
  :after (projectile consult)
  :bind (("C-c p h" . consult-projectile)
         ("C-c p F" . consult-projectile-find-file)
         ("C-c p B" . consult-projectile-switch-to-buffer)
         ("C-c p P" . consult-projectile-switch-project)))

;; ============================================================================
;; TREEMACS - ADVANCED FILE TREE
;; ============================================================================
(use-package treemacs
  :defer t
  :init
  (setq treemacs-width 35
        treemacs-follow-mode t
        treemacs-filewatch-mode t
        treemacs-fringe-indicator-mode 'always
        treemacs-git-mode 'deferred
        treemacs-collapse-dirs 3
        treemacs-show-hidden-files t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc)
  :bind (("C-c T" . treemacs)
         ("C-c t f" . treemacs-find-file)
         ("C-c t t" . treemacs-select-window)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

;; ============================================================================
;; RECENTF - RECENT FILES
;; ============================================================================
(use-package recentf
  :straight nil
  :demand t
  :init
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 50
        recentf-auto-cleanup 'never
        recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$"
                         "\\.revive$" "/TAGS$" "^/var/folders\\.*"
                         "COMMIT_EDITMSG\\'" "^/sudo:" "^/scp:"
                         "\\.emacs\\.d/.*" "\\.cache/.*"
                         "bookmarks" "recentf"))
  :config
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list))

;; ============================================================================
;; BOOKMARKS
;; ============================================================================
(use-package bookmark
  :straight nil
  :init
  (setq bookmark-save-flag 1
        bookmark-default-file (expand-file-name "bookmarks" 
                                               (expand-file-name "var" user-emacs-directory))
        bookmark-version-control t)
  :bind (("C-c b s" . bookmark-set)
         ("C-c b j" . bookmark-jump)
         ("C-c b l" . bookmark-bmenu-list)
         ("C-c b d" . bookmark-delete)))

;; ============================================================================
;; PROJECT UTILITY FUNCTIONS
;; ============================================================================
(defun emacs-ide-project-root ()
  "Get current project root safely."
  (or (and (fboundp 'projectile-project-root)
          (projectile-project-root))
      default-directory))

(defun emacs-ide-project-find-file-at-point ()
  "Find file at point in project."
  (interactive)
  (let ((file (thing-at-point 'filename)))
    (when file
      (if (fboundp 'projectile-find-file-in-known-projects)
          (projectile-find-file-in-known-projects file)
        (message "⚠️  Projectile not available")))))

(defun emacs-ide-project-info ()
  "Display project information."
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
  "Compile project intelligently with validation."
  (interactive)
  (let ((default-directory (emacs-ide-project-root)))
    (cond
     ((file-exists-p "Makefile")
      (compile "make"))
     ((file-exists-p "CMakeLists.txt")
      (compile "cmake --build build"))
     ((file-exists-p "Cargo.toml")
      (compile "cargo build"))
     ((file-exists-p "package.json")
      (when (executable-find "npm")
        (compile "npm run build")))
     ((file-exists-p "pom.xml")
      (when (executable-find "mvn")
        (compile "mvn compile")))
     ((file-exists-p "build.gradle")
      (when (executable-find "gradle")
        (compile "gradle build")))
     (t
      (call-interactively 'compile)))))

(defun emacs-ide-project-run ()
  "Run project intelligently with validation."
  (interactive)
  (let ((default-directory (emacs-ide-project-root)))
    (cond
     ((file-exists-p "Cargo.toml")
      (compile "cargo run"))
     ((file-exists-p "package.json")
      (when (executable-find "npm")
        (compile "npm start")))
     ((file-exists-p "manage.py")
      (when (executable-find "python3")
        (compile "python3 manage.py runserver")))
     ((file-exists-p "main.go")
      (when (executable-find "go")
        (compile "go run .")))
     ((fboundp 'projectile-run-project)
      (projectile-run-project))
     (t
      (message "⚠️  No run command detected for this project")))))

(defun emacs-ide-project-test ()
  "Test project intelligently with validation."
  (interactive)
  (let ((default-directory (emacs-ide-project-root)))
    (cond
     ((file-exists-p "Cargo.toml")
      (compile "cargo test"))
     ((file-exists-p "package.json")
      (when (executable-find "npm")
        (compile "npm test")))
     ((file-exists-p "pytest.ini")
      (when (executable-find "pytest")
        (compile "pytest")))
     ((file-exists-p "go.mod")
      (when (executable-find "go")
        (compile "go test ./...")))
     ((fboundp 'projectile-test-project)
      (projectile-test-project))
     (t
      (message "⚠️  No test command detected for this project")))))

;; ============================================================================
;; PROJECT TEMPLATES
;; ============================================================================
(defun emacs-ide-project-create-python ()
  "Create new Python project structure."
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
  "Create new Rust project (requires cargo)."
  (interactive)
  (if (executable-find "cargo")
      (let ((name (read-string "Project name: ")))
        (unless (string-empty-p name)
          (shell-command (format "cargo new %s" name))
          (message "✓ Created Rust project: %s" name)))
    (message "⚠️  Cargo not found. Install Rust to create projects.")))

(defun emacs-ide-project-create-go ()
  "Create new Go project structure."
  (interactive)
  (let ((name (read-string "Project name: ")))
    (unless (string-empty-p name)
      (make-directory name)
      (with-temp-file (concat name "/main.go")
        (insert "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello, World!\")\n}\n"))
      (with-temp-file (concat name "/go.mod")
        (insert (format "module %s\n\ngo 1.21\n" name)))
      (message "✓ Created Go project: %s" name))))

;; ============================================================================
;; KEYBINDINGS FOR PROJECT UTILITIES
;; ============================================================================
(global-set-key (kbd "C-c p I") 'emacs-ide-project-info)

(provide 'tools-project)
;;; tools-project.el ends here