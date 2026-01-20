;;; tools-project.el --- Project Management with Projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Professional project management and navigation
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
        
        ;; Project search paths (can be overridden by config.yml)
        projectile-project-search-path '(("~/projects" . 2)
                                         ("~/work" . 2)
                                         ("~/code" . 2))
        
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
        
        ;; Use ripgrep if available
        projectile-use-git-grep (not (executable-find "rg"))
        projectile-generic-command
        (cond
         ((executable-find "rg") "rg --files --hidden --follow --glob '!.git'")
         ((executable-find "fd") "fd . -0 -H -E .git")
         (t "find . -type f -print0")))
  
  :config
  (projectile-mode +1)
  
  ;; Load config-based project paths if available
  (when (and (boundp 'emacs-ide-config-data)
             emacs-ide-config-data)
    (when-let ((project-config (cdr (assoc 'project emacs-ide-config-data))))
      (when-let ((paths (cdr (assoc 'search-paths project-config))))
        (setq projectile-project-search-path
              (mapcar (lambda (p) (cons p 2)) paths)))))
  
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
              ("C-c p T" . projectile-find-test-file)
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
;; FILE TREE - NEOTREE
;; ============================================================================
(use-package neotree
  :bind (("<f8>" . neotree-toggle)
         ("C-c n" . emacs-ide-neotree-projectile-action))
  :init
  (setq neo-smart-open t
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-window-width 30
        neo-create-file-auto-open t
        neo-auto-indent-point t
        neo-modern-sidebar t
        neo-show-updir-line nil
        neo-vc-integration '(face))
  :config
  (defun emacs-ide-neotree-projectile-action ()
    "Open NeoTree at project root or current directory."
    (interactive)
    (let ((project-root (projectile-project-root)))
      (neotree-toggle)
      (if project-root
          (neotree-dir project-root)
        (neotree-dir default-directory)))))

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
  :ensure nil
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
  :ensure nil
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
  "Get current project root."
  (or (projectile-project-root)
      default-directory))

(defun emacs-ide-project-find-file-at-point ()
  "Find file at point in project."
  (interactive)
  (let ((file (thing-at-point 'filename)))
    (when file
      (projectile-find-file-in-known-projects file))))

(defun emacs-ide-project-info ()
  "Display project information."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (message "Project: %s | Type: %s | Files: %d"
               (projectile-project-name)
               (projectile-project-type)
               (length (projectile-current-project-files)))
    (message "Not in a project")))

(defun emacs-ide-project-compile ()
  "Compile project intelligently."
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
      (compile "npm run build"))
     ((file-exists-p "pom.xml")
      (compile "mvn compile"))
     ((file-exists-p "build.gradle")
      (compile "gradle build"))
     (t
      (call-interactively 'compile)))))

(defun emacs-ide-project-run ()
  "Run project intelligently."
  (interactive)
  (let ((default-directory (emacs-ide-project-root)))
    (cond
     ((file-exists-p "Cargo.toml")
      (compile "cargo run"))
     ((file-exists-p "package.json")
      (compile "npm start"))
     ((file-exists-p "manage.py")
      (compile "python manage.py runserver"))
     ((file-exists-p "main.go")
      (compile "go run ."))
     (t
      (call-interactively 'projectile-run-project)))))

(defun emacs-ide-project-test ()
  "Test project intelligently."
  (interactive)
  (let ((default-directory (emacs-ide-project-root)))
    (cond
     ((file-exists-p "Cargo.toml")
      (compile "cargo test"))
     ((file-exists-p "package.json")
      (compile "npm test"))
     ((file-exists-p "pytest.ini")
      (compile "pytest"))
     ((file-exists-p "go.mod")
      (compile "go test ./..."))
     (t
      (call-interactively 'projectile-test-project)))))

;; ============================================================================
;; PROJECT TEMPLATES
;; ============================================================================
(defun emacs-ide-project-create-python ()
  "Create new Python project structure."
  (interactive)
  (let ((name (read-string "Project name: ")))
    (make-directory name)
    (make-directory (concat name "/tests"))
    (with-temp-file (concat name "/README.md")
      (insert (format "# %s\n\n" name)))
    (with-temp-file (concat name "/requirements.txt") (insert ""))
    (with-temp-file (concat name "/.gitignore")
      (insert "__pycache__/\n*.pyc\n.venv/\n"))
    (message "Created Python project: %s" name)))

(defun emacs-ide-project-create-rust ()
  "Create new Rust project."
  (interactive)
  (let ((name (read-string "Project name: ")))
    (shell-command (format "cargo new %s" name))
    (message "Created Rust project: %s" name)))

(defun emacs-ide-project-create-go ()
  "Create new Go project."
  (interactive)
  (let ((name (read-string "Project name: ")))
    (make-directory name)
    (with-temp-file (concat name "/main.go")
      (insert "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello, World!\")\n}\n"))
    (with-temp-file (concat name "/go.mod")
      (insert (format "module %s\n\ngo 1.21\n" name)))
    (message "Created Go project: %s" name)))

;; ============================================================================
;; KEYBINDINGS FOR PROJECT UTILITIES
;; ============================================================================
(global-set-key (kbd "C-c p C") 'emacs-ide-project-compile)
(global-set-key (kbd "C-c p R") 'emacs-ide-project-run)
(global-set-key (kbd "C-c p T") 'emacs-ide-project-test)
(global-set-key (kbd "C-c p I") 'emacs-ide-project-info)

(provide 'tools-project)
;;; tools-project.el ends here
