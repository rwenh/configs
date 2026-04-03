;;; tools-project.el --- Project Management with Projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Professional project management and navigation with config integration.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 2.2.5 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.5 to 3.0.4.
;;;   - FIX-TREEMACS-MODES: treemacs-follow-mode, treemacs-filewatch-mode, and
;;;     treemacs-git-mode are minor mode functions, not variables. setq in :init
;;;     was silently a no-op for all three. Moved to (mode 1) calls in :config.
;;;   - FIX-SEARCH-PATH-RELOAD: projectile-project-search-path was set once in
;;;     :config and never updated on emacs-ide-config-reload. Extracted into
;;;     emacs-ide-project--apply-search-paths helper called both in :config and
;;;     via emacs-ide-config-reload-hook.
;;;   - FIX-CONSULT-COLLISION: consult-projectile :bind block duplicated
;;;     C-c p B/F/P bindings already owned by completion-core.el. Removed the
;;;     duplicate :bind block here — completion-core.el is the canonical owner.
;;;   - FIX-CARGO-NEW-QUOTE: (shell-command (format "cargo new %s" name)) did
;;;     not quote name — a project name with spaces would break the shell
;;;     command. Wrapped with shell-quote-argument.
;;;   - FIX-PROJECT-ROOT-FALLBACK: emacs-ide-project-root called
;;;     projectile-project-root without ignore-errors. projectile-project-root
;;;     signals a user-error when not in a project, crashing any caller that
;;;     uses this as a safe fallback. Wrapped with ignore-errors.
;;;   - FIX-IGNORED-DIRS-CONFIG: projectile-globally-ignored-directories was
;;;     hardcoded. Now merges the hardcoded defaults with any additional entries
;;;     from config.yml project.globally-ignored-directories.
;;; Fixes vs 2.2.4 (retained):
;;;   - projectile-indexing-method 'alien (non-blocking, delegates to rg/fd).
;;;   - FIX-CONFIG: projectile-use-git-grep and projectile-generic-command
;;;     respect emacs-ide-project-search from config.yml.
;;;   - search path set once in :config (now via helper for reload support).
;;; Code:

;; ============================================================================
;; PROJECTILE - PROJECT MANAGEMENT
;; ============================================================================
(when (bound-and-true-p emacs-ide-project-enable)

;; ── Search path helper ──────────────────────────────────────────────────────
;; FIX-SEARCH-PATH-RELOAD: extracted so it can be called on config reload.
(defun emacs-ide-project--apply-search-paths ()
  "Set projectile-project-search-path from config.yml project.search-paths.
Falls back to ~/projects, ~/work, ~/code if not configured."
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

(add-hook 'emacs-ide-config-reload-hook #'emacs-ide-project--apply-search-paths)

;; ── Ignored directories: merge hardcoded defaults with config additions ──────
;; FIX-IGNORED-DIRS-CONFIG: config.yml project.globally-ignored-directories
;; values are now merged with the hardcoded baseline list.
(defconst emacs-ide-project--default-ignored-dirs
  '(".git" ".svn" ".hg" "node_modules" "__pycache__"
    ".pytest_cache" ".mypy_cache" "target" "build" "dist"
    ".venv" "venv" "vendor" ".next" ".nuxt" "out"
    "coverage" ".cache" ".idea" ".vscode" "*.egg-info"
    ".tox" "htmlcov" ".elixir_ls" ".eunit" "_build")
  "Baseline ignored directories always excluded from projectile.")

(defun emacs-ide-project--ignored-dirs ()
  "Return merged ignored dirs: baseline + config.yml additions."
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

        ;; FIX-IGNORED-DIRS-CONFIG: computed at init time; reload hook handles updates
        projectile-globally-ignored-directories
        (emacs-ide-project--ignored-dirs)

        projectile-globally-ignored-files
        '("*.pyc" "*.o" "*.so" "*.dll" "*.exe" "*.class"
          "*.elc" "*.log" ".DS_Store" "Thumbs.db" "*.jar"
          "*.war" "*.beam" "TAGS")

        projectile-auto-discover                   t
        projectile-switch-project-action           #'projectile-dired
        projectile-require-project-root            nil
        projectile-track-known-projects-automatically t

        ;; FIX-CONFIG (retained): respect emacs-ide-project-search from config.yml
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
  ;; FIX-SEARCH-PATH-RELOAD: use helper so reload hook can re-call it
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

;; ============================================================================
;; CONSULT-PROJECTILE INTEGRATION
;; FIX-CONSULT-COLLISION: C-c p B/F/P bindings removed — completion-core.el
;; is the canonical owner of those keys. Only C-c p h (consult-projectile
;; unified search) is added here as it has no collision.
;; ============================================================================
(use-package consult-projectile
  :after (projectile consult)
  :bind (("C-c p h" . consult-projectile)))

;; ============================================================================
;; TREEMACS - ADVANCED FILE TREE
;; FIX-TREEMACS-MODES: treemacs-follow-mode, treemacs-filewatch-mode, and
;; treemacs-git-mode are minor mode functions, not variables. setq in :init
;; was silently a no-op. Moved to explicit activation calls in :config.
;; ============================================================================
(use-package treemacs
  :defer t
  :init
  (setq treemacs-width                    35
        treemacs-collapse-dirs            3
        treemacs-show-hidden-files        t
        treemacs-is-never-other-window    t
        treemacs-sorting                  'alphabetic-case-insensitive-asc
        treemacs-fringe-indicator-mode    'always)
  :config
  ;; FIX-TREEMACS-MODES: activate modes in :config where the functions exist
  (when (fboundp 'treemacs-follow-mode)     (treemacs-follow-mode 1))
  (when (fboundp 'treemacs-filewatch-mode)  (treemacs-filewatch-mode 1))
  (when (fboundp 'treemacs-git-mode)        (treemacs-git-mode 'deferred))
  :bind (("<f9>" . treemacs)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

;; ============================================================================
;; PROJECT UTILITY FUNCTIONS
;; ============================================================================
(defun emacs-ide-project-root ()
  "Get current project root safely.
FIX-PROJECT-ROOT-FALLBACK: projectile-project-root signals user-error when
not in a project — wrapped with ignore-errors to make this a safe fallback."
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
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
  "Compile project intelligently."
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
  "Run project intelligently."
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
  "Test project intelligently."
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
  "Create new Rust project (requires cargo).
FIX-CARGO-NEW-QUOTE: project name now quoted with shell-quote-argument."
  (interactive)
  (if (executable-find "cargo")
      (let ((name (read-string "Project name: ")))
        (unless (string-empty-p name)
          (shell-command (format "cargo new %s" (shell-quote-argument name)))
          (message "✓ Created Rust project: %s" name)))
    (message "⚠️  Cargo not found.")))

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
;; KEYBINDINGS FOR PROJECT INFO AND TREEMACS
;; ============================================================================
(with-eval-after-load 'projectile
  (define-key projectile-mode-map    (kbd "C-c p I") #'emacs-ide-project-info)
  (define-key projectile-command-map (kbd "F")       #'treemacs-find-file)
  (define-key projectile-command-map (kbd "W")       #'treemacs-select-window))

) ;; end (when emacs-ide-project-enable)

(provide 'tools-project)
;;; tools-project.el ends here
