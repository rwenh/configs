;;; tools-config.el --- Professional Development Tools -*- lexical-binding: t -*-
;;; Commentary:
;;; LSP, Flycheck, Projectile, Magit, VTerm - Elite configuration
;;; Code:

;; ============================================================================
;; FLYCHECK
;; ============================================================================
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-c ! n" . flycheck-next-error)
              ("C-c ! p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)
              ("C-c ! v" . flycheck-verify-setup)
              ("C-c ! c" . flycheck-clear)
              ("C-c ! C" . flycheck-compile)
              ("C-c ! s" . flycheck-select-checker)
              ("C-c ! h" . flycheck-display-error-at-point))
  :init
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled new-line)
        flycheck-indication-mode 'left-margin
        flycheck-display-errors-delay 0.1
        flycheck-idle-change-delay 0.5
        flycheck-highlighting-mode 'lines
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-disabled-checkers '(emacs-lisp-checkdoc)
        flycheck-temp-prefix ".flycheck"
        flycheck-global-modes '(not org-mode)))

(use-package flycheck-pos-tip
  :after flycheck
  :if (display-graphic-p)
  :config
  (flycheck-pos-tip-mode 1))

;; ============================================================================
;; LSP-MODE - PROFESSIONAL
;; ============================================================================
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode objc-mode cuda-mode) . lsp-deferred)
         ((python-mode python-ts-mode) . lsp-deferred)
         ((rust-mode rust-ts-mode) . lsp-deferred)
         ((go-mode go-ts-mode) . lsp-deferred)
         ((java-mode java-ts-mode) . lsp-deferred)
         ((js-mode js-ts-mode js2-mode) . lsp-deferred)
         ((typescript-mode typescript-ts-mode tsx-ts-mode) . lsp-deferred)
         ((web-mode html-mode css-mode) . lsp-deferred)
         ((php-mode ruby-mode elixir-mode) . lsp-deferred)
         ((kotlin-mode scala-mode swift-mode) . lsp-deferred)
         ((csharp-mode csharp-ts-mode) . lsp-deferred)
         ((zig-mode nim-mode lua-mode) . lsp-deferred)
         ((yaml-mode dockerfile-mode terraform-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
  :bind (:map lsp-mode-map
              ("C-c l r" . lsp-rename)
              ("C-c l f" . lsp-format-buffer)
              ("C-c l F" . lsp-format-region)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l d" . lsp-describe-thing-at-point)
              ("C-c l g" . lsp-find-definition)
              ("C-c l G" . lsp-find-declaration)
              ("C-c l R" . lsp-find-references)
              ("C-c l i" . lsp-find-implementation)
              ("C-c l t" . lsp-find-type-definition)
              ("C-c l o" . lsp-organize-imports)
              ("C-c l s" . lsp-signature-activate)
              ("C-c l h" . lsp-document-highlight)
              ("C-c l I" . lsp-ui-imenu)
              ("C-c l e" . lsp-treemacs-errors-list)
              ("C-c l w" . lsp-restart-workspace)
              ("C-c l W" . lsp-shutdown-workspace))
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-completion-provider :none
        lsp-idle-delay 0.3
        lsp-log-io nil
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 10000
        lsp-enable-folding t
        lsp-enable-links t
        lsp-enable-snippet t
        lsp-keep-workspace-alive nil
        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable t
        lsp-modeline-workspace-status-enable t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-eldoc-enable-hover t
        lsp-eldoc-render-all nil
        lsp-headerline-breadcrumb-enable t
        lsp-semantic-tokens-enable t
        lsp-enable-symbol-highlighting t
        lsp-lens-enable t
        lsp-enable-on-type-formatting t
        lsp-enable-indentation t
        lsp-before-save-edits t
        lsp-auto-guess-root t
        lsp-restart 'auto-restart
        lsp-enable-suggest-server-download t
        lsp-inlay-hint-enable t
        lsp-use-plists t
        lsp-warn-no-matched-clients nil
        lsp-diagnostics-provider :flycheck
        lsp-auto-configure t)
  :config
  (defun emacs-ide-lsp-optimize ()
    "Optimize LSP for large files."
    (when (> (buffer-size) 500000)
      (setq-local lsp-enable-symbol-highlighting nil
                  lsp-enable-on-type-formatting nil
                  lsp-enable-folding nil
                  lsp-lens-enable nil)))
  (add-hook 'lsp-mode-hook #'emacs-ide-lsp-optimize))

(use-package lsp-ui
  :after lsp-mode
  :bind (:map lsp-ui-mode-map
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-?" . lsp-ui-peek-find-references)
              ("C-c l ." . lsp-ui-peek-find-workspace-symbol)
              ("C-c l p" . lsp-ui-peek-jump-backward)
              ("C-c l n" . lsp-ui-peek-jump-forward))
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-width 120
        lsp-ui-doc-max-height 30
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-delay 0.2
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 20
        lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'left))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

;; ============================================================================
;; LSP LANGUAGE SERVERS
;; ============================================================================
(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :init
  (setq lsp-pyright-multi-root nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths t
        lsp-pyright-diagnostic-mode "workspace"
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-venv-path "~/.pyenv/versions"))

(use-package lsp-java
  :after lsp-mode
  :init
  (setq lsp-java-vmargs '("-XX:+UseParallelGC"
                         "-XX:GCTimeRatio=4"
                         "-XX:AdaptiveSizePolicyWeight=90"
                         "-Dsun.zip.disableMemoryMapping=true"
                         "-Xmx4G" "-Xms100m")
        lsp-java-java-path (or (executable-find "java") "/usr/bin/java")
        lsp-java-configuration-runtimes '[(:name "JavaSE-17"
                                          :path "/usr/lib/jvm/java-17-openjdk"
                                          :default t)]
        lsp-java-import-gradle-enabled t
        lsp-java-maven-download-sources t
        lsp-java-autobuild-enabled t
        lsp-java-format-enabled t))

(use-package lsp-haskell
  :after lsp-mode)

(use-package ccls
  :if (executable-find "ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp-deferred)))
  :init
  (setq ccls-executable (executable-find "ccls")
        ccls-sem-highlight-method 'font-lock
        ccls-enable-skipped-ranges t
        ccls-initialization-options
        '(:cache (:directory ".ccls-cache")
          :compilationDatabaseDirectory "build"
          :index (:threads 8)
          :completion (:detailedLabel t))))

;; ============================================================================
;; DUMB-JUMP
;; ============================================================================
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g q" . dumb-jump-quick-look))
  :init
  (setq dumb-jump-selector 'completing-read
        dumb-jump-aggressive t
        dumb-jump-prefer-searcher 'rg)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; ============================================================================
;; PROJECTILE - PROFESSIONAL
;; ============================================================================
(use-package projectile
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
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
              ("C-c p D" . projectile-dired))
  :init
  (setq projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'hybrid
        projectile-sort-order 'recentf
        projectile-project-search-path '(("~/projects" . 2)
                                         ("~/work" . 2)
                                         ("~/code" . 2))
        projectile-globally-ignored-directories
        '(".git" ".svn" ".hg" "node_modules" "__pycache__"
          ".pytest_cache" ".mypy_cache" "target" "build" "dist"
          ".venv" "venv" "vendor" ".next" ".nuxt" "out"
          "coverage" ".cache" ".idea" ".vscode" "*.egg-info"
          ".tox" "htmlcov" ".elixir_ls" ".eunit" "_build")
        projectile-globally-ignored-files
        '("*.pyc" "*.o" "*.so" "*.dll" "*.exe" "*.class"
          "*.elc" "*.log" ".DS_Store" "Thumbs.db" "*.jar"
          "*.war" "*.beam" "TAGS")
        projectile-auto-discover t
        projectile-switch-project-action #'projectile-dired
        projectile-require-project-root nil
        projectile-track-known-projects-automatically t)
  :config
  (projectile-mode +1))

;; ============================================================================
;; MAGIT - PROFESSIONAL
;; ============================================================================
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g" . magit-file-dispatch)
         ("C-c M-g" . magit-dispatch)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g f" . magit-log-head)
         ("C-c g c" . magit-clone)
         ("C-c g i" . magit-init))
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-diff-refine-hunk 'all
        magit-save-repository-buffers 'dontask
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-auto-revert-mode t
        magit-refresh-status-buffer t
        magit-process-popup-time 10
        magit-no-confirm '(stage-all-changes unstage-all-changes)
        git-commit-summary-max-length 72
        git-commit-fill-column 72))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :init
  (setq git-gutter:update-interval 0.3
        git-gutter:modified-sign "│"
        git-gutter:added-sign "│"
        git-gutter:deleted-sign "│"))

(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode))

;; ============================================================================
;; FORMAT-ALL
;; ============================================================================
(use-package format-all
  :bind (("C-c f" . format-all-buffer)
         ("C-c F" . format-all-region-or-buffer))
  :hook (prog-mode . format-all-mode)
  :init
  (setq-default format-all-formatters
                '(("C" (clang-format))
                  ("C++" (clang-format))
                  ("Python" (black) (isort))
                  ("Go" (gofmt) (goimports))
                  ("Rust" (rustfmt))
                  ("JavaScript" (prettier))
                  ("TypeScript" (prettier))
                  ("JSON" (prettier))
                  ("HTML" (prettier))
                  ("CSS" (prettier))
                  ("YAML" (prettier))
                  ("Markdown" (prettier))
                  ("Ruby" (rubocop))
                  ("PHP" (prettier))
                  ("Java" (clang-format))
                  ("SQL" (sqlformat))
                  ("Lua" (lua-fmt))
                  ("Shell" (shfmt))
                  ("Haskell" (ormolu))
                  ("Elixir" (mix-format))
                  ("Nim" (nimpretty))
                  ("Zig" (zig-fmt))
                  ("Swift" (swift-format))
                  ("Kotlin" (ktlint))
                  ("Scala" (scalafmt)))))

;; ============================================================================
;; VTERM
;; ============================================================================
(use-package vterm
  :bind (("C-c t" . vterm)
         ("C-c T" . vterm-other-window))
  :init
  (setq vterm-max-scrollback 100000
        vterm-shell (or (getenv "SHELL") "/bin/bash")
        vterm-kill-buffer-on-exit t
        vterm-term-environment-variable "xterm-256color"
        vterm-timer-delay 0.01))

(use-package multi-vterm
  :after vterm
  :bind (("C-c M-t" . multi-vterm)
         ("C-c M-n" . multi-vterm-next)
         ("C-c M-p" . multi-vterm-prev)))

;; ============================================================================
;; DOCKER
;; ============================================================================
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :mode "docker-compose.*\\.ya?ml\\'")

(use-package docker
  :bind ("C-c d" . docker))

;; ============================================================================
;; KUBERNETES
;; ============================================================================
(use-package kubernetes
  :commands (kubernetes-overview)
  :bind ("C-c k" . kubernetes-overview))

;; ============================================================================
;; COMPILATION
;; ============================================================================
(setq compilation-scroll-output 'first-error
      compilation-window-height 20
      compilation-ask-about-save nil
      compilation-always-kill t
      compilation-skip-threshold 2)

;; ============================================================================
;; ELDOC
;; ============================================================================
(global-eldoc-mode 1)
(setq eldoc-idle-delay 0.1
      eldoc-echo-area-use-multiline-p nil
      eldoc-echo-area-prefer-doc-buffer t)

(use-package eldoc-box
  :bind (("C-c e" . eldoc-box-help-at-point)
         ("C-c E" . eldoc-box-eglot-help-at-point)))

;; ============================================================================
;; HELPFUL
;; ============================================================================
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         ("C-c C-d" . helpful-at-point)))

;; ============================================================================
;; TOOL STATUS
;; ============================================================================
(defun emacs-ide-lsp-status ()
  "Display LSP status."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (message "LSP: %s | Workspace: %s | Server: %s"
               (if (lsp-workspaces) "✓ Connected" "✗ Disconnected")
               (or (lsp-workspace-root) "None")
               (if (lsp-workspaces)
                   (lsp--workspace-print (car (lsp-workspaces)))
                 "None"))
    (message "LSP: Not active")))

(provide 'tools-config)
;;; tools-config.el ends here