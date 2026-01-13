;;; tools-config.el --- Enhanced Development Tools Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; LSP, Eglot, Flycheck, Projectile, Magit, VTerm, and advanced dev tools
;;; Save as: ~/.emacs.d/modules/tools-config.el
;;;
;;; Code:

;; ============================================================================
;; FLYCHECK (Syntax Checking)
;; ============================================================================
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-c ! n" . flycheck-next-error)
              ("C-c ! p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)
              ("C-c ! v" . flycheck-verify-setup)
              ("C-c ! c" . flycheck-clear))
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-indication-mode 'left-margin)
  (flycheck-display-errors-delay 0.25)
  (flycheck-idle-change-delay 1.0)
  (flycheck-highlighting-mode 'lines)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; ============================================================================
;; LSP-MODE (Language Server Protocol) - ENHANCED
;; ============================================================================
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (objc-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (php-mode . lsp-deferred)
         (ruby-mode . lsp-deferred)
         (haskell-mode . lsp-deferred)
         (elixir-mode . lsp-deferred)
         (nim-mode . lsp-deferred)
         (zig-mode . lsp-deferred)
         (kotlin-mode . lsp-deferred)
         (scala-mode . lsp-deferred)
         (swift-mode . lsp-deferred)
         (csharp-mode . lsp-deferred)
         (lua-mode . lsp-deferred)
         (cmake-mode . lsp-deferred)
         (dockerfile-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("C-c l r" . lsp-rename)
              ("C-c l f" . lsp-format-buffer)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l d" . lsp-describe-thing-at-point)
              ("C-c l g" . lsp-find-definition)
              ("C-c l R" . lsp-find-references)
              ("C-c l i" . lsp-find-implementation)
              ("C-c l t" . lsp-find-type-definition)
              ("C-c l o" . lsp-organize-imports)
              ("C-c l s" . lsp-signature-activate)
              ("C-c l h" . lsp-document-highlight)
              ("C-c l =" . lsp-format-region)
              ("C-c l I" . lsp-ui-imenu))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 5000)
  (lsp-enable-folding t)
  (lsp-enable-links t)
  (lsp-enable-snippet t)
  (lsp-keep-workspace-alive nil)
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-workspace-status-enable t)
  (lsp-signature-auto-activate t)
  (lsp-signature-render-documentation t)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-semantic-tokens-enable t)
  (lsp-enable-symbol-highlighting t)
  (lsp-lens-enable t)
  (lsp-enable-on-type-formatting t)
  (lsp-enable-indentation t)
  (lsp-before-save-edits t)
  (lsp-auto-guess-root t)
  (lsp-restart 'auto-restart)
  (lsp-enable-suggest-server-download t)
  (lsp-inlay-hint-enable t)
  :config
  (setq lsp-warn-no-matched-clients nil)
  
  ;; Performance tuning
  (setq lsp-use-plists t)
  
  ;; Language-specific optimizations
  (with-eval-after-load 'lsp-mode
    (defun emacs-ide-lsp-optimize-large-files ()
      "Disable heavy LSP features in large files."
      (when (> (buffer-size) 100000)
        (setq-local lsp-enable-symbol-highlighting nil
                    lsp-enable-on-type-formatting nil
                    lsp-enable-folding nil
                    lsp-lens-enable nil)))
    (add-hook 'lsp-mode-hook #'emacs-ide-lsp-optimize-large-files)))

;; LSP UI - Enhanced interface
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :bind (:map lsp-ui-mode-map
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-?" . lsp-ui-peek-find-references)
              ("C-c l ." . lsp-ui-peek-find-workspace-symbol))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-imenu-enable t))

;; ============================================================================
;; EGLOT (Alternative LSP Client - Lighter)
;; ============================================================================
(use-package eglot
  :ensure t
  :commands eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 1)
  :config
  ;; Uncomment languages you want to use with Eglot instead of lsp-mode
  ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  )

;; ============================================================================
;; LANGUAGE-SPECIFIC LSP CONFIGURATIONS
;; ============================================================================

;; Python - Pyright
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; Java
(use-package lsp-java
  :ensure t
  :after lsp-mode
  :custom
  (lsp-java-java-path (or (executable-find "java") "/usr/bin/java"))
  (lsp-java-vmargs '("-XX:+UseParallelGC"
                     "-XX:GCTimeRatio=4"
                     "-XX:AdaptiveSizePolicyWeight=90"
                     "-Dsun.zip.disableMemoryMapping=true"
                     "-Xmx2G" "-Xms100m")))

;; Haskell
(use-package lsp-haskell
  :ensure t
  :after lsp-mode)

;; C/C++ - CCLS (alternative to clangd)
(use-package ccls
  :ensure t
  :if (executable-find "ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp-deferred)))
  :custom
  (ccls-executable (executable-find "ccls"))
  (ccls-initialization-options
   '(:cache (:directory ".ccls-cache")
     :compilationDatabaseDirectory "build")))

;; ============================================================================
;; DUMB-JUMP (Fallback when LSP unavailable)
;; ============================================================================
(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g q" . dumb-jump-quick-look))
  :custom
  (dumb-jump-selector 'completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; ============================================================================
;; PROJECTILE (Project Management) - ENHANCED
;; ============================================================================
(use-package projectile
  :ensure t
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-mode-map
              ("C-c p s s" . projectile-ag)
              ("C-c p s r" . projectile-ripgrep))
  :custom
  (projectile-completion-system 'ido)
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)
  (projectile-sort-order 'recentf)
  (projectile-project-search-path '("~/projects" "~/work" "~/code"))
  (projectile-globally-ignored-directories
   '(".git" ".svn" ".hg" "node_modules" "__pycache__" ".pytest_cache"
     ".mypy_cache" "target" "build" "dist" ".venv" "venv" "vendor"
     ".next" ".nuxt" "out" "coverage" ".cache" ".idea" ".vscode"
     "*.egg-info" ".tox" "htmlcov"))
  (projectile-globally-ignored-files
   '("*.pyc" "*.o" "*.so" "*.dll" "*.exe" "*.class" "*.elc"
     "*.log" ".DS_Store" "Thumbs.db" "*.jar" "*.war"))
  :config
  (projectile-mode +1)
  
  ;; Auto-discover projects
  (dolist (dir '("~/projects" "~/work" "~/code"))
    (when (file-directory-p dir)
      (projectile-discover-projects-in-directory dir))))

;; ============================================================================
;; MAGIT (Git Interface) - ENHANCED
;; ============================================================================
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g" . magit-file-dispatch)
         ("C-c M-g" . magit-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-diff-refine-hunk 'all)
  (magit-save-repository-buffers 'dontask)
  (magit-revision-show-gravatars nil)
  (magit-auto-revert-mode t))

;; Git gutter
(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 0.5))

;; Git timemachine
(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine))

;; Diff highlight
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode))

;; ============================================================================
;; FORMAT-ALL (Code Formatting) - ENHANCED
;; ============================================================================
(use-package format-all
  :ensure t
  :bind (("C-c f" . format-all-buffer)
         ("C-c F" . format-all-region-or-buffer))
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C" clang-format)
                  ("C++" clang-format)
                  ("Objective-C" clang-format)
                  ("Java" clang-format)
                  ("Python" (black))
                  ("Go" (gofmt))
                  ("Ruby" (rubocop))
                  ("JavaScript" (prettier))
                  ("TypeScript" (prettier))
                  ("HTML" (prettier))
                  ("CSS" (prettier))
                  ("SCSS" (prettier))
                  ("JSON" (prettier))
                  ("YAML" (prettier))
                  ("Markdown" (prettier))
                  ("PHP" (prettier))
                  ("Rust" (rustfmt))
                  ("SQL" (sqlformat))
                  ("Lua" (lua-fmt))
                  ("Shell" (shfmt))
                  ("Haskell" (ormolu))
                  ("OCaml" (ocamlformat))
                  ("Elixir" (mix-format))
                  ("Nim" (nimpretty))
                  ("Zig" (zig-fmt))
                  ("Kotlin" (ktlint))
                  ("Scala" (scalafmt))
                  ("Swift" (swift-format))
                  ("CMake" (cmake-format))
                  ("Dockerfile" (dockfmt))
                  ("Terraform" (terraform-fmt)))))

;; ============================================================================
;; VTERM (Terminal Emulator) - ENHANCED
;; ============================================================================
(use-package vterm
  :ensure t
  :bind (("C-c t" . vterm)
         ("C-c M-t" . vterm-other-window))
  :custom
  (vterm-max-scrollback 10000)
  (vterm-shell (or (getenv "SHELL") "/bin/bash"))
  (vterm-kill-buffer-on-exit t)
  (vterm-term-environment-variable "xterm-256color")
  :config
  (when (getenv "WAYLAND_DISPLAY")
    (setq vterm-timer-delay 0.01)))

;; Multi-term as alternative
(use-package multi-term
  :ensure t
  :bind (("C-c T" . multi-term))
  :custom
  (multi-term-program (or (getenv "SHELL") "/bin/bash")))

;; ============================================================================
;; DOCKER SUPPORT
;; ============================================================================
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :ensure t
  :mode "docker-compose.*\\.ya?ml\\'")

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; ============================================================================
;; KUBERNETES SUPPORT
;; ============================================================================
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :bind ("C-c k" . kubernetes-overview))

;; ============================================================================
;; RESTCLIENT (API Testing)
;; ============================================================================
(use-package restclient
  :ensure t
  :mode ("\\.rest\\'" "\\.http\\'"))

;; ============================================================================
;; COMPILATION SETTINGS - ENHANCED
;; ============================================================================
(setq compilation-scroll-output t
      compilation-window-height 15
      compilation-ask-about-save nil
      compilation-always-kill t)

(defun emacs-ide-auto-close-compilation-buffer ()
  "Auto-close compilation buffer if successful."
  (when (and (eq major-mode 'compilation-mode)
             (eq (buffer-local-value 'compilation-exit-message-function
                                    (current-buffer))
                 nil))
    (run-with-timer 2 nil
                   (lambda (buf)
                     (when (buffer-live-p buf)
                       (delete-windows-on buf)))
                   (current-buffer))))

;; ============================================================================
;; ELDOC (Inline Documentation) - ENHANCED
;; ============================================================================
(global-eldoc-mode 1)
(setq eldoc-idle-delay 0.2
      eldoc-echo-area-use-multiline-p nil)

(use-package eldoc-box
  :ensure t
  :bind (("C-c e" . eldoc-box-help-at-point)
         ("C-c E" . eldoc-box-eglot-help-at-point)))

;; ============================================================================
;; HELPFUL (Better Help System)
;; ============================================================================
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;; ============================================================================
;; TOOL AVAILABILITY CHECKS - ENHANCED
;; ============================================================================
(defun emacs-ide-check-tools ()
  "Check availability of external tools and report missing ones."
  (interactive)
  (let ((tools '(("git" . "Version control")
                 ("gcc" . "C compiler")
                 ("g++" . "C++ compiler")
                 ("clangd" . "C/C++ LSP server")
                 ("python3" . "Python")
                 ("pyright" . "Python LSP server")
                 ("node" . "JavaScript runtime")
                 ("typescript-language-server" . "TypeScript LSP")
                 ("rustc" . "Rust compiler")
                 ("rust-analyzer" . "Rust LSP server")
                 ("go" . "Go compiler")
                 ("gopls" . "Go LSP server")
                 ("java" . "Java runtime")
                 ("jdtls" . "Java LSP server")
                 ("docker" . "Docker")
                 ("kubectl" . "Kubernetes CLI")))
        (missing '())
        (found '()))
    (dolist (tool tools)
      (if (executable-find (car tool))
          (push tool found)
        (push tool missing)))
    (with-output-to-temp-buffer "*Tool Status*"
      (princ "=== DEVELOPMENT TOOLS STATUS ===\n\n")
      (princ "AVAILABLE TOOLS:\n")
      (dolist (tool found)
        (princ (format "  ✓ %s - %s\n" (car tool) (cdr tool))))
      (when missing
        (princ "\nMISSING TOOLS:\n")
        (dolist (tool missing)
          (princ (format "  ✗ %s - %s\n" (car tool) (cdr tool))))))))

;; LSP Status helper
(defun emacs-ide-lsp-status ()
  "Display LSP connection status."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (message "LSP: %s | Workspace: %s | Server: %s"
               (if (lsp-workspaces) "Connected" "Disconnected")
               (or (lsp-workspace-root) "None")
               (if (lsp-workspaces)
                   (lsp--workspace-print (car (lsp-workspaces)))
                 "None"))
    (message "LSP: Not active in this buffer")))

(provide 'tools-config)
;;; tools-config.el ends here