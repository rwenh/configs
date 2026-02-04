;;; tools-lsp.el --- LSP Mode Configuration (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; Language Server Protocol configuration with performance optimizations
;;; Code:

;; Check if LSP is enabled in config
(unless (bound-and-true-p emacs-ide-lsp-enable)
  (message "⚠️  LSP disabled in config")
  (provide 'tools-lsp)
  (throw 'skip-module nil))

;; ============================================================================
;; LSP OPTIMIZATION FOR LARGE FILES
;; ============================================================================
(defun emacs-ide-lsp-optimize-large-files ()
  "Optimize LSP for large files before activation."
  (let ((threshold (or (bound-and-true-p emacs-ide-lsp-large-file-threshold) 100000)))
    (when (> (buffer-size) threshold)
      (setq-local lsp-enable-symbol-highlighting nil
                  lsp-enable-on-type-formatting nil
                  lsp-enable-folding nil
                  lsp-lens-enable nil
                  lsp-semantic-tokens-enable nil
                  lsp-enable-indentation nil)
      (message "LSP optimizations enabled for large file"))))

(defun emacs-ide-lsp-deferred-optimized ()
  "Defer LSP with optimizations."
  (emacs-ide-lsp-optimize-large-files)
  (when (fboundp 'lsp-deferred)
    (lsp-deferred)))

;; ============================================================================
;; FLYCHECK - SYNTAX CHECKING
;; ============================================================================
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled new-line)
        flycheck-indication-mode 'left-margin
        flycheck-display-errors-delay 0.1
        flycheck-idle-change-delay 0.5
        flycheck-highlighting-mode 'lines
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-disabled-checkers '(emacs-lisp-checkdoc)
        flycheck-temp-prefix ".flycheck"
        flycheck-global-modes '(not org-mode))
  :bind (:map flycheck-mode-map
              ("C-c ! n" . flycheck-next-error)
              ("C-c ! p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)
              ("C-c ! v" . flycheck-verify-setup)))

(use-package flycheck-pos-tip
  :after flycheck
  :if (display-graphic-p)
  :config
  (when (fboundp 'flycheck-pos-tip-mode)
    (flycheck-pos-tip-mode 1)))

;; ============================================================================
;; LSP-MODE - CORE CONFIGURATION (DEFERRED)
;; ============================================================================
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (;; Programming modes with LSP
         (c-mode . emacs-ide-lsp-deferred-optimized)
         (c++-mode . emacs-ide-lsp-deferred-optimized)
         (python-mode . emacs-ide-lsp-deferred-optimized)
         (rust-mode . emacs-ide-lsp-deferred-optimized)
         (go-mode . emacs-ide-lsp-deferred-optimized)
         (java-mode . emacs-ide-lsp-deferred-optimized)
         (js-mode . emacs-ide-lsp-deferred-optimized)
         (typescript-mode . emacs-ide-lsp-deferred-optimized)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l"
        
        ;; Performance - use config values if available
        lsp-completion-provider :none
        lsp-idle-delay (or (bound-and-true-p emacs-ide-completion-delay) 0.3)
        lsp-log-io nil
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 2000
        lsp-keep-workspace-alive nil
        
        ;; Features - use config values if available
        lsp-enable-folding t
        lsp-enable-links t
        lsp-enable-snippet t
        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable t
        lsp-modeline-workspace-status-enable t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-eldoc-enable-hover t
        lsp-eldoc-render-all nil
        lsp-headerline-breadcrumb-enable t
        lsp-semantic-tokens-enable (or (bound-and-true-p emacs-ide-lsp-semantic-tokens) t)
        lsp-enable-symbol-highlighting t
        lsp-lens-enable t
        lsp-enable-on-type-formatting t
        lsp-enable-indentation t
        lsp-before-save-edits t
        lsp-auto-guess-root t
        lsp-restart 'auto-restart
        lsp-enable-suggest-server-download t
        lsp-inlay-hint-enable (or (bound-and-true-p emacs-ide-lsp-enable-inlay-hints) t)
        lsp-use-plists t
        lsp-warn-no-matched-clients nil
        lsp-diagnostics-provider :flycheck
        lsp-auto-configure t
        
        ;; Ignored directories
        lsp-watch-file-ignore-regexps
        '("[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$"
          "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$"
          "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$"
          "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$"
          "[/\\\\]dist$" "[/\\\\]dist-newstyle$" "[/\\\\]\\.stack-work$"
          "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$"
          "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.vscode$" "[/\\\\]\\.deps$"
          "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$"
          "[/\\\\]\\.reference$" "[/\\\\]\\.pytest_cache$"
          "[/\\\\]\\.mypy_cache$" "[/\\\\]__pycache__$"
          "[/\\\\]\\.venv$" "[/\\\\]venv$"))
  :bind (:map lsp-mode-map
              ("C-c l r" . lsp-rename)
              ("C-c l f" . lsp-format-buffer)
              ("C-c l F" . lsp-format-region)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l d" . lsp-describe-thing-at-point)
              ("C-c l R" . lsp-find-references)
              ("C-c l i" . lsp-find-implementation)
              ("C-c l t" . lsp-find-type-definition)
              ("C-c l o" . lsp-organize-imports)
              ("C-c l h" . lsp-document-highlight))
  :config
  (defun emacs-ide-lsp-status ()
    "Display LSP status."
    (interactive)
    (if (bound-and-true-p lsp-mode)
        (message "LSP: %s | Workspace: %s"
                 (if (lsp-workspaces) "✓ Connected" "✗ Disconnected")
                 (or (lsp-workspace-root) "None"))
      (message "LSP: Not active")))
  
  (defun emacs-ide-lsp-restart-all ()
    "Restart all LSP workspaces."
    (interactive)
    (when (bound-and-true-p lsp-mode)
      (lsp-restart-workspace)
      (message "✓ LSP workspace restarted"))))

;; ============================================================================
;; LSP-UI - ENHANCED UI (DEFERRED)
;; ============================================================================
(use-package lsp-ui
  :after lsp-mode
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
        lsp-ui-imenu-kind-position 'left)
  :bind (:map lsp-ui-mode-map
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-?" . lsp-ui-peek-find-references)))

;; ============================================================================
;; LSP-TREEMACS - TREE VIEW (DEFERRED)
;; ============================================================================
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (when (fboundp 'lsp-treemacs-sync-mode)
    (lsp-treemacs-sync-mode 1)))

;; ============================================================================
;; LANGUAGE-SPECIFIC LSP SERVERS
;; ============================================================================

;; Python - Pyright (if available)
(use-package lsp-pyright
  :after lsp-mode
  :if (executable-find "pyright")
  :hook (python-mode . (lambda ()
                        (require 'lsp-pyright nil t)
                        (emacs-ide-lsp-deferred-optimized)))
  :init
  (setq lsp-pyright-multi-root nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths t))

;; Rust - rust-analyzer (if available)
(with-eval-after-load 'rust-mode
  (when (executable-find "rust-analyzer")
    (require 'lsp-rust nil t)))

;; Go - gopls (if available)
(with-eval-after-load 'go-mode
  (when (executable-find "gopls")
    (require 'lsp-go nil t)))

;; ============================================================================
;; DUMB-JUMP - FALLBACK NAVIGATION
;; ============================================================================
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back))
  :init
  (setq dumb-jump-selector 'completing-read
        dumb-jump-aggressive t
        dumb-jump-prefer-searcher 'rg)
  :config
  (when (fboundp 'dumb-jump-xref-activate)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))

;; ============================================================================
;; ELDOC - DOCUMENTATION (BUILTIN)
;; ============================================================================
(use-package eldoc
  :straight nil
  :init
  (setq eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-prefer-doc-buffer t)
  :config
  (global-eldoc-mode 1))

;; ============================================================================
;; HELPFUL - BETTER HELP
;; ============================================================================
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;; ============================================================================
;; LSP SERVER AVAILABILITY CHECK
;; ============================================================================
(defun emacs-ide-lsp-check-servers ()
  "Check LSP server availability."
  (interactive)
  (let ((servers '(("pyright" . "python")
                  ("rust-analyzer" . "rust")
                  ("gopls" . "go")
                  ("typescript-language-server" . "typescript")
                  ("clangd" . "c/c++")))
        (available '())
        (missing '()))
    (dolist (server servers)
      (if (executable-find (car server))
          (push server available)
        (push server missing)))
    (with-output-to-temp-buffer "*LSP Servers*"
      (princ "=== LSP SERVERS STATUS ===\n\n")
      (princ (format "Available: %d\n" (length available)))
      (dolist (srv available)
        (princ (format "  ✓ %s (%s)\n" (car srv) (cdr srv))))
      (when missing
        (princ (format "\nMissing: %d\n" (length missing)))
        (dolist (srv missing)
          (princ (format "  ✗ %s (%s)\n" (car srv) (cdr srv))))))))

(provide 'tools-lsp)
;;; tools-lsp.el ends here