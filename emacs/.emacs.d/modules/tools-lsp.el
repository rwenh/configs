;;; tools-lsp.el --- LSP Configuration -*- lexical-binding: t -*-
;;; Version: 3.1.0 | Fix: LSP vars moved to :config
;;; Code:

(when (bound-and-true-p emacs-ide-lsp-enable)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode python-mode rust-mode go-mode java-mode js-mode typescript-mode)
         . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-completion-provider :none
        lsp-idle-delay 0.3
        lsp-enable-snippet t
        lsp-semantic-tokens-enable t
        lsp-lens-enable t
        lsp-headerline-breadcrumb-enable t)
  :config
  ;; FIX v3.0.4: LSP-specific vars now in :config where lsp-rust/lsp-lua are loaded
  (with-eval-after-load 'lsp-rust
    (setq lsp-rust-analyzer-inlay-hints-mode t
          lsp-rust-analyzer-cargo-watch-command "clippy"))
  (with-eval-after-load 'lsp-lua
    (setq lsp-lua-hint-enable t
          lsp-lua-diagnostics-globals '()))
  (with-eval-after-load 'lsp-typescript
    (setq lsp-typescript-display-return-type-hints t
          lsp-typescript-display-parameter-type-hints t))
  :bind (:map lsp-mode-map
              ("C-c l r" . lsp-rename)
              ("C-c l f" . lsp-format-buffer)
              ("C-c l a" . lsp-execute-code-action)))

(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t
        lsp-ui-peek-enable t))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.5))

) ;; end lsp-enable

(defun emacs-ide-lsp-status ()
  (interactive)
  (message "LSP: %s" (if (bound-and-true-p lsp-mode) "✓ active" "✗ inactive")))

(provide 'tools-lsp)
;;; tools-lsp.el ends here
