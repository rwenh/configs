;;; tools-lsp.el --- LSP Configuration -*- lexical-binding: t -*-
;;; Version: 3.2.1 | PATCH: Added emacs-ide-lsp-check-servers (was missing, spot-check required it)
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
        lsp-lens-enable             t
        lsp-headerline-breadcrumb-enable t
        ;; Read from config var set by emacs-ide-config-apply — respects lsp.inlay-hints
        lsp-inlay-hints-enable      (bound-and-true-p emacs-ide-lsp-enable-inlay-hints))
  :config
  ;; Propagate the config-driven inlay-hints flag to per-language LSP packages
  (with-eval-after-load 'lsp-rust
    (let ((hints (bound-and-true-p emacs-ide-lsp-enable-inlay-hints)))
      (when (boundp 'lsp-rust-analyzer-inlay-hints-mode)
        (setq lsp-rust-analyzer-inlay-hints-mode hints)))
    (when (boundp 'lsp-rust-analyzer-cargo-watch-command)
      (setq lsp-rust-analyzer-cargo-watch-command "clippy")))
  (with-eval-after-load 'lsp-lua
    (let ((hints (bound-and-true-p emacs-ide-lsp-enable-inlay-hints)))
      (when (boundp 'lsp-lua-hint-enable)
        (setq lsp-lua-hint-enable hints)))
    (when (boundp 'lsp-lua-diagnostics-globals)
      (setq lsp-lua-diagnostics-globals '())))
  (with-eval-after-load 'lsp-typescript
    (let ((hints (bound-and-true-p emacs-ide-lsp-enable-inlay-hints)))
      (when (boundp 'lsp-typescript-display-return-type-hints)
        (setq lsp-typescript-display-return-type-hints hints))
      (when (boundp 'lsp-typescript-display-parameter-type-hints)
        (setq lsp-typescript-display-parameter-type-hints hints))))
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
  "Show LSP status for the current buffer."
  (interactive)
  (message "LSP: %s" (if (bound-and-true-p lsp-mode) "✓ active" "✗ inactive")))

(defun emacs-ide-lsp-check-servers ()
  "Check which LSP servers are installed and available on PATH."
  (interactive)
  (let ((servers '(("pyright"                    . "Python")
                   ("pylsp"                       . "Python (pylsp)")
                   ("rust-analyzer"               . "Rust")
                   ("gopls"                       . "Go")
                   ("typescript-language-server"  . "JavaScript / TypeScript")
                   ("clangd"                      . "C / C++")
                   ("jdtls"                       . "Java")
                   ("kotlin-language-server"      . "Kotlin")
                   ("lua-language-server"          . "Lua")
                   ("bash-language-server"        . "Shell / Bash")
                   ("yaml-language-server"        . "YAML")
                   ("sqls"                        . "SQL")
                   ("solargraph"                  . "Ruby")
                   ("elixir-ls"                   . "Elixir")
                   ("clojure-lsp"                 . "Clojure")
                   ("haskell-language-server"     . "Haskell")
                   ("zls"                         . "Zig")
                   ("nil"                         . "Nix")
                   ("metals"                      . "Scala")
                   ("r-languageserver"            . "R"))))
    (with-output-to-temp-buffer "*LSP Server Status*"
      (princ "=== LSP SERVER STATUS ===\n\n")
      (princ (format "LSP enabled in config: %s\n\n"
                     (if (bound-and-true-p emacs-ide-lsp-enable) "yes" "no")))
      (let ((found 0) (missing 0))
        (dolist (srv servers)
          (let ((available (executable-find (car srv))))
            (if available (cl-incf found) (cl-incf missing))
            (princ (format "  %s %-36s %s\n"
                           (if available "✓" "✗")
                           (cdr srv)
                           (if available (car srv) "(not on PATH)")))))
        (princ (format "\n%d installed, %d not found.\n" found missing))
        (when (= found 0)
          (princ "\nTip: Install language servers — see README for per-distro instructions.\n"))))))

(provide 'tools-lsp)
;;; tools-lsp.el ends here
