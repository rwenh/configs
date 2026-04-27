;;; tools-lsp.el --- LSP Configuration -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(when (bound-and-true-p emacs-ide-lsp-enable)

;;; ─── lsp-mode ────────────────────────────────────────────────────────────────

(use-package lsp-mode
  :commands (lsp lsp-deferred)

  ;; Hook list covers BOTH classic and tree-sitter mode variants.
  ;; Lang modules add their own hooks too — this list acts as a safety net
  ;; for the race condition where a ts-mode activates before the lang
  ;; module's 0.5-second pre-warm idle timer fires (bug #LSP-ts-race).
  :hook ((;; C / C++
          c-mode c++-mode c-ts-mode c++-ts-mode
          ;; Python
          python-mode python-ts-mode
          ;; Rust
          rust-mode rust-ts-mode
          ;; Go
          go-mode go-ts-mode
          ;; Java  — FIX #24: java-mode was in this list but java-ts-mode
          ;; was missing; lsp-java only hooked java-ts-mode, leaving
          ;; classic java-mode users with no LSP.
          java-mode java-ts-mode
          ;; JavaScript / TypeScript
          js-mode js-ts-mode
          typescript-mode typescript-ts-mode tsx-ts-mode)
         . lsp-deferred)

  :init
  (setq
   lsp-keymap-prefix        "C-c l"
   lsp-completion-provider  :none
   lsp-idle-delay           emacs-ide-lsp-idle-delay
   lsp-enable-snippet       t
   lsp-semantic-tokens-enable        emacs-ide-lsp-semantic-tokens
   lsp-lens-enable                   emacs-ide-lsp-lens
   lsp-headerline-breadcrumb-enable  emacs-ide-lsp-breadcrumb
   lsp-inlay-hints-enable            emacs-ide-lsp-enable-inlay-hints)

  :config
  ;; ── Propagate inlay-hints flag to per-language packages ──────────────────

  (with-eval-after-load 'lsp-rust
    (when (boundp 'lsp-rust-analyzer-inlay-hints-mode)
      (setq lsp-rust-analyzer-inlay-hints-mode
            emacs-ide-lsp-enable-inlay-hints))
    (when (boundp 'lsp-rust-analyzer-cargo-watch-command)
      (setq lsp-rust-analyzer-cargo-watch-command "clippy"))
    (when (boundp 'lsp-rust-analyzer-checkOnSave-command)
      (setq lsp-rust-analyzer-checkOnSave-command "clippy")))

  (with-eval-after-load 'lsp-lua
    (when (boundp 'lsp-lua-hint-enable)
      (setq lsp-lua-hint-enable emacs-ide-lsp-enable-inlay-hints))
    (when (boundp 'lsp-lua-diagnostics-globals)
      (setq lsp-lua-diagnostics-globals '())))

  (with-eval-after-load 'lsp-typescript
    (when (boundp 'lsp-typescript-display-return-type-hints)
      (setq lsp-typescript-display-return-type-hints
            emacs-ide-lsp-enable-inlay-hints))
    (when (boundp 'lsp-typescript-display-parameter-type-hints)
      (setq lsp-typescript-display-parameter-type-hints
            emacs-ide-lsp-enable-inlay-hints))
    (when (boundp 'lsp-typescript-display-variable-type-hints)
      (setq lsp-typescript-display-variable-type-hints
            emacs-ide-lsp-enable-inlay-hints)))

  ;; ── Re-apply config on explicit reload ───────────────────────────────────
  ;; If the user changes lsp.inlay-hints in config.yml and reloads, update
  ;; the global lsp-mode variable so new buffers pick up the new value.
  (add-hook 'emacs-ide-config-reload-hook
            (lambda ()
              (setq lsp-inlay-hints-enable      emacs-ide-lsp-enable-inlay-hints
                    lsp-idle-delay              emacs-ide-lsp-idle-delay
                    lsp-semantic-tokens-enable  emacs-ide-lsp-semantic-tokens
                    lsp-lens-enable             emacs-ide-lsp-lens
                    lsp-headerline-breadcrumb-enable emacs-ide-lsp-breadcrumb)))

  :bind (:map lsp-mode-map
              ("C-c l r" . lsp-rename)
              ("C-c l f" . lsp-format-buffer)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l d" . lsp-find-definition)
              ("C-c l R" . lsp-find-references)
              ("C-c l i" . lsp-find-implementation)
              ("C-c l h" . lsp-describe-thing-at-point)))

;;; ─── lsp-ui ──────────────────────────────────────────────────────────────────

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable       t
        lsp-ui-doc-position     'at-point
        lsp-ui-sideline-enable  emacs-ide-lsp-sideline
        lsp-ui-peek-enable      t
        lsp-ui-imenu-enable     t))

;;; ─── flycheck ────────────────────────────────────────────────────────────────

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically
        '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.5))

) ;; end (when emacs-ide-lsp-enable)

;;; ─── Public commands (always available) ──────────────────────────────────────
;; These are defined unconditionally so that keybindings and spot-check always
;; see them, even when lsp.enable is false in config.yml.

(defun emacs-ide-lsp-status ()
  "Show LSP connection status for the current buffer."
  (interactive)
  (if (not (bound-and-true-p emacs-ide-lsp-enable))
      (message "LSP: disabled in config.yml (lsp.enable: false)")
    (message "LSP: %s"
             (if (bound-and-true-p lsp-mode)
                 (format "✓ active  server: %s"
                         (or (and (fboundp 'lsp--workspace-print)
                                  (mapconcat #'lsp--workspace-print
                                             (lsp-workspaces) ", "))
                             "connected"))
               "✗ inactive (not started in this buffer)"))))

(defun emacs-ide-lsp-check-servers ()
  "Display which LSP servers are installed and available on PATH."
  (interactive)
  (let ((servers '(("pyright"                   . "Python")
                   ("pylsp"                      . "Python (pylsp)")
                   ("rust-analyzer"              . "Rust")
                   ("gopls"                      . "Go")
                   ("typescript-language-server" . "JavaScript / TypeScript")
                   ("clangd"                     . "C / C++")
                   ("jdtls"                      . "Java")
                   ("kotlin-language-server"     . "Kotlin")
                   ("lua-language-server"         . "Lua")
                   ("bash-language-server"       . "Shell / Bash")
                   ("yaml-language-server"       . "YAML")
                   ("sqls"                       . "SQL")
                   ("solargraph"                 . "Ruby")
                   ("elixir-ls"                  . "Elixir")
                   ("clojure-lsp"                . "Clojure")
                   ("haskell-language-server"    . "Haskell")
                   ("zls"                        . "Zig")
                   ("nil"                        . "Nix")
                   ("metals"                     . "Scala")
                   ("r-languageserver"           . "R"))))
    (with-output-to-temp-buffer "*LSP Server Status*"
      (princ "=== LSP SERVER STATUS ===\n\n")
      (princ (format "LSP enabled:  %s\n"
                     (if (bound-and-true-p emacs-ide-lsp-enable) "yes" "no")))
      (princ (format "Inlay hints:  %s\n"
                     (if (bound-and-true-p emacs-ide-lsp-enable-inlay-hints)
                         "yes" "no")))
      (princ (format "Idle delay:   %.2fs\n\n"
                     (if (boundp 'emacs-ide-lsp-idle-delay)
                         emacs-ide-lsp-idle-delay 0.3)))
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
          (princ "\nTip: Install language servers — see README §Tool Installation.\n"))))))

(provide 'tools-lsp)
;;; tools-lsp.el ends here
