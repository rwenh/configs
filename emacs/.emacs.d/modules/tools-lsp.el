;;; tools-lsp.el --- LSP Configuration -*- lexical-binding: t -*-
;;; Version: 3.4.0
;;; Code:

(require 'cl-lib)

;;;; ── Compute the minimal lsp-client-packages list ───────────────────────────

(defun emacs-ide-lsp--enabled-client-packages ()
  "Return a list of lsp-* feature symbols for languages enabled in config.yml.
Falls back to the full client list if config is unavailable."
  (let ((all-map
         '(;; Tier 1 — always useful
           ("python"     . (lsp-pyright))
           ("javascript" . (lsp-javascript))
           ("typescript" . (lsp-javascript))
           ("rust"       . (lsp-rust))
           ("go"         . (lsp-go))
           ("c"          . (lsp-clangd))
           ("java"       . (lsp-java))
           ;; Tier 2
           ("kotlin"     . (lsp-kotlin))
           ("scala"      . (lsp-metals))
           ("lua"        . (lsp-lua))
           ("shell"      . (lsp-bash))
           ("sql"        . (lsp-sqls))
           ("groovy"     . (lsp-groovy))
           ("r"          . (lsp-r))
           ("julia"      . ())           ; uses lsp-julia, installed separately
           ;; Tier 3
           ("haskell"    . (lsp-haskell))
           ("clojure"    . (lsp-clojure))
           ("elixir"     . (lsp-elixir))
           ("ocaml"      . (lsp-ocaml))
           ("erlang"     . ())
           ("zig"        . (lsp-zig))
           ("nix"        . (lsp-nix))
           ("ruby"       . (lsp-ruby-syntax-tree))
           ("php"        . (lsp-intelephense))
           ("csharp"     . (lsp-csharp))
           ("dart"       . (lsp-dart))
           ;; Prose
           ("prose"      . (lsp-yaml lsp-json)))))
    (if (not (fboundp 'emacs-ide-dev-lang-enabled-p))
        ;; Config not loaded yet — return nil to use lsp-mode default
        nil
      (let (result)
        (dolist (entry all-map)
          (when (emacs-ide-dev-lang-enabled-p (car entry))
            (dolist (pkg (cdr entry))
              (cl-pushnew pkg result))))
        ;; Always include the base client loader
        (cl-pushnew 'lsp-completion result)
        (nreverse result)))))

(when (bound-and-true-p emacs-ide-lsp-enable)

;;; ─── lsp-mode ────────────────────────────────────────────────────────────────

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((;; C / C++
          c-mode c++-mode c-ts-mode c++-ts-mode
          ;; Python
          python-mode python-ts-mode
          ;; Rust
          rust-mode rust-ts-mode
          ;; Go
          go-mode go-ts-mode
          ;; Java
          java-mode java-ts-mode
          ;; JavaScript / TypeScript
          js-mode js-ts-mode
          typescript-mode typescript-ts-mode tsx-ts-mode)
         . lsp-deferred)

  :init
  (let ((scoped (emacs-ide-lsp--enabled-client-packages)))
    (when scoped
      (setq lsp-client-packages scoped)))

  (setq
   lsp-keymap-prefix                         "C-c l"
   lsp-completion-provider                   :none
   lsp-idle-delay                            emacs-ide-lsp-idle-delay
   lsp-enable-snippet                        t
   lsp-semantic-tokens-enable               emacs-ide-lsp-semantic-tokens
   lsp-lens-enable                           emacs-ide-lsp-lens
   lsp-headerline-breadcrumb-enable          emacs-ide-lsp-breadcrumb
   lsp-inlay-hints-enable                    emacs-ide-lsp-enable-inlay-hints
   ;; New vars from config
   lsp-signature-auto-activate               (when emacs-ide-lsp-signature-help
                                               '(:on-trigger-char
                                                 :on-server-request))
   lsp-ui-doc-enable                         emacs-ide-lsp-hover-docs
   lsp-enable-symbol-highlighting            emacs-ide-lsp-symbol-highlighting
   lsp-before-save-edits                     emacs-ide-lsp-organize-imports
   lsp-large-file-warning-threshold          emacs-ide-lsp-large-file-threshold
   ;; Performance: don't watch too many files
   lsp-file-watch-threshold                  1000
   ;; Avoid logging spam in *lsp-log*
   lsp-log-io                                nil
   ;; Don't show the "servers support this file" info message
   lsp-warn-no-matched-clients               nil)

  :config
  ;; ── Inlay hints per language ──────────────────────────────────────────────
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

  ;; ── Diagnostics provider ─────────────────────────────────────────────────
  ;; Apply diagnostics-provider from config (flycheck or flymake)
  (let ((provider (bound-and-true-p emacs-ide-lsp-diagnostics-provider)))
    (cond
     ((eq provider 'flycheck)
      (setq lsp-diagnostics-provider :flycheck))
     ((eq provider 'flymake)
      (setq lsp-diagnostics-provider :flymake))
     (t
      (setq lsp-diagnostics-provider :flycheck))))

  ;; ── Re-apply config on explicit reload ───────────────────────────────────
  (add-hook 'emacs-ide-config-reload-hook
            (lambda ()
              (setq lsp-inlay-hints-enable             emacs-ide-lsp-enable-inlay-hints
                    lsp-idle-delay                     emacs-ide-lsp-idle-delay
                    lsp-semantic-tokens-enable         emacs-ide-lsp-semantic-tokens
                    lsp-lens-enable                    emacs-ide-lsp-lens
                    lsp-headerline-breadcrumb-enable   emacs-ide-lsp-breadcrumb
                    lsp-enable-symbol-highlighting     emacs-ide-lsp-symbol-highlighting
                    lsp-before-save-edits              emacs-ide-lsp-organize-imports
                    lsp-large-file-warning-threshold   emacs-ide-lsp-large-file-threshold)
              ;; Re-scope client packages on reload
              (let ((scoped (emacs-ide-lsp--enabled-client-packages)))
                (when scoped
                  (setq lsp-client-packages scoped)))))

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
  (setq lsp-ui-doc-enable           emacs-ide-lsp-hover-docs
        lsp-ui-doc-position         'at-point
        lsp-ui-doc-show-with-mouse  nil
        lsp-ui-sideline-enable      emacs-ide-lsp-sideline
        lsp-ui-peek-enable          t
        lsp-ui-imenu-enable         t))

;;; ─── flycheck ────────────────────────────────────────────────────────────────

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically
        '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.5))

;;; ─── lsp-treemacs — deferred, on-demand only ─────────────────────────────────

(use-package lsp-treemacs
  :after lsp-mode
  :commands (lsp-treemacs-errors-list
             lsp-treemacs-symbols
             lsp-treemacs-references
             lsp-treemacs-implementations
             lsp-treemacs-call-hierarchy
             lsp-treemacs-type-hierarchy)
  :config
  ;; Only wire up when treemacs is already loaded — no eager pull
  (when (featurep 'treemacs)
    (when (fboundp 'lsp-treemacs-sync-mode)
      (lsp-treemacs-sync-mode 1))))

) ;; end (when emacs-ide-lsp-enable)

;;; ─── dumb-jump — xref fallback ───────────────────────────────────────────────

(use-package dumb-jump
  :demand t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher
        (cond
         ((executable-find "rg") 'rg)
         ((executable-find "ag") 'ag)
         (t                      'grep))
        dumb-jump-aggressive  nil
        dumb-jump-selector    'completing-read)

  (when (fboundp 'defhydra)
    (defhydra dumb-jump-hydra (:color blue :columns 3)
      "Dumb Jump"
      ("j" dumb-jump-go              "Go")
      ("o" dumb-jump-go-other-window "Other window")
      ("e" dumb-jump-go-prefer-external "Go external")
      ("x" dumb-jump-go-prefer-external-other-window "Go ext. other win")
      ("i" dumb-jump-go-prompt       "Prompt")
      ("l" dumb-jump-quick-look      "Quick look")
      ("b" dumb-jump-back            "Back"))
    (global-set-key (kbd "C-M-y") #'dumb-jump-hydra/body)))

;;; ─── Public commands (always available) ──────────────────────────────────────

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
               (format "✗ inactive — dumb-jump fallback %s"
                       (if (fboundp 'dumb-jump-xref-activate)
                           "active" "unavailable"))))))

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
                   ("lua-language-server"        . "Lua")
                   ("bash-language-server"       . "Shell / Bash")
                   ("yaml-language-server"       . "YAML")
                   ("sqls"                       . "SQL")
                   ("solargraph"                 . "Ruby")
                   ("ruby-lsp"                   . "Ruby (ruby-lsp)")
                   ("elixir-ls"                  . "Elixir")
                   ("clojure-lsp"                . "Clojure")
                   ("haskell-language-server"    . "Haskell")
                   ("zls"                        . "Zig")
                   ("nil"                        . "Nix")
                   ("metals"                     . "Scala")
                   ("r-languageserver"           . "R")
                   ("omnisharp"                  . "C#")
                   ("intelephense"               . "PHP")
                   ("dart"                       . "Dart"))))
    (with-output-to-temp-buffer "*LSP Server Status*"
      (princ "=== LSP SERVER STATUS ===\n\n")
      (princ (format "LSP enabled:    %s\n"
                     (if (bound-and-true-p emacs-ide-lsp-enable) "yes" "no")))
      (princ (format "Inlay hints:    %s\n"
                     (if (bound-and-true-p emacs-ide-lsp-enable-inlay-hints)
                         "yes" "no")))
      (princ (format "Idle delay:     %.2fs\n"
                     (if (boundp 'emacs-ide-lsp-idle-delay)
                         emacs-ide-lsp-idle-delay 0.3)))
      (princ (format "Client scope:   %s\n"
                     (if (boundp 'lsp-client-packages)
                         (format "%d packages" (length lsp-client-packages))
                       "default (all)")))
      (princ (format "Dumb-jump:      %s  (fallback when LSP absent)\n\n"
                     (if (fboundp 'dumb-jump-xref-activate) "✓ loaded" "✗ not loaded")))
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
          (princ "\nTip: Install language servers — see README §Tool Installation.\n"))
        (princ "\nNote: dumb-jump covers any language not listed above.\n")))))

(provide 'tools-lsp)
;;; tools-lsp.el ends here
