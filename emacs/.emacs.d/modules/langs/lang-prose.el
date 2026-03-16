;;; lang-prose.el --- Prose & Config IDE layer -*- lexical-binding: t -*-
;;; Markdown · Org · YAML · TOML · JSON · Dockerfile · Terraform · Ansible · GraphQL · Proto
;;; Version: 1.0.0
;;; Code:
(require 'core-dev)
(emacs-ide-dev-register "prose" :tier 4 :lsp-server nil
  :formatter "prettier" :test-cmd nil :repl nil
  :modes '(markdown-mode yaml-mode toml-mode json-mode))

(when (emacs-ide-dev-lang-enabled-p "prose")
(emacs-ide-dev-ensure-treesit 'markdown)
(emacs-ide-dev-ensure-treesit 'yaml)
(emacs-ide-dev-ensure-treesit 'json)
(emacs-ide-dev-ensure-treesit 'toml)

;; ── Markdown ──────────────────────────────────────────────────────────────
(use-package markdown-mode
  :defer t :mode (("\\.md\\'" . gfm-mode) ("\\.markdown\\'" . gfm-mode))
  :init (setq markdown-command (or (executable-find "pandoc") "markdown")
              markdown-fontify-code-blocks-natively t
              markdown-enable-math t
              markdown-enable-wiki-links t))

;; ── YAML ──────────────────────────────────────────────────────────────────
(use-package yaml-mode :defer t :mode "\\.ya?ml\\'")
(use-package lsp-mode :if (executable-find "yaml-language-server")
  :hook (yaml-mode . lsp-deferred))
(with-eval-after-load 'apheleia (emacs-ide-dev-attach-formatter 'prettier 'yaml-mode))

;; ── TOML ──────────────────────────────────────────────────────────────────
(use-package toml-mode :defer t :mode "\\.toml\\'")
(with-eval-after-load 'apheleia (emacs-ide-dev-attach-formatter 'prettier 'toml-mode))

;; ── JSON ──────────────────────────────────────────────────────────────────
(use-package json-mode :defer t :mode "\\.json\\'")
(use-package lsp-mode :if (executable-find "vscode-json-language-server")
  :hook (json-mode . lsp-deferred))
(with-eval-after-load 'apheleia (emacs-ide-dev-attach-formatter 'prettier 'json-mode))

;; ── CSV ───────────────────────────────────────────────────────────────────
(use-package csv-mode :defer t :mode "\\.csv\\'")

;; ── Dockerfile ────────────────────────────────────────────────────────────
(use-package dockerfile-mode :defer t :mode "\\`Dockerfile")
(use-package lsp-mode :if (executable-find "docker-langserver")
  :hook (dockerfile-mode . lsp-deferred))

;; ── Terraform ─────────────────────────────────────────────────────────────
(use-package terraform-mode :if (executable-find "terraform")
  :defer t :mode "\\.tf\\'"
  :hook (terraform-mode . lsp-deferred))
(use-package lsp-mode :if (executable-find "terraform-ls")
  :hook (terraform-mode . lsp-deferred))

;; ── Ansible ───────────────────────────────────────────────────────────────
(use-package ansible :defer t)
(use-package ansible-doc :after ansible)

;; ── GraphQL ───────────────────────────────────────────────────────────────
(use-package graphql-mode :defer t :mode "\\.graphql\\'")
(use-package lsp-mode :if (executable-find "graphql-language-service-cli")
  :hook (graphql-mode . lsp-deferred))

;; ── Protobuf ──────────────────────────────────────────────────────────────
(use-package protobuf-mode :defer t :mode "\\.proto\\'")

;; ── ENV files ─────────────────────────────────────────────────────────────
(use-package dotenv-mode :defer t :mode "\\.env\\'")

;; ── REST (HTTP files) ─────────────────────────────────────────────────────
(use-package restclient :defer t :mode "\\.http\\'")

) (provide 'lang-prose)
;;; lang-prose.el ends here
