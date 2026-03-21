;;; lang-prose.el --- Prose & Config IDE layer -*- lexical-binding: t -*-
;;; Markdown · Org · YAML · TOML · JSON · Terraform · Ansible · GraphQL · Proto
;;; Version: 1.0.2
;;; Fixes vs 1.0.1:
;;;   - FIX-TERRAFORM-DOUBLE: terraform-mode use-package had :hook (terraform-mode
;;;     . lsp-deferred) AND a separate lsp-mode use-package also hooked
;;;     (terraform-mode . lsp-deferred). LSP started twice on every .tf file.
;;;     Fix: removed the :hook from terraform-mode use-package; the dedicated
;;;     lsp-mode block guarded by (executable-find "terraform-ls") is the
;;;     canonical hook owner.
;;;   - FIX-LSP-GUARD: All lsp-deferred hooks were unguarded — fired even when
;;;     emacs-ide-lsp-enable is nil in config.yml. Fixed by adding
;;;     (bound-and-true-p emacs-ide-lsp-enable) :if guard to every lsp-mode
;;;     use-package block, consistent with lang-c.el, lang-rust.el pattern.
;;; Fixes vs 1.0.0:
;;;   - FIX-12: Removed duplicate dockerfile-mode (already in tools-terminal.el).
;;;   - FIX-13: Removed duplicate restclient (already in tools-rest.el).
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
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
           (executable-find "yaml-language-server"))
  :hook (yaml-mode . lsp-deferred))
(with-eval-after-load 'apheleia (emacs-ide-dev-attach-formatter 'prettier 'yaml-mode))

;; ── TOML ──────────────────────────────────────────────────────────────────
(use-package toml-mode :defer t :mode "\\.toml\\'")
(with-eval-after-load 'apheleia (emacs-ide-dev-attach-formatter 'prettier 'toml-mode))

;; ── JSON ──────────────────────────────────────────────────────────────────
(use-package json-mode :defer t :mode "\\.json\\'")
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
           (executable-find "vscode-json-language-server"))
  :hook (json-mode . lsp-deferred))
(with-eval-after-load 'apheleia (emacs-ide-dev-attach-formatter 'prettier 'json-mode))

;; ── CSV ───────────────────────────────────────────────────────────────────
(use-package csv-mode :defer t :mode "\\.csv\\'")

;; ── Dockerfile ────────────────────────────────────────────────────────────
;; FIX-12: dockerfile-mode installed and registered in tools-terminal.el.
;;   Duplicate use-package here caused double registration in straight's
;;   recipe cache. Use with-eval-after-load for LSP only.
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
           (executable-find "docker-langserver"))
  :hook (dockerfile-mode . lsp-deferred))

;; ── Terraform ─────────────────────────────────────────────────────────────
;; FIX-TERRAFORM-DOUBLE: terraform-mode use-package previously also had
;; :hook (terraform-mode . lsp-deferred), duplicating the hook below.
;; The :hook is removed from terraform-mode; lsp-mode block is sole owner.
(use-package terraform-mode :if (executable-find "terraform")
  :defer t :mode "\\.tf\\'")
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
           (executable-find "terraform-ls"))
  :hook (terraform-mode . lsp-deferred))

;; ── Ansible ───────────────────────────────────────────────────────────────
(use-package ansible :defer t)
(use-package ansible-doc :after ansible)

;; ── GraphQL ───────────────────────────────────────────────────────────────
(use-package graphql-mode :defer t :mode "\\.graphql\\'")
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
           (executable-find "graphql-language-service-cli"))
  :hook (graphql-mode . lsp-deferred))

;; ── Protobuf ──────────────────────────────────────────────────────────────
(use-package protobuf-mode :defer t :mode "\\.proto\\'")

;; ── ENV files ─────────────────────────────────────────────────────────────
(use-package dotenv-mode :defer t :mode "\\.env\\'")

;; ── REST (HTTP files) ─────────────────────────────────────────────────────
;; FIX-13: restclient installed and :mode registered in tools-rest.el.
;;   No use-package needed here.

) (provide 'lang-prose)
;;; lang-prose.el ends here
