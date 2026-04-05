;;; lang-prose.el --- Prose & Config IDE layer -*- lexical-binding: t -*-
;;; Markdown · Org · YAML · TOML · JSON · Terraform · Ansible · GraphQL · Proto
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.2 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.2 to 3.0.4.
;;;   - FIX-MARKDOWN-COMMAND-FALLBACK: markdown-command fell back to "markdown"
;;;     string — if neither pandoc nor markdown is on PATH this silently fails.
;;;     Now falls back to nil (disables preview) rather than a broken command.
;;;   - FIX-TOML-PRETTIER: explicit prettier attach for toml-mode conflicted
;;;     with apheleia-langs-patch.el which maps toml to taplo when available.
;;;     Removed; the patch handles TOML formatting correctly.
;;;   - FIX-YAML-TS-MODE: yaml-ts-mode (Emacs 29+ treesitter mode) had no
;;;     LSP hook alongside yaml-mode. Added.
;;;   - FIX-JSON-TS-MODE: json-ts-mode had no LSP hook alongside json-mode. Added.
;;;   - FIX-TERRAFORM-GUARD: terraform-mode :if (executable-find "terraform")
;;;     prevented loading the mode when terraform is installed as "tofu"
;;;     (OpenTofu) or via other names. Mode now loads unconditionally; only
;;;     the LSP block requires terraform-ls on PATH.
;;;   - FIX-ANSIBLE-ALWAYS-LOAD: (use-package ansible :defer t) had no guard
;;;     — ansible and ansible-doc loaded for all users regardless of need.
;;;     Now gated on ansible/ansible-playbook being on PATH.
;;;   - FIX-MULTIPLE-LSP-BLOCKS: Consolidated LSP hooks into a single
;;;     with-eval-after-load 'lsp-mode block guarded by emacs-ide-lsp-enable,
;;;     replacing the multiple (use-package lsp-mode :if ... :hook ...) blocks.
;;; Fixes vs 1.0.1 (retained):
;;;   - FIX-TERRAFORM-DOUBLE: lsp-mode is the sole hook owner for terraform.
;;;   - FIX-LSP-GUARD: all LSP hooks guarded by emacs-ide-lsp-enable.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-12: dockerfile-mode owned by tools-terminal.el.
;;;   - FIX-13: restclient owned by tools-rest.el.
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

;; ============================================================================
;; Markdown
;; FIX-MARKDOWN-COMMAND-FALLBACK: falls back to nil (disable preview) rather
;; than the string "markdown" which silently fails when not on PATH.
;; ============================================================================
(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init
  (setq markdown-command
        (or (executable-find "pandoc")
            (executable-find "markdown")
            nil)   ; nil disables preview cleanly rather than failing silently
        markdown-fontify-code-blocks-natively t
        markdown-enable-math                  t
        markdown-enable-wiki-links            t))

;; ============================================================================
;; YAML
;; FIX-YAML-TS-MODE: yaml-ts-mode hook added alongside yaml-mode.
;; ============================================================================
(use-package yaml-mode
  :defer t
  :mode "\\.ya?ml\\'")

;; ============================================================================
;; TOML
;; FIX-TOML-PRETTIER: explicit prettier attach removed — apheleia-langs-patch.el
;; handles TOML formatting (taplo when available, prettier as fallback).
;; ============================================================================
(use-package toml-mode
  :defer t
  :mode "\\.toml\\'")

;; ============================================================================
;; JSON
;; FIX-JSON-TS-MODE: json-ts-mode hook added alongside json-mode.
;; ============================================================================
(use-package json-mode
  :defer t
  :mode "\\.json\\'")

;; ============================================================================
;; CSV
;; ============================================================================
(use-package csv-mode
  :defer t
  :mode "\\.csv\\'")

;; ============================================================================
;; Dockerfile — owned by tools-terminal.el (FIX-12 retained)
;; ============================================================================

;; ============================================================================
;; Terraform
;; FIX-TERRAFORM-GUARD: mode loads unconditionally; only LSP needs terraform-ls.
;; ============================================================================
(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'")

;; ============================================================================
;; Ansible
;; FIX-ANSIBLE-ALWAYS-LOAD: guarded by ansible/ansible-playbook on PATH.
;; ============================================================================
(use-package ansible
  :if (or (executable-find "ansible")
          (executable-find "ansible-playbook"))
  :defer t)

(use-package ansible-doc
  :if (or (executable-find "ansible")
          (executable-find "ansible-playbook"))
  :after ansible)

;; ============================================================================
;; GraphQL
;; ============================================================================
(use-package graphql-mode
  :defer t
  :mode "\\.graphql\\'")

;; ============================================================================
;; Protobuf
;; ============================================================================
(use-package protobuf-mode
  :defer t
  :mode "\\.proto\\'")

;; ============================================================================
;; ENV files
;; ============================================================================
(use-package dotenv-mode
  :defer t
  :mode "\\.env\\'")

;; ============================================================================
;; REST (HTTP files) — owned by tools-rest.el (FIX-13 retained)
;; ============================================================================

;; ============================================================================
;; LSP HOOKS — consolidated into a single guarded block
;; FIX-MULTIPLE-LSP-BLOCKS: replaced multiple (use-package lsp-mode :if :hook)
;; blocks with a single with-eval-after-load that adds hooks only when
;; emacs-ide-lsp-enable is t and the required server binary is present.
;; FIX-YAML-TS-MODE + FIX-JSON-TS-MODE: ts-mode variants now included.
;; ============================================================================
(when (bound-and-true-p emacs-ide-lsp-enable)
  (with-eval-after-load 'lsp-mode

    ;; YAML + yaml-ts-mode
    (when (executable-find "yaml-language-server")
      (add-hook 'yaml-mode-hook    #'lsp-deferred)
      (add-hook 'yaml-ts-mode-hook #'lsp-deferred))

    ;; JSON + json-ts-mode
    (when (executable-find "vscode-json-language-server")
      (add-hook 'json-mode-hook    #'lsp-deferred)
      (add-hook 'json-ts-mode-hook #'lsp-deferred))

    ;; Dockerfile (mode owned by tools-terminal.el)
    (when (executable-find "docker-langserver")
      (add-hook 'dockerfile-mode-hook #'lsp-deferred))

    ;; Terraform — terraform-ls required (not the terraform binary)
    (when (executable-find "terraform-ls")
      (add-hook 'terraform-mode-hook #'lsp-deferred))

    ;; GraphQL
    (when (executable-find "graphql-language-service-cli")
      (add-hook 'graphql-mode-hook #'lsp-deferred))))

;; ============================================================================
;; FORMATTERS — apheleia
;; prettier handles JSON, YAML, Markdown, GraphQL
;; TOML handled by apheleia-langs-patch.el (taplo or prettier)
;; ============================================================================
(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'prettier 'yaml-mode)
  (emacs-ide-dev-attach-formatter 'prettier 'yaml-ts-mode)
  (emacs-ide-dev-attach-formatter 'prettier 'json-mode)
  (emacs-ide-dev-attach-formatter 'prettier 'json-ts-mode)
  (emacs-ide-dev-attach-formatter 'prettier 'markdown-mode)
  (emacs-ide-dev-attach-formatter 'prettier 'gfm-mode)
  (emacs-ide-dev-attach-formatter 'prettier 'graphql-mode))

) ;; end (when (emacs-ide-dev-lang-enabled-p "prose"))

(provide 'lang-prose)
;;; lang-prose.el ends here
