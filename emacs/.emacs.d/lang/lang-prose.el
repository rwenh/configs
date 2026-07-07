;;; lang-prose.el --- Prose & Config IDE layer -*- lexical-binding: t -*-
;;;
;;; Version: 3.0.4

(require 'core-dev)

(emacs-ide-dev-register "prose" :tier 4 :lsp-server nil
  :formatter "prettier" :test-cmd nil :repl nil
  :modes '(markdown-mode yaml-mode toml-mode json-mode))

(when (emacs-ide-dev-lang-enabled-p "prose")

(emacs-ide-dev-ensure-treesit 'markdown)
(emacs-ide-dev-ensure-treesit 'yaml)
(emacs-ide-dev-ensure-treesit 'json)
(emacs-ide-dev-ensure-treesit 'toml)

(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init
  (setq markdown-command
        (or (executable-find "pandoc")
            (executable-find "markdown")
            nil)
        markdown-fontify-code-blocks-natively t
        markdown-enable-math                  t
        markdown-enable-wiki-links            t))

(use-package yaml-mode
  :defer t
  :mode "\\.ya?ml\\'")

(use-package toml-mode
  :defer t
  :mode "\\.toml\\'")

(use-package json-mode
  :defer t
  :mode "\\.json\\'")

(use-package csv-mode
  :defer t
  :mode "\\.csv\\'")

(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'")

(use-package ansible
  :if (or (executable-find "ansible")
          (executable-find "ansible-playbook"))
  :defer t)

(use-package ansible-doc
  :if (or (executable-find "ansible")
          (executable-find "ansible-playbook"))
  :after ansible)

(use-package graphql-mode
  :defer t
  :mode "\\.graphql\\'")

(use-package protobuf-mode
  :defer t
  :mode "\\.proto\\'")

(use-package dotenv-mode
  :defer t
  :mode "\\.env\\'")

(when (bound-and-true-p emacs-ide-lsp-enable)
  (with-eval-after-load 'lsp-mode
    (when (executable-find "yaml-language-server")
      (add-hook 'yaml-mode-hook    #'lsp-deferred)
      (add-hook 'yaml-ts-mode-hook #'lsp-deferred))
    (when (executable-find "vscode-json-language-server")
      (add-hook 'json-mode-hook    #'lsp-deferred)
      (add-hook 'json-ts-mode-hook #'lsp-deferred))
    (when (executable-find "docker-langserver")
      (add-hook 'dockerfile-mode-hook #'lsp-deferred))
    (when (executable-find "terraform-ls")
      (add-hook 'terraform-mode-hook #'lsp-deferred))
    (when (executable-find "graphql-language-service-cli")
      (add-hook 'graphql-mode-hook #'lsp-deferred))))

(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'prettier 'yaml-mode)
  (emacs-ide-dev-attach-formatter 'prettier 'yaml-ts-mode)
  (emacs-ide-dev-attach-formatter 'prettier 'json-mode)
  (emacs-ide-dev-attach-formatter 'prettier 'json-ts-mode)
  (emacs-ide-dev-attach-formatter 'prettier 'markdown-mode)
  (emacs-ide-dev-attach-formatter 'prettier 'gfm-mode)
  (emacs-ide-dev-attach-formatter 'prettier 'graphql-mode))

)

(provide 'lang-prose)
;;; lang-prose.el ends here
