;;; lang-web.el --- JavaScript/TypeScript -*- lexical-binding: t -*-
;;; Version: 3.1.0 | Fix: LSP vars in :config
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "javascript" :tier 1 :lsp-server "typescript-language-server"
  :formatter "prettier" :test-cmd "jest" :repl "node"
  :modes '(js-mode js2-mode js-ts-mode))

(emacs-ide-dev-register "typescript" :tier 1 :lsp-server "typescript-language-server"
  :formatter "prettier" :test-cmd "jest" :repl "node"
  :modes '(typescript-mode typescript-ts-mode tsx-ts-mode))

(when (or (emacs-ide-dev-lang-enabled-p "javascript")
          (emacs-ide-dev-lang-enabled-p "typescript"))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (executable-find "typescript-language-server"))
  :hook ((js2-mode js-ts-mode typescript-mode typescript-ts-mode) . lsp-deferred)
  :config
  ;; FIX v3.0.4: moved to :config
  (with-eval-after-load 'lsp-typescript
    (setq lsp-typescript-display-return-type-hints t
          lsp-typescript-display-parameter-type-hints t
          lsp-typescript-display-variable-type-hints t
          lsp-typescript-inlay-hints-include-inlay-variable-hints-when-parameter-type-hints-enabled t)))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :init (setq js-indent-level 2))

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :init (setq typescript-indent-level 2))

(with-eval-after-load 'apheleia
  (dolist (m '(js2-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode))
    (setf (alist-get m apheleia-mode-alist) 'prettier)))

(defun emacs-ide-web-test-project ()
  (interactive)
  (cond ((executable-find "jest") (compile "jest"))
        ((executable-find "npm") (compile "npm test"))))

(with-eval-after-load 'tools-test
  (dolist (m '(js2-mode js-ts-mode typescript-mode typescript-ts-mode))
    (emacs-ide-test-register-runner m :project-fn #'emacs-ide-web-test-project)))

) ;; end js/ts enabled

(provide 'lang-web)
;;; lang-web.el ends here
