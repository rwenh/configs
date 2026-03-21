;;; lang-lua.el --- Lua IDE layer -*- lexical-binding: t -*-
;;; Version: 1.0.1
;;; Fixes vs 1.0.0:
;;;   - FIX-LSP-GUARD: lsp-mode :hook (lua-mode . lsp-deferred) was unguarded.
;;;     Fired even when emacs-ide-lsp-enable is nil. Added guard.
;;; Code:
(require 'core-dev)
(emacs-ide-dev-register "lua" :tier 2 :lsp-server "lua-language-server"
  :formatter "stylua" :test-cmd "busted" :repl "lua" :modes '(lua-mode))
(when (emacs-ide-dev-lang-enabled-p "lua")
(use-package lua-mode
  :defer t :mode "\\.lua\\'" :interpreter "lua"
  :init (setq lua-indent-level 2)
  :config
  (defun emacs-ide-lua-run ()
    (interactive)
    (if (executable-find "lua") (compile (format "lua %s" (shell-quote-argument (buffer-file-name))))
      (message "lang-lua: lua not found")))
  (emacs-ide-dev-bind-compile lua-mode-map #'emacs-ide-lua-run))
(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
  :hook (lua-mode . lsp-deferred)
  :init (setq lsp-lua-hint-enable t lsp-lua-hint-set-type t))
(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'stylua 'lua-mode))
(defun emacs-ide-lua-repl ()
  (interactive)
  (if (executable-find "lua") (progn (require 'comint) (make-comint "lua-repl" "lua") (switch-to-buffer "*lua-repl*"))
    (message "lang-lua: lua not found")))
(with-eval-after-load 'lua-mode (emacs-ide-dev-attach-repl lua-mode-map #'emacs-ide-lua-repl))
) (provide 'lang-lua)
;;; lang-lua.el ends here
