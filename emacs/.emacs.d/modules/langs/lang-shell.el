;;; lang-shell.el --- Shell IDE layer (Bash/Zsh/Fish/POSIX) -*- lexical-binding: t -*-
;;; Version: 1.0.1
;;; Fixes vs 1.0.0:
;;;   - FIX-LSP-GUARD: lsp-mode :hook (sh-mode . lsp-deferred) was unguarded.
;;;     Fired even when emacs-ide-lsp-enable is nil, attempting to start LSP
;;;     on every shell file. Added (bound-and-true-p emacs-ide-lsp-enable) guard.
;;; Code:
(require 'core-dev)
(emacs-ide-dev-register "shell" :tier 2 :lsp-server "bash-language-server"
  :formatter "shfmt" :test-cmd "bats" :repl "bash" :modes '(sh-mode bash-ts-mode))
(when (emacs-ide-dev-lang-enabled-p "shell")
(emacs-ide-dev-ensure-treesit 'bash)
(use-package sh-script :straight nil :defer t
  :mode (("\\.sh\\'" . sh-mode) ("\\.bash\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode) ("\\.fish\\'" . sh-mode)
         ("\\`#!/.*\\(bash\\|zsh\\|sh\\)" . sh-mode))
  :init (setq sh-basic-offset 2 sh-indentation 2)
  :config
  (defun emacs-ide-shell-run ()
    (interactive)
    (let ((interp (or (and (buffer-file-name)
                           (save-excursion (goto-char 1)
                             (when (looking-at "#!\\(.+\\)") (match-string 1))))
                      (executable-find "bash"))))
      (if interp (compile (format "%s %s" interp (shell-quote-argument (buffer-file-name))))
        (message "lang-shell: no interpreter found"))))
  (emacs-ide-dev-bind-compile sh-mode-map #'emacs-ide-shell-run))
(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
  :hook (sh-mode . lsp-deferred))
(use-package flymake-shellcheck :if (executable-find "shellcheck")
  :hook (sh-mode . flymake-shellcheck-auto))
(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'shfmt 'sh-mode))
) (provide 'lang-shell)
;;; lang-shell.el ends here
