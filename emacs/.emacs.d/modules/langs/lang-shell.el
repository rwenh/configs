;;; lang-shell.el --- Shell IDE layer (Bash/Zsh/Fish/POSIX) -*- lexical-binding: t -*-
;;; Version: 3.0.4

(require 'core-dev)

(emacs-ide-dev-register "shell" :tier 2 :lsp-server "bash-language-server"
  :formatter "shfmt" :test-cmd "bats" :repl "bash"
  :modes '(sh-mode bash-ts-mode))

(when (emacs-ide-dev-lang-enabled-p "shell")

(emacs-ide-dev-ensure-treesit 'bash)

(defun emacs-ide-shell--detect-interpreter ()
  "Detect the shell interpreter from the shebang line or fall back to bash."
  (when (buffer-file-name)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at "#!\\s-*\\(\\S-+\\)")
        (let ((shebang (match-string 1)))
          (if (string-match-p "/env$" shebang)
              (progn
                (when (looking-at "#!\\s-*\\S-+\\s-+\\(\\S-+\\)")
                  (match-string 1)))
            shebang))))))

(defun emacs-ide-shell-run ()
  "Run the current shell script using its shebang interpreter or bash."
  (interactive)
  (let ((interp (or (emacs-ide-shell--detect-interpreter)
                    (executable-find "bash")
                    "sh")))
    (if (and interp (buffer-file-name))
        (compile (format "%s %s"
                         (shell-quote-argument interp)
                         (shell-quote-argument (buffer-file-name))))
      (message "lang-shell: no interpreter found"))))

(defun emacs-ide-shell-repl ()
  "Open a bash REPL."
  (interactive)
  (if (executable-find "bash")
      (progn (require 'comint)
             (make-comint "bash-repl" "bash")
             (switch-to-buffer "*bash-repl*"))
    (message "lang-shell: bash not found")))

(defun emacs-ide-shell-test-file ()
  "Run bats tests for the current file."
  (interactive)
  (if (and (executable-find "bats") (buffer-file-name))
      (compile (format "bats %s" (shell-quote-argument (buffer-file-name))))
    (message "lang-shell: bats not found")))

(defun emacs-ide-shell-test-project ()
  "Run all bats tests in the project."
  (interactive)
  (if (executable-find "bats")
      (compile "bats .")
    (message "lang-shell: bats not found")))

(use-package sh-script
  :straight nil
  :defer t
  :mode (("\\.sh\\'"   . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.zsh\\'"  . sh-mode)
         ("\\`#!/.*\\(bash\\|zsh\\|sh\\)" . sh-mode))
  :init
  (setq sh-basic-offset 2
        sh-indentation  2)
  :config
  (emacs-ide-dev-bind-compile sh-mode-map #'emacs-ide-shell-run)
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'sh-mode
      :launch         #'emacs-ide-shell-repl
      :buffer-name    "*bash-repl*"
      :send-region-fn nil))
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'sh-mode
      :file-fn    #'emacs-ide-shell-test-file
      :project-fn #'emacs-ide-shell-test-project)))

(use-package fish-mode
  :if (locate-library "fish-mode")
  :defer t
  :mode "\\.fish\\'")

(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :hook ((sh-mode      . lsp-deferred)
         (bash-ts-mode . lsp-deferred)))

(use-package flymake-shellcheck
  :if (executable-find "shellcheck")
  :hook ((sh-mode      . flymake-shellcheck-auto)
         (bash-ts-mode . flymake-shellcheck-auto)))

(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'shfmt 'sh-mode)
  (emacs-ide-dev-attach-formatter 'shfmt 'bash-ts-mode))

)

(provide 'lang-shell)
;;; lang-shell.el ends here
