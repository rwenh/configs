;;; lang-go.el --- Go IDE layer -*- lexical-binding: t -*-
;;; Version: 1.0.2
;;; Fixes vs 1.0.1:
;;;   - FIX-LSP: Removed go-mode :hook from lsp-mode use-package.
;;;     tools-lsp.el already hooks go-mode to emacs-ide-lsp-deferred-optimized.
;;;     go-ts-mode hook kept.
;;; Fixes vs 1.0.0:
;;;   - FIX-5: gofmt-before-save guarded against double-format with apheleia.
;;; Code:
(require 'core-dev)
(emacs-ide-dev-register "go" :tier 1 :lsp-server "gopls"
  :formatter "gofmt" :test-cmd "go test ./..." :repl "gore" :modes '(go-mode go-ts-mode))
(when (emacs-ide-dev-lang-enabled-p "go")
(emacs-ide-dev-ensure-treesit 'go)
(use-package go-mode
  :if (executable-find "go")
  :defer t :mode "\\.go\\'"
  :init (setq gofmt-command (or (executable-find "goimports") "gofmt"))
  :config
  (defun emacs-ide-go-run ()
    (interactive)
    (if (executable-find "go") (compile (format "go run %s" (shell-quote-argument (buffer-file-name))))
      (message "lang-go: go not found")))
  (add-hook 'go-mode-hook (lambda ()
                            ;; FIX-5: Only add gofmt-before-save when apheleia is
                            ;; not active. apheleia-langs-patch.el maps go-mode →
                            ;; gofmt already; both active = double format on save.
                            (unless (bound-and-true-p apheleia-global-mode)
                              (add-hook 'before-save-hook #'gofmt-before-save nil t))))
  (emacs-ide-dev-bind-compile go-mode-map #'emacs-ide-go-run)
  (define-key go-mode-map (kbd "C-c C-b") (lambda () (interactive) (compile "go build -v")))
  (define-key go-mode-map (kbd "C-c C-v") (lambda () (interactive) (compile "go vet ./..."))))
(use-package lsp-mode ;; FIX-LSP: go-mode hook removed — tools-lsp.el owns it.
  :hook (go-ts-mode . lsp-deferred)
  :init (setq lsp-go-analyses '((shadow . t) (staticcheck . t))
              lsp-go-use-gofumpt t))
(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'gofmt 'go-mode)
  (emacs-ide-dev-attach-formatter 'gofmt 'go-ts-mode))
(defun emacs-ide-go-repl ()
  (interactive)
  (if (executable-find "gore") (progn (require 'comint) (make-comint "go-repl" "gore") (switch-to-buffer "*go-repl*"))
    (message "lang-go: install gore: go install github.com/x-motemen/gore/cmd/gore@latest")))
(with-eval-after-load 'go-mode (emacs-ide-dev-attach-repl go-mode-map #'emacs-ide-go-repl))
(defun emacs-ide-go-test-file () (interactive) (if (executable-find "go") (compile "go test -v ./...") (message "lang-go: go not found")))
(with-eval-after-load 'tools-test (when (fboundp 'emacs-ide-test-register-runner)
  (emacs-ide-test-register-runner 'go-mode :file-fn #'emacs-ide-go-test-file :project-fn #'emacs-ide-go-test-file)))
(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "Go :: Delve" 'dap-dlv-go)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template "Go :: Delve :: file"
      (list :type "go" :request "launch" :name "Go file" :mode "debug" :program "${file}"))))
) (provide 'lang-go)
;;; lang-go.el ends here
