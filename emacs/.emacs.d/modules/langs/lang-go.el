;;; lang-go.el --- Go IDE layer -*- lexical-binding: t -*-
;;; Version: 3.0.4

(require 'core-dev)

(emacs-ide-dev-register "go" :tier 1 :lsp-server "gopls"
  :formatter "gofmt" :test-cmd "go test ./..." :repl "gore"
  :modes '(go-mode go-ts-mode))

(when (emacs-ide-dev-lang-enabled-p "go")

(emacs-ide-dev-ensure-treesit 'go)
(emacs-ide-dev-ensure-treesit 'gomod)

(defun emacs-ide-go-run ()
  "Run the current Go file via `go run`."
  (interactive)
  (if (executable-find "go")
      (compile (format "go run %s" (shell-quote-argument (buffer-file-name))))
    (message "lang-go: go not found")))

(defun emacs-ide-go-test-file ()
  "Run tests for the current Go package."
  (interactive)
  (if (executable-find "go")
      (let ((pkg-dir (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       ".")))
        (compile (format "go test -v %s"
                         (shell-quote-argument pkg-dir))))
    (message "lang-go: go not found")))

(defun emacs-ide-go-test-project ()
  "Run all Go tests in the project."
  (interactive)
  (if (executable-find "go")
      (compile "go test -v ./...")
    (message "lang-go: go not found")))

(defun emacs-ide-go-repl ()
  "Open a Gore REPL for Go."
  (interactive)
  (if (executable-find "gore")
      (progn (require 'comint)
             (make-comint "go-repl" "gore")
             (switch-to-buffer "*go-repl*"))
    (message "lang-go: install gore: go install github.com/x-motemen/gore/cmd/gore@latest")))

(defun emacs-ide-go--maybe-gofmt-on-save ()
  "Add gofmt-before-save hook when apheleia is not active."
  (unless (bound-and-true-p apheleia-global-mode)
    (add-hook 'before-save-hook #'gofmt-before-save nil t)))

(use-package go-mode
  :if (executable-find "go")
  :defer t
  :mode "\\.go\\'"
  :hook (go-mode . emacs-ide-go--maybe-gofmt-on-save)
  :init
  (setq gofmt-command (or (executable-find "goimports") "gofmt"))
  :config
  (emacs-ide-dev-bind-compile go-mode-map #'emacs-ide-go-run)
  (define-key go-mode-map (kbd "C-c C-b")
    (lambda () (interactive) (compile "go build -v")))
  (define-key go-mode-map (kbd "C-c C-v")
    (lambda () (interactive) (compile "go vet ./...")))
  (emacs-ide-dev-attach-repl go-mode-map #'emacs-ide-go-repl
                              (kbd "C-c x r"))
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'go-mode
      :launch         #'emacs-ide-go-repl
      :buffer-name    "*go-repl*"
      :send-region-fn nil))
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'go-mode
      :file-fn    #'emacs-ide-go-test-file
      :project-fn #'emacs-ide-go-test-project)))

(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :hook (go-ts-mode . lsp-deferred)
  :init
  (setq lsp-go-analyses    '((shadow . t) (staticcheck . t))
        lsp-go-use-gofumpt t))

(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'gofmt 'go-mode)
  (emacs-ide-dev-attach-formatter 'gofmt 'go-ts-mode))

(with-eval-after-load 'tools-test
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'go-mode
      :file-fn    #'emacs-ide-go-test-file
      :project-fn #'emacs-ide-go-test-project)))

(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "Go :: Delve" 'dap-dlv-go)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template "Go :: Delve :: file"
      (list :type    "go"
            :request "launch"
            :name    "Go file"
            :mode    "debug"
            :program (lambda () (buffer-file-name))
            :cwd     (lambda ()
                       (or (and (fboundp 'projectile-project-root)
                                (ignore-errors (projectile-project-root)))
                           default-directory))))))

)

(provide 'lang-go)
;;; lang-go.el ends here
