;;; lang-go.el --- Go IDE layer -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "go"
  :tier       1
  :lsp-server "gopls"
  :formatter  "gofmt"
  :test-cmd   "go test ./..."
  :repl       "gore"
  :modes      '(go-mode go-ts-mode))

(when (emacs-ide-dev-lang-enabled-p "go")

;;;; ── Tree-sitter grammars ────────────────────────────────────────────────────

(emacs-ide-dev-ensure-treesit 'go)
(emacs-ide-dev-ensure-treesit 'gomod)

;;;; ── Run helpers ─────────────────────────────────────────────────────────────

(defun emacs-ide-go-run ()
  "Run the current Go file via `go run'."
  (interactive)
  (if (executable-find "go")
      (compile (format "go run %s"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-go: go not found on PATH")))

;;;; ── Test helpers ────────────────────────────────────────────────────────────

(defun emacs-ide-go-test-file ()
  "Run tests for the current Go package."
  (interactive)
  (if (executable-find "go")
      (compile (format "go test -v %s"
                       (shell-quote-argument
                        (if (buffer-file-name)
                            (file-name-directory (buffer-file-name))
                          "."))))
    (message "lang-go: go not found on PATH")))

(defun emacs-ide-go-test-project ()
  "Run all Go tests in the project."
  (interactive)
  (if (executable-find "go")
      (compile "go test -v ./...")
    (message "lang-go: go not found on PATH")))

;;;; ── REPL (gore) ─────────────────────────────────────────────────────────────

(defun emacs-ide-go-repl ()
  "Open a Gore REPL for Go."
  (interactive)
  (if (executable-find "gore")
      (progn
        (require 'comint)
        (make-comint "go-repl" "gore")
        (switch-to-buffer "*go-repl*"))
    (message
     "lang-go: gore not found.  Install: go install github.com/x-motemen/gore/cmd/gore@latest")))

;;;; ── gofmt-on-save fallback ──────────────────────────────────────────────────

(defun emacs-ide-go--maybe-gofmt-on-save ()
  "Enable gofmt-before-save locally when apheleia is not active."
  (unless (bound-and-true-p apheleia-global-mode)
    (add-hook 'before-save-hook #'gofmt-before-save nil t)))

;;;; ── go-mode ─────────────────────────────────────────────────────────────────

(use-package go-mode
  :if (executable-find "go")
  :defer t
  :mode "\\.go\\'"
  :hook (go-mode . emacs-ide-go--maybe-gofmt-on-save)
  :init
  ;; Prefer goimports over gofmt when available — handles imports too
  (setq gofmt-command (or (executable-find "goimports") "gofmt"))
  :config
  ;; Compile / run keybinds
  (emacs-ide-dev-bind-compile go-mode-map #'emacs-ide-go-run)
  (define-key go-mode-map (kbd "C-c C-b")
    (lambda () (interactive) (compile "go build -v")))
  (define-key go-mode-map (kbd "C-c C-v")
    (lambda () (interactive) (compile "go vet ./...")))
  ;; REPL hub registration — go-mode
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'go-mode
      :launch         #'emacs-ide-go-repl
      :buffer-name    "*go-repl*"
      :send-region-fn nil)))

;;;; ── go-ts-mode ────────────────────────────────────────────────────

(with-eval-after-load 'go-ts-mode
  ;; gofmt-on-save fallback for ts-mode buffers
  (add-hook 'go-ts-mode-hook #'emacs-ide-go--maybe-gofmt-on-save)

  ;; Compile / run keybinds on go-ts-mode-map
  (when (boundp 'go-ts-mode-map)
    (emacs-ide-dev-bind-compile go-ts-mode-map #'emacs-ide-go-run)
    (define-key go-ts-mode-map (kbd "C-c C-b")
      (lambda () (interactive) (compile "go build -v")))
    (define-key go-ts-mode-map (kbd "C-c C-v")
      (lambda () (interactive) (compile "go vet ./..."))))

  ;; REPL hub registration — go-ts-mode
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'go-ts-mode
      :launch         #'emacs-ide-go-repl
      :buffer-name    "*go-repl*"
      :send-region-fn nil)))

;;;; ── LSP (gopls) ────────────────────────────────────────────────────────────

(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :hook ((go-mode go-ts-mode) . lsp-deferred)
  :config
  ;; Static analysis passes — always enabled when gopls is present
  (when (boundp 'lsp-go-analyses)
    (setq lsp-go-analyses '((shadow . t) (staticcheck . t))))
  (when (and (boundp 'lsp-go-use-gofumpt)
             (executable-find "gofumpt"))
    (setq lsp-go-use-gofumpt t)))

;;;; ── Formatter ───────────────────────────────────────────────────────────────

(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'gofmt 'go-mode)
  (emacs-ide-dev-attach-formatter 'gofmt 'go-ts-mode))

;;;; ── Test runner ─────────────────────────────────────────────────────────────

(with-eval-after-load 'tools-test-runner-registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(go-mode go-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-go-test-file
        :project-fn #'emacs-ide-go-test-project))))

;;;; ── DAP (Delve) ─────────────────────────────────────────────────────────────

(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "Go :: Delve" 'dap-dlv-go)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template "Go :: Delve :: file"
      (list :type    "go"
            :request "launch"
            :name    "Go file (Delve)"
            :mode    "debug"
            :program (lambda () (buffer-file-name))
            :cwd     (lambda ()
                       (or (and (fboundp 'projectile-project-root)
                                (ignore-errors (projectile-project-root)))
                           default-directory))))))

) ;; end go-enabled

(provide 'lang-go)
;;; lang-go.el ends here
