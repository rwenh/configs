;;; lang-rust.el --- Rust Language Support -*- lexical-binding: t -*-
;;; Version: 3.4.0
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "rust"
  :tier 1
  :lsp-server "rust-analyzer"
  :formatter  "rustfmt"
  :test-cmd   "cargo test"
  :repl       "evcxr"
  :modes      '(rust-mode rust-ts-mode))

(when (emacs-ide-dev-lang-enabled-p "rust")

;;;; ── rust-mode ───────────────────────────────────────────────────────────────

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (define-key rust-mode-map (kbd "C-c C-c")
    (lambda ()
      (interactive)
      (compile "cargo run")))
  (with-eval-after-load 'rust-ts-mode
    (when (boundp 'rust-ts-mode-map)
      (define-key rust-ts-mode-map (kbd "C-c C-c")
        (lambda () (interactive) (compile "cargo run"))))))

;;;; ── LSP (rust-analyzer) ─────────────────────────────────────────────────────

(use-package lsp-rust
  :after  (rust-mode lsp-mode)
  :if     (and (bound-and-true-p emacs-ide-lsp-enable)
               (executable-find "rust-analyzer"))
  :hook   ((rust-mode rust-ts-mode) . lsp-deferred)
  :config
  (when (boundp 'lsp-rust-analyzer-cargo-watch-command)
    (setq lsp-rust-analyzer-cargo-watch-command "clippy"))
  (when (boundp 'lsp-rust-analyzer-checkOnSave-command)
    (setq lsp-rust-analyzer-checkOnSave-command "clippy"))
  (when (boundp 'lsp-rust-analyzer-proc-macro-enable)
    (setq lsp-rust-analyzer-proc-macro-enable t)))

;;;; ── cargo ───────────────────────────────────────────────────────────────────

(use-package cargo
  :after rust-mode
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))

;;;; ── Formatter ───────────────────────────────────────────────────────────────

(with-eval-after-load 'apheleia
  (setf (alist-get 'rust-mode    apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt))

;;;; ── Test runners ────────────────────────────────────────────────────────────

(defun emacs-ide-rust-test-file ()
  "Run cargo test for the library unit tests."
  (interactive)
  (compile "cargo test -- --lib"))

(defun emacs-ide-rust-test-project ()
  "Run all cargo tests in the project."
  (interactive)
  (compile "cargo test"))

(with-eval-after-load 'tools-test-runner-registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(rust-mode rust-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-rust-test-file
        :project-fn #'emacs-ide-rust-test-project))))

;;;; ── DAP (CodeLLDB / LLDB) ──────────────────────────────────────────────────

(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "Rust :: CodeLLDB" 'dap-lldb)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template "Rust :: cargo run"
      (list :type    "lldb"
            :request "launch"
            :name    "Rust binary (cargo)"
            :cargo   (list :args ["build"])
            :cwd     (lambda ()
                       (or (and (fboundp 'projectile-project-root)
                                (ignore-errors (projectile-project-root)))
                           default-directory))))))

) ;; end rust-enabled

(provide 'lang-rust)
;;; lang-rust.el ends here
