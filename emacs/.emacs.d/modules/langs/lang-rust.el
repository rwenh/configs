;;; lang-rust.el --- Rust Language Support -*- lexical-binding: t -*-
;;; Version: 3.0.4

(require 'core-dev)

(emacs-ide-dev-register "rust"
  :tier 1
  :lsp-server "rust-analyzer"
  :formatter "rustfmt"
  :test-cmd "cargo test"
  :repl "evcxr"
  :modes '(rust-mode rust-ts-mode))

(when (emacs-ide-dev-lang-enabled-p "rust")

(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save nil)
  :config
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'rust-mode
      :launch         (lambda ()
                        (if (executable-find "evcxr")
                            (progn (make-comint "rust-repl" "evcxr")
                                   (switch-to-buffer "*rust-repl*"))
                          (message "lang-rust: install evcxr: cargo install evcxr_repl")))
      :buffer-name    "*rust-repl*"
      :send-region-fn nil)))

(use-package lsp-rust
  :after (rust-mode lsp-mode)
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (executable-find "rust-analyzer"))
  :hook ((rust-mode rust-ts-mode) . lsp-deferred)
  :config
  (setq lsp-rust-analyzer-server-display-inlay-hints          t
        lsp-rust-analyzer-inlay-hints-mode                    t
        lsp-rust-analyzer-inlay-hints-render-colons            t
        lsp-rust-analyzer-inlay-hints-render-closing-brace     t
        lsp-rust-analyzer-cargo-watch-command                  "clippy"
        lsp-rust-analyzer-cargo-watch-args                     "--all-targets --all-features"
        lsp-rust-analyzer-checkOnSave-command                  "clippy"
        lsp-rust-analyzer-checkOnSave-extraArgs                "--all-targets --all-features"
        lsp-rust-analyzer-proc-macro-enable                    t
        lsp-rust-analyzer-completion-auto-self-enable          t
        lsp-rust-analyzer-completion-add-call-parenthesis      t
        lsp-rust-analyzer-completion-add-call-argument-snippets t
        lsp-rust-analyzer-hover-documentation                  t))

(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode)
  :init
  (setq cargo-process-echo-in-minibuffer t))

(with-eval-after-load 'apheleia
  (when (executable-find "rustfmt")
    (setf (alist-get 'rust-mode    apheleia-mode-alist) 'rustfmt)
    (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt)))

(defun emacs-ide-rust-test-file ()
  "Run cargo test on the current module."
  (interactive)
  (if (executable-find "cargo")
      (compile "cargo test -- --test-threads=1")
    (message "lang-rust: cargo not found")))

(defun emacs-ide-rust-test-project ()
  "Run all cargo tests in the project."
  (interactive)
  (if (executable-find "cargo")
      (compile "cargo test")
    (message "lang-rust: cargo not found")))

(with-eval-after-load 'tools-test-runner-registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'rust-mode
      :file-fn    #'emacs-ide-rust-test-file
      :project-fn #'emacs-ide-rust-test-project)
    (emacs-ide-test-register-runner 'rust-ts-mode
      :file-fn    #'emacs-ide-rust-test-file
      :project-fn #'emacs-ide-rust-test-project)))

(with-eval-after-load 'dap-mode
  (when (fboundp 'dap-register-debug-template)
    (require 'dap-lldb nil t)
    (dap-register-debug-template
     "Rust :: LLDB"
     (list :type    "lldb"
           :request "launch"
           :name    "Rust binary"
           :program (lambda ()
                      (let* ((root (or (and (fboundp 'projectile-project-root)
                                            (ignore-errors (projectile-project-root)))
                                       default-directory))
                             (name (file-name-nondirectory
                                    (directory-file-name root))))
                        (expand-file-name
                         (format "target/debug/%s" name)
                         root)))
           :cwd     (lambda ()
                      (or (and (fboundp 'projectile-project-root)
                               (ignore-errors (projectile-project-root)))
                          default-directory))
           :args    []
           :stopOnEntry :json-false))))

)

(provide 'lang-rust)
;;; lang-rust.el ends here
