;;; lang-rust.el --- Rust Language Support -*- lexical-binding: t -*-
;;; Version: 3.1.1 | PATCH: LSP vars moved from :init to :config (FIX #3)
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "rust" :tier 1 :lsp-server "rust-analyzer"
  :formatter "rustfmt" :test-cmd "cargo test" :repl "evcxr" :modes '(rust-mode rust-ts-mode))

(when (emacs-ide-dev-lang-enabled-p "rust")

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (define-key rust-mode-map (kbd "C-c C-c")
    (lambda () (interactive) (compile (format "cargo run --manifest-path %s"
                                              (shell-quote-argument (buffer-file-name)))))))

(use-package lsp-rust
  :after (rust-mode lsp-mode)
  :if (and (bound-and-true-p emacs-ide-lsp-enable) (executable-find "rust-analyzer"))
  :hook ((rust-mode rust-ts-mode) . lsp-deferred)
  :init
  ;; Nothing here — these need :config where lsp-rust is loaded
  (message "")
  :config
  ;; FIX #3: MOVED FROM :init — lsp-rust not loaded until :config
  ;; Now these boundp checks actually work because the package is loaded
  (when (boundp 'lsp-rust-analyzer-inlay-hints-mode)
    (setq lsp-rust-analyzer-inlay-hints-mode t))
  (when (boundp 'lsp-rust-analyzer-cargo-watch-command)
    (setq lsp-rust-analyzer-cargo-watch-command "clippy"))
  (when (boundp 'lsp-rust-analyzer-checkOnSave-command)
    (setq lsp-rust-analyzer-checkOnSave-command "clippy")))

(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

(with-eval-after-load 'apheleia
  (setf (alist-get 'rust-mode apheleia-mode-alist) 'rustfmt))

(defun emacs-ide-rust-test-project ()
  (interactive)
  (compile "cargo test"))

(with-eval-after-load 'tools-test
  (emacs-ide-test-register-runner 'rust-mode
    :file-fn (lambda () (compile "cargo test -- --lib"))
    :project-fn #'emacs-ide-rust-test-project))

) ;; end rust-enabled

(provide 'lang-rust)
;;; lang-rust.el ends here
