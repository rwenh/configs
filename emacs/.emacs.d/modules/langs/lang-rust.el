;;; lang-rust.el --- Rust Language Support -*- lexical-binding: t -*-
;;; Version: 3.1.0 | Fix: LSP vars in :config
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
  :config
  ;; FIX v3.0.4: moved from :init to :config where lsp-rust is guaranteed loaded
  (setq lsp-rust-analyzer-inlay-hints-mode t
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-checkOnSave-command "clippy"))

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
