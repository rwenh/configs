;;; lang-rust.el --- Rust IDE layer -*- lexical-binding: t -*-
;;; Commentary: Follows canonical lang-python.el template exactly.
;;; Version: 1.0.0
;;; Code:

(require 'core-dev)

;; 1. Registration
(emacs-ide-dev-register "rust"
  :tier 1 :lsp-server "rust-analyzer"
  :formatter "rustfmt" :test-cmd "cargo test" :repl nil
  :modes '(rust-mode rust-ts-mode))

;; 2. Config guard
(when (emacs-ide-dev-lang-enabled-p "rust")

;; 3. Treesitter
(emacs-ide-dev-ensure-treesit 'rust)

;; 4. Major mode
(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save nil  ; apheleia owns formatting
        rust-indent-offset   4)
  :config
  (defun emacs-ide-rust-run ()
    "cargo run in project root."
    (interactive)
    (if (executable-find "cargo") (compile "cargo run")
      (message "lang-rust: cargo not found")))
  (defun emacs-ide-rust-build ()
    (interactive)
    (if (executable-find "cargo") (compile "cargo build")
      (message "lang-rust: cargo not found")))
  (emacs-ide-dev-bind-compile rust-mode-map #'emacs-ide-rust-run)
  (define-key rust-mode-map (kbd "C-c C-b") #'emacs-ide-rust-build))

;; 5. LSP — rust-analyzer
(use-package lsp-mode
  :after rust-mode
  :hook ((rust-mode rust-ts-mode) . lsp-deferred)
  :init
  (setq lsp-rust-analyzer-cargo-watch-command       "clippy"
        lsp-rust-analyzer-display-inlay-hints        t
        lsp-rust-analyzer-inlay-hints-mode           t
        lsp-rust-analyzer-display-chaining-hints     t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-import-granularity         "module"
        lsp-rust-analyzer-server-display-inlay-hints t))

;; 6. Formatter
(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'rustfmt 'rust-mode)
  (emacs-ide-dev-attach-formatter 'rustfmt 'rust-ts-mode))

;; 7. REPL — evcxr
(defun emacs-ide-rust-repl ()
  (interactive)
  (if (executable-find "evcxr")
      (progn (require 'comint)
             (make-comint "rust-repl" "evcxr")
             (switch-to-buffer "*rust-repl*"))
    (message "lang-rust: install evcxr for REPL: cargo install evcxr_repl")))
(with-eval-after-load 'rust-mode
  (emacs-ide-dev-attach-repl rust-mode-map #'emacs-ide-rust-repl))

;; 8. Test runner
(defun emacs-ide-rust-test-file ()
  (interactive)
  (if (executable-find "cargo") (compile "cargo test")
    (message "lang-rust: cargo not found")))
(defun emacs-ide-rust-test-at-point ()
  (interactive)
  (let ((fn (which-function)))
    (if (and fn (executable-find "cargo"))
        (compile (format "cargo test %s" (shell-quote-argument fn)))
      (message "lang-rust: cannot determine test name at point"))))
(with-eval-after-load 'tools-test
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'rust-mode
      :file-fn    #'emacs-ide-rust-test-file
      :project-fn #'emacs-ide-rust-test-file
      :point-fn   #'emacs-ide-rust-test-at-point)))

;; 9. Debugger — lldb
(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "Rust :: LLDB" 'dap-lldb)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template
     "Rust :: LLDB :: binary"
     (list :type     "lldb"
           :request  "launch"
           :name     "Rust binary"
           :program  "${workspaceFolder}/target/debug/${workspaceFolderBasename}"
           :cwd      "${workspaceFolder}"))))

;; 10. Project mgmt — cargo
(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

) ;; end rust guard

(provide 'lang-rust)
;;; lang-rust.el ends here
