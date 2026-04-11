;;; lang-rust.el --- Rust Language Support -*- lexical-binding: t -*-
;;; Commentary:
;;; Complete Rust IDE with rust-analyzer LSP, cargo integration, and debugging.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (recalibration):
;;;   - FIX-REQUIRE: Was (require 'emacs-ide-dev); feature is provided as
;;;     'core-dev. Fixed to (require 'core-dev).
;;;   - FIX-API-REGISTER-LANG: emacs-ide-dev-register-lang does not exist in
;;;     core-dev.el. The correct function is emacs-ide-dev-register. Rewired.
;;;   - FIX-API-TEST-RUNNER: emacs-ide-dev-register-test-runner does not exist
;;;     in core-dev.el. Replaced with emacs-ide-test-register-runner via
;;;     with-eval-after-load 'tools-test-runner-registry.
;;;   - FIX-DAP-VS-CODE-VARS: DAP template used "${workspaceFolder}" and
;;;     "${workspaceFolderBasename}" which are VS Code variables not expanded
;;;     by dap-mode. Replaced with lambdas reading project root at launch time.
;;;   - FIX-LSP-INIT-VARS: lsp-rust-analyzer-* variables moved from :init to
;;;     :config (retained from prior audit).
;;; Code:

(require 'core-dev)

;; ============================================================================
;; REGISTRATION
;; FIX-API-REGISTER-LANG: emacs-ide-dev-register is the correct function.
;; ============================================================================
(emacs-ide-dev-register "rust"
  :tier 1
  :lsp-server "rust-analyzer"
  :formatter "rustfmt"
  :test-cmd "cargo test"
  :repl "evcxr"
  :modes '(rust-mode rust-ts-mode))

(when (emacs-ide-dev-lang-enabled-p "rust")

;; ============================================================================
;; RUST MODE
;; ============================================================================
(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save nil)  ; apheleia owns formatting
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

;; ============================================================================
;; LSP: RUST-ANALYZER
;; FIX-LSP-INIT-VARS: Moved from :init to :config.
;; ============================================================================
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

;; ============================================================================
;; CARGO — Rust build system
;; ============================================================================
(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode)
  :init
  (setq cargo-process-echo-in-minibuffer t))

;; ============================================================================
;; FORMATTER — rustfmt via apheleia
;; ============================================================================
(with-eval-after-load 'apheleia
  (when (executable-find "rustfmt")
    (setf (alist-get 'rust-mode    apheleia-mode-alist) 'rustfmt)
    (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt)))

;; ============================================================================
;; TEST RUNNER
;; FIX-API-TEST-RUNNER: emacs-ide-dev-register-test-runner does not exist.
;; Use emacs-ide-test-register-runner from tools-test-runner-registry.el.
;; ============================================================================
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

;; ============================================================================
;; DEBUGGING — LLDB
;; FIX-DAP-VS-CODE-VARS: "${workspaceFolder}" / "${workspaceFolderBasename}"
;; are VS Code variables not expanded by dap-mode. Replaced with lambdas.
;; ============================================================================
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

) ;; end (when (emacs-ide-dev-lang-enabled-p "rust"))

(provide 'lang-rust)
;;; lang-rust.el ends here
