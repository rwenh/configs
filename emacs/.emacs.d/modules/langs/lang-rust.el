;;; lang-rust.el --- Rust Language Support -*- lexical-binding: t -*-
;;; Commentary:
;;; Complete Rust IDE with rust-analyzer LSP, cargo integration, and debugging.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.2 to 3.0.4.
;;;   - FIX-LSP-INIT-VARS: lsp-rust-analyzer-* variables moved from :init to
;;;     :config block. In :init with (boundp) guards, these variables are set
;;;     BEFORE lsp-rust is loaded, so they take no effect. Moved to :config
;;;     where lsp-rust is guaranteed to be loaded and registration takes place.
;;; Code:

(require 'emacs-ide-dev)

;; ============================================================================
;; RUST MODE
;; ============================================================================
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (when (emacs-ide-dev-lang-enabled-p "rust")
    (emacs-ide-dev-register-lang
     "rust"
     :modes '(rust-mode rust-ts-mode)
     :lsp t
     :repl "evcxr")))

;; ============================================================================
;; LSP: RUST-ANALYZER
;; ============================================================================
(use-package lsp-rust-analyzer
  :after (rust-mode lsp-mode)
  :if (executable-find "rust-analyzer")
  ;; FIX-LSP-INIT-VARS: Moved configuration from :init to :config
  ;; so that lsp-rust is guaranteed to be loaded before these settings apply.
  :config
  (setq lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-inlay-hints-mode t
        lsp-rust-analyzer-inlay-hints-render-colons t
        lsp-rust-analyzer-inlay-hints-render-closing-brace t
        lsp-rust-analyzer-cargo-override-command '("clippy" "--all-targets" "--all-features" "--")
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-cargo-watch-args "--all-targets --all-features"
        lsp-rust-analyzer-checkOnSave-command "clippy"
        lsp-rust-analyzer-checkOnSave-extraArgs "--all-targets --all-features"
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-completion-auto-self-enable t
        lsp-rust-analyzer-completion-add-call-parenthesis t
        lsp-rust-analyzer-completion-add-call-argument-snippets t
        lsp-rust-analyzer-hover-documentation t))

;; ============================================================================
;; CARGO — Rust build system
;; ============================================================================
(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode)
  :init
  (setq cargo-process-echo-in-minibuffer t))

;; ============================================================================
;; DEBUGGING
;; ============================================================================
(use-package dap-lldb
  :after dap-mode
  :if (executable-find "lldb-mi")
  :config
  ;; Register LLDB for Rust debugging
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :cwd "${workspaceFolder}"
         :name "Rust::LLDB Run"
         :request "launch"
         :program "${workspaceFolder}/target/debug/${workspaceFolderBasename}"
         :args []
         :stopOnEntry :json-false)))

;; ============================================================================
;; TESTING
;; ============================================================================
(use-package emacs-ide-dev-lang-test
  :config
  (emacs-ide-dev-register-test-runner
   "rust"
   :command '("cargo" "test" "--" "--test-threads=1")
   :watch-command '("cargo" "test" "--" "--test-threads=1" "--nocapture")))

(provide 'lang-rust)
;;; lang-rust.el ends here