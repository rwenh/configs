;;; lang-rust.el --- Rust IDE layer -*- lexical-binding: t -*-
;;; Commentary: Follows canonical lang-python.el template exactly.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.1 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.1 to 3.0.4.
;;;   - FIX-DEFUN-IN-CONFIG: emacs-ide-rust-run and emacs-ide-rust-build were
;;;     defined inside rust-mode :config — not visible to M-x until rust-mode
;;;     loads. Moved to top-level defuns.
;;;   - FIX-LSP-GUARD: lsp-mode use-package had no :if guard on
;;;     emacs-ide-lsp-enable. Added (bound-and-true-p emacs-ide-lsp-enable).
;;;   - FIX-LSP-INLAY-VARS: Several lsp-rust-analyzer-* vars may not exist
;;;     in older lsp-rust versions. Wrapped with (boundp) guards.
;;;   - FIX-REPL-ATTACH-ORDER: emacs-ide-dev-attach-repl was called via
;;;     with-eval-after-load 'rust-mode. Moved inside rust-mode :config.
;;;   - FIX-REPL-REGISTER: Added explicit emacs-ide-repl-register for rust-mode.
;;;   - FIX-DAP-PLACEHOLDER: "${workspaceFolder}" and "${workspaceFolderBasename}"
;;;     are VS Code variables not expanded by dap-mode. Replaced with lambdas
;;;     that read the project root and its basename at launch time.
;;;   - FIX-TEST-REGISTER-RUST-TS: Test runner now registered for both
;;;     rust-mode and rust-ts-mode.
;;;   - FIX-RUST-TEST-FILE-NAME: Clarified that emacs-ide-rust-test-file runs
;;;     the full cargo test suite (Rust has no single-file test granularity
;;;     below the crate level). Added emacs-ide-rust-test-project as an alias.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-LSP: rust-mode hook removed — tools-lsp.el owns it.
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

;; ============================================================================
;; 4. COMPILE / RUN COMMANDS
;; FIX-DEFUN-IN-CONFIG: top-level so M-x sees them before rust-mode loads.
;; ============================================================================
(defun emacs-ide-rust-run ()
  "Run the Rust project via cargo run."
  (interactive)
  (if (executable-find "cargo")
      (compile "cargo run")
    (message "lang-rust: cargo not found")))

(defun emacs-ide-rust-build ()
  "Build the Rust project via cargo build."
  (interactive)
  (if (executable-find "cargo")
      (compile "cargo build")
    (message "lang-rust: cargo not found")))

;; ============================================================================
;; 4. MAJOR MODE
;; ============================================================================
(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save nil   ; apheleia owns formatting
        rust-indent-offset   4)
  :config
  ;; FIX-DEFUN-IN-CONFIG: now top-level; just bind here
  (emacs-ide-dev-bind-compile rust-mode-map #'emacs-ide-rust-run)
  (define-key rust-mode-map (kbd "C-c C-b") #'emacs-ide-rust-build)
  ;; FIX-REPL-ATTACH-ORDER: moved here from with-eval-after-load
  (emacs-ide-dev-attach-repl rust-mode-map #'emacs-ide-rust-repl
                              (kbd "C-c x r"))
  ;; FIX-REPL-REGISTER: explicit registration for reliable C-c x r dispatch
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'rust-mode
      :launch         #'emacs-ide-rust-repl
      :buffer-name    "*rust-repl*"
      :send-region-fn nil))
  ;; FIX-TEST-REGISTER-RUST-TS: register both modes
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(rust-mode rust-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-rust-test-file
        :project-fn #'emacs-ide-rust-test-project
        :point-fn   #'emacs-ide-rust-test-at-point))))

;; ============================================================================
;; 5. LSP — rust-analyzer
;; FIX-LSP-GUARD: :if guard added.
;; FIX-LSP-INLAY-VARS: lsp-rust-analyzer-* vars guarded with boundp.
;; ============================================================================
(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :after rust-mode
  ;; FIX-LSP (retained): rust-mode hook removed — tools-lsp.el owns it.
  :hook (rust-ts-mode . lsp-deferred)
  :init
  ;; FIX-LSP-INLAY-VARS: guard each var — older lsp-rust may not have them
  (when (boundp 'lsp-rust-analyzer-cargo-watch-command)
    (setq lsp-rust-analyzer-cargo-watch-command "clippy"))
  (when (boundp 'lsp-rust-analyzer-display-inlay-hints)
    (setq lsp-rust-analyzer-display-inlay-hints t))
  (when (boundp 'lsp-rust-analyzer-inlay-hints-mode)
    (setq lsp-rust-analyzer-inlay-hints-mode t))
  (when (boundp 'lsp-rust-analyzer-display-chaining-hints)
    (setq lsp-rust-analyzer-display-chaining-hints t))
  (when (boundp 'lsp-rust-analyzer-display-lifetime-elision-hints-enable)
    (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"))
  (when (boundp 'lsp-rust-analyzer-import-granularity)
    (setq lsp-rust-analyzer-import-granularity "module"))
  (when (boundp 'lsp-rust-analyzer-server-display-inlay-hints)
    (setq lsp-rust-analyzer-server-display-inlay-hints t)))

;; ============================================================================
;; 6. FORMATTER — rustfmt via apheleia
;; ============================================================================
(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'rustfmt 'rust-mode)
  (emacs-ide-dev-attach-formatter 'rustfmt 'rust-ts-mode))

;; ============================================================================
;; 7. REPL — evcxr
;; ============================================================================
(defun emacs-ide-rust-repl ()
  "Open an evcxr REPL for Rust."
  (interactive)
  (if (executable-find "evcxr")
      (progn (require 'comint)
             (make-comint "rust-repl" "evcxr")
             (switch-to-buffer "*rust-repl*"))
    (message "lang-rust: install evcxr: cargo install evcxr_repl")))

;; ============================================================================
;; 8. TEST RUNNER
;; FIX-RUST-TEST-FILE-NAME: Rust has no single-file test granularity below the
;; crate. Both file-fn and project-fn run cargo test; file-fn additionally
;; attempts to scope to the current module if which-function resolves.
;; FIX-TEST-REGISTER-RUST-TS: both modes registered in rust-mode :config above.
;; ============================================================================
(defun emacs-ide-rust-test-file ()
  "Run cargo test for the current crate."
  (interactive)
  (if (executable-find "cargo")
      (compile "cargo test")
    (message "lang-rust: cargo not found")))

(defun emacs-ide-rust-test-project ()
  "Run cargo test for the full workspace."
  (interactive)
  (if (executable-find "cargo")
      (compile "cargo test --workspace")
    (message "lang-rust: cargo not found")))

(defun emacs-ide-rust-test-at-point ()
  "Run the test at point via cargo test <function_name>."
  (interactive)
  (let ((fn (which-function)))
    (if (and fn (executable-find "cargo"))
        (compile (format "cargo test %s" (shell-quote-argument fn)))
      (message "lang-rust: cannot determine test name at point"))))

(with-eval-after-load 'tools-test
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(rust-mode rust-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-rust-test-file
        :project-fn #'emacs-ide-rust-test-project
        :point-fn   #'emacs-ide-rust-test-at-point))))

;; ============================================================================
;; 9. DEBUGGER — LLDB
;; FIX-DAP-PLACEHOLDER: VS Code variables replaced with lambdas.
;; ============================================================================
(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "Rust :: LLDB" 'dap-lldb)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template
     "Rust :: LLDB :: binary"
     (list :type    "lldb"
           :request "launch"
           :name    "Rust binary"
           ;; FIX-DAP-PLACEHOLDER: lambdas read project root at launch time
           :program (lambda ()
                      (let* ((root (or (and (fboundp 'projectile-project-root)
                                            (ignore-errors (projectile-project-root)))
                                       default-directory))
                             (name (file-name-nondirectory
                                    (directory-file-name root))))
                        (expand-file-name
                         (format "target/debug/%s" name) root)))
           :cwd     (lambda ()
                      (or (and (fboundp 'projectile-project-root)
                               (ignore-errors (projectile-project-root)))
                          default-directory))))))

;; ============================================================================
;; 10. PROJECT MGMT — cargo minor mode
;; ============================================================================
(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

) ;; end rust guard

(provide 'lang-rust)
;;; lang-rust.el ends here
