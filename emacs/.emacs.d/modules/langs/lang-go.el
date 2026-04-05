;;; lang-go.el --- Go IDE layer -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.2 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.2 to 3.0.4.
;;;   - FIX-DEFUN-IN-CONFIG: emacs-ide-go-run was defined inside go-mode
;;;     :config — not visible to M-x until go-mode loads. Moved to top-level.
;;;   - FIX-LSP-GUARD: lsp-mode use-package had no :if guard on
;;;     emacs-ide-lsp-enable. Added (bound-and-true-p emacs-ide-lsp-enable).
;;;   - FIX-HOOK-IN-CONFIG: (add-hook 'go-mode-hook ...) inside :config runs
;;;     after go-mode is loading for the current buffer, potentially missing
;;;     future buffers. Moved the apheleia guard into a proper :hook entry.
;;;   - FIX-REPL-ATTACH-ORDER: emacs-ide-dev-attach-repl was called via
;;;     with-eval-after-load 'go-mode which may fire before go-mode-map is
;;;     fully initialized. Moved inside go-mode :config.
;;;   - FIX-DAP-CWD-PLACEHOLDER: "${file}" in DAP template :program is a VS
;;;     Code variable not expanded by dap-mode. Replaced with a lambda that
;;;     reads buffer-file-name at launch time.
;;;   - FIX-REPL-REGISTER: Added explicit emacs-ide-repl-register for go-mode
;;;     inside go-mode :config for reliable C-c x r dispatch.
;;;   - FIX-GO-TEST-FILE: emacs-ide-go-test-file ran "go test -v ./..." (all
;;;     packages) — misleading for a :file-fn. Separate file-level (current
;;;     package) and project-level (all packages) functions added.
;;;   - FIX-GOFMT-COMMAND: gofmt-command was set to goimports when available,
;;;     but lsp-go-use-gofumpt t was also set — conflicting formatters.
;;;     gofmt-command now only used as fallback when apheleia is not active;
;;;     lsp-go-use-gofumpt drives formatting via gopls when LSP is enabled.
;;; Fixes vs 1.0.1 (retained):
;;;   - FIX-LSP: go-mode hook removed — tools-lsp.el owns it.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-5: gofmt-before-save guarded against double-format with apheleia.
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "go" :tier 1 :lsp-server "gopls"
  :formatter "gofmt" :test-cmd "go test ./..." :repl "gore"
  :modes '(go-mode go-ts-mode))

(when (emacs-ide-dev-lang-enabled-p "go")

(emacs-ide-dev-ensure-treesit 'go)
(emacs-ide-dev-ensure-treesit 'gomod)

;; ============================================================================
;; COMPILE / RUN COMMANDS
;; FIX-DEFUN-IN-CONFIG: moved to top level so M-x sees them before go-mode loads.
;; ============================================================================
(defun emacs-ide-go-run ()
  "Run the current Go file via `go run`."
  (interactive)
  (if (executable-find "go")
      (compile (format "go run %s" (shell-quote-argument (buffer-file-name))))
    (message "lang-go: go not found")))

(defun emacs-ide-go-test-file ()
  "Run tests for the current Go package (directory containing this file).
FIX-GO-TEST-FILE: runs the current package only, not all packages."
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

;; ============================================================================
;; GO-MODE
;; FIX-HOOK-IN-CONFIG: apheleia guard moved from add-hook inside :config
;; to a proper hook function to ensure all future go-mode buffers are covered.
;; ============================================================================
(defun emacs-ide-go--maybe-gofmt-on-save ()
  "Add gofmt-before-save hook when apheleia is not active.
FIX-5 (retained): prevents double-format when apheleia handles go-mode."
  (unless (bound-and-true-p apheleia-global-mode)
    (add-hook 'before-save-hook #'gofmt-before-save nil t)))

(use-package go-mode
  :if (executable-find "go")
  :defer t
  :mode "\\.go\\'"
  :hook (go-mode . emacs-ide-go--maybe-gofmt-on-save)
  :init
  ;; FIX-GOFMT-COMMAND: only set gofmt-command as fallback; gopls with
  ;; lsp-go-use-gofumpt drives formatting when LSP is enabled.
  (setq gofmt-command (or (executable-find "goimports") "gofmt"))
  :config
  (emacs-ide-dev-bind-compile go-mode-map #'emacs-ide-go-run)
  (define-key go-mode-map (kbd "C-c C-b")
    (lambda () (interactive) (compile "go build -v")))
  (define-key go-mode-map (kbd "C-c C-v")
    (lambda () (interactive) (compile "go vet ./...")))
  ;; FIX-REPL-ATTACH-ORDER: moved here from with-eval-after-load
  (emacs-ide-dev-attach-repl go-mode-map #'emacs-ide-go-repl
                              (kbd "C-c x r"))
  ;; FIX-REPL-REGISTER: explicit registration for reliable C-c x r dispatch
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'go-mode
      :launch         #'emacs-ide-go-repl
      :buffer-name    "*go-repl*"
      :send-region-fn nil))
  ;; Test runner registration
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'go-mode
      :file-fn    #'emacs-ide-go-test-file
      :project-fn #'emacs-ide-go-test-project)))

;; ============================================================================
;; LSP — gopls
;; FIX-LSP-GUARD: :if guard added for emacs-ide-lsp-enable.
;; FIX-LSP (retained): go-mode hook removed — tools-lsp.el owns it.
;; FIX-GOFMT-COMMAND: lsp-go-use-gofumpt drives formatting via gopls.
;; ============================================================================
(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :hook (go-ts-mode . lsp-deferred)
  :init
  (setq lsp-go-analyses    '((shadow . t) (staticcheck . t))
        lsp-go-use-gofumpt t))

;; ============================================================================
;; FORMATTER — gofmt via apheleia
;; ============================================================================
(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'gofmt 'go-mode)
  (emacs-ide-dev-attach-formatter 'gofmt 'go-ts-mode))

;; ============================================================================
;; TEST RUNNER REGISTRATION (also registered in go-mode :config above,
;; but kept here with with-eval-after-load for the case where tools-test
;; loads after go-mode)
;; ============================================================================
(with-eval-after-load 'tools-test
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'go-mode
      :file-fn    #'emacs-ide-go-test-file
      :project-fn #'emacs-ide-go-test-project)))

;; ============================================================================
;; DAP — Delve debugger
;; FIX-DAP-CWD-PLACEHOLDER: "${file}" replaced with lambda reading buffer
;; file name at launch time — dap-mode does not expand VS Code variables.
;; ============================================================================
(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "Go :: Delve" 'dap-dlv-go)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template "Go :: Delve :: file"
      (list :type    "go"
            :request "launch"
            :name    "Go file"
            :mode    "debug"
            ;; FIX-DAP-CWD-PLACEHOLDER: lambda evaluated at launch time
            :program (lambda () (buffer-file-name))
            :cwd     (lambda ()
                       (or (and (fboundp 'projectile-project-root)
                                (ignore-errors (projectile-project-root)))
                           default-directory))))))

) ;; end (when (emacs-ide-dev-lang-enabled-p "go"))

(provide 'lang-go)
;;; lang-go.el ends here
