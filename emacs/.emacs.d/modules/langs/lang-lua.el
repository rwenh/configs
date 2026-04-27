;;; lang-lua.el --- Lua Language Support -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;;   Also: lua-ts-mode added to LSP hook and REPL registration.
;;;
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "lua"
  :tier 2
  :lsp-server "lua-language-server"
  :formatter  "stylua"
  :test-cmd   "busted"
  :repl       "lua"
  :modes      '(lua-mode))

(when (emacs-ide-dev-lang-enabled-p "lua")

;;;; ── lua-mode ────────────────────────────────────────────────────────────────

(use-package lua-mode
  :mode        "\\.lua\\'"
  :interpreter "lua"
  :init
  (setq lua-indent-level      2
        lua-default-application "lua"))

;;;; ── LSP (lua-language-server) ───────────────────────────────────────────────

(use-package lsp-lua
  :after (lua-mode lsp-mode)
  :if    (and (bound-and-true-p emacs-ide-lsp-enable)
              (executable-find "lua-language-server"))
  :hook  (lua-mode . lsp-deferred)
  :config
  ;; ── Hover display limits ─────────────────────────────────────────────────
  (when (boundp 'lsp-lua-hover-enumsLimit)
    (setq lsp-lua-hover-enumsLimit 10))
  (when (boundp 'lsp-lua-hover-previewFields)
    (setq lsp-lua-hover-previewFields 100))
  (when (boundp 'lsp-lua-hover-viewStringMax)
    (setq lsp-lua-hover-viewStringMax 1000))
  ;; ── Runtime version ──────────────────────────────────────────────────────
  ;; Read from lang-settings.lua.runtime-version if available; fall back to 5.4.
  (when (boundp 'lsp-lua-runtime-version)
    (setq lsp-lua-runtime-version
          (let* ((settings (and (fboundp 'emacs-ide-dev--config-lang-settings)
                                (emacs-ide-dev--config-lang-settings "lua")))
                 (ver (and settings (cdr (assoc 'runtime-version settings)))))
            (if (stringp ver) ver "Lua 5.4"))))
  ;; ── Diagnostics / workspace ──────────────────────────────────────────────
  (when (boundp 'lsp-lua-diagnostics-globals)
    (setq lsp-lua-diagnostics-globals '()))
  (when (boundp 'lsp-lua-workspace-checkThirdParty)
    (setq lsp-lua-workspace-checkThirdParty nil)))

;;;; ── Formatter ───────────────────────────────────────────────────────────────

(with-eval-after-load 'apheleia
  (when (executable-find "stylua")
    (unless (assq 'stylua apheleia-formatters)
      (push '(stylua "stylua" "-") apheleia-formatters))
    (setf (alist-get 'lua-mode apheleia-mode-alist) 'stylua)))

;;;; ── REPL ────────────────────────────────────────────────────────────────────

(defun emacs-ide-lua-repl ()
  "Open a Lua REPL."
  (interactive)
  (if (executable-find "lua")
      (progn
        (require 'comint)
        (make-comint "lua-repl" "lua")
        (switch-to-buffer "*lua-repl*"))
    (message "lang-lua: lua not found on PATH")))

(with-eval-after-load 'tools-repl
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'lua-mode
      :launch         #'emacs-ide-lua-repl
      :buffer-name    "*lua-repl*"
      :send-region-fn nil)))

;;;; ── DAP ─────────────────────────────────────────────────────────────────────

(with-eval-after-load 'dap-mode
  (when (executable-find "lua-debug-server")
    (require 'dap-lua nil t)))

;;;; ── Test runners ────────────────────────────────────────────────────────────

(defun emacs-ide-lua-test-file ()
  "Run busted on the current Lua file."
  (interactive)
  (if (and (executable-find "busted") (buffer-file-name))
      (compile (format "busted %s"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-lua: busted not found on PATH")))

(defun emacs-ide-lua-test-project ()
  "Run busted on the whole project."
  (interactive)
  (if (executable-find "busted")
      (compile "busted .")
    (message "lang-lua: busted not found on PATH")))

(with-eval-after-load 'tools-test-runner-registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'lua-mode
      :file-fn    #'emacs-ide-lua-test-file
      :project-fn #'emacs-ide-lua-test-project)))

) ;; end lua-enabled

(provide 'lang-lua)
;;; lang-lua.el ends here
