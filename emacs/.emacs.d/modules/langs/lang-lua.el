;;; lang-lua.el --- Lua Language Support -*- lexical-binding: t -*-
;;; Version: 3.1.1 | PATCH: LSP vars moved from :init to :config (FIX #5)
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "lua"
  :tier 2
  :lsp-server "lua-language-server"
  :formatter "stylua"
  :test-cmd "busted"
  :repl "lua"
  :modes '(lua-mode))

(when (emacs-ide-dev-lang-enabled-p "lua")

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :init
  (setq lua-indent-level 2
        lua-default-application "lua"))

(use-package lsp-lua
  :after (lua-mode lsp-mode)
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (executable-find "lua-language-server"))
  :hook (lua-mode . lsp-deferred)
  :init
  ;; Nothing here
  (message "")
  :config
  ;; FIX #5: MOVED FROM :init — now lsp-lua is loaded
  (when (boundp 'lsp-lua-hint-enable)
    (setq lsp-lua-hint-enable t))
  (when (boundp 'lsp-lua-hint-setType)
    (setq lsp-lua-hint-setType t))
  (when (boundp 'lsp-lua-hint-paramType)
    (setq lsp-lua-hint-paramType t))
  (when (boundp 'lsp-lua-hint-paramName)
    (setq lsp-lua-hint-paramName t))
  (when (boundp 'lsp-lua-hint-await)
    (setq lsp-lua-hint-await t))
  (when (boundp 'lsp-lua-hover-enumsLimit)
    (setq lsp-lua-hover-enumsLimit 10))
  (when (boundp 'lsp-lua-hover-previewFields)
    (setq lsp-lua-hover-previewFields 100))
  (when (boundp 'lsp-lua-hover-viewStringMax)
    (setq lsp-lua-hover-viewStringMax 1000))
  (when (boundp 'lsp-lua-runtime-version)
    (setq lsp-lua-runtime-version "Lua 5.1"))
  (when (boundp 'lsp-lua-diagnostics-globals)
    (setq lsp-lua-diagnostics-globals '()))
  (when (boundp 'lsp-lua-workspace-checkThirdParty)
    (setq lsp-lua-workspace-checkThirdParty t)))

(with-eval-after-load 'apheleia
  (when (executable-find "stylua")
    (unless (assq 'stylua apheleia-formatters)
      (push '(stylua "stylua" "-") apheleia-formatters))
    (setf (alist-get 'lua-mode apheleia-mode-alist) 'stylua)))

(defun emacs-ide-lua-repl ()
  "Open a Lua REPL."
  (interactive)
  (if (executable-find "lua")
      (progn (require 'comint)
             (make-comint "lua-repl" "lua")
             (switch-to-buffer "*lua-repl*"))
    (message "lang-lua: lua not found on PATH")))

(with-eval-after-load 'lua-mode
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'lua-mode
      :launch         #'emacs-ide-lua-repl
      :buffer-name    "*lua-repl*"
      :send-region-fn nil)))

(with-eval-after-load 'dap-mode
  (when (executable-find "lua-debug-server")
    (require 'dap-lua nil t)))

(defun emacs-ide-lua-test-file ()
  "Run busted on the current Lua file."
  (interactive)
  (if (and (executable-find "busted") (buffer-file-name))
      (compile (format "busted %s" (shell-quote-argument (buffer-file-name))))
    (message "lang-lua: busted not found")))

(defun emacs-ide-lua-test-project ()
  "Run busted on the whole project."
  (interactive)
  (if (executable-find "busted")
      (compile "busted .")
    (message "lang-lua: busted not found")))

(with-eval-after-load 'tools-test-runner-registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'lua-mode
      :file-fn    #'emacs-ide-lua-test-file
      :project-fn #'emacs-ide-lua-test-project)))

)

(provide 'lang-lua)
;;; lang-lua.el ends here
