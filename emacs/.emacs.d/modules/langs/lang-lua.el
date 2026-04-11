;;; lang-lua.el --- Lua Language Support -*- lexical-binding: t -*-
;;; Commentary:
;;; Lua development environment with lua-language-server LSP support.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (recalibration):
;;;   - FIX-REQUIRE: Was (require 'emacs-ide-dev); feature is provided as
;;;     'core-dev. Fixed to (require 'core-dev).
;;;   - FIX-API-REGISTER-LANG: emacs-ide-dev-register-lang does not exist in
;;;     core-dev.el. The correct function is emacs-ide-dev-register. Rewired
;;;     to use the real API with the correct keyword arguments.
;;;   - FIX-API-TEST-RUNNER: emacs-ide-dev-register-test-runner does not exist
;;;     in core-dev.el. The correct function is emacs-ide-test-register-runner
;;;     from tools-test-runner-registry.el. All test runner registration now
;;;     uses the correct function via with-eval-after-load.
;;;   - FIX-LSP-INIT-VARS: lsp-lua-hint-* variables moved from :init to :config
;;;     (retained from prior audit).
;;; Code:

(require 'core-dev)

;; ============================================================================
;; REGISTRATION
;; FIX-API-REGISTER-LANG: emacs-ide-dev-register is the correct function.
;; ============================================================================
(emacs-ide-dev-register "lua"
  :tier 2
  :lsp-server "lua-language-server"
  :formatter "stylua"
  :test-cmd "busted"
  :repl "lua"
  :modes '(lua-mode))

(when (emacs-ide-dev-lang-enabled-p "lua")

;; ============================================================================
;; LUA MODE
;; ============================================================================
(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :init
  (setq lua-indent-level 2
        lua-default-application "lua"))

;; ============================================================================
;; LSP: LUA LANGUAGE SERVER
;; FIX-LSP-INIT-VARS: Moved from :init to :config so lsp-lua is loaded first.
;; ============================================================================
(use-package lsp-lua
  :after (lua-mode lsp-mode)
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (executable-find "lua-language-server"))
  :hook (lua-mode . lsp-deferred)
  :config
  (setq lsp-lua-hint-enable                  t
        lsp-lua-hint-setType                 t
        lsp-lua-hint-paramType               t
        lsp-lua-hint-paramName               t
        lsp-lua-hint-await                   t
        lsp-lua-hover-enumsLimit             10
        lsp-lua-hover-previewFields          100
        lsp-lua-hover-viewStringMax          1000
        lsp-lua-runtime-version              "Lua 5.1"
        lsp-lua-diagnostics-globals          '()
        lsp-lua-workspace-checkThirdParty    t))

;; ============================================================================
;; FORMATTER — stylua via apheleia
;; ============================================================================
(with-eval-after-load 'apheleia
  (when (executable-find "stylua")
    (unless (assq 'stylua apheleia-formatters)
      (push '(stylua "stylua" "-") apheleia-formatters))
    (setf (alist-get 'lua-mode apheleia-mode-alist) 'stylua)))

;; ============================================================================
;; REPL
;; ============================================================================
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

;; ============================================================================
;; DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-mode
  (when (executable-find "lua-debug-server")
    (require 'dap-lua nil t)))

;; ============================================================================
;; TEST RUNNER
;; FIX-API-TEST-RUNNER: emacs-ide-dev-register-test-runner does not exist.
;; Use emacs-ide-test-register-runner from tools-test-runner-registry.el.
;; ============================================================================
(defun emacs-ide-lua-test-file ()
  "Run busted on the current Lua file."
  (interactive)
  (if (and (executable-find "busted") (buffer-file-name))
      (compile (format "busted %s" (shell-quote-argument (buffer-file-name))))
    (message "lang-lua: busted not found. Install: luarocks install busted")))

(defun emacs-ide-lua-test-project ()
  "Run busted on the whole project."
  (interactive)
  (if (executable-find "busted")
      (compile "busted .")
    (message "lang-lua: busted not found. Install: luarocks install busted")))

(with-eval-after-load 'tools-test-runner-registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'lua-mode
      :file-fn    #'emacs-ide-lua-test-file
      :project-fn #'emacs-ide-lua-test-project)))

) ;; end (when (emacs-ide-dev-lang-enabled-p "lua"))

(provide 'lang-lua)
;;; lang-lua.el ends here
