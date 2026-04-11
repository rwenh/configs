;;; lang-lua.el --- Lua Language Support -*- lexical-binding: t -*-
;;; Commentary:
;;; Lua development environment with lua-language-server LSP support.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.0 to 3.0.4.
;;;   - FIX-LSP-INIT-VARS: lsp-lua-hint-* variables moved from :init to
;;;     :config. In :init with (boundp) guards, these are set BEFORE lsp-lua
;;;     loads, so they take no effect. Moved to :config where lsp-lua is
;;;     guaranteed to be loaded and registration occurs.
;;; Code:

(require 'emacs-ide-dev)

;; ============================================================================
;; LUA MODE
;; ============================================================================
(use-package lua-mode
  :mode "\\.lua\\'"
  :config
  (when (emacs-ide-dev-lang-enabled-p "lua")
    (emacs-ide-dev-register-lang
     "lua"
     :modes '(lua-mode)
     :lsp t
     :repl "lua")))

;; ============================================================================
;; LSP: LUA LANGUAGE SERVER
;; ============================================================================
(use-package lsp-lua
  :after (lua-mode lsp-mode)
  :if (executable-find "lua-language-server")
  ;; FIX-LSP-INIT-VARS: Moved configuration from :init to :config
  ;; so that lsp-lua is guaranteed to be loaded before settings apply.
  :config
  ;; Inlay hints configuration
  (setq lsp-lua-hint-enable t
        lsp-lua-hint-setType t
        lsp-lua-hint-paramType t
        lsp-lua-hint-paramName t
        lsp-lua-hint-await t
        lsp-lua-hover-enumsLimit 10
        lsp-lua-hover-previewFields 100
        lsp-lua-hover-viewStringMax 1000
        lsp-lua-runtime-version "Lua 5.1"
        lsp-lua-diagnostics-globals '()
        lsp-lua-workspace-checkThirdParty t
        lsp-lua-workspace-library-uri-mapping '()))

;; ============================================================================
;; DEBUGGING
;; ============================================================================
(use-package dap-lua
  :after dap-mode
  :if (executable-find "lua-debug-server")
  :config
  (require 'dap-lua))

;; ============================================================================
;; TESTING
;; ============================================================================
(use-package emacs-ide-dev-lang-test
  :config
  (emacs-ide-dev-register-test-runner
   "lua"
   :command '("lua" "test.lua")
   :watch-command '("lua" "test.lua")))

(provide 'lang-lua)
;;; lang-lua.el ends here