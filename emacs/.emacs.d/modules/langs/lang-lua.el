;;; lang-lua.el --- Lua IDE layer -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.1 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.1 to 3.0.4.
;;;   - FIX-DEFUN-IN-CONFIG: emacs-ide-lua-run was defined inside lua-mode
;;;     :config — not visible to M-x until lua-mode loads. Moved to top-level.
;;;   - FIX-REPL-ATTACH-ORDER: emacs-ide-dev-attach-repl was called via
;;;     with-eval-after-load 'lua-mode which may fire before lua-mode-map is
;;;     fully initialised. Moved inside lua-mode :config.
;;;   - FIX-REPL-REGISTER: Added explicit emacs-ide-repl-register for lua-mode
;;;     inside lua-mode :config for reliable C-c x r dispatch.
;;;   - FIX-TEST-REGISTER: Added emacs-ide-test-register-runner for lua-mode
;;;     using busted (the standard Lua test framework) with fallback message.
;;;   - FIX-LSP-HINT-VARS: lsp-lua-hint-enable and lsp-lua-hint-set-type are
;;;     sumneko-specific vars that may not exist in all server versions.
;;;     Wrapped with (boundp) guards to prevent void-variable errors.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-LSP-GUARD: (bound-and-true-p emacs-ide-lsp-enable) guard.
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "lua" :tier 2 :lsp-server "lua-language-server"
  :formatter "stylua" :test-cmd "busted" :repl "lua" :modes '(lua-mode))

(when (emacs-ide-dev-lang-enabled-p "lua")

;; ============================================================================
;; COMPILE / RUN / REPL COMMANDS
;; FIX-DEFUN-IN-CONFIG: top-level so M-x sees them before lua-mode loads.
;; ============================================================================
(defun emacs-ide-lua-run ()
  "Run the current Lua file."
  (interactive)
  (if (executable-find "lua")
      (compile (format "lua %s" (shell-quote-argument (buffer-file-name))))
    (message "lang-lua: lua not found")))

(defun emacs-ide-lua-repl ()
  "Open a Lua REPL."
  (interactive)
  (if (executable-find "lua")
      (progn (require 'comint)
             (make-comint "lua-repl" "lua")
             (switch-to-buffer "*lua-repl*"))
    (message "lang-lua: lua not found")))

(defun emacs-ide-lua-test-project ()
  "Run Lua project tests via busted."
  (interactive)
  (if (executable-find "busted")
      (compile "busted")
    (message "lang-lua: busted not found. Install: luarocks install busted")))

;; ============================================================================
;; LUA-MODE
;; ============================================================================
(use-package lua-mode
  :defer t
  :mode "\\.lua\\'"
  :interpreter "lua"
  :init
  (setq lua-indent-level 2)
  :config
  (emacs-ide-dev-bind-compile lua-mode-map #'emacs-ide-lua-run)
  ;; FIX-REPL-ATTACH-ORDER: moved here from with-eval-after-load
  (emacs-ide-dev-attach-repl lua-mode-map #'emacs-ide-lua-repl
                              (kbd "C-c x r"))
  ;; FIX-REPL-REGISTER: explicit registration for reliable C-c x r dispatch
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'lua-mode
      :launch         #'emacs-ide-lua-repl
      :buffer-name    "*lua-repl*"
      :send-region-fn nil))
  ;; FIX-TEST-REGISTER: register busted as the Lua test runner
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'lua-mode
      :project-fn #'emacs-ide-lua-test-project
      :file-fn    (lambda ()
                    (interactive)
                    (if (and (executable-find "busted") (buffer-file-name))
                        (compile (format "busted %s"
                                         (shell-quote-argument (buffer-file-name))))
                      (message "lang-lua: busted not found"))))))

;; ============================================================================
;; LSP — lua-language-server
;; FIX-LSP-HINT-VARS: lsp-lua-hint-* vars guarded with boundp — they are
;; sumneko-specific and may not exist in all server versions.
;; ============================================================================
(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :hook (lua-mode . lsp-deferred)
  :init
  (when (boundp 'lsp-lua-hint-enable)
    (setq lsp-lua-hint-enable t))
  (when (boundp 'lsp-lua-hint-set-type)
    (setq lsp-lua-hint-set-type t)))

;; ============================================================================
;; FORMATTER — stylua via apheleia
;; ============================================================================
(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'stylua 'lua-mode))

) ;; end (when (emacs-ide-dev-lang-enabled-p "lua"))

(provide 'lang-lua)
;;; lang-lua.el ends here
