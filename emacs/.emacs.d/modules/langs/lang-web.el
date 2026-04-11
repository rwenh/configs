;;; lang-web.el --- Web Languages (JavaScript/TypeScript) -*- lexical-binding: t -*-
;;; Commentary:
;;; Complete web development support: JavaScript, TypeScript, JSX, TSX.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 1.1.0 to 3.0.4.
;;;   - FIX-LSP-INIT-VARS: lsp-typescript-* inlay hint variables moved from
;;;     :init to :config. In :init with (boundp) guards, these settings are
;;;     applied BEFORE lsp-typescript loads, so they have no effect. Moved to
;;;     :config where lsp-typescript is guaranteed loaded and registration occurs.
;;;   - FIX-JS2-REFACTOR-BIND: js2-refactor was bound with :bind-keymap in an
;;;     incorrect context, causing errors on C-c m. Removed conflicting binding.
;;; Code:

(require 'emacs-ide-dev)

;; ============================================================================
;; JavaScript / TypeScript Modes
;; ============================================================================
(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode)
  :config
  (when (emacs-ide-dev-lang-enabled-p "typescript")
    (emacs-ide-dev-register-lang
     "typescript"
     :modes '(typescript-mode typescript-ts-mode)
     :lsp t)))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  (when (emacs-ide-dev-lang-enabled-p "javascript")
    (emacs-ide-dev-register-lang
     "javascript"
     :modes '(js-mode js2-mode js-ts-mode)
     :lsp t
     :repl "node")))

;; ============================================================================
;; LSP: TYPESCRIPT-LANGUAGE-SERVER
;; ============================================================================
(use-package lsp-typescript
  :after (typescript-mode js2-mode lsp-mode)
  :if (executable-find "typescript-language-server")
  ;; FIX-LSP-INIT-VARS: Moved configuration from :init to :config
  ;; so that lsp-typescript is guaranteed to be loaded before settings apply.
  :config
  ;; Inlay hints configuration
  (setq lsp-typescript-display-return-type-hints t
        lsp-typescript-display-parameter-type-hints t
        lsp-typescript-display-variable-type-hints t
        lsp-typescript-display-enum-member-value-hints t
        lsp-typescript-display-inline-variable-hints t
        lsp-typescript-display-inline-parameter-hints t
        lsp-typescript-display-inline-enum-member-value-hints t
        lsp-typescript-inlay-hints-include-inlay-variable-hints-when-parameter-type-hints-enabled t
        lsp-typescript-inlay-hints-include-inlay-parameter-hints-when-argument-matches-parameter-name t
        lsp-typescript-inlay-hints-include-inlay-function-parameter-type-hints t))

;; ============================================================================
;; JS2-REFACTOR
;; ============================================================================
(use-package js2-refactor
  :after js2-mode
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  ;; FIX-JS2-REFACTOR-BIND: Removed problematic :bind-keymap that caused
  ;; errors on C-c m. Use add-keybindings-with-prefix instead.
  (add-hook 'js2-mode-hook (lambda ()
                             (js2r-add-keybindings-with-prefix "C-c C-r"))))

;; ============================================================================
;; TIDE (Alternative TypeScript Integration)
;; ============================================================================
(use-package tide
  :after (typescript-mode lsp-mode)
  :hook (typescript-mode . tide-setup)
  :init
  (setq tide-completion-enable-autoimport-suggestions t
        tide-completion-ts-config-overrides '()
        tide-user-preferences '((quotePreference . "single")
                                (importModuleSpecifierPreference . "relative"))))

;; ============================================================================
;; PRETTIER (Formatter)
;; ============================================================================
(use-package prettier-js
  :hook ((js-mode js2-mode typescript-mode) . prettier-js-mode)
  :init
  (setq prettier-js-args '("--print-width" "100"
                           "--tab-width" "2"
                           "--use-tabs" "false"
                           "--single-quote" "true"
                           "--semi" "true"
                           "--trailing-comma" "es5")))

(provide 'lang-web)
;;; lang-web.el ends here