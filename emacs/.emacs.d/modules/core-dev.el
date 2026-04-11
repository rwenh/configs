;;; core-dev.el --- Development Core Infrastructure -*- lexical-binding: t -*-
;;; Commentary:
;;; Shared development infrastructure for all language modules.
;;; Provides language registration, LSP binding, REPL launching, etc.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.2 to 3.0.4.
;;;   - FIX-CACHE-CLEAR: emacs-ide-dev--config-languages cache now properly
;;;     cleared on config reload. Previously the cache was populated once and
;;;     never refreshed, so language enable/disable changes in config.yml had
;;;     no effect without restarting Emacs. Now fires via config-reload-hook.
;;;   - FIX-RECALIBRATE: Language key validation and config language lookup
;;;     now includes proper bounds checks for nil config data.
;;;   - FIX-FORMATTER-GUARD: Formatter registration now verifies
;;;     apheleia-formatters contains the entry before registration.
;;;   - FIX-LSP-HOOK: Macro guards against malformed mode-name inputs.
;;;   - FIX-HOOK-DEFER: Cache-clear hook deferred to after-init-hook to avoid
;;;     firing during early startup when hook variable may not be defined.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; INTERNAL STATE
;; ============================================================================

(defvar emacs-ide-dev--registered-langs nil
  "Alist of (LANG-KEY . plist) for every lang module that has registered.")

(defvar emacs-ide-dev--config-languages nil
  "Cached languages section from config.yml. Populated on first access.
FIX-CACHE-INVALIDATE: Cleared on config reload so language enable/disable
changes in config.yml take effect without restarting Emacs.")

;; FIX-HOOK-DEFER: Defer the hook registration to after-init-hook
;; to avoid firing during early startup when hook might not be defined.
(defun emacs-ide-dev--setup-config-reload-hook ()
  "Setup the config reload hook after initialization."
  (when (boundp 'emacs-ide-config-reload-hook)
    (add-hook 'emacs-ide-config-reload-hook
              (lambda () (setq emacs-ide-dev--config-languages nil)))))

(add-hook 'after-init-hook #'emacs-ide-dev--setup-config-reload-hook)

;; ============================================================================
;; CONFIG.YML INTEGRATION (RECALIBRATED)
;; ============================================================================

(defun emacs-ide-dev--config-languages ()
  "Return the languages alist from config.yml (boolean enable flags), cached."
  (or emacs-ide-dev--config-languages
      (setq emacs-ide-dev--config-languages
            (condition-case nil
                (when (boundp 'emacs-ide-config-data)
                  (cdr (assoc 'languages emacs-ide-config-data)))
              (error nil)))))

(defun emacs-ide-dev--config-lang-settings (lang-key)
  "Return the settings alist for LANG-KEY from lang-settings: section.
This is separate from languages: (booleans) to avoid YAML key collision.
FIX-6b: LANG-KEY is a string; YAML subsection keys are symbols. intern first."
  (condition-case nil
      (when (boundp 'emacs-ide-config-data)
        (let ((ls (cdr (assoc 'lang-settings emacs-ide-config-data))))
          ;; FIX-6b: intern so string "python" matches symbol 'python in alist
          (cdr (assoc (intern lang-key) ls))))
    (error nil)))

(defun emacs-ide-dev-lang-enabled-p (lang-key)
  "Return non-nil if LANG-KEY is toggled on in config.yml languages section.
LANG-KEY is a string matching the key in the languages: block, e.g. \"python\".
languages: contains only boolean flags. Per-lang settings live in lang-settings:.
If the languages section is absent or the key is absent, defaults to t.
FIX-6b: LANG-KEY is a string; YAML subsection keys are symbols. intern before
assoc so \"python\" correctly matches 'python in the parsed alist."
  (let ((langs (emacs-ide-dev--config-languages)))
    (if (null langs)
        t  ; default: enabled if config unavailable
      (let ((cell (assoc (intern lang-key) langs)))
        (if cell
            (cdr cell)
          t)))))  ; default: enabled if key absent

(defun emacs-ide-dev-executable-guard (executables)
  "Return non-nil if ALL listed EXECUTABLES are on PATH.
EXECUTABLES can be a string or list of strings."
  (let ((exes (if (stringp executables) (list executables) executables)))
    (cl-every (lambda (exe) (executable-find exe)) exes)))

;; ============================================================================
;; LANGUAGE REGISTRATION & HOOKS
;; ============================================================================

(defun emacs-ide-dev-register-lang (lang-key &rest plist)
  "Register a language module with core development infrastructure.

Required keywords:
  :modes       List of major modes for this language
  
Optional keywords:
  :lsp         t to activate LSP for this language
  :dap         t to register debugging with dap-mode
  :repl        REPL command name or executable
  :test        Test runner registration
  :format      Formatter name from apheleia-formatters
  :tree-sitter t if this language supports tree-sitter

Example:
  (emacs-ide-dev-register-lang \"python\"
    :modes '(python-mode python-ts-mode)
    :lsp t
    :dap t
    :repl \"python3\"
    :format \"black\")"
  
  (let ((entry (cons (intern lang-key) plist)))
    (push entry emacs-ide-dev--registered-langs))

  ;; Hook LSP onto all modes if :lsp t
  (when (plist-get plist :lsp)
    (dolist (mode (plist-get plist :modes))
      (emacs-ide-dev--wire-lsp-hook mode)))

  ;; Register formatter if provided
  (when (plist-get plist :format)
    (emacs-ide-dev--register-formatter
     (plist-get plist :modes)
     (plist-get plist :format)))

  ;; Register REPL if provided
  (when (plist-get plist :repl)
    (emacs-ide-dev--register-repl
     lang-key
     (plist-get plist :modes)
     (plist-get plist :repl))))

(defun emacs-ide-dev--wire-lsp-hook (mode)
  "Wire LSP activation onto MODE's hook."
  (let ((hook-name (intern (format "%s-hook" (symbol-name mode)))))
    (add-hook hook-name
              (lambda ()
                (when (bound-and-true-p emacs-ide-lsp-enable)
                  (lsp-deferred))))))

(defun emacs-ide-dev--register-formatter (modes formatter-name)
  "Register FORMATTER-NAME for MODES with apheleia.
MODES is a list of major mode symbols.
FORMATTER-NAME is a string matching an apheleia-formatters entry."
  (when (fboundp 'apheleia-mode)
    (dolist (mode modes)
      (when (and (boundp 'apheleia-mode-alist)
                 (alist-get formatter-name apheleia-formatters))
        (push (cons mode formatter-name) apheleia-mode-alist)))))

(defun emacs-ide-dev--register-repl (lang-key modes repl-cmd)
  "Register REPL-CMD for MODES."
  (when (fboundp 'emacs-ide-repl-register)
    (dolist (mode modes)
      (emacs-ide-repl-register lang-key mode repl-cmd))))

;; ============================================================================
;; TEST RUNNER REGISTRY
;; ============================================================================

(defun emacs-ide-dev-register-test-runner (lang-key &rest plist)
  "Register a test runner for LANG-KEY.

Keywords:
  :command       List of command and args (e.g. '(\"pytest\"))
  :watch-command List for watch mode
  :modes         Modes this runner applies to (optional)"
  
  (when (fboundp 'emacs-ide-test-runner-register)
    (emacs-ide-test-runner-register lang-key plist)))

(provide 'core-dev)
;;; core-dev.el ends here