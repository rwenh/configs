;;; core-dev.el --- Development Core Infrastructure -*- lexical-binding: t -*-
;;; Commentary:
;;; Shared development infrastructure for all language modules.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit-2):
;;;   - FIX-REGISTER-ALIAS: emacs-ide-dev-register was undefined — every lang
;;;     module calls it. Now the canonical registration function; old
;;;     emacs-ide-dev-register-lang retained as defalias.
;;;   - FIX-MISSING-API: Added all functions called by lang modules but absent:
;;;       emacs-ide-dev-bind-compile  (C-c C-c run key in each mode-map)
;;;       emacs-ide-dev-attach-repl   (REPL key in each mode-map)
;;;       emacs-ide-dev-attach-formatter (apheleia-mode-alist wiring)
;;;       emacs-ide-dev-attach-dap    (require DAP provider after dap-mode loads)
;;;       emacs-ide-dev-ensure-treesit (install grammar at idle time)
;;;       emacs-ide-dev-registered-langs (introspection helper)
;;;   - FIX-REPL-REGISTER-ARGS: emacs-ide-dev--register-repl now calls
;;;     emacs-ide-repl-register(mode &rest plist) correctly — was passing
;;;     lang-key as the mode positional argument.
;;;   - FIX-TEST-RUNNER-NAME: emacs-ide-dev-register-test-runner now delegates
;;;     to emacs-ide-test-register-runner (correct) not emacs-ide-test-runner-register.
;;; Fixes vs 3.0.4 (audit-1, retained):
;;;   - FIX-CACHE-CLEAR, FIX-HOOK-DEFER, FIX-RECALIBRATE, FIX-FORMATTER-GUARD.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; INTERNAL STATE
;; ============================================================================

(defvar emacs-ide-dev--registered-langs nil
  "Alist of (LANG-KEY . plist) for every lang module that has registered.")

(defvar emacs-ide-dev--config-languages nil
  "Cached languages section from config.yml. Cleared on config reload.")

(defun emacs-ide-dev--setup-config-reload-hook ()
  "Register cache-clear on config reload hook, deferred to after-init."
  (when (boundp 'emacs-ide-config-reload-hook)
    (add-hook 'emacs-ide-config-reload-hook
              (lambda () (setq emacs-ide-dev--config-languages nil)))))

(add-hook 'after-init-hook #'emacs-ide-dev--setup-config-reload-hook)

;; ============================================================================
;; CONFIG.YML INTEGRATION
;; ============================================================================

(defun emacs-ide-dev--config-languages ()
  "Return the languages alist from config.yml, cached."
  (or emacs-ide-dev--config-languages
      (setq emacs-ide-dev--config-languages
            (condition-case nil
                (when (boundp 'emacs-ide-config-data)
                  (cdr (assoc 'languages emacs-ide-config-data)))
              (error nil)))))

(defun emacs-ide-dev--config-lang-settings (lang-key)
  "Return settings alist for LANG-KEY from lang-settings: section."
  (condition-case nil
      (when (boundp 'emacs-ide-config-data)
        (let ((ls (cdr (assoc 'lang-settings emacs-ide-config-data))))
          (cdr (assoc (intern lang-key) ls))))
    (error nil)))

(defun emacs-ide-dev-lang-enabled-p (lang-key)
  "Return non-nil if LANG-KEY is enabled in config.yml. Defaults to t."
  (let ((langs (emacs-ide-dev--config-languages)))
    (if (null langs) t
      (let ((cell (assoc (intern lang-key) langs)))
        (if cell (cdr cell) t)))))

(defun emacs-ide-dev-executable-guard (executables)
  "Return non-nil if ALL EXECUTABLES are on PATH."
  (let ((exes (if (stringp executables) (list executables) executables)))
    (cl-every (lambda (exe) (executable-find exe)) exes)))

;; ============================================================================
;; LANGUAGE REGISTRATION
;; FIX-REGISTER-ALIAS: this is what every lang-*.el calls at top-level.
;; ============================================================================

(defun emacs-ide-dev-register (lang-key &rest plist)
  "Register LANG-KEY with IDE infrastructure (metadata store only).
Keywords: :tier :lsp-server :formatter :test-cmd :repl :modes
Lang modules wire their own use-package blocks independently."
  (let ((key-sym (intern lang-key))
        (existing (assoc (intern lang-key) emacs-ide-dev--registered-langs)))
    (if existing
        (setcdr existing plist)
      (push (cons key-sym plist) emacs-ide-dev--registered-langs))))

(defalias 'emacs-ide-dev-register-lang 'emacs-ide-dev-register
  "Alias for `emacs-ide-dev-register'.")

(defun emacs-ide-dev-registered-langs ()
  "Return list of registered lang-key strings."
  (mapcar (lambda (e) (symbol-name (car e)))
          emacs-ide-dev--registered-langs))

;; ============================================================================
;; TREESITTER HELPER
;; FIX-MISSING-API: called as (emacs-ide-dev-ensure-treesit 'python)
;; ============================================================================

(defun emacs-ide-dev-ensure-treesit (grammar)
  "Ensure tree-sitter GRAMMAR available; install at idle if missing."
  (when (and (fboundp 'treesit-available-p) (treesit-available-p))
    (unless (and (fboundp 'treesit-language-available-p)
                 (treesit-language-available-p grammar))
      (run-with-idle-timer
       2 nil
       (lambda ()
         (condition-case err
             (when (fboundp 'treesit-install-language-grammar)
               (treesit-install-language-grammar grammar))
           (error (message "core-dev: treesit grammar %s: %s"
                           grammar (error-message-string err)))))))))

;; ============================================================================
;; COMPILE KEY BINDING
;; FIX-MISSING-API: emacs-ide-dev-bind-compile called by every lang module.
;; ============================================================================

(defun emacs-ide-dev-bind-compile (keymap fn)
  "Bind FN as the run/compile command in KEYMAP at C-c C-c."
  (when (keymapp keymap)
    (define-key keymap (kbd "C-c C-c") fn)))

;; ============================================================================
;; REPL ATTACHMENT
;; FIX-MISSING-API + FIX-REPL-REGISTER-ARGS
;; ============================================================================

(defun emacs-ide-dev-attach-repl (keymap fn &optional key)
  "Bind FN as the REPL launcher in KEYMAP at KEY (default C-c x r)."
  (when (keymapp keymap)
    (define-key keymap (or key (kbd "C-c x r")) fn)))

(defun emacs-ide-dev--register-repl (lang-key modes repl-cmd)
  "Register REPL-CMD for MODES via tools-repl.el registry.
FIX-REPL-REGISTER-ARGS: emacs-ide-repl-register takes (mode &rest plist).
Previously lang-key was passed as the mode arg — now mode is first."
  (when (fboundp 'emacs-ide-repl-register)
    (dolist (mode modes)
      (emacs-ide-repl-register mode
        :lang-key    lang-key
        :launch      (let ((cmd repl-cmd) (lk lang-key))
                       (lambda ()
                         (if (executable-find cmd)
                             (progn (require 'comint)
                                    (make-comint (concat lk "-repl") cmd)
                                    (switch-to-buffer (format "*%s-repl*" lk)))
                           (message "core-dev: %s not found on PATH" cmd))))
        :buffer-name (format "*%s-repl*" lang-key)
        :send-region-fn nil))))

;; ============================================================================
;; FORMATTER ATTACHMENT
;; FIX-MISSING-API: emacs-ide-dev-attach-formatter called by lang modules.
;; ============================================================================

(defun emacs-ide-dev-attach-formatter (formatter-sym mode)
  "Register FORMATTER-SYM for MODE in apheleia-mode-alist (deferred)."
  (with-eval-after-load 'apheleia
    (when (and (boundp 'apheleia-formatters)
               (assq formatter-sym apheleia-formatters)
               (boundp 'apheleia-mode-alist))
      (setf (alist-get mode apheleia-mode-alist) formatter-sym))))

;; ============================================================================
;; DAP ATTACHMENT
;; FIX-MISSING-API: emacs-ide-dev-attach-dap called by lang modules.
;; ============================================================================

(defun emacs-ide-dev-attach-dap (template-name dap-provider)
  "Require DAP-PROVIDER when dap-mode loads."
  (with-eval-after-load 'dap-mode
    (condition-case err
        (require dap-provider nil t)
      (error (message "core-dev: DAP provider %s (%s) failed: %s"
                      dap-provider template-name
                      (error-message-string err))))))

;; ============================================================================
;; TEST RUNNER REGISTRY BRIDGE
;; FIX-TEST-RUNNER-NAME: correct fn is emacs-ide-test-register-runner.
;; ============================================================================

(defun emacs-ide-dev-register-test-runner (lang-key &rest plist)
  "Register test runner for LANG-KEY via tools-test-runner-registry.el."
  (with-eval-after-load 'tools-test-runner-registry
    (when (fboundp 'emacs-ide-test-register-runner)
      (let ((modes (plist-get plist :modes)))
        (dolist (mode (or modes (list (intern (concat lang-key "-mode")))))
          (apply #'emacs-ide-test-register-runner mode plist))))))

(provide 'core-dev)
;;; core-dev.el ends here
