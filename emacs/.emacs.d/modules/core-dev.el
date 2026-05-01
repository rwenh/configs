;;; core-dev.el --- Development Core Infrastructure -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(require 'cl-lib)

;;;; ── Language registry ───────────────────────────────────────────────────────

(defvar emacs-ide-dev--registered-langs nil
  "Alist of (LANG-KEY-SYMBOL . plist) for every lang module that has registered.
Keys are interned symbols matching the lang-key strings used in config.yml.")

(defvar emacs-ide-dev--config-languages nil
  "Cached `languages' section from config.yml.
Set lazily on first call to `emacs-ide-dev--config-languages'.
Cleared on `emacs-ide-config-reload-hook' so a reload picks up changes.")

;;;; ── Config reload: cache invalidation ──────────────────────────────────────

(add-hook 'emacs-ide-config-reload-hook
          (lambda ()
            (setq emacs-ide-dev--config-languages nil)))

;;;; ── Config access helpers ───────────────────────────────────────────────────

(defun emacs-ide-dev--config-languages ()
  "Return the parsed `languages' alist from config.yml, caching the result.
Returns nil when config is not loaded or the section is absent."
  (or emacs-ide-dev--config-languages
      (setq emacs-ide-dev--config-languages
            (condition-case nil
                (when (boundp 'emacs-ide-config-data)
                  (cdr (assoc 'languages emacs-ide-config-data)))
              (error nil)))))

(defun emacs-ide-dev--config-lang-settings (lang-key)
  "Return the lang-settings alist for LANG-KEY from config.yml, or nil.
Example: (emacs-ide-dev--config-lang-settings \"sql\") →
  ((dialect . postgres) (lsp-server . sqls) ...)"
  (condition-case nil
      (when (boundp 'emacs-ide-config-data)
        (let ((ls (cdr (assoc 'lang-settings emacs-ide-config-data))))
          (cdr (assoc (intern lang-key) ls))))
    (error nil)))

;;;; ── Language enable check ───────────────────────────────────────────────────

(defun emacs-ide-dev-lang-enabled-p (lang-key)
  "Return non-nil if LANG-KEY is enabled in config.yml.
When the `languages' section is absent or empty, all languages default to
enabled (opt-in-by-default policy — see Session 1 notes on bug #43)."
  (let ((langs (emacs-ide-dev--config-languages)))
    (if (null langs)
        t  ; no languages section → everything enabled
      (let ((cell (assoc (intern lang-key) langs)))
        ;; cell absent → key not listed → default enabled
        ;; cell present → use its value (nil = disabled, t = enabled)
        (if cell (cdr cell) t)))))

;;;; ── Executable guard ────────────────────────────────────────────────────────

(defun emacs-ide-dev-executable-guard (executables)
  "Return non-nil when all EXECUTABLES are found on PATH.
EXECUTABLES may be a string (single binary) or a list of strings."
  (let ((exes (if (stringp executables)
                  (list executables)
                executables)))
    (cl-every #'executable-find exes)))

;;;; ── Language registration ───────────────────────────────────────────────────

(defun emacs-ide-dev-register (lang-key &rest plist)
  "Register or replace the lang entry for LANG-KEY.
PLIST should contain at minimum :tier, :lsp-server, :formatter,
:test-cmd, :repl, and :modes.

Example:
  (emacs-ide-dev-register \"python\"
    :tier 1 :lsp-server \"pyright\" :formatter \"black\"
    :test-cmd \"pytest\" :repl \"ipython\"
    :modes \\='(python-mode python-ts-mode))"
  (let* ((key-sym  (intern lang-key))
         (existing (assoc key-sym emacs-ide-dev--registered-langs)))
    (if existing
        (setcdr existing plist)
      (push (cons key-sym plist) emacs-ide-dev--registered-langs))))

;; Backward-compat alias
(defalias 'emacs-ide-dev-register-lang #'emacs-ide-dev-register)

(defun emacs-ide-dev-registered-langs ()
  "Return a list of all registered language key strings."
  (mapcar (lambda (e) (symbol-name (car e)))
          emacs-ide-dev--registered-langs))

(defun emacs-ide-dev-get-lang-prop (lang-key prop)
  "Return property PROP from LANG-KEY's registration plist, or nil."
  (let ((entry (assoc (intern lang-key) emacs-ide-dev--registered-langs)))
    (and entry (plist-get (cdr entry) prop))))

;;;; ── Tree-sitter grammar installer ──────────────────────────────────────────

(defun emacs-ide-dev-ensure-treesit (grammar)
  "Install GRAMMAR tree-sitter language if not already available.
Runs on a 2-second idle timer to avoid blocking startup.  Safe to call
multiple times — skips installation if the grammar is already present."
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
    (unless (and (fboundp 'treesit-language-available-p)
                 (treesit-language-available-p grammar))
      (run-with-idle-timer
       2 nil
       (lambda ()
         (condition-case err
             (when (fboundp 'treesit-install-language-grammar)
               (treesit-install-language-grammar grammar))
           (error
            (message "core-dev: tree-sitter grammar %s install failed: %s"
                     grammar (error-message-string err)))))))))

;;;; ── Keymap helpers ──────────────────────────────────────────────────────────

(defun emacs-ide-dev-bind-compile (keymap fn)
  "Bind C-c C-c to FN in KEYMAP (the canonical compile/run key)."
  (when (keymapp keymap)
    (define-key keymap (kbd "C-c C-c") fn)))

(defun emacs-ide-dev-attach-repl (keymap fn &optional key)
  "Bind KEY (default C-c x r) to FN in KEYMAP (the canonical REPL key)."
  (when (keymapp keymap)
    (define-key keymap (or key (kbd "C-c x r")) fn)))

;;;; ── REPL hub helper ─────────────────────────────────────────────────────────

(defun emacs-ide-dev--register-repl (lang-key modes repl-cmd)
  "Register a simple comint-based REPL for LANG-KEY across MODES.
REPL-CMD is the executable name.  Each mode gets a registry entry that
launches the REPL via `make-comint'.

Most lang modules call `emacs-ide-repl-register' directly for finer control;
this helper is for simple cases where a bare comint REPL suffices."
  (when (fboundp 'emacs-ide-repl-register)
    (dolist (mode modes)
      (emacs-ide-repl-register mode
        :lang-key    lang-key
        :launch      (let ((cmd repl-cmd) (lk lang-key))
                       (lambda ()
                         (if (executable-find cmd)
                             (progn
                               (require 'comint)
                               (make-comint (concat lk "-repl") cmd)
                               (switch-to-buffer
                                (format "*%s-repl*" lk)))
                           (message "core-dev: %s not found on PATH" cmd))))
        :buffer-name (format "*%s-repl*" lang-key)
        :send-region-fn nil))))

;;;; ── Formatter helper ────────────────────────────────────────────────────────

(defun emacs-ide-dev-attach-formatter (formatter-sym mode)
  "Map apheleia FORMATTER-SYM to major MODE when both are available.
Only sets the mapping if the formatter entry exists in `apheleia-formatters'
(i.e., the formatter binary is on PATH and was registered by the formatter
module).  Safe to call before apheleia loads."
  (with-eval-after-load 'apheleia
    (when (and (boundp 'apheleia-formatters)
               (assq formatter-sym apheleia-formatters)
               (boundp 'apheleia-mode-alist))
      (setf (alist-get mode apheleia-mode-alist) formatter-sym))))

;;;; ── DAP helper ──────────────────────────────────────────────────────────────

(defun emacs-ide-dev-attach-dap (template-name dap-provider)
  "Load DAP-PROVIDER when dap-mode is available.
TEMPLATE-NAME is used only for the error message.  The provider symbol
(e.g. \\='dap-python) is required via `require' with noerror."
  (with-eval-after-load 'dap-mode
    (condition-case err
        (require dap-provider nil t)
      (error
       (message "core-dev: DAP provider %s (%s) failed to load: %s"
                dap-provider template-name
                (error-message-string err))))))

;;;; ── Test runner helper ──────────────────────────────────────────────────────

(defun emacs-ide-dev-register-test-runner (lang-key &rest plist)
  "Register test runners for LANG-KEY's modes via the runner registry.
PLIST should contain :file-fn, :project-fn, :point-fn, :watch-fn, and
optionally :modes (defaults to (lang-key-mode)).

This helper is the preferred API when a lang module wants to register
multiple mode variants without repeating the with-eval-after-load boilerplate."
  (with-eval-after-load 'tools-test-runner-registry
    (when (fboundp 'emacs-ide-test-register-runner)
      (let ((modes (plist-get plist :modes)))
        (dolist (mode (or modes
                          (list (intern (concat lang-key "-mode")))))
          (apply #'emacs-ide-test-register-runner mode plist))))))

(provide 'core-dev)
;;; core-dev.el ends here
