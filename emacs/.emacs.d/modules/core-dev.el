;;; core-dev.el --- Development Core Infrastructure -*- lexical-binding: t -*-
;;; Version: 3.3.1
;;;
;;; Code:

(require 'cl-lib)

;;;; ── Language registry ───────────────────────────────────────────────────────

(defvar emacs-ide-dev--registered-langs nil
  "Alist of (LANG-KEY-SYMBOL . plist) for every lang module that has registered.")

(defvar emacs-ide-dev--config-languages nil
  "Cached `languages' section from config.yml.
Cleared on `emacs-ide-config-reload-hook' so a reload picks up changes.")

;;;; ── Config reload: cache invalidation ──────────────────────────────────────

(add-hook 'emacs-ide-config-reload-hook
          (lambda ()
            (setq emacs-ide-dev--config-languages nil)))

;;;; ── Config access helpers ───────────────────────────────────────────────────

(defun emacs-ide-dev--config-languages ()
  "Return the parsed `languages' alist from config.yml, caching the result."
  (or emacs-ide-dev--config-languages
      (setq emacs-ide-dev--config-languages
            (condition-case nil
                (when (boundp 'emacs-ide-config-data)
                  (cdr (assoc 'languages emacs-ide-config-data)))
              (error nil)))))

(defun emacs-ide-dev--config-lang-settings (lang-key)
  "Return the lang-settings alist for LANG-KEY from config.yml, or nil."
  (condition-case nil
      (when (boundp 'emacs-ide-config-data)
        (let ((ls (cdr (assoc 'lang-settings emacs-ide-config-data))))
          (cdr (assoc (intern lang-key) ls))))
    (error nil)))

;;;; ── Language enable check ───────────────────────────────────────────────────

(defun emacs-ide-dev-lang-enabled-p (lang-key)
  "Return non-nil if LANG-KEY is enabled in config.yml.
When the `languages' section is absent or empty, all languages default to
enabled (opt-in-by-default policy)."
  (let ((langs (emacs-ide-dev--config-languages)))
    (if (null langs)
        t
      (let ((cell (assoc (intern lang-key) langs)))
        (if cell (cdr cell) t)))))

;;;; ── Executable guard ────────────────────────────────────────────────────────

(defun emacs-ide-dev-executable-guard (executables)
  "Return non-nil when all EXECUTABLES are found on PATH."
  (let ((exes (if (stringp executables)
                  (list executables)
                executables)))
    (cl-every #'executable-find exes)))

;;;; ── Language registration ───────────────────────────────────────────────────

(defun emacs-ide-dev-register (lang-key &rest plist)
  "Register or replace the lang entry for LANG-KEY."
  (let* ((key-sym  (intern lang-key))
         (existing (assoc key-sym emacs-ide-dev--registered-langs)))
    (if existing
        (setcdr existing plist)
      (push (cons key-sym plist) emacs-ide-dev--registered-langs))))

(defalias 'emacs-ide-dev-register-lang #'emacs-ide-dev-register)

(defun emacs-ide-dev-registered-langs ()
  "Return a list of all registered language key strings."
  (mapcar (lambda (e) (symbol-name (car e)))
          emacs-ide-dev--registered-langs))

(defun emacs-ide-dev-get-lang-prop (lang-key prop)
  "Return property PROP from LANG-KEY's registration plist, or nil."
  (let ((entry (assoc (intern lang-key) emacs-ide-dev--registered-langs)))
    (and entry (plist-get (cdr entry) prop))))

;;;; ── treesit-auto (CALIBRATION) ─────────────────────────────────────────────
;;

(use-package treesit-auto
  :if (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  :demand t
  :config
  (setq treesit-auto-install 'prompt)

  (global-treesit-auto-mode 1)

  (treesit-auto-add-to-auto-mode-alist 'all))

(defun emacs-ide-dev-ensure-treesit (grammar)
  "No-op shim: GRAMMAR management is now handled by treesit-auto.
This function is kept only for backward compatibility with lang modules
that call it.  treesit-auto installs grammars on demand when a file of
the corresponding type is opened."
  ;; Intentionally empty — treesit-auto handles everything.
  (ignore grammar))

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
  "Register a simple comint-based REPL for LANG-KEY across MODES."
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
  "Map apheleia FORMATTER-SYM to major MODE when both are available."
  (with-eval-after-load 'apheleia
    (when (and (boundp 'apheleia-formatters)
               (assq formatter-sym apheleia-formatters)
               (boundp 'apheleia-mode-alist))
      (setf (alist-get mode apheleia-mode-alist) formatter-sym))))

;;;; ── DAP helper ──────────────────────────────────────────────────────────────

(defun emacs-ide-dev-attach-dap (template-name dap-provider)
  "Load DAP-PROVIDER when dap-mode is available."
  (with-eval-after-load 'dap-mode
    (condition-case err
        (require dap-provider nil t)
      (error
       (message "core-dev: DAP provider %s (%s) failed to load: %s"
                dap-provider template-name
                (error-message-string err))))))

;;;; ── Test runner helper ──────────────────────────────────────────────────────

(defun emacs-ide-dev-register-test-runner (lang-key &rest plist)
  "Register test runners for LANG-KEY's modes via the runner registry."
  (with-eval-after-load 'tools-test-runner-registry
    (when (fboundp 'emacs-ide-test-register-runner)
      (let ((modes (plist-get plist :modes)))
        (dolist (mode (or modes
                          (list (intern (concat lang-key "-mode")))))
          (apply #'emacs-ide-test-register-runner mode plist))))))

(provide 'core-dev)
;;; core-dev.el ends here
