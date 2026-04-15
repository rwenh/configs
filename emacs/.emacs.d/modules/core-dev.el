;;; core-dev.el --- Development Core Infrastructure -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-dev--registered-langs nil
  "Alist of (LANG-KEY . plist) for every lang module that has registered.")

(defvar emacs-ide-dev--config-languages nil
  "Cached languages section from config.yml. Cleared on config reload.")

(defun emacs-ide-dev--setup-config-reload-hook ()
  (when (boundp 'emacs-ide-config-reload-hook)
    (add-hook 'emacs-ide-config-reload-hook
              (lambda () (setq emacs-ide-dev--config-languages nil)))))

(add-hook 'after-init-hook #'emacs-ide-dev--setup-config-reload-hook)

(defun emacs-ide-dev--config-languages ()
  (or emacs-ide-dev--config-languages
      (setq emacs-ide-dev--config-languages
            (condition-case nil
                (when (boundp 'emacs-ide-config-data)
                  (cdr (assoc 'languages emacs-ide-config-data)))
              (error nil)))))

(defun emacs-ide-dev--config-lang-settings (lang-key)
  (condition-case nil
      (when (boundp 'emacs-ide-config-data)
        (let ((ls (cdr (assoc 'lang-settings emacs-ide-config-data))))
          (cdr (assoc (intern lang-key) ls))))
    (error nil)))

(defun emacs-ide-dev-lang-enabled-p (lang-key)
  (let ((langs (emacs-ide-dev--config-languages)))
    (if (null langs) t
      (let ((cell (assoc (intern lang-key) langs)))
        (if cell (cdr cell) t)))))

(defun emacs-ide-dev-executable-guard (executables)
  (let ((exes (if (stringp executables) (list executables) executables)))
    (cl-every (lambda (exe) (executable-find exe)) exes)))

(defun emacs-ide-dev-register (lang-key &rest plist)
  (let ((key-sym (intern lang-key))
        (existing (assoc (intern lang-key) emacs-ide-dev--registered-langs)))
    (if existing
        (setcdr existing plist)
      (push (cons key-sym plist) emacs-ide-dev--registered-langs))))

(defalias 'emacs-ide-dev-register-lang 'emacs-ide-dev-register)

(defun emacs-ide-dev-registered-langs ()
  (mapcar (lambda (e) (symbol-name (car e)))
          emacs-ide-dev--registered-langs))

(defun emacs-ide-dev-ensure-treesit (grammar)
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

(defun emacs-ide-dev-bind-compile (keymap fn)
  (when (keymapp keymap)
    (define-key keymap (kbd "C-c C-c") fn)))

(defun emacs-ide-dev-attach-repl (keymap fn &optional key)
  (when (keymapp keymap)
    (define-key keymap (or key (kbd "C-c x r")) fn)))

(defun emacs-ide-dev--register-repl (lang-key modes repl-cmd)
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

(defun emacs-ide-dev-attach-formatter (formatter-sym mode)
  (with-eval-after-load 'apheleia
    (when (and (boundp 'apheleia-formatters)
               (assq formatter-sym apheleia-formatters)
               (boundp 'apheleia-mode-alist))
      (setf (alist-get mode apheleia-mode-alist) formatter-sym))))

(defun emacs-ide-dev-attach-dap (template-name dap-provider)
  (with-eval-after-load 'dap-mode
    (condition-case err
        (require dap-provider nil t)
      (error (message "core-dev: DAP provider %s (%s) failed: %s"
                      dap-provider template-name
                      (error-message-string err))))))

(defun emacs-ide-dev-register-test-runner (lang-key &rest plist)
  (with-eval-after-load 'tools-test-runner-registry
    (when (fboundp 'emacs-ide-test-register-runner)
      (let ((modes (plist-get plist :modes)))
        (dolist (mode (or modes (list (intern (concat lang-key "-mode")))))
          (apply #'emacs-ide-test-register-runner mode plist))))))

(provide 'core-dev)
;;; core-dev.el ends here
