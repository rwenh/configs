;;; debug-core.el --- Debugging Infrastructure -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-debug-enable t
  "Master switch for DAP debugging, read from config.yml debug.enable.")

(use-package dap-mode
  :defer t
  :commands (dap-debug dap-debug-edit-template)
  :init
  (setq dap-auto-configure-features '(sessions locals controls tooltip)
        dap-ui-controls-enabled      t
        dap-ui-locals-enabled        t
        dap-ui-sessions-enabled      t
        dap-ui-expressions-enabled   t
        dap-ui-repl-open             t
        dap-ui-repl-prompt           "DAP> "
        dap-ui-repl-history-enabled  t
        dap-ui-repl-update-on-refresh t)
  :config
  (condition-case nil
      (progn
        (require 'dap-ui)
        (dap-ui-mode 1)
        (dap-ui-locals-mode 1)
        (dap-ui-sessions-mode 1))
    (error (message "⚠️  dap-ui not available"))))

(defun emacs-ide-debug-toggle-breakpoint ()
  (interactive)
  (if (fboundp 'dap-breakpoint-toggle)
      (dap-breakpoint-toggle)
    (message "DAP mode not loaded — use M-x dap-debug first")))

(defun emacs-ide-debug-delete-all-breakpoints ()
  (interactive)
  (if (fboundp 'dap-breakpoint-delete-all)
      (dap-breakpoint-delete-all)
    (message "DAP mode not loaded")))

(defun emacs-ide-debug-set-conditional-breakpoint (condition)
  (interactive "sBreakpoint condition: ")
  (if (fboundp 'dap-breakpoint-condition)
      (dap-breakpoint-condition condition)
    (message "DAP mode not loaded")))

(defun emacs-ide-debug-repl ()
  (interactive)
  (if (fboundp 'dap-ui-repl)
      (dap-ui-repl)
    (message "DAP REPL not available")))

(provide 'debug-core)
;;; debug-core.el ends here
