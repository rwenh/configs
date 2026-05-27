;;; debug-core.el --- Debugging Infrastructure -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(require 'cl-lib)

;;;; ── DAP mode (conditional on debug.enable) ─────────────────────────────────

(when (bound-and-true-p emacs-ide-debug-enable)

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

) ;; end (when emacs-ide-debug-enable)

;;;; ── Helper commands (always available) ─────────────────────────────────────

(defun emacs-ide-debug-toggle-breakpoint ()
  "Toggle a DAP breakpoint at point."
  (interactive)
  (if (fboundp 'dap-breakpoint-toggle)
      (dap-breakpoint-toggle)
    (message "DAP mode not loaded — use M-x dap-debug to start a session first")))

(defun emacs-ide-debug-delete-all-breakpoints ()
  "Delete all DAP breakpoints."
  (interactive)
  (if (fboundp 'dap-breakpoint-delete-all)
      (dap-breakpoint-delete-all)
    (message "DAP mode not loaded")))

(defun emacs-ide-debug-set-conditional-breakpoint (condition)
  "Set a conditional DAP breakpoint at point."
  (interactive "sBreakpoint condition: ")
  (if (fboundp 'dap-breakpoint-condition)
      (dap-breakpoint-condition condition)
    (message "DAP mode not loaded")))

(defun emacs-ide-debug-repl ()
  "Open the DAP debug REPL."
  (interactive)
  (if (fboundp 'dap-ui-repl)
      (dap-ui-repl)
    (message "DAP REPL not available — start a debug session first")))

(provide 'debug-core)
;;; debug-core.el ends here
