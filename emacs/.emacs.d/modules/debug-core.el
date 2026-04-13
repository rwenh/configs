;;; debug-core.el --- Debugging Infrastructure -*- lexical-binding: t -*-
;;; Commentary:
;;; DAP fully deferred — loads only when dap-debug or dap-debug-edit-template
;;; is called. No adapters are required at startup.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (post-audit calibration):
;;;   - FIX-HYDRA-ORPHAN: Removed emacs-ide-hydra-debug. tools-hydra.el defines
;;;     hydra-debug (bound to C-c h d) as the canonical debug hydra. The renamed
;;;     version here was never bound to any key and was unreachable. Having two
;;;     parallel debug hydra definitions is confusing and wastes eval time.
;;; Fixes vs 3.0.4-patched (retained):
;;;   - Startup fix: removed all eager (require 'dap-*) calls; dap-ui deferred.
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-debug-enable t
  "Master switch for DAP debugging, read from config.yml debug.enable.")

;; ============================================================================
;; DAP-MODE — fully deferred, only loads when a debug session is started
;; ============================================================================
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
  ;; dap-ui loaded lazily inside :config — only runs when dap-mode first loads
  (condition-case nil
      (progn
        (require 'dap-ui)
        (dap-ui-mode 1)
        (dap-ui-locals-mode 1)
        (dap-ui-sessions-mode 1))
    (error (message "⚠️  dap-ui not available"))))

;; ============================================================================
;; DEBUG HYDRA
;; The debug hydra is defined in tools-hydra.el as hydra-debug, bound to
;; C-c h d. It is NOT duplicated here — tools-hydra.el is the canonical owner.
;; Use M-x hydra-debug/body or C-c h d to open the debug hydra.
;; ============================================================================

;; ============================================================================
;; BREAKPOINT COMMANDS
;; ============================================================================
(defun emacs-ide-debug-toggle-breakpoint ()
  "Toggle breakpoint at point."
  (interactive)
  (if (fboundp 'dap-breakpoint-toggle)
      (dap-breakpoint-toggle)
    (message "DAP mode not loaded — use M-x dap-debug first")))

(defun emacs-ide-debug-delete-all-breakpoints ()
  "Delete all breakpoints."
  (interactive)
  (if (fboundp 'dap-breakpoint-delete-all)
      (dap-breakpoint-delete-all)
    (message "DAP mode not loaded")))

(defun emacs-ide-debug-set-conditional-breakpoint (condition)
  "Set conditional breakpoint with CONDITION."
  (interactive "sBreakpoint condition: ")
  (if (fboundp 'dap-breakpoint-condition)
      (dap-breakpoint-condition condition)
    (message "DAP mode not loaded")))

(defun emacs-ide-debug-repl ()
  "Open the DAP REPL for interactive debugging."
  (interactive)
  (if (fboundp 'dap-ui-repl)
      (dap-ui-repl)
    (message "DAP REPL not available")))

(provide 'debug-core)
;;; debug-core.el ends here
