;;; debug-core.el --- Debugging Infrastructure -*- lexical-binding: t -*-
;;; Commentary:
;;; DAP fully deferred — loads only when dap-debug or dap-debug-edit-template
;;; is called. No adapters are required at startup.
;;; Version: 3.0.4-patched
;;; Startup fix: removed all eager (require 'dap-*) calls; dap-ui deferred.
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
;; ============================================================================
(with-eval-after-load 'hydra
  (defhydra emacs-ide-hydra-debug (:hint nil :color pink)
    "
  debug
  ────────────────────────────────────────────────────────
  control  _s_ step in    _n_ next       _o_ step out
           _c_ continue   _r_ restart    _q_ quit session
  break    _b_ toggle     _B_ condition  _L_ log msg
           _D_ del all
  inspect  _l_ locals     _e_ eval expr  _w_ watch
           _u_ up frame   _d_ down frame _R_ repl
  launch   _5_ debug      _6_ restart
  ────────────────────────────────────────────────────────
  _ESC_ close
"
    ("s" (when (fboundp 'dap-step-in)               (dap-step-in)))
    ("n" (when (fboundp 'dap-next)                  (dap-next)))
    ("o" (when (fboundp 'dap-step-out)              (dap-step-out)))
    ("c" (when (fboundp 'dap-continue)              (dap-continue)))
    ("r" (when (fboundp 'dap-debug-restart)         (dap-debug-restart)))
    ("q" (when (fboundp 'dap-disconnect)            (dap-disconnect)) :color blue)
    ("b" (when (fboundp 'dap-breakpoint-toggle)     (dap-breakpoint-toggle)) :color red)
    ("B" (when (fboundp 'dap-breakpoint-condition)  (dap-breakpoint-condition)))
    ("L" (when (fboundp 'dap-breakpoint-log-message) (dap-breakpoint-log-message)))
    ("D" (when (fboundp 'dap-breakpoint-delete-all) (dap-breakpoint-delete-all)))
    ("l" (when (fboundp 'dap-ui-locals)             (dap-ui-locals)))
    ("e" (when (fboundp 'dap-eval-thing-at-point)   (dap-eval-thing-at-point)))
    ("w" (when (fboundp 'dap-ui-expressions)        (dap-ui-expressions)))
    ("u" (when (fboundp 'dap-up-stack-frame)        (dap-up-stack-frame)))
    ("d" (when (fboundp 'dap-down-stack-frame)      (dap-down-stack-frame)))
    ("R" (when (fboundp 'dap-ui-repl)               (dap-ui-repl)))
    ("5" (when (fboundp 'dap-debug)                 (call-interactively #'dap-debug)))
    ("6" (when (fboundp 'dap-debug-restart)         (dap-debug-restart)))
    ("ESC" nil :color blue)))

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
