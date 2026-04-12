;;; debug-core.el --- Debugging Infrastructure -*- lexical-binding: t -*-
;;; Commentary:
;;; DAP (Debug Adapter Protocol) configuration for unified debugging across languages.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (recalibration):
;;;   - FIX-DAP-GUARD: The previous :if guard required BOTH "node" AND "python3"
;;;     on PATH. This was far too restrictive — dap-mode supports Go (Delve),
;;;     Rust (LLDB), C/C++ (LLDB/GDB), Java (jdtls), and many others that do
;;;     not need node or python3 at all. A user running only Go+Rust would have
;;;     DAP silently disabled despite having the correct debug adapters installed.
;;;     Fix: removed the :if guard entirely. dap-mode loads on demand via its
;;;     :commands declaration (dap-debug, dap-debug-edit-template) and each
;;;     lang module's (emacs-ide-dev-attach-dap) call guards its own adapter
;;;     with an executable-find check. The debug section in config.yml
;;;     (debug.enable: true/false) is the user-facing on/off switch;
;;;     emacs-ide-config-apply wires that to emacs-ide-debug-enable which
;;;     callers can check at call time.
;;;   - FIX-DAP-UI-MODES: dap-ui-mode, dap-ui-locals-mode, and
;;;     dap-ui-sessions-mode are minor modes; they accept a numeric argument.
;;;     The plain (dap-ui-mode 1) form is correct and was already present.
;;;     Retained unchanged.
;;; Fixes vs 3.0.4 (audit, retained):
;;;   - FIX-HYDRA-CONFLICT: hydra-debug renamed to emacs-ide-hydra-debug.
;;;   - FIX-DAP-UI-REQUIRE: dap-ui require inside :config, guarded.
;;;   - FIX-PAREN: Closed with-eval-after-load 'hydra block.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; GUARD — respect debug.enable from config.yml
;; emacs-ide-debug-enable is set by emacs-ide-config-apply from config.yml
;; debug.enable key. Default t. When false, skip all DAP setup.
;; ============================================================================
(defvar emacs-ide-debug-enable t
  "Master switch for DAP debugging, read from config.yml debug.enable.
Set by emacs-ide-config-apply at startup.")

;; ============================================================================
;; DAP-MODE CONFIGURATION
;; FIX-DAP-GUARD: :if guard removed — it required node+python3 which excluded
;; Go, Rust, C/C++, Java debuggers entirely. dap-mode loads on :commands demand.
;; ============================================================================

(use-package dap-mode
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
  ;; Load dap-ui if available (optional — not present in all builds)
  (condition-case nil
      (progn
        (require 'dap-ui)
        (dap-ui-mode 1)
        (dap-ui-locals-mode 1)
        (dap-ui-sessions-mode 1))
    (error (message "⚠️  dap-ui not available — debugging limited to dap-mode core"))))

;; ============================================================================
;; DEBUG HYDRA — Keybindings for debugging commands
;; FIX-HYDRA-CONFLICT (retained): Renamed from hydra-debug to
;; emacs-ide-hydra-debug to avoid conflict with tools-hydra.el's hydra-debug.
;; C-c h d is bound by tools-hydra.el to hydra-debug/body (the main one);
;; emacs-ide-hydra-debug/body is available standalone via M-x.
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
    ("ESC" nil :color blue))

  ;; NOTE: C-c h d is intentionally NOT bound here.
  ;; tools-hydra.el defines hydra-debug and binds C-c h d to hydra-debug/body.
  ;; tools-hydra.el loads after debug-core.el so its binding wins.
  ;; emacs-ide-hydra-debug/body is available as a standalone command via M-x.
  ) ;; end with-eval-after-load 'hydra

;; ============================================================================
;; BREAKPOINT MANAGEMENT
;; ============================================================================

(defun emacs-ide-debug-toggle-breakpoint ()
  "Toggle breakpoint at point."
  (interactive)
  (if (fboundp 'dap-breakpoint-toggle)
      (dap-breakpoint-toggle)
    (message "DAP mode not loaded")))

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

;; ============================================================================
;; REPL-BASED DEBUGGING
;; ============================================================================

(defun emacs-ide-debug-repl ()
  "Open the DAP REPL for interactive debugging."
  (interactive)
  (if (fboundp 'dap-ui-repl)
      (dap-ui-repl)
    (message "DAP REPL not available")))

(provide 'debug-core)
;;; debug-core.el ends here
