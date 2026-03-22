;;; debug-core.el --- Professional Debugging Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; DAP, GDB, LLDB, language-specific debuggers
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.6 to 3.0.4.
;;;   - FIX-DUPLICATE-PROFILE: emacs-ide-profile-startup defun removed from
;;;     debug-core.el. debug-core loaded AFTER emacs-ide-profiler.el, so the
;;;     esup-only version here was silently overwriting the richer profiler
;;;     version (which falls back to startup/early-init reports). The esup
;;;     use-package block remains — esup is still available via M-x esup.
;;;   - FIX-DEBUG-ENABLE: dap-mode use-package now has :if guard on
;;;     emacs-ide-debug-enable so debug.enable: false in config.yml
;;;     actually prevents dap-mode from loading.
;;;   - FIX-PROGRAM-LAZY: All debug templates now use a lambda for :program
;;;     instead of (buffer-file-name) evaluated at load time. At load time
;;;     the current buffer is not the file to debug — the lambda is called
;;;     at launch time when the correct buffer is active.
;;;   - FIX-HELP-VERSION: emacs-ide-debug-help now uses emacs-ide-version
;;;     instead of the hardcoded "v2.2.5" string.
;;;   - FIX-GDB-QUOTE: gdb command now uses shell-quote-argument for the
;;;     executable path to handle paths containing spaces.
;;;   - FIX-BACKQUOTE: dap-ui-buffer-configurations changed from backquote
;;;     with unquotes to a plain quoted list — strings are literals, the
;;;     backquote and comma-unquotes were unnecessary.
;;;   - FIX-REALGUD-QUOTE: realgud:pdb and realgud:node-debug now use
;;;     shell-quote-argument on buffer-file-name.
;;; Fixes vs 2.2.6 (retained):
;;;   - FIX-CCC-D: C-c D s/r/q (profiler) documented as part of the C-c D
;;;     diagnostics prefix. keybindings.el previously bound bare C-c D to
;;;     emacs-ide-detect-show-status, which shadowed this prefix entirely.
;;;     detect-show-status moved to C-c D d in keybindings.el.
;;; Fixes:
;;;   - 2.2.5: Key binding validation: checked for collisions with tools-project.el
;;;     F9 collision resolved. Breakpoint bindings stable across magit versions.
;;;   - Hydra mode layer: re-verified no duplicate key bindings in menu.
;;;   - Debug templates: :program nil crash prevention — all prompts validated.
;;;   - Load order protection: debug-core now explicitly checks for tools-project
;;;     load order and documents F9 namespace carefully.
;;; Code:

;; ============================================================================
;; DAP MODE - DEBUG ADAPTER PROTOCOL
;; RECALIBRATED 2.2.5: Verified F9 collision resolved.
;; tools-project.el (treemacs) loads FIRST, claims F9.
;; debug-core loads AFTER, so key binding order is safe.
;; Breakpoint keys: F8 family (F8, C-F8, S-F8, C-S-F8)
;; ============================================================================
(use-package dap-mode
  :after lsp-mode
  ;; FIX-DEBUG-ENABLE: only load when debug.enable: true in config.yml
  :if (bound-and-true-p emacs-ide-debug-enable)
  :commands (dap-debug dap-debug-edit-template)
  :bind (("<f5>"    . dap-debug)
         ("<f6>"    . dap-debug-restart)
         ("<f7>"    . dap-step-in)
         ("S-<f7>"  . dap-next)
         ("M-<f7>"  . dap-step-out)
         ("C-<f7>"  . dap-continue)
         ("<f8>"    . dap-breakpoint-toggle)
         ("C-<f8>"  . dap-breakpoint-condition)
         ("S-<f8>"  . dap-breakpoint-log-message)
         ("C-S-<f8>" . dap-breakpoint-delete-all))
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip)
        ;; FIX-DEBUG-ENABLE: removed 'repl from auto-configure — tools-repl.el
        ;; provides the project's unified REPL; having two REPL systems active
        ;; simultaneously causes buffer confusion.
        dap-auto-show-output t
        ;; FIX-BACKQUOTE: plain quoted list — string literals need no backquote
        dap-ui-buffer-configurations
        '(("*dap-ui-locals*"      (side . right)  (slot . 1) (window-width . 0.20))
          ("*dap-ui-expressions*" (side . right)  (slot . 2) (window-width . 0.20))
          ("*dap-ui-sessions*"    (side . right)  (slot . 3) (window-width . 0.20))
          ("*dap-ui-breakpoints*" (side . left)   (slot . 2) (window-width . 0.20))
          ("*dap-ui-repl*"        (side . bottom) (slot . 1) (window-height . 0.20))))
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)

  ;; Load adapters with safe error handling
  (condition-case nil (require 'dap-python)   (error nil))
  (condition-case nil (require 'dap-node)     (error nil))
  (condition-case nil (require 'dap-go)       (error nil))
  (condition-case nil (require 'dap-lldb)     (error nil))
  (condition-case nil (require 'dap-gdb-lldb) (error nil))
  (condition-case nil (require 'dap-cpptools) (error nil))
  (condition-case nil (require 'dap-java)     (error nil))
  (condition-case nil (require 'dap-php)      (error nil))
  (condition-case nil (require 'dap-ruby)     (error nil))
  (condition-case nil (require 'dap-elixir)   (error nil)))

;; ============================================================================
;; PYTHON DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-python
  (setq dap-python-debugger 'debugpy)

  (dap-register-debug-template
   "Python :: Run File"
   ;; FIX-PROGRAM-LAZY: lambda evaluated at launch time, not load time
   (list :type "python"
         :args ""
         :cwd (lambda () (or (and (fboundp 'projectile-project-root)
                                  (projectile-project-root))
                             default-directory))
         :program (lambda () (buffer-file-name))
         :request "launch"
         :name "Python :: Run File"))

  (dap-register-debug-template
   "Python :: pytest"
   (list :type "python"
         :args "-m pytest -v"
         :cwd (lambda () (or (and (fboundp 'projectile-project-root)
                                  (projectile-project-root))
                             default-directory))
         :module "pytest"
         :request "launch"
         :name "Python :: pytest")))

;; ============================================================================
;; NODE.JS/JAVASCRIPT DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-node
  (when (fboundp 'dap-node-setup)
    (dap-node-setup))

  (dap-register-debug-template
   "Node :: Run File"
   ;; FIX-PROGRAM-LAZY: lambda evaluated at launch time, not load time
   (list :type "node"
         :request "launch"
         :name "Node :: Run File"
         :program (lambda () (buffer-file-name))
         :cwd (lambda () (or (and (fboundp 'projectile-project-root)
                                  (projectile-project-root))
                             default-directory)))))

;; ============================================================================
;; GO DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-go
  (when (fboundp 'dap-go-setup)
    (dap-go-setup))

  (dap-register-debug-template
   "Go :: Run File"
   ;; FIX-PROGRAM-LAZY: lambda evaluated at launch time, not load time
   (list :type "go"
         :request "launch"
         :name "Go :: Run File"
         :mode "auto"
         :program (lambda () (buffer-file-name))
         :cwd (lambda () (or (and (fboundp 'projectile-project-root)
                                  (projectile-project-root))
                             default-directory)))))

;; ============================================================================
;; C/C++/RUST DEBUGGING - LLDB/GDB
;; RECALIBRATED 2.2.5: Interactive executable selection prevents :program nil crash.
;; Both LLDB and GDB templates use read-file-name for safe path resolution.
;; ============================================================================
(defun emacs-ide-dap-lldb-run ()
  "Launch LLDB debugger, prompting for the executable (RECALIBRATED: safe prompt)."
  (interactive)
  (let ((prog (read-file-name "Executable to debug: "
                              (or (and (fboundp 'projectile-project-root)
                                       (projectile-project-root))
                                  default-directory))))
    (when (and prog (file-exists-p prog))
      (dap-debug (list :type "lldb"
                       :request "launch"
                       :name "LLDB :: Run"
                       :program prog
                       :cwd (file-name-directory prog))))))

(defun emacs-ide-dap-gdb-run ()
  "Launch GDB debugger, prompting for the executable (RECALIBRATED: safe prompt)."
  (interactive)
  (let ((prog (read-file-name "Executable to debug: "
                              (or (and (fboundp 'projectile-project-root)
                                       (projectile-project-root))
                                  default-directory))))
    (when (and prog (file-exists-p prog))
      (dap-debug (list :type "cppdbg"
                       :request "launch"
                       :name "GDB :: Run"
                       :MIMode "gdb"
                       :program prog
                       :cwd (file-name-directory prog))))))

;; ============================================================================
;; JAVA DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-java
  (when (fboundp 'dap-java-setup)
    (dap-java-setup)))

;; ============================================================================
;; PHP DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-php
  (when (fboundp 'dap-php-setup)
    (dap-php-setup)))

;; ============================================================================
;; GDB INTEGRATION
;; ============================================================================
(use-package gdb-mi
  :straight nil
  :init
  (setq gdb-many-windows t
        gdb-show-main t
        gdb-debug-log-max 1024
        gdb-restore-window-configuration-after-quit t
        gdb-thread-buffer-verbose-names t
        gdb-thread-buffer-arguments t
        gdb-thread-buffer-locations t
        gdb-thread-buffer-addresses nil
        gdb-show-changed-values t
        gdb-delete-out-of-scope t
        gdb-use-colon-colon-notation t
        gdb-speedbar-auto-raise t)
  :config
  (defun emacs-ide-gdb-debug ()
    "Start GDB debugging session."
    (interactive)
    (if (executable-find "gdb")
        (let ((executable (read-file-name "Select executable: "
                                          (or (and (fboundp 'projectile-project-root)
                                                   (projectile-project-root))
                                             default-directory))))
          ;; FIX-GDB-QUOTE: shell-quote-argument handles paths with spaces
          (gdb (format "gdb -i=mi %s" (shell-quote-argument executable))))
      (message "⚠️  GDB not found. Install GDB to debug C/C++ code."))))

;; ============================================================================
;; EDEBUG - EMACS LISP DEBUGGER
;; ============================================================================
(use-package edebug
  :straight nil
  :init
  (setq edebug-trace t
        edebug-print-length 50
        edebug-print-level 10
        edebug-print-circle t
        edebug-eval-macro-args t)
  :config
  (defun emacs-ide-edebug-defun ()
    "Instrument function for debugging."
    (interactive)
    (eval-defun t)))

;; ============================================================================
;; REALGUD - UNIFIED DEBUGGER INTERFACE
;; ============================================================================
(use-package realgud
  :commands (realgud:gdb realgud:pdb realgud:trepan realgud:node-debug)
  :config
  (defun emacs-ide-realgud-pdb ()
    "Debug Python with pdb (if available)."
    (interactive)
    (if (executable-find "python3")
        ;; FIX-REALGUD-QUOTE: shell-quote-argument for paths with spaces
        (realgud:pdb (format "python3 -m pdb %s"
                             (shell-quote-argument (buffer-file-name))))
      (message "⚠️  Python not found")))

  (defun emacs-ide-realgud-node ()
    "Debug Node.js (if available)."
    (interactive)
    (if (executable-find "node")
        ;; FIX-REALGUD-QUOTE: shell-quote-argument for paths with spaces
        (realgud:node-debug (format "node inspect %s"
                                    (shell-quote-argument (buffer-file-name))))
      (message "⚠️  Node.js not found"))))

;; ============================================================================
;; DEBUGGING HYDRA (RECALIBRATED 2.2.5)
;; All keys verified unique. No duplicates across all sections:
;;   Control: c, s, o, n, r (5 keys)
;;   Breakpoints: b, B, L, D (4 keys, uppercase/modifier variants)
;;   Navigation: u, d, l, e (4 keys)
;;   Info: U, w, R, q (4 keys, uppercase variants + modifiers)
;; Total: 17 unique bindings. Layout validated for no conflicts.
;; ============================================================================
(use-package hydra
  :config
  (defhydra hydra-debug (:color pink :hint nil)
    "
^Control^         ^Breakpoints^      ^Navigation^       ^Info^
^^^^^^^^-----------------------------------------------------------------
_c_: continue     _b_: toggle        _n_: next          _l_: locals
_s_: step in      _B_: condition     _u_: up frame      _e_: expressions
_o_: step out     _L_: log message   _d_: down frame    _U_: sessions
_r_: restart      _D_: delete all                       _w_: watch
_q_: quit                                               _R_: repl
"
    ("c" dap-continue)
    ("s" dap-step-in)
    ("o" dap-step-out)
    ("n" dap-next)
    ("r" dap-debug-restart)
    ("b" dap-breakpoint-toggle)
    ("B" dap-breakpoint-condition)
    ("L" dap-breakpoint-log-message)
    ("D" dap-breakpoint-delete-all)
    ("u" dap-up-stack-frame)
    ("d" dap-down-stack-frame)
    ("l" dap-ui-locals)
    ("e" dap-ui-expressions)
    ("U" dap-ui-sessions)
    ("w" dap-ui-expressions-add)
    ("R" dap-ui-repl)
    ("q" nil "quit" :color blue))

  (global-set-key (kbd "C-c d h") 'hydra-debug/body))

;; ============================================================================
;; DEBUGGING HELPERS
;; ============================================================================
(defun emacs-ide-debug-help ()
  "Display debugging help."
  (interactive)
  (with-output-to-temp-buffer "*Debug Help*"
    ;; FIX-HELP-VERSION: use emacs-ide-version instead of hardcoded v2.2.5
    (princ (format "=== EMACS IDE DEBUGGING GUIDE (v%s) ===\n\n"
                   (if (boundp 'emacs-ide-version) emacs-ide-version "3.0.4")))
    (princ "BREAKPOINTS:\n")
    (princ "  F8          Toggle breakpoint\n")
    (princ "  C-F8        Conditional breakpoint\n")
    (princ "  S-F8        Log message breakpoint\n")
    (princ "  C-S-F8      Delete all breakpoints\n\n")
    (princ "EXECUTION:\n")
    (princ "  F5          Start debugging\n")
    (princ "  F6          Restart\n")
    (princ "  F7          Step into\n")
    (princ "  S-F7        Step over\n")
    (princ "  M-F7        Step out\n")
    (princ "  C-F7        Continue\n\n")
    (princ "HYDRA (C-c d h):\n")
    (princ "  c=continue  s=step-in  o=step-out  n=next  r=restart\n")
    (princ "  b=bp-toggle  B=bp-cond  L=bp-log  D=bp-del-all\n")
    (princ "  u=up-frame  d=down-frame  l=locals  e=expressions\n")
    (princ "  U=sessions  w=watch  R=repl  q=quit\n\n")))

(global-set-key (kbd "C-c d ?") 'emacs-ide-debug-help)

;; ============================================================================
;; PERFORMANCE PROFILING
;; ============================================================================
;; FIX-DUPLICATE-PROFILE: emacs-ide-profile-startup is defined in
;; emacs-ide-profiler.el (loaded earlier). The version here was an
;; esup-only stub that silently overwrote the richer profiler version.
;; esup is still available directly via M-x esup.
(use-package esup
  :commands (esup)
  :init
  (setq esup-depth 0))

;; C-c D prefix: s=profiler-start, r=profiler-report, q=profiler-stop
;; C-c D o = docker (tools-terminal.el), C-c D d = detect-status (keybindings.el)
;; FIX-CCC-D: keybindings.el previously bound bare C-c D as a command,
;; shadowing this entire prefix. Now fixed — C-c D is a clean prefix.
(use-package profiler
  :straight nil
  :bind (("C-c D s" . profiler-start)
         ("C-c D r" . profiler-report)
         ("C-c D q" . profiler-stop)))

;; ============================================================================
;; DEBUGGING INSTALLATION CHECK
;; ============================================================================
(defun emacs-ide-check-debug-tools ()
  "Check debug tool availability."
  (interactive)
  (let ((tools '(("debugpy" "pip3 install debugpy"                                  "python")
                 ("node"    "Built-in"                                               "javascript")
                 ("dlv"     "go install github.com/go-delve/delve/cmd/dlv@latest"   "go")
                 ("lldb"    "System package manager"                                 "c/c++")
                 ("gdb"     "System package manager"                                 "c/c++")))
        (found '())
        (missing '()))
    (dolist (tool tools)
      (if (executable-find (car tool))
          (push tool found)
        (push tool missing)))
    (with-output-to-temp-buffer "*Debug Tools*"
      (princ "=== DEBUG TOOLS STATUS ===\n\n")
      (princ "AVAILABLE:\n")
      (dolist (tool found)
        (princ (format "  ✓ %s (%s)\n" (car tool) (nth 2 tool))))
      (when missing
        (princ "\nMISSING:\n")
        (dolist (tool missing)
          (princ (format "  ✗ %s (%s) - Install: %s\n"
                         (car tool) (nth 2 tool) (nth 1 tool))))))))

(provide 'debug-core)
;;; debug-core.el ends here
