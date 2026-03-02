;;; debug-core.el --- Professional Debugging Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; DAP, GDB, LLDB, language-specific debuggers
;;; Version: 2.2.3
;;; Fixes:
;;;   - 2.2.3: Hydra duplicate keys fully resolved. Previously "s" and "i"
;;;     both mapped to dap-step-in (comment claimed it was fixed but wasn't).
;;;     New unambiguous layout:
;;;       n=next, s=step-in, o=step-out, c=continue, r=restart
;;;       b=bp-toggle, B=bp-condition, L=bp-log, D=bp-delete-all
;;;       u=up-frame, d=down-frame, l=locals, e=expressions,
;;;       U=sessions, w=watch, R=repl, q=quit
;;;     "i" key removed entirely (was redundant alias for dap-step-in).
;;;   - LLDB and GDB debug templates: :program nil crashes dap on launch;
;;;     replaced with a prompt-based helper that reads the executable path.
;;; Code:

;; ============================================================================
;; DAP MODE - DEBUG ADAPTER PROTOCOL
;; ============================================================================
(use-package dap-mode
  :after lsp-mode
  :commands (dap-debug dap-debug-edit-template)
  :bind (("<f5>"    . dap-debug)
         ("<f6>"    . dap-debug-restart)
         ("<f7>"    . dap-step-in)
         ("S-<f7>"  . dap-next)
         ("M-<f7>"  . dap-step-out)
         ("C-<f7>"  . dap-continue)
         ("<f9>"    . dap-breakpoint-toggle)
         ("C-<f9>"  . dap-breakpoint-condition)
         ("S-<f9>"  . dap-breakpoint-log-message)
         ("C-S-<f9>" . dap-breakpoint-delete-all))
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip repl)
        dap-auto-show-output t
        dap-ui-buffer-configurations
        `((,"*dap-ui-locals*"      (side . right)  (slot . 1) (window-width . 0.20))
          (,"*dap-ui-expressions*" (side . right)  (slot . 2) (window-width . 0.20))
          (,"*dap-ui-sessions*"    (side . right)  (slot . 3) (window-width . 0.20))
          (,"*dap-ui-breakpoints*" (side . left)   (slot . 2) (window-width . 0.20))
          (,"*dap-ui-repl*"        (side . bottom) (slot . 1) (window-height . 0.20))))
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)

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
   (list :type "python"
         :args ""
         :cwd (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                  default-directory)
         :program (buffer-file-name)
         :request "launch"
         :name "Python :: Run File"))

  (dap-register-debug-template
   "Python :: pytest"
   (list :type "python"
         :args "-m pytest -v"
         :cwd (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                  default-directory)
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
   (list :type "node"
         :request "launch"
         :name "Node :: Run File"
         :program (buffer-file-name)
         :cwd (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                  default-directory))))

;; ============================================================================
;; GO DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-go
  (when (fboundp 'dap-go-setup)
    (dap-go-setup))

  (dap-register-debug-template
   "Go :: Run File"
   (list :type "go"
         :request "launch"
         :name "Go :: Run File"
         :mode "auto"
         :program (buffer-file-name)
         :cwd (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                  default-directory))))

;; ============================================================================
;; C/C++/RUST DEBUGGING - LLDB/GDB
;; FIX: :program nil would crash dap immediately on launch.
;;      Replaced with an interactive helper that prompts for the executable.
;; ============================================================================
(defun emacs-ide-dap-lldb-run ()
  "Launch LLDB debugger, prompting for the executable."
  (interactive)
  (let ((prog (read-file-name "Executable to debug: "
                              (or (and (fboundp 'projectile-project-root)
                                       (projectile-project-root))
                                  default-directory))))
    (dap-debug (list :type "lldb"
                     :request "launch"
                     :name "LLDB :: Run"
                     :program prog
                     :cwd (file-name-directory prog)))))

(defun emacs-ide-dap-gdb-run ()
  "Launch GDB debugger, prompting for the executable."
  (interactive)
  (let ((prog (read-file-name "Executable to debug: "
                              (or (and (fboundp 'projectile-project-root)
                                       (projectile-project-root))
                                  default-directory))))
    (dap-debug (list :type "cppdbg"
                     :request "launch"
                     :name "GDB :: Run"
                     :MIMode "gdb"
                     :program prog
                     :cwd (file-name-directory prog)))))

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
          (gdb (format "gdb -i=mi %s" executable)))
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
        (realgud:pdb (format "python3 -m pdb %s" (buffer-file-name)))
      (message "⚠️  Python not found")))

  (defun emacs-ide-realgud-node ()
    "Debug Node.js (if available)."
    (interactive)
    (if (executable-find "node")
        (realgud:node-debug (format "node inspect %s" (buffer-file-name)))
      (message "⚠️  Node.js not found"))))

;; ============================================================================
;; DEBUGGING HYDRA
;; FIX 2.2.3: All duplicate keys removed. Previous version still had both
;;   "s" and "i" mapped to dap-step-in despite the comment claiming otherwise.
;;   Final unambiguous layout (no key appears more than once):
;;     n=next         s=step-in     o=step-out    c=continue    r=restart
;;     b=bp-toggle    B=bp-cond     L=bp-log      D=bp-del-all
;;     u=up-frame     d=down-frame  l=locals       e=expressions
;;     U=sessions     w=watch       R=repl         q=quit
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
    (princ "=== EMACS IDE DEBUGGING GUIDE ===\n\n")
    (princ "BREAKPOINTS:\n")
    (princ "  F9          Toggle breakpoint\n")
    (princ "  C-F9        Conditional breakpoint\n")
    (princ "  S-F9        Log message breakpoint\n")
    (princ "  C-S-F9      Delete all breakpoints\n\n")
    (princ "EXECUTION:\n")
    (princ "  F5          Start debugging\n")
    (princ "  F6          Restart\n")
    (princ "  F7          Step into\n")
    (princ "  S-F7        Step over\n")
    (princ "  M-F7        Step out\n")
    (princ "  C-F7        Continue\n\n")
    (princ "HYDRA (C-c d h):\n")
    (princ "  n=next  s=step-in  o=step-out  c=continue  r=restart\n")
    (princ "  b=bp-toggle  B=bp-cond  L=bp-log  D=bp-del-all\n")
    (princ "  u=up-frame  d=down-frame\n")
    (princ "  l=locals  e=expressions  U=sessions  w=watch  R=repl\n\n")))

(global-set-key (kbd "C-c d ?") 'emacs-ide-debug-help)

;; ============================================================================
;; PERFORMANCE PROFILING
;; ============================================================================
(use-package esup
  :commands (esup)
  :init
  (setq esup-depth 0)
  :config
  (defun emacs-ide-profile-startup ()
    "Profile Emacs startup."
    (interactive)
    (if (fboundp 'esup)
        (esup)
      (message "⚠️  esup not installed."))))

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
