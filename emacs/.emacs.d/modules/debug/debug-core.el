;;; debug-config.el --- Professional Debugging Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; DAP, GDB, LLDB, language-specific debuggers
;;; Code:

;; ============================================================================
;; DAP MODE - DEBUG ADAPTER PROTOCOL
;; ============================================================================
(use-package dap-mode
  :after lsp-mode
  :commands (dap-debug dap-debug-edit-template)
  :bind (("<f5>" . dap-debug)
         ("<f6>" . dap-debug-restart)
         ("<f7>" . dap-step-in)
         ("S-<f7>" . dap-next)
         ("M-<f7>" . dap-step-out)
         ("C-<f7>" . dap-continue)
         ("<f9>" . dap-breakpoint-toggle)
         ("C-<f9>" . dap-breakpoint-condition)
         ("S-<f9>" . dap-breakpoint-log-message)
         ("C-S-<f9>" . dap-breakpoint-delete-all)
         :map dap-mode-map
         ("C-c d d" . dap-debug)
         ("C-c d r" . dap-debug-restart)
         ("C-c d l" . dap-debug-last)
         ("C-c d e" . dap-debug-edit-template)
         ("C-c d b" . dap-breakpoint-toggle)
         ("C-c d c" . dap-breakpoint-condition)
         ("C-c d h" . dap-hydra)
         ("C-c d w" . dap-ui-repl))
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip repl)
        dap-auto-show-output t
        dap-ui-buffer-configurations
        `((,"*dap-ui-locals*" (side . right) (slot . 1) (window-width . 0.20))
          (,"*dap-ui-expressions*" (side . right) (slot . 2) (window-width . 0.20))
          (,"*dap-ui-sessions*" (side . right) (slot . 3) (window-width . 0.20))
          (,"*dap-ui-breakpoints*" (side . left) (slot . 2) (window-width . 0.20))
          (,"*dap-ui-repl*" (side . bottom) (slot . 1) (window-height . 0.20))))
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  
  (require 'dap-python nil t)
  (require 'dap-node nil t)
  (require 'dap-go nil t)
  (require 'dap-lldb nil t)
  (require 'dap-gdb-lldb nil t)
  (require 'dap-cpptools nil t)
  (require 'dap-java nil t)
  (require 'dap-php nil t)
  (require 'dap-ruby nil t)
  (require 'dap-elixir nil t))

;; ============================================================================
;; PYTHON DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-python
  (setq dap-python-debugger 'debugpy)
  
  (dap-register-debug-template
   "Python :: Run File"
   (list :type "python"
         :args ""
         :cwd (projectile-project-root)
         :program (buffer-file-name)
         :request "launch"
         :name "Python :: Run File"))
  
  (dap-register-debug-template
   "Python :: Run pytest"
   (list :type "python"
         :args "-m pytest -v"
         :cwd (projectile-project-root)
         :module "pytest"
         :request "launch"
         :name "Python :: pytest"))
  
  (dap-register-debug-template
   "Python :: Django"
   (list :type "python"
         :args "runserver --noreload"
         :cwd (projectile-project-root)
         :program "manage.py"
         :request "launch"
         :name "Python :: Django")))

;; ============================================================================
;; NODE.JS/JAVASCRIPT DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-node
  (dap-node-setup)
  
  (dap-register-debug-template
   "Node :: Run File"
   (list :type "node"
         :request "launch"
         :name "Node :: Run File"
         :program (buffer-file-name)
         :cwd (projectile-project-root)))
  
  (dap-register-debug-template
   "Node :: Run Jest Test"
   (list :type "node"
         :request "launch"
         :name "Node :: Jest"
         :program "${workspaceFolder}/node_modules/.bin/jest"
         :args '("--runInBand")
         :cwd (projectile-project-root))))

;; ============================================================================
;; GO DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-go
  (dap-go-setup)
  
  (dap-register-debug-template
   "Go :: Run File"
   (list :type "go"
         :request "launch"
         :name "Go :: Run File"
         :mode "auto"
         :program (buffer-file-name)
         :cwd (projectile-project-root)))
  
  (dap-register-debug-template
   "Go :: Debug Test"
   (list :type "go"
         :request "launch"
         :name "Go :: Debug Test"
         :mode "test"
         :program (buffer-file-name)
         :cwd (projectile-project-root))))

;; ============================================================================
;; C/C++/RUST DEBUGGING - LLDB/GDB
;; ============================================================================
(with-eval-after-load 'dap-lldb
  (dap-register-debug-template
   "LLDB :: Run"
   (list :type "lldb"
         :request "launch"
         :name "LLDB :: Run"
         :program nil
         :cwd (projectile-project-root)))
  
  (dap-register-debug-template
   "LLDB :: Attach"
   (list :type "lldb"
         :request "attach"
         :name "LLDB :: Attach"
         :pid nil)))

(with-eval-after-load 'dap-cpptools
  (dap-register-debug-template
   "GDB :: Run"
   (list :type "cppdbg"
         :request "launch"
         :name "GDB :: Run"
         :MIMode "gdb"
         :program nil
         :cwd (projectile-project-root))))

;; ============================================================================
;; RUST DEBUGGING
;; ============================================================================
(defun emacs-ide-rust-debug ()
  "Debug Rust program with LLDB."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (target-dir (concat project-root "target/debug/"))
         (executable (read-file-name "Select executable: " target-dir)))
    (dap-debug
     (list :type "lldb"
           :request "launch"
           :name "Rust :: Debug"
           :program executable
           :cwd project-root
           :sourceMap (list (cons "/rustc/*" nil))))))

;; ============================================================================
;; JAVA DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-java
  (dap-java-setup))

;; ============================================================================
;; PHP DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-php
  (dap-php-setup))

;; ============================================================================
;; RUBY DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-ruby
  (dap-ruby-setup))

;; ============================================================================
;; ELIXIR DEBUGGING
;; ============================================================================
(with-eval-after-load 'dap-elixir
  (dap-elixir-setup))

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
    (let ((executable (read-file-name "Select executable: "
                                     (projectile-project-root))))
      (gdb (format "gdb -i=mi %s" executable)))))

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
    "Debug Python with pdb."
    (interactive)
    (realgud:pdb (format "python -m pdb %s" (buffer-file-name))))
  
  (defun emacs-ide-realgud-node ()
    "Debug Node.js."
    (interactive)
    (realgud:node-debug (format "node inspect %s" (buffer-file-name)))))

;; ============================================================================
;; DEBUGGING HYDRA
;; ============================================================================
(use-package hydra
  :config
  (defhydra hydra-debug (:color pink :hint nil)
    "
^Control^         ^Breakpoints^      ^Navigation^       ^Info^
^^^^^^^^-----------------------------------------------------------------
_c_: continue     _b_: toggle        _n_: next          _l_: locals
_s_: step in      _C_: condition     _i_: step in       _e_: expressions
_S_: step out     _L_: log message   _o_: step out      _s_: stack
_r_: restart      _D_: delete all    _u_: up frame      _w_: watch
_q_: quit                            _d_: down frame    _R_: repl
"
    ("c" dap-continue)
    ("s" dap-step-in)
    ("S" dap-step-out)
    ("n" dap-next)
    ("r" dap-debug-restart)
    ("b" dap-breakpoint-toggle)
    ("C" dap-breakpoint-condition)
    ("L" dap-breakpoint-log-message)
    ("D" dap-breakpoint-delete-all)
    ("i" dap-step-in)
    ("o" dap-step-out)
    ("u" dap-up-stack-frame)
    ("d" dap-down-stack-frame)
    ("l" dap-ui-locals)
    ("e" dap-ui-expressions)
    ("s" dap-ui-sessions)
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
    (princ "WINDOWS:\n")
    (princ "  C-c d l     Locals\n")
    (princ "  C-c d e     Expressions\n")
    (princ "  C-c d b     Breakpoints\n")
    (princ "  C-c d s     Sessions/Stack\n")
    (princ "  C-c d w     REPL\n")
    (princ "  C-c d h     Debug hydra\n\n")
    (princ "LANGUAGE-SPECIFIC:\n")
    (princ "  Python:     debugpy (pip install debugpy)\n")
    (princ "  Node.js:    Built-in node inspect\n")
    (princ "  Go:         Delve (go install github.com/go-delve/delve/cmd/dlv@latest)\n")
    (princ "  C/C++/Rust: LLDB/GDB\n")
    (princ "  Java:       JDTLS (LSP)\n\n")
    (princ "QUICK START:\n")
    (princ "  1. Set breakpoint with F9\n")
    (princ "  2. Start debug with F5\n")
    (princ "  3. Step through with F7/S-F7/M-F7\n")
    (princ "  4. Inspect variables in locals window\n")
    (princ "  5. Continue with C-F7\n\n")))

(global-set-key (kbd "C-c d ?") 'emacs-ide-debug-help)

;; ============================================================================
;; PERFORMANCE PROFILING
;; ============================================================================
(use-package esup
  :commands (esup)
  :init
  (setq esup-depth 0))

(defun emacs-ide-profile-startup ()
  "Profile Emacs startup."
  (interactive)
  (esup))

(use-package profiler
  :straight nil
  :bind (("C-c D s" . profiler-start)
         ("C-c D r" . profiler-report)
         ("C-c D q" . profiler-stop)))

;; ============================================================================
;; BENCHMARK
;; ============================================================================
(defun emacs-ide-benchmark-init ()
  "Benchmark init time."
  (interactive)
  (let ((start-time (current-time)))
    (load-file user-init-file)
    (message "Init loaded in %.2fs"
             (float-time (time-subtract (current-time) start-time)))))

;; ============================================================================
;; DEBUGGING INSTALLATION CHECK
;; ============================================================================
(defun emacs-ide-check-debug-tools ()
  "Check debug tool availability."
  (interactive)
  (let ((tools '(("debugpy" . "pip3 install debugpy")
                 ("node" . "Built-in")
                 ("dlv" . "go install github.com/go-delve/delve/cmd/dlv@latest")
                 ("lldb" . "System package manager")
                 ("gdb" . "System package manager")))
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
        (princ (format "  ✓ %s\n" (car tool))))
      (when missing
        (princ "\nMISSING:\n")
        (dolist (tool missing)
          (princ (format "  ✗ %s - Install: %s\n" (car tool) (cdr tool))))))))

(provide 'debug-config)
;;; debug-config.el ends here