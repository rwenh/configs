;;; debug-config.el --- Debugging Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; DAP (Debug Adapter Protocol) and debugging support for multiple languages
;;; Save as: ~/.emacs.d/modules/debug-config.el
;;;
;;; Code:

;; ============================================================================
;; DAP MODE (Debug Adapter Protocol)
;; ============================================================================
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :commands (dap-debug dap-debug-edit-template)
  :bind (:map dap-mode-map
              ("<f7>" . dap-step-in)
              ("S-<f7>" . dap-next)
              ("C-<f7>" . dap-continue)
              ("M-<f7>" . dap-step-out)
              ("C-<f5>" . dap-breakpoint-toggle)
              ("C-S-<f5>" . dap-breakpoint-delete-all))
  :custom
  (dap-auto-configure-features '(sessions locals breakpoints expressions repl controls tooltip))
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  
  ;; Enable debugging for specific languages
  (require 'dap-python nil t)
  (require 'dap-node nil t)
  (require 'dap-go nil t)
  (require 'dap-lldb nil t)
  (require 'dap-gdb-lldb nil t)
  
  ;; Python debugging setup
  (when (featurep 'dap-python)
    (setq dap-python-debugger 'debugpy))
  
  ;; Node.js debugging setup
  (when (featurep 'dap-node)
    (dap-node-setup))
  
  ;; Go debugging setup
  (when (featurep 'dap-go)
    (dap-go-setup))
  
  ;; C/C++/Rust debugging with LLDB
  (when (and (featurep 'dap-lldb)
             (executable-find "lldb-vscode"))
    (dap-register-debug-template
     "LLDB::Run"
     (list :type "lldb-vscode"
           :request "launch"
           :name "LLDB::Run"
           :program nil
           :cwd nil))))

;; ============================================================================
;; DAP UI - Debugging Interface
;; ============================================================================
(use-package dap-ui
  :ensure nil
  :after dap-mode
  :config
  (dap-ui-mode 1))

;; ============================================================================
;; LANGUAGE-SPECIFIC DEBUGGING CONFIGURATIONS
;; ============================================================================

;; Python Debugging
(with-eval-after-load 'dap-python
  (defun emacs-ide-python-debug ()
    "Start Python debugging session."
    (interactive)
    (dap-debug
     (list :type "python"
           :args ""
           :cwd (projectile-project-root)
           :target-module (buffer-file-name)
           :request "launch"
           :name "Python :: Run file")))
  
  ;; Python test debugging
  (defun emacs-ide-python-debug-test ()
    "Debug current Python test."
    (interactive)
    (dap-debug
     (list :type "python"
           :args "-m pytest"
           :cwd (projectile-project-root)
           :module "pytest"
           :request "launch"
           :name "Python :: Debug Test"))))

;; Node.js/JavaScript Debugging
(with-eval-after-load 'dap-node
  (defun emacs-ide-node-debug ()
    "Start Node.js debugging session."
    (interactive)
    (dap-debug
     (list :type "node"
           :request "launch"
           :name "Node :: Run file"
           :program (buffer-file-name)
           :cwd (projectile-project-root)))))

;; Go Debugging
(with-eval-after-load 'dap-go
  (defun emacs-ide-go-debug ()
    "Start Go debugging session."
    (interactive)
    (dap-debug
     (list :type "go"
           :request "launch"
           :name "Go :: Debug"
           :mode "auto"
           :program (buffer-file-name)
           :buildFlags nil
           :args nil
           :env nil
           :cwd (projectile-project-root))))
  
  (defun emacs-ide-go-debug-test ()
    "Debug current Go test."
    (interactive)
    (dap-debug
     (list :type "go"
           :request "launch"
           :name "Go :: Debug Test"
           :mode "test"
           :program (buffer-file-name)
           :cwd (projectile-project-root)))))

;; C/C++/Rust Debugging with LLDB
(defun emacs-ide-lldb-debug ()
  "Start LLDB debugging session."
  (interactive)
  (let ((program (read-file-name "Select executable: "
                                 (projectile-project-root))))
    (dap-debug
     (list :type "lldb-vscode"
           :request "launch"
           :name "LLDB :: Debug"
           :program program
           :cwd (file-name-directory program)))))

;; Rust-specific debug config
(defun emacs-ide-rust-debug ()
  "Start Rust debugging session."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (target-dir (concat project-root "target/debug/"))
         (executable (read-file-name "Select executable: " target-dir)))
    (dap-debug
     (list :type "lldb-vscode"
           :request "launch"
           :name "Rust :: Debug"
           :program executable
           :cwd project-root
           :sourceMap (list (cons "/rustc/*" nil))))))

;; ============================================================================
;; GDB INTEGRATION (Traditional Debugger)
;; ============================================================================
(use-package gdb-mi
  :ensure nil
  :custom
  (gdb-many-windows t)
  (gdb-show-main t)
  :config
  (defun emacs-ide-gdb-debug ()
    "Start GDB debugging session."
    (interactive)
    (let ((executable (read-file-name "Select executable: "
                                     (projectile-project-root))))
      (gdb (format "gdb -i=mi %s" executable)))))

;; ============================================================================
;; EDEBUG (Emacs Lisp Debugger)
;; ============================================================================
(use-package edebug
  :ensure nil
  :custom
  (edebug-trace t)
  (edebug-print-length 50)
  (edebug-print-level 10)
  :config
  (defun emacs-ide-edebug-defun ()
    "Instrument function for debugging."
    (interactive)
    (eval-defun t)))

;; ============================================================================
;; DEBUGGING KEYBINDINGS SUMMARY
;; ============================================================================
(defun emacs-ide-show-debug-help ()
  "Display debugging keybindings and commands."
  (interactive)
  (with-output-to-temp-buffer "*Debug Help*"
    (princ "=== EMACS IDE DEBUGGING GUIDE ===\n\n")
    (princ "BREAKPOINT MANAGEMENT:\n")
    (princ "  C-<f5>      Toggle breakpoint at point\n")
    (princ "  C-S-<f5>    Delete all breakpoints\n\n")
    (princ "DEBUGGING CONTROLS:\n")
    (princ "  <f7>        Step into function\n")
    (princ "  S-<f7>      Step over (next line)\n")
    (princ "  M-<f7>      Step out of function\n")
    (princ "  C-<f7>      Continue execution\n\n")
    (princ "LANGUAGE-SPECIFIC DEBUG COMMANDS:\n")
    (princ "  M-x emacs-ide-python-debug      - Debug Python file\n")
    (princ "  M-x emacs-ide-python-debug-test - Debug Python test\n")
    (princ "  M-x emacs-ide-node-debug        - Debug Node.js file\n")
    (princ "  M-x emacs-ide-go-debug          - Debug Go program\n")
    (princ "  M-x emacs-ide-go-debug-test     - Debug Go test\n")
    (princ "  M-x emacs-ide-rust-debug        - Debug Rust program\n")
    (princ "  M-x emacs-ide-lldb-debug        - Debug with LLDB\n")
    (princ "  M-x emacs-ide-gdb-debug         - Debug with GDB\n\n")
    (princ "DAP UI WINDOWS:\n")
    (princ "  - Locals: View local variables\n")
    (princ "  - Expressions: Evaluate expressions\n")
    (princ "  - Breakpoints: Manage all breakpoints\n")
    (princ "  - Stack: View call stack\n")
    (princ "  - REPL: Interactive debugging console\n\n")
    (princ "REQUIREMENTS:\n")
    (princ "  Python:     pip install debugpy\n")
    (princ "  Node.js:    npm install -g node-inspect\n")
    (princ "  Go:         go install github.com/go-delve/delve/cmd/dlv@latest\n")
    (princ "  C/C++/Rust: Install lldb or gdb\n\n")
    (princ "Press q to close this buffer.\n")))

;; ============================================================================
;; DEBUGGING INSTALLATION HELPER
;; ============================================================================
(defun emacs-ide-install-debug-adapters ()
  "Guide user through installing debug adapters."
  (interactive)
  (let ((adapters '(("Python (debugpy)" . "pip3 install debugpy")
                    ("Node.js" . "npm install -g vscode-node-debug2")
                    ("Go (Delve)" . "go install github.com/go-delve/delve/cmd/dlv@latest")
                    ("C/C++/Rust (LLDB)" . "System package manager: lldb"))))
    (with-output-to-temp-buffer "*Debug Adapter Installation*"
      (princ "=== DEBUG ADAPTER INSTALLATION GUIDE ===\n\n")
      (princ "Run these commands to install debug adapters:\n\n")
      (dolist (adapter adapters)
        (princ (format "• %s:\n  %s\n\n" (car adapter) (cdr adapter))))
      (princ "\nAfter installation, restart Emacs and use:\n")
      (princ "  M-x dap-debug\n")
      (princ "to start debugging.\n"))))

;; ============================================================================
;; PERFORMANCE PROFILING
;; ============================================================================
(use-package esup
  :ensure t
  :commands (esup)
  :custom
  (esup-depth 0))

(defun emacs-ide-profile-startup ()
  "Profile Emacs startup time."
  (interactive)
  (esup))

;; ============================================================================
;; BENCHMARK HELPER
;; ============================================================================
(defun emacs-ide-benchmark-init ()
  "Benchmark init file loading."
  (interactive)
  (let ((start-time (current-time)))
    (load-file user-init-file)
    (message "Init loaded in %.2fs"
             (float-time (time-subtract (current-time) start-time)))))

(provide 'debug-config)
;;; debug-config.el ends here