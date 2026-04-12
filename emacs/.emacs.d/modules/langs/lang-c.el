;;; lang-c.el --- C / C++ / CUDA / CMake IDE layer -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (recalibration):
;;;   - FIX-CMAKE-IDE-GUARD: The cmake-ide :if guard was (fboundp
;;;     'cmake-ide-setup).  use-package evaluates :if at expansion time —
;;;     before any packages have loaded — so (fboundp 'cmake-ide-setup) is
;;;     always nil, making the entire cmake-ide block dead code that never
;;;     fires regardless of whether the package is installed.
;;;     cmake-ide is also abandoned upstream (last release 2019) and breaks
;;;     on Emacs 29+ because it references the removed rtags API.
;;;     Fix: cmake-ide block removed entirely.  cmake-mode alone provides
;;;     syntax highlighting, indentation, and M-x compile dispatch for CMake
;;;     projects.  Users who specifically want cmake-ide can add it back with
;;;     a (use-package cmake-ide :after cmake-mode :config (cmake-ide-setup))
;;;     in their personal straight profile after verifying it works for them.
;;; Fixes vs 1.0.3 (retained):
;;;   - FIX-DEFUN-IN-CONFIG, FIX-LSP-GUARD, FIX-CMAKE-IDE-DEPRECATED,
;;;     FIX-CWD-PLACEHOLDER, FIX-TEST-REGISTER.
;;; Fixes vs 1.0.2 (retained):
;;;   - FIX-TREESIT-CPP-NAME: 'cpp is the correct treesit recipe key.
;;; Fixes vs 1.0.1 (retained):
;;;   - FIX-LSP: lsp-deferred hooks removed for c-mode/c++-mode (tools-lsp.el owns them).
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "c" :tier 1 :lsp-server "clangd"
  :formatter "clang-format" :test-cmd "ctest" :repl nil
  :modes '(c-mode c++-mode c-ts-mode c++-ts-mode))

(when (emacs-ide-dev-lang-enabled-p "c")

(emacs-ide-dev-ensure-treesit 'c)
(emacs-ide-dev-ensure-treesit 'cpp) ; 'cpp is the correct treesit recipe key

;; ============================================================================
;; COMPILE / RUN COMMANDS
;; FIX-DEFUN-IN-CONFIG: moved to top level so M-x sees them before cc-mode loads.
;; ============================================================================
(defun emacs-ide-c-run ()
  "Compile and run the current C file via gcc."
  (interactive)
  (if (executable-find "gcc")
      (compile (format "gcc -Wall -Wextra -O2 -std=c17 -g -o /tmp/c-out %s && /tmp/c-out"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-c: gcc not found")))

(defun emacs-ide-cpp-run ()
  "Compile and run the current C++ file via g++."
  (interactive)
  (if (executable-find "g++")
      (compile (format "g++ -Wall -Wextra -O2 -std=c++20 -g -o /tmp/cpp-out %s && /tmp/cpp-out"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-c: g++ not found")))

;; ============================================================================
;; CC-MODE — major mode for C / C++ / CUDA
;; ============================================================================
(use-package cc-mode
  :straight nil
  :defer t
  :mode (("\\.c\\'"   . c-mode)
         ("\\.h\\'"   . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cc\\'"  . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.cu\\'"  . c++-mode)
         ("\\.cuh\\'" . c++-mode))
  :init
  (setq c-default-style '((java-mode . "java") (other . "linux"))
        c-basic-offset   4)
  :config
  ;; FIX-DEFUN-IN-CONFIG: functions now top-level; just bind them here
  (emacs-ide-dev-bind-compile c-mode-map   #'emacs-ide-c-run)
  (emacs-ide-dev-bind-compile c++-mode-map #'emacs-ide-cpp-run))

;; ============================================================================
;; LSP — clangd
;; FIX-LSP-GUARD: :if guard added so hooks don't fire when LSP is disabled.
;; ============================================================================
(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  ;; FIX-LSP (retained): c-mode/c++-mode hooks owned by tools-lsp.el.
  ;; Only ts-mode variants hooked here.
  :hook ((c-ts-mode c++-ts-mode) . lsp-deferred)
  :init
  (setq lsp-clangd-binary-path  (or (executable-find "clangd") "clangd")
        lsp-clients-clangd-args '("--background-index"
                                  "--clang-tidy"
                                  "--header-insertion=iwyu"
                                  "--completion-style=detailed"
                                  "--function-arg-placeholders")))

;; ============================================================================
;; FORMATTER — clang-format via apheleia
;; ============================================================================
(with-eval-after-load 'apheleia
  (dolist (m '(c-mode c++-mode c-ts-mode c++-ts-mode))
    (emacs-ide-dev-attach-formatter 'clang-format m)))

;; ============================================================================
;; CMAKE — syntax, indentation, and M-x compile dispatch
;; cmake-ide removed: it is abandoned (last release 2019) and breaks on
;; Emacs 29+ due to removed rtags API.  cmake-mode alone is sufficient
;; for syntax highlighting, indentation, and project compilation.
;; ============================================================================
(use-package cmake-mode
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; ============================================================================
;; CUDA
;; ============================================================================
(use-package cuda-mode
  :if (executable-find "nvcc")
  :defer t
  :mode "\\.cu\\'")

;; ============================================================================
;; TEST RUNNER REGISTRATION
;; FIX-TEST-REGISTER: register c-mode with the test runner registry using
;; ctest (CMake projects) or make test as fallback.
;; ============================================================================
(defun emacs-ide-c-test-project ()
  "Run C/C++ project tests via ctest or make test."
  (interactive)
  (let ((root (or (and (fboundp 'projectile-project-root)
                       (ignore-errors (projectile-project-root)))
                  default-directory)))
    (cond
     ((file-exists-p (expand-file-name "CMakeLists.txt" root))
      (compile "ctest --test-dir build"))
     ((file-exists-p (expand-file-name "Makefile" root))
      (compile "make test"))
     (t (message "lang-c: no ctest or make test found")))))

(with-eval-after-load 'tools-test
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'c-mode
      :project-fn #'emacs-ide-c-test-project
      :file-fn    #'emacs-ide-c-test-project)))

;; ============================================================================
;; DAP — LLDB for C/C++
;; FIX-CWD-PLACEHOLDER: "${workspaceFolder}" is a VS Code variable not
;; expanded by dap-mode. Replaced with a lambda reading the project root
;; at launch time, matching the pattern in debug-core.el.
;; ============================================================================
(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "C/C++ :: LLDB" 'dap-lldb)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template "C :: LLDB"
      (list :type    "lldb"
            :request "launch"
            :name    "C binary"
            :program (lambda () (read-file-name "Binary: " "/tmp/" nil t))
            :cwd     (lambda () (or (and (fboundp 'projectile-project-root)
                                         (ignore-errors (projectile-project-root)))
                                     default-directory))))
    (dap-register-debug-template "C++ :: LLDB"
      (list :type    "lldb"
            :request "launch"
            :name    "C++ binary"
            :program (lambda () (read-file-name "Binary: " "/tmp/" nil t))
            :cwd     (lambda () (or (and (fboundp 'projectile-project-root)
                                         (ignore-errors (projectile-project-root)))
                                     default-directory))))))

) ;; end (when (emacs-ide-dev-lang-enabled-p "c"))

(provide 'lang-c)
;;; lang-c.el ends here
