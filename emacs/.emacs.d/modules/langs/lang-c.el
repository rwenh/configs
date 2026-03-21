;;; lang-c.el --- C / C++ / CUDA / CMake IDE layer -*- lexical-binding: t -*-
;;; Version: 1.0.3
;;; Fixes vs 1.0.2:
;;;   - FIX-TREESIT-CPP-NAME: Reverted (emacs-ide-dev-ensure-treesit 'c++) back
;;;     to (emacs-ide-dev-ensure-treesit 'cpp). The treesit recipe key for C++
;;;     is 'cpp — matching the shared library name libtree-sitter-cpp.so and the
;;;     convention in treesit-language-source-alist. Using 'c++ failed because
;;;     Emacs looks up the symbol directly as the alist key. The real fix is in
;;;     core-dev.el v1.0.4 which populates treesit-language-source-alist with
;;;     the source URL before calling treesit-install-language-grammar.
;;; Fixes vs 1.0.1:
;;;   - FIX-LSP: Removed lsp-deferred :hook from cc-mode use-package.
;;;     tools-lsp.el already hooks c-mode and c++-mode to
;;;     emacs-ide-lsp-deferred-optimized. The extra hook here caused LSP
;;;     to start twice in every C/C++ buffer.
;;; Code:
(require 'core-dev)
(emacs-ide-dev-register "c" :tier 1 :lsp-server "clangd"
  :formatter "clang-format" :test-cmd "ctest" :repl nil :modes '(c-mode c++-mode c-ts-mode c++-ts-mode))
(when (emacs-ide-dev-lang-enabled-p "c")
(emacs-ide-dev-ensure-treesit 'c)
(emacs-ide-dev-ensure-treesit 'cpp)   ; FIX-TREESIT-CPP-NAME: 'cpp is correct recipe key
(use-package cc-mode
  :straight nil :defer t
  :mode (("\\.c\\'" . c-mode) ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode) ("\\.hpp\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode) ("\\.cxx\\'" . c++-mode)
         ("\\.cu\\'" . c++-mode) ("\\.cuh\\'" . c++-mode))
  :init (setq c-default-style '((java-mode . "java") (other . "linux"))
              c-basic-offset 4)
  :config
  (defun emacs-ide-c-run ()
    (interactive)
    (if (executable-find "gcc")
        (compile (format "gcc -Wall -Wextra -O2 -std=c17 -g -o /tmp/c-out %s && /tmp/c-out"
                         (shell-quote-argument (buffer-file-name))))
      (message "lang-c: gcc not found")))
  (defun emacs-ide-cpp-run ()
    (interactive)
    (if (executable-find "g++")
        (compile (format "g++ -Wall -Wextra -O2 -std=c++20 -g -o /tmp/cpp-out %s && /tmp/cpp-out"
                         (shell-quote-argument (buffer-file-name))))
      (message "lang-c: g++ not found")))
  (emacs-ide-dev-bind-compile c-mode-map   #'emacs-ide-c-run)
  (emacs-ide-dev-bind-compile c++-mode-map #'emacs-ide-cpp-run))
(use-package lsp-mode
  ;; FIX-LSP: :hook removed for c-mode and c++-mode — tools-lsp.el already
  ;; hooks those. c-ts-mode and c++-ts-mode hooks kept (tools-lsp does not own them).
  :hook ((c-ts-mode c++-ts-mode) . lsp-deferred)
  :init (setq lsp-clangd-binary-path       (or (executable-find "clangd") "clangd")
              lsp-clients-clangd-args      '("--background-index" "--clang-tidy"
                                             "--header-insertion=iwyu" "--completion-style=detailed"
                                             "--function-arg-placeholders")))
(with-eval-after-load 'apheleia
  (dolist (m '(c-mode c++-mode c-ts-mode c++-ts-mode))
    (emacs-ide-dev-attach-formatter 'clang-format m)))
;; CMake
(use-package cmake-mode :defer t :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(use-package cmake-ide :after cmake-mode :hook (cmake-mode . cmake-ide-setup))
;; CUDA
(use-package cuda-mode :if (executable-find "nvcc") :defer t :mode "\\.cu\\'")
;; DAP — LLDB for C/C++
(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "C/C++ :: LLDB" 'dap-lldb)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template "C :: LLDB"
      (list :type "lldb" :request "launch" :name "C binary"
            :program (lambda () (read-file-name "Binary: " "/tmp/" nil t))
            :cwd "${workspaceFolder}"))
    (dap-register-debug-template "C++ :: LLDB"
      (list :type "lldb" :request "launch" :name "C++ binary"
            :program (lambda () (read-file-name "Binary: " "/tmp/" nil t))
            :cwd "${workspaceFolder}"))))
) (provide 'lang-c)
;;; lang-c.el ends here
