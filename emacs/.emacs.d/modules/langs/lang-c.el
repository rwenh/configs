;;; lang-c.el --- C / C++ / CUDA / CMake IDE layer -*- lexical-binding: t -*-
;;; Version: 3.0.4

(require 'core-dev)

(emacs-ide-dev-register "c" :tier 1 :lsp-server "clangd"
  :formatter "clang-format" :test-cmd "ctest" :repl nil
  :modes '(c-mode c++-mode c-ts-mode c++-ts-mode))

(when (emacs-ide-dev-lang-enabled-p "c")

(emacs-ide-dev-ensure-treesit 'c)
(emacs-ide-dev-ensure-treesit 'cpp)

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
  (emacs-ide-dev-bind-compile c-mode-map   #'emacs-ide-c-run)
  (emacs-ide-dev-bind-compile c++-mode-map #'emacs-ide-cpp-run))

(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :hook ((c-ts-mode c++-ts-mode) . lsp-deferred)
  :init
  (setq lsp-clangd-binary-path  (or (executable-find "clangd") "clangd")
        lsp-clients-clangd-args '("--background-index"
                                  "--clang-tidy"
                                  "--header-insertion=iwyu"
                                  "--completion-style=detailed"
                                  "--function-arg-placeholders")))

(with-eval-after-load 'apheleia
  (dolist (m '(c-mode c++-mode c-ts-mode c++-ts-mode))
    (emacs-ide-dev-attach-formatter 'clang-format m)))

(use-package cmake-mode
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cuda-mode
  :if (executable-find "nvcc")
  :defer t
  :mode "\\.cu\\'")

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

)

(provide 'lang-c)
;;; lang-c.el ends here
