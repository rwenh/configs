;;; lang-c.el --- C / C++ / CUDA / CMake IDE layer -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "c"
  :tier 1
  :lsp-server "clangd"
  :formatter  "clang-format"
  :test-cmd   "ctest"
  :repl       nil
  :modes      '(c-mode c++-mode c-ts-mode c++-ts-mode))

(when (emacs-ide-dev-lang-enabled-p "c")

;;;; ── Tree-sitter grammars ────────────────────────────────────────────────────

(emacs-ide-dev-ensure-treesit 'c)
(emacs-ide-dev-ensure-treesit 'cpp)

;;;; ── Run helpers ─────────────────────────────────────────────────────────────

(defun emacs-ide-c-run ()
  "Compile and run the current C file via gcc."
  (interactive)
  (if (executable-find "gcc")
      (compile (format "gcc -Wall -Wextra -O2 -std=c17 -g -o /tmp/c-out %s && /tmp/c-out"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-c: gcc not found on PATH")))

(defun emacs-ide-cpp-run ()
  "Compile and run the current C++ file via g++."
  (interactive)
  (if (executable-find "g++")
      (compile (format "g++ -Wall -Wextra -O2 -std=c++20 -g -o /tmp/cpp-out %s && /tmp/cpp-out"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-c: g++ not found on PATH")))

;;;; ── cc-mode (classic C / C++) ───────────────────────────────────────────────

(use-package cc-mode
  :straight nil
  :defer t
  :mode (("\\.c\\'"   . c-mode)
         ("\\.h\\'"   . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cc\\'"  . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.cu\\'"  . c++-mode)   ;; CUDA source (fallback if cuda-mode absent)
         ("\\.cuh\\'" . c++-mode))
  :init
  (setq c-default-style '((java-mode . "java") (other . "linux"))
        c-basic-offset  4)
  :config
  (emacs-ide-dev-bind-compile c-mode-map   #'emacs-ide-c-run)
  (emacs-ide-dev-bind-compile c++-mode-map #'emacs-ide-cpp-run)
  ;; Also bind compile key on ts-mode maps when available
  (with-eval-after-load 'c-ts-mode
    (when (boundp 'c-ts-mode-map)
      (emacs-ide-dev-bind-compile c-ts-mode-map   #'emacs-ide-c-run))
    (when (boundp 'c++-ts-mode-map)
      (emacs-ide-dev-bind-compile c++-ts-mode-map #'emacs-ide-cpp-run))))

;;;; ── LSP (clangd) ────────────────────────────────────────────────────────────

(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode) . lsp-deferred)
  :init
  ;; Nothing language-specific here — all clangd vars live in lsp-clangd.el
  ;; which is not loaded yet at :init time.  See :config below.
  nil
  :config
  (when (boundp 'lsp-clangd-binary-path)
    (setq lsp-clangd-binary-path
          (or (executable-find "clangd") "clangd")))
  (when (boundp 'lsp-clients-clangd-args)
    (setq lsp-clients-clangd-args
          '("--background-index"
            "--clang-tidy"
            "--header-insertion=iwyu"
            "--completion-style=detailed"
            "--function-arg-placeholders"
            "--pch-storage=memory"))))

;;;; ── Formatter (apheleia) ────────────────────────────────────────────────────

(with-eval-after-load 'apheleia
  (dolist (m '(c-mode c++-mode c-ts-mode c++-ts-mode))
    (emacs-ide-dev-attach-formatter 'clang-format m)))

;;;; ── CMake ───────────────────────────────────────────────────────────────────

(use-package cmake-mode
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;;;; ── CUDA ────────────────────────────────────────────────────────────────────

(use-package cuda-mode
  :if (executable-find "nvcc")
  :defer t
  :mode "\\.cu\\'")

;;;; ── Test runner ─────────────────────────────────────────────────────────────

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
     (t
      (message "lang-c: no CMakeLists.txt or Makefile found in %s" root)))))

(defun emacs-ide-c-test-file ()
  "Compile and run the current C/C++ file as a standalone test binary."
  (interactive)
  (when buffer-file-name
    (let* ((src  buffer-file-name)
           (ext  (file-name-extension src))
           (out  (expand-file-name "c-test-out" temporary-file-directory))
           (cc   (cond ((member ext '("c"))             "gcc -Wall -g")
                       ((member ext '("cpp" "cc" "cxx")) "g++ -Wall -g")
                       (t "gcc -Wall -g"))))
      (compile (format "%s -o %s %s && %s"
                       cc
                       (shell-quote-argument out)
                       (shell-quote-argument src)
                       (shell-quote-argument out))))))

(with-eval-after-load 'tools-test
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(c-mode c++-mode c-ts-mode c++-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-c-test-file
        :project-fn #'emacs-ide-c-test-project))))

;;;; ── DAP (LLDB) ──────────────────────────────────────────────────────────────

(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "C/C++ :: LLDB" 'dap-lldb)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template "C :: LLDB"
      (list :type    "lldb"
            :request "launch"
            :name    "C binary"
            :program (lambda ()
                       (read-file-name "Binary to debug: " "/tmp/" nil t))
            :cwd     (lambda ()
                       (or (and (fboundp 'projectile-project-root)
                                (ignore-errors (projectile-project-root)))
                           default-directory))))
    (dap-register-debug-template "C++ :: LLDB"
      (list :type    "lldb"
            :request "launch"
            :name    "C++ binary"
            :program (lambda ()
                       (read-file-name "Binary to debug: " "/tmp/" nil t))
            :cwd     (lambda ()
                       (or (and (fboundp 'projectile-project-root)
                                (ignore-errors (projectile-project-root)))
                           default-directory))))))

) ;; end (when c enabled)

(provide 'lang-c)
;;; lang-c.el ends here
