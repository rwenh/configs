;;; lang-core.el --- Professional Language Support (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; 50+ languages with LSP, Tree-sitter, compile-and-run, debugging
;;; Code:

;; ============================================================================
;; TREE-SITTER GRAMMAR SOURCE CONFIGURATION
;; ============================================================================
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  
  ;; Configure grammar sources BEFORE attempting installation
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/Azganoth/tree-sitter-lua")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

;; ============================================================================
;; TREE-SITTER AUTO WITH GRAMMAR INSTALLER
;; ============================================================================
(use-package treesit-auto
  :if (fboundp 'treesit-available-p)
  :init
  (setq treesit-auto-install 'prompt
        treesit-font-lock-level 4)
  :config
  (when (fboundp 'global-treesit-auto-mode)
    (global-treesit-auto-mode)))

;; Tree-sitter grammar installer helper
(defun emacs-ide-install-treesit-grammars ()
  "Install all tree-sitter grammars safely."
  (interactive)
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
    (let ((langs '(c cpp python rust go java javascript typescript tsx json yaml toml css)))
      (dolist (lang langs)
        (unless (treesit-language-available-p lang)
          (message "Installing tree-sitter grammar: %s" lang)
          (condition-case err
              (when (fboundp 'treesit-install-language-grammar)
                (treesit-install-language-grammar lang))
            (error (message "⚠️  Failed to install %s: %s" lang (error-message-string err)))))))))

;; Auto-install on first run if Python grammar is missing
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (not (treesit-language-available-p 'python)))
  (run-with-idle-timer 5 nil #'emacs-ide-install-treesit-grammars))

;; ============================================================================
;; C/C++/CUDA
;; ============================================================================
(use-package cc-mode
  :straight nil
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.hxx\\'" . c++-mode)
         ("\\.cu\\'" . c++-mode)
         ("\\.cuh\\'" . c++-mode))
  :init
  (setq c-default-style '((java-mode . "java")
                         (awk-mode . "awk")
                         (other . "linux"))
        c-basic-offset 4
        c-tab-always-indent t)
  :config
  (defun emacs-ide-c-compile-run ()
    "Compile and run C with optimization safely."
    (interactive)
    (if (executable-find "gcc")
        (compile (format "gcc -Wall -Wextra -O2 -std=c17 -g -o /tmp/a.out %s && /tmp/a.out"
                        (shell-quote-argument (buffer-file-name))))
      (message "⚠️  GCC not found")))

  (defun emacs-ide-cpp-compile-run ()
    "Compile and run C++ with optimization safely."
    (interactive)
    (if (executable-find "g++")
        (compile (format "g++ -Wall -Wextra -O2 -std=c++20 -g -o /tmp/a.out %s && /tmp/a.out"
                        (shell-quote-argument (buffer-file-name))))
      (message "⚠️  G++ not found")))

  (define-key c-mode-map (kbd "C-c C-c") 'emacs-ide-c-compile-run)
  (define-key c++-mode-map (kbd "C-c C-c") 'emacs-ide-cpp-compile-run))

(use-package cuda-mode
  :if (executable-find "nvcc")
  :mode "\\.cu\\'"
  :config
  (defun emacs-ide-cuda-compile ()
    "Compile CUDA safely."
    (interactive)
    (if (executable-find "nvcc")
        (compile (format "nvcc -o /tmp/cuda.out %s && /tmp/cuda.out"
                        (shell-quote-argument (buffer-file-name))))
      (message "⚠️  NVCC not found")))
  (define-key cuda-mode-map (kbd "C-c C-c") 'emacs-ide-cuda-compile))

;; ============================================================================
;; PYTHON
;; ============================================================================
(use-package python
  :straight nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" "python3")
  :init
  (setq python-indent-offset 4
        python-shell-interpreter (if (executable-find "python3") "python3" "python")
        python-shell-completion-native-enable nil
        python-indent-guess-indent-offset-verbose nil)
  :config
  (defun emacs-ide-python-run ()
    "Run Python file safely."
    (interactive)
    (if (executable-find "python3")
        (compile (format "python3 %s" (shell-quote-argument (buffer-file-name))))
      (message "⚠️  Python3 not found")))

  (defun emacs-ide-python-pytest ()
    "Run pytest safely."
    (interactive)
    (if (executable-find "pytest")
        (compile (format "pytest -vv %s" (shell-quote-argument (buffer-file-name))))
      (message "⚠️  pytest not found")))

  (define-key python-mode-map (kbd "C-c C-c") 'emacs-ide-python-run)
  (define-key python-mode-map (kbd "C-c C-t") 'emacs-ide-python-pytest))

;; ============================================================================
;; RUST
;; ============================================================================
(use-package rust-mode
  :if (executable-find "rustc")
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save (executable-find "rustfmt")
        rust-rustfmt-bin (or (executable-find "rustfmt") "rustfmt"))
  :config
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-compile)
  (define-key rust-mode-map (kbd "C-c C-r") 'rust-run))

(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode)
  :config
  (define-key cargo-minor-mode-map (kbd "C-c C-b") 'cargo-process-build)
  (define-key cargo-minor-mode-map (kbd "C-c C-r") 'cargo-process-run))

;; ============================================================================
;; GO
;; ============================================================================
(use-package go-mode
  :if (executable-find "go")
  :mode "\\.go\\'"
  :init
  (setq gofmt-command (if (executable-find "gofmt") "gofmt" "go fmt"))
  :config
  (defun emacs-ide-go-run ()
    "Run Go file safely."
    (interactive)
    (if (executable-find "go")
        (compile (format "go run %s" (shell-quote-argument (buffer-file-name))))
      (message "⚠️  Go not found")))

  (defun emacs-ide-go-build ()
    "Build Go binary safely."
    (interactive)
    (if (executable-find "go")
        (compile "go build -v")
      (message "⚠️  Go not found")))

  (add-hook 'before-save-hook 'gofmt-before-save nil t)
  (define-key go-mode-map (kbd "C-c C-c") 'emacs-ide-go-run)
  (define-key go-mode-map (kbd "C-c C-b") 'emacs-ide-go-build))

;; ============================================================================
;; JAVASCRIPT/TYPESCRIPT
;; ============================================================================
(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (setq js2-basic-offset 2
        js2-mode-show-parse-errors nil
        js2-strict-missing-semi-warning nil)
  :config
  (defun emacs-ide-node-run ()
    "Run with Node safely."
    (interactive)
    (if (executable-find "node")
        (compile (format "node %s" (shell-quote-argument (buffer-file-name))))
      (message "⚠️  Node not found")))

  (define-key js2-mode-map (kbd "C-c C-c") 'emacs-ide-node-run))

(use-package typescript-mode
  :if (executable-find "node")
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :init
  (setq typescript-indent-level 2)
  :config
  (defun emacs-ide-typescript-run ()
    "Run with ts-node safely."
    (interactive)
    (if (executable-find "ts-node")
        (compile (format "ts-node %s" (shell-quote-argument (buffer-file-name))))
      (message "⚠️  ts-node not found")))

  (define-key typescript-mode-map (kbd "C-c C-c") 'emacs-ide-typescript-run))

;; ============================================================================
;; WEB DEVELOPMENT
;; ============================================================================
(use-package web-mode
  :mode ("\\.html\\'" "\\.htm\\'" "\\.vue\\'" "\\.jsx\\'" "\\.tsx\\'")
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-indentation t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t))

(use-package emmet-mode
  :hook ((web-mode html-mode css-mode) . emmet-mode))

(use-package php-mode
  :if (executable-find "php")
  :mode "\\.php\\'"
  :config
  (defun emacs-ide-php-run ()
    "Run PHP safely."
    (interactive)
    (if (executable-find "php")
        (compile (format "php %s" (shell-quote-argument (buffer-file-name))))
      (message "⚠️  PHP not found")))

  (define-key php-mode-map (kbd "C-c C-c") 'emacs-ide-php-run))

;; ============================================================================
;; RUBY
;; ============================================================================
(use-package ruby-mode
  :if (executable-find "ruby")
  :mode "\\.rb\\'"
  :init
  (setq ruby-indent-level 2)
  :config
  (defun emacs-ide-ruby-run ()
    "Run Ruby safely."
    (interactive)
    (if (executable-find "ruby")
        (compile (format "ruby %s" (shell-quote-argument (buffer-file-name))))
      (message "⚠️  Ruby not found")))

  (define-key ruby-mode-map (kbd "C-c C-c") 'emacs-ide-ruby-run))

;; ============================================================================
;; HASKELL
;; ============================================================================
(use-package haskell-mode
  :if (executable-find "runhaskell")
  :mode "\\.hs\\'"
  :init
  (setq haskell-process-type 'stack-ghci)
  :config
  (defun emacs-ide-haskell-run ()
    "Run Haskell safely."
    (interactive)
    (if (executable-find "runhaskell")
        (compile (format "runhaskell %s" (shell-quote-argument (buffer-file-name))))
      (message "⚠️  runhaskell not found")))

  (define-key haskell-mode-map (kbd "C-c C-c") 'emacs-ide-haskell-run))

;; ============================================================================
;; SHELL SCRIPT
;; ============================================================================
(use-package sh-script
  :straight nil
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode))
  :config
  (defun emacs-ide-shell-run ()
    "Run shell script safely."
    (interactive)
    (if (executable-find "bash")
        (compile (format "bash %s" (shell-quote-argument (buffer-file-name))))
      (message "⚠️  Bash not found")))

  (define-key sh-mode-map (kbd "C-c C-c") 'emacs-ide-shell-run))

;; ============================================================================
;; DATA FORMATS
;; ============================================================================
(use-package json-mode
  :mode "\\.json\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

;; ============================================================================
;; MARKUP & DOCUMENTATION
;; ============================================================================
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init
  (setq markdown-command (if (executable-find "pandoc") "pandoc" "markdown")
        markdown-fontify-code-blocks-natively t
        markdown-enable-math t))

;; ============================================================================
;; CMAKE
;; ============================================================================
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; ============================================================================
;; ORG MODE (BUILTIN)
;; ============================================================================
(use-package org
  :straight nil
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-startup-indented t
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-startup-folded 'content))

(provide 'lang-core)
;;; lang-core.el ends here