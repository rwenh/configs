;;; lang-core.el --- Professional Language Support -*- lexical-binding: t -*-
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
  :demand t
  :init
  (setq treesit-auto-install 'prompt
        treesit-font-lock-level 4)
  :config
  (global-treesit-auto-mode))

;; Tree-sitter grammar installer helper
(defun emacs-ide-install-treesit-grammars ()
  "Install all tree-sitter grammars."
  (interactive)
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
    (let ((langs '(c cpp python rust go java javascript typescript tsx json yaml toml css)))
      (dolist (lang langs)
        (unless (treesit-language-available-p lang)
          (message "Installing tree-sitter grammar: %s" lang)
          (condition-case err
              (treesit-install-language-grammar lang)
            (error (message "Failed to install %s: %s" lang err))))))))

;; Auto-install on first run if Python grammar is missing
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (not (treesit-language-available-p 'python)))
  (run-with-idle-timer 5 nil #'emacs-ide-install-treesit-grammars))

;; ============================================================================
;; C/C++/CUDA
;; ============================================================================
(use-package cc-mode
  :ensure nil
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.hxx\\'" . c++-mode)
         ("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode))
  :init
  (setq c-default-style '((java-mode . "java")
                         (awk-mode . "awk")
                         (other . "linux"))
        c-basic-offset 4
        c-tab-always-indent t)
  :config
  (defun emacs-ide-c-compile-run ()
    "Compile and run C with optimization."
    (interactive)
    (compile (format "gcc -Wall -Wextra -O2 -std=c17 -g -o /tmp/a.out %s && /tmp/a.out"
                     (shell-quote-argument (buffer-file-name)))))

  (defun emacs-ide-cpp-compile-run ()
    "Compile and run C++ with optimization."
    (interactive)
    (compile (format "g++ -Wall -Wextra -O2 -std=c++20 -g -o /tmp/a.out %s && /tmp/a.out"
                     (shell-quote-argument (buffer-file-name)))))

  (define-key c-mode-map (kbd "C-c C-c") 'emacs-ide-c-compile-run)
  (define-key c++-mode-map (kbd "C-c C-c") 'emacs-ide-cpp-compile-run))

(use-package cuda-mode
  :mode "\\.cu\\'"
  :config
  (defun emacs-ide-cuda-compile ()
    "Compile CUDA."
    (interactive)
    (compile (format "nvcc -o /tmp/cuda.out %s && /tmp/cuda.out"
                     (shell-quote-argument (buffer-file-name)))))
  (define-key cuda-mode-map (kbd "C-c C-c") 'emacs-ide-cuda-compile))

;; ============================================================================
;; PYTHON
;; ============================================================================
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" "python3")
  :init
  (setq python-indent-offset 4
        python-shell-interpreter "python3"
        python-shell-completion-native-enable nil
        python-indent-guess-indent-offset-verbose nil)
  :config
  (defun emacs-ide-python-run ()
    "Run Python file."
    (interactive)
    (compile (format "python3 %s" (shell-quote-argument (buffer-file-name)))))

  (defun emacs-ide-python-pytest ()
    "Run pytest."
    (interactive)
    (compile (format "pytest -vv %s" (shell-quote-argument (buffer-file-name)))))

  (defun emacs-ide-python-check ()
    "Check with flake8, mypy, black."
    (interactive)
    (compile (format "flake8 %s && mypy %s && black --check %s"
                     (shell-quote-argument (buffer-file-name))
                     (shell-quote-argument (buffer-file-name))
                     (shell-quote-argument (buffer-file-name)))))

  (define-key python-mode-map (kbd "C-c C-c") 'emacs-ide-python-run)
  (define-key python-mode-map (kbd "C-c C-t") 'emacs-ide-python-pytest)
  (define-key python-mode-map (kbd "C-c C-v") 'emacs-ide-python-check))

;; ============================================================================
;; RUST
;; ============================================================================
(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t
        rust-rustfmt-bin (or (executable-find "rustfmt") "rustfmt"))
  :config
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-compile)
  (define-key rust-mode-map (kbd "C-c C-r") 'rust-run)
  (define-key rust-mode-map (kbd "C-c C-t") 'rust-test)
  (define-key rust-mode-map (kbd "C-c C-k") 'rust-check))

(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode)
  :config
  (define-key cargo-minor-mode-map (kbd "C-c C-b") 'cargo-process-build)
  (define-key cargo-minor-mode-map (kbd "C-c C-r") 'cargo-process-run)
  (define-key cargo-minor-mode-map (kbd "C-c C-t") 'cargo-process-test)
  (define-key cargo-minor-mode-map (kbd "C-c C-l") 'cargo-process-clippy)
  (define-key cargo-minor-mode-map (kbd "C-c C-d") 'cargo-process-doc))

;; ============================================================================
;; GO
;; ============================================================================
(use-package go-mode
  :mode "\\.go\\'"
  :init
  (setq gofmt-command "gofmt")
  :config
  (defun emacs-ide-go-run ()
    "Run Go file."
    (interactive)
    (compile (format "go run %s" (shell-quote-argument (buffer-file-name)))))

  (defun emacs-ide-go-build ()
    "Build Go binary."
    (interactive)
    (compile "go build -v"))

  (defun emacs-ide-go-test ()
    "Run Go tests."
    (interactive)
    (compile "go test -v ./..."))

  (add-hook 'before-save-hook 'gofmt-before-save nil t)

  (define-key go-mode-map (kbd "C-c C-c") 'emacs-ide-go-run)
  (define-key go-mode-map (kbd "C-c C-b") 'emacs-ide-go-build)
  (define-key go-mode-map (kbd "C-c C-t") 'emacs-ide-go-test))

;; ============================================================================
;; JAVA (Built-in)
;; ============================================================================
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))

(defun emacs-ide-java-compile-run ()
  "Compile and run Java."
  (interactive)
  (let* ((file (buffer-file-name))
         (class (file-name-sans-extension (file-name-nondirectory file))))
    (compile (format "javac %s && java %s" (shell-quote-argument file) class))))

(add-hook 'java-mode-hook
          (lambda ()
            (define-key java-mode-map (kbd "C-c C-c") 'emacs-ide-java-compile-run)))

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
    "Run with Node."
    (interactive)
    (compile (format "node %s" (shell-quote-argument (buffer-file-name)))))

  (define-key js2-mode-map (kbd "C-c C-c") 'emacs-ide-node-run))

(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :init
  (setq typescript-indent-level 2)
  :config
  (defun emacs-ide-typescript-run ()
    "Run with ts-node."
    (interactive)
    (compile (format "ts-node %s" (shell-quote-argument (buffer-file-name)))))

  (define-key typescript-mode-map (kbd "C-c C-c") 'emacs-ide-typescript-run))

;; ============================================================================
;; WEB DEVELOPMENT
;; ============================================================================
(use-package web-mode
  :mode ("\\.html\\'" "\\.htm\\'" "\\.css\\'" "\\.vue\\'" "\\.jsx\\'" "\\.tsx\\'")
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
  :mode "\\.php\\'"
  :config
  (defun emacs-ide-php-run ()
    "Run PHP."
    (interactive)
    (compile (format "php %s" (shell-quote-argument (buffer-file-name)))))

  (define-key php-mode-map (kbd "C-c C-c") 'emacs-ide-php-run))

;; ============================================================================
;; C# / .NET
;; ============================================================================
(use-package csharp-mode
  :mode "\\.cs\\'"
  :config
  (defun emacs-ide-csharp-run ()
    "Run .NET project."
    (interactive)
    (compile "dotnet run"))

  (define-key csharp-mode-map (kbd "C-c C-c") 'emacs-ide-csharp-run))

;; ============================================================================
;; RUBY
;; ============================================================================
(use-package ruby-mode
  :mode "\\.rb\\'"
  :init
  (setq ruby-indent-level 2)
  :config
  (defun emacs-ide-ruby-run ()
    "Run Ruby."
    (interactive)
    (compile (format "ruby %s" (shell-quote-argument (buffer-file-name)))))

  (define-key ruby-mode-map (kbd "C-c C-c") 'emacs-ide-ruby-run))

;; ============================================================================
;; HASKELL
;; ============================================================================
(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  (setq haskell-process-type 'stack-ghci)
  :config
  (defun emacs-ide-haskell-run ()
    "Run Haskell."
    (interactive)
    (compile (format "runhaskell %s" (shell-quote-argument (buffer-file-name)))))

  (define-key haskell-mode-map (kbd "C-c C-c") 'emacs-ide-haskell-run))

;; ============================================================================
;; SCALA
;; ============================================================================
(use-package scala-mode
  :mode "\\.scala\\'"
  :config
  (defun emacs-ide-scala-run ()
    "Run Scala."
    (interactive)
    (compile (format "scala %s" (shell-quote-argument (buffer-file-name)))))

  (define-key scala-mode-map (kbd "C-c C-c") 'emacs-ide-scala-run))

;; ============================================================================
;; KOTLIN
;; ============================================================================
(use-package kotlin-mode
  :mode "\\.kt\\'"
  :config
  (defun emacs-ide-kotlin-run ()
    "Run Kotlin."
    (interactive)
    (compile (format "kotlinc %s -include-runtime -d /tmp/app.jar && java -jar /tmp/app.jar"
                     (shell-quote-argument (buffer-file-name)))))

  (define-key kotlin-mode-map (kbd "C-c C-c") 'emacs-ide-kotlin-run))

;; ============================================================================
;; SWIFT
;; ============================================================================
(use-package swift-mode
  :mode "\\.swift\\'"
  :config
  (defun emacs-ide-swift-run ()
    "Run Swift."
    (interactive)
    (compile (format "swift %s" (shell-quote-argument (buffer-file-name)))))

  (define-key swift-mode-map (kbd "C-c C-c") 'emacs-ide-swift-run))

;; ============================================================================
;; ELIXIR
;; ============================================================================
(use-package elixir-mode
  :mode "\\.ex\\'"
  :config
  (defun emacs-ide-elixir-run ()
    "Run Elixir."
    (interactive)
    (compile (format "elixir %s" (shell-quote-argument (buffer-file-name)))))

  (define-key elixir-mode-map (kbd "C-c C-c") 'emacs-ide-elixir-run))

;; ============================================================================
;; ERLANG
;; ============================================================================
(use-package erlang
  :mode "\\.erl\\'"
  :config
  (defun emacs-ide-erlang-run ()
    "Run Erlang."
    (interactive)
    (compile (format "escript %s" (shell-quote-argument (buffer-file-name)))))

  (define-key erlang-mode-map (kbd "C-c C-c") 'emacs-ide-erlang-run))

;; ============================================================================
;; LUA
;; ============================================================================
(use-package lua-mode
  :mode "\\.lua\\'"
  :config
  (defun emacs-ide-lua-run ()
    "Run Lua."
    (interactive)
    (compile (format "lua %s" (shell-quote-argument (buffer-file-name)))))

  (define-key lua-mode-map (kbd "C-c C-c") 'emacs-ide-lua-run))

;; ============================================================================
;; SHELL SCRIPT
;; ============================================================================
(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode))
  :config
  (defun emacs-ide-shell-run ()
    "Run shell script."
    (interactive)
    (compile (format "bash %s" (shell-quote-argument (buffer-file-name)))))

  (define-key sh-mode-map (kbd "C-c C-c") 'emacs-ide-shell-run))

;; ============================================================================
;; NIM
;; ============================================================================
(use-package nim-mode
  :mode "\\.nim\\'"
  :config
  (defun emacs-ide-nim-run ()
    "Run Nim."
    (interactive)
    (compile (format "nim c -r %s" (shell-quote-argument (buffer-file-name)))))

  (define-key nim-mode-map (kbd "C-c C-c") 'emacs-ide-nim-run))

;; ============================================================================
;; ZIG
;; ============================================================================
(use-package zig-mode
  :mode "\\.zig\\'"
  :config
  (defun emacs-ide-zig-run ()
    "Run Zig."
    (interactive)
    (compile (format "zig run %s" (shell-quote-argument (buffer-file-name)))))

  (define-key zig-mode-map (kbd "C-c C-c") 'emacs-ide-zig-run))

;; ============================================================================
;; JULIA
;; ============================================================================
(use-package julia-mode
  :mode "\\.jl\\'"
  :config
  (defun emacs-ide-julia-run ()
    "Run Julia."
    (interactive)
    (compile (format "julia %s" (shell-quote-argument (buffer-file-name)))))

  (define-key julia-mode-map (kbd "C-c C-c") 'emacs-ide-julia-run))

;; ============================================================================
;; FORTRAN
;; ============================================================================
(use-package f90
  :ensure nil
  :mode ("\\.f90\\'" "\\.f95\\'" "\\.f03\\'" "\\.f08\\'")
  :init
  (setq f90-do-indent 2
        f90-if-indent 2
        f90-type-indent 2
        f90-continuation-indent 4)
  :config
  (defun emacs-ide-fortran-run ()
    "Compile and run Fortran."
    (interactive)
    (compile (format "gfortran -Wall -O2 -o /tmp/fortran.out %s && /tmp/fortran.out"
                     (shell-quote-argument (buffer-file-name)))))

  (define-key f90-mode-map (kbd "C-c C-c") 'emacs-ide-fortran-run))

;; ============================================================================
;; OCAML
;; ============================================================================
(use-package tuareg
  :mode ("\\.ml\\'" "\\.mli\\'")
  :config
  (defun emacs-ide-ocaml-run ()
    "Run OCaml."
    (interactive)
    (compile (format "ocaml %s" (shell-quote-argument (buffer-file-name)))))

  (define-key tuareg-mode-map (kbd "C-c C-c") 'emacs-ide-ocaml-run))

;; ============================================================================
;; VERILOG/VHDL
;; ============================================================================
(use-package verilog-mode
  :mode (("\\.v\\'" . verilog-mode)
         ("\\.sv\\'" . verilog-mode))
  :init
  (setq verilog-indent-level 2
        verilog-auto-endcomments t)
  :config
  (defun emacs-ide-verilog-compile ()
    "Compile Verilog."
    (interactive)
    (let* ((file (buffer-file-name))
           (vvp (concat (file-name-sans-extension file) ".vvp")))
      (compile (format "iverilog -o %s %s && vvp %s"
                       (shell-quote-argument vvp)
                       (shell-quote-argument file)
                       (shell-quote-argument vvp)))))

  (define-key verilog-mode-map (kbd "C-c C-c") 'emacs-ide-verilog-compile))

(use-package vhdl-mode
  :ensure nil
  :mode ("\\.vhd\\'" "\\.vhdl\\'")
  :init
  (setq vhdl-basic-offset 2))

;; ============================================================================
;; ASSEMBLY
;; ============================================================================
(use-package nasm-mode
  :mode "\\.asm\\'"
  :config
  (defun emacs-ide-asm-compile ()
    "Compile assembly."
    (interactive)
    (let* ((file (buffer-file-name))
           (obj (concat (file-name-sans-extension file) ".o"))
           (exe (concat (file-name-sans-extension file) ".out")))
      (compile (format "nasm -f elf64 %s && ld -o %s %s && %s"
                       (shell-quote-argument file)
                       (shell-quote-argument exe)
                       (shell-quote-argument obj)
                       (shell-quote-argument exe)))))

  (define-key nasm-mode-map (kbd "C-c C-c") 'emacs-ide-asm-compile))

;; ============================================================================
;; SQL
;; ============================================================================
(use-package sql
  :ensure nil
  :mode "\\.sql\\'"
  :init
  (setq sql-product 'postgres))

;; ============================================================================
;; DATA FORMATS
;; ============================================================================
(use-package json-mode
  :mode "\\.json\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package graphql-mode
  :mode "\\.graphql\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

;; ============================================================================
;; MARKUP
;; ============================================================================
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init
  (setq markdown-command "pandoc"
        markdown-fontify-code-blocks-natively t
        markdown-enable-math t))

;; ============================================================================
;; CMAKE
;; ============================================================================
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; ============================================================================
;; ORG MODE
;; ============================================================================
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-startup-indented t
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-startup-folded 'content
        org-agenda-files '("~/org/")
        org-log-done 'time))

(provide 'lang-core)
;;; lang-core.el ends here