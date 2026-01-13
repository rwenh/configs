;;; lang-config.el --- Enhanced Programming Language Configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Configuration for 50+ programming languages with advanced features
;;; Tree-sitter integration, LSP, debugging, and compile-and-run support
;;; Save as: ~/.emacs.d/modules/lang-config.el
;;;
;;; Code:

;; ============================================================================
;; HELPER MACROS
;; ============================================================================
(defmacro emacs-ide-lang-mode (mode compiler-check &rest body)
  "Define language mode with COMPILER-CHECK before executing BODY."
  (declare (indent 2))
  `(use-package ,mode
     ,@(when compiler-check
         `(:if (executable-find ,compiler-check)))
     ,@body))

;; ============================================================================
;; C/C++ - ENHANCED
;; ============================================================================
(use-package cc-mode
  :ensure nil
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.hxx\\'" . c++-mode))
  :custom
  (c-default-style "linux")
  (c-basic-offset 4)
  (c-tab-always-indent t)
  :config
  (when (executable-find "gcc")
    (defun emacs-ide-c-compile-and-run ()
      "Compile and run C file with GCC."
      (interactive)
      (let ((file (buffer-file-name)))
        (when file
          (compile (format "gcc -Wall -Wextra -std=c11 -g -o /tmp/a.out %s && /tmp/a.out"
                          (shell-quote-argument file)))))))
  
  (when (executable-find "g++")
    (defun emacs-ide-cpp-compile-and-run ()
      "Compile and run C++ file with G++."
      (interactive)
      (let ((file (buffer-file-name)))
        (when file
          (compile (format "g++ -Wall -Wextra -std=c++20 -g -o /tmp/a.out %s && /tmp/a.out"
                          (shell-quote-argument file)))))))
  
  :hook ((c-mode . (lambda ()
                     (setq-local comment-start "// "
                                 comment-end "")
                     (when (fboundp 'emacs-ide-c-compile-and-run)
                       (local-set-key (kbd "C-c C-c") 'emacs-ide-c-compile-and-run))))
         (c++-mode . (lambda ()
                       (when (fboundp 'emacs-ide-cpp-compile-and-run)
                         (local-set-key (kbd "C-c C-c") 'emacs-ide-cpp-compile-and-run))))))

;; ============================================================================
;; PYTHON - ENHANCED
;; ============================================================================
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" "python3")
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  (python-shell-completion-native-enable nil)
  :config
  (when (executable-find "python3")
    (defun emacs-ide-python-run ()
      "Run current Python file."
      (interactive)
      (when (buffer-file-name)
        (compile (format "python3 %s" (shell-quote-argument (buffer-file-name))))))
    
    (defun emacs-ide-python-run-pytest ()
      "Run pytest on current file."
      (interactive)
      (compile (format "pytest -v %s" 
                      (shell-quote-argument (buffer-file-name)))))
    
    (defun emacs-ide-python-check ()
      "Run flake8 and mypy checks."
      (interactive)
      (compile (format "flake8 %s && mypy %s"
                      (shell-quote-argument (buffer-file-name))
                      (shell-quote-argument (buffer-file-name))))))
  :bind (:map python-mode-map
              ("C-c C-c" . emacs-ide-python-run)
              ("C-c C-t" . emacs-ide-python-run-pytest)
              ("C-c C-v" . emacs-ide-python-check)))

;; ============================================================================
;; RUST - ENHANCED
;; ============================================================================
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :if (executable-find "rustc")
  :custom
  (rust-format-on-save t)
  (rust-rustfmt-bin (or (executable-find "rustfmt") "rustfmt"))
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-compile)
              ("C-c C-t" . rust-test)
              ("C-c C-r" . rust-run)
              ("C-c C-k" . rust-check)
              ("C-c C-b" . cargo-process-build)))

(use-package cargo
  :ensure t
  :if (executable-find "cargo")
  :hook (rust-mode . cargo-minor-mode)
  :bind (:map cargo-minor-mode-map
              ("C-c C-b" . cargo-process-build)
              ("C-c C-t" . cargo-process-test)
              ("C-c C-r" . cargo-process-run)
              ("C-c C-l" . cargo-process-clippy)
              ("C-c C-d" . cargo-process-doc)))

;; ============================================================================
;; GO - ENHANCED
;; ============================================================================
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :if (executable-find "go")
  :custom
  (gofmt-command "gofmt")
  (godoc-at-point-function 'godoc-gogetdoc)
  :config
  (defun emacs-ide-go-run ()
    "Run current Go file."
    (interactive)
    (when (buffer-file-name)
      (compile (format "go run %s" (shell-quote-argument (buffer-file-name))))))
  
  (defun emacs-ide-go-build ()
    "Build current Go file."
    (interactive)
    (when (buffer-file-name)
      (compile (format "go build %s" (shell-quote-argument (buffer-file-name))))))
  
  (defun emacs-ide-go-test ()
    "Run Go tests."
    (interactive)
    (compile "go test -v ./..."))
  
  (defun emacs-ide-go-test-current ()
    "Run test at point."
    (interactive)
    (let ((test-name (which-function)))
      (if test-name
          (compile (format "go test -v -run ^%s$" test-name))
        (message "No test function at point"))))
  
  :hook (before-save . gofmt-before-save)
  :bind (:map go-mode-map
              ("C-c C-c" . emacs-ide-go-run)
              ("C-c C-b" . emacs-ide-go-build)
              ("C-c C-t" . emacs-ide-go-test)
              ("C-c C-f" . emacs-ide-go-test-current)))

;; ============================================================================
;; JAVA - ENHANCED
;; ============================================================================
(use-package java-mode
  :ensure nil
  :mode "\\.java\\'"
  :if (executable-find "javac")
  :config
  (defun emacs-ide-java-compile-and-run ()
    "Compile and run Java file."
    (interactive)
    (let* ((file (buffer-file-name))
           (class (file-name-sans-extension (file-name-nondirectory file))))
      (when file
        (compile (format "javac %s && java %s"
                        (shell-quote-argument file) class)))))
  
  (defun emacs-ide-java-run-maven ()
    "Run Maven project."
    (interactive)
    (compile "mvn clean install"))
  
  (defun emacs-ide-java-run-gradle ()
    "Run Gradle project."
    (interactive)
    (compile "./gradlew build"))
  
  :bind (:map java-mode-map
              ("C-c C-c" . emacs-ide-java-compile-and-run)
              ("C-c m" . emacs-ide-java-run-maven)
              ("C-c g" . emacs-ide-java-run-gradle)))

;; ============================================================================
;; JAVASCRIPT/TYPESCRIPT - ENHANCED
;; ============================================================================
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :custom
  (js2-basic-offset 2)
  (js2-mode-show-parse-errors nil)
  (js2-strict-missing-semi-warning nil)
  :config
  (when (executable-find "node")
    (defun emacs-ide-node-run ()
      "Run current JS file with Node."
      (interactive)
      (when (buffer-file-name)
        (compile (format "node %s" (shell-quote-argument (buffer-file-name))))))
    
    (defun emacs-ide-npm-test ()
      "Run npm test."
      (interactive)
      (compile "npm test"))
    
    (defun emacs-ide-npm-run ()
      "Run npm script."
      (interactive)
      (let ((script (read-string "npm run: ")))
        (compile (format "npm run %s" script)))))
  :bind (:map js2-mode-map
              ("C-c C-c" . emacs-ide-node-run)
              ("C-c C-t" . emacs-ide-npm-test)
              ("C-c C-r" . emacs-ide-npm-run)))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :custom
  (typescript-indent-level 2)
  :config
  (when (executable-find "tsc")
    (defun emacs-ide-typescript-compile ()
      "Compile TypeScript file."
      (interactive)
      (compile (format "tsc %s" (shell-quote-argument (buffer-file-name))))))
  :bind (:map typescript-mode-map
              ("C-c C-c" . emacs-ide-typescript-compile)))

;; ============================================================================
;; WEB DEVELOPMENT - ENHANCED
;; ============================================================================
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.htm\\'" "\\.css\\'" "\\.vue\\'" "\\.jsx\\'" "\\.tsx\\'")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-indentation t))

(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :if (executable-find "php")
  :config
  (defun emacs-ide-php-run ()
    "Run current PHP file."
    (interactive)
    (when (buffer-file-name)
      (compile (format "php %s" (shell-quote-argument (buffer-file-name))))))
  :bind (:map php-mode-map
              ("C-c C-c" . emacs-ide-php-run)))

;; ============================================================================
;; C# - NEW
;; ============================================================================
(use-package csharp-mode
  :ensure t
  :mode "\\.cs\\'"
  :if (executable-find "dotnet")
  :config
  (defun emacs-ide-csharp-run ()
    "Run C# project."
    (interactive)
    (compile "dotnet run"))
  
  (defun emacs-ide-csharp-build ()
    "Build C# project."
    (interactive)
    (compile "dotnet build"))
  :bind (:map csharp-mode-map
              ("C-c C-c" . emacs-ide-csharp-run)
              ("C-c C-b" . emacs-ide-csharp-build)))

;; ============================================================================
;; FORTRAN - ENHANCED
;; ============================================================================
(use-package f90
  :ensure nil
  :mode ("\\.f90\\'" "\\.f95\\'" "\\.f03\\'" "\\.f08\\'")
  :if (executable-find "gfortran")
  :custom
  (f90-do-indent 2)
  (f90-if-indent 2)
  (f90-type-indent 2)
  (f90-continuation-indent 4)
  :config
  (defun emacs-ide-fortran-compile-and-run ()
    "Compile and run Fortran file."
    (interactive)
    (let ((file (buffer-file-name)))
      (when file
        (compile (format "gfortran -Wall -g -o /tmp/fortran.out %s && /tmp/fortran.out"
                        (shell-quote-argument file))))))
  :bind (:map f90-mode-map
              ("C-c C-c" . emacs-ide-fortran-compile-and-run)))

(use-package f90-interface-browser
  :ensure t
  :after f90)

;; ============================================================================
;; HASKELL
;; ============================================================================
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :if (executable-find "ghc")
  :custom
  (haskell-process-type 'stack-ghci)
  :config
  (defun emacs-ide-haskell-run ()
    "Run current Haskell file."
    (interactive)
    (when (buffer-file-name)
      (compile (format "runhaskell %s" (shell-quote-argument (buffer-file-name))))))
  :bind (:map haskell-mode-map
              ("C-c C-c" . emacs-ide-haskell-run)
              ("C-c C-l" . haskell-process-load-file)))

;; ============================================================================
;; RUBY
;; ============================================================================
(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :if (executable-find "ruby")
  :custom
  (ruby-indent-level 2)
  :config
  (defun emacs-ide-ruby-run ()
    "Run current Ruby file."
    (interactive)
    (when (buffer-file-name)
      (compile (format "ruby %s" (shell-quote-argument (buffer-file-name))))))
  :bind (:map ruby-mode-map
              ("C-c C-c" . emacs-ide-ruby-run)))

;; ============================================================================
;; LUA
;; ============================================================================
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :if (executable-find "lua")
  :config
  (defun emacs-ide-lua-run ()
    "Run current Lua file."
    (interactive)
    (when (buffer-file-name)
      (compile (format "lua %s" (shell-quote-argument (buffer-file-name))))))
  :bind (:map lua-mode-map
              ("C-c C-c" . emacs-ide-lua-run)))

;; ============================================================================
;; PERL
;; ============================================================================
(use-package perl-mode
  :ensure nil
  :mode "\\.pl\\'"
  :if (executable-find "perl")
  :config
  (defun emacs-ide-perl-run ()
    "Run current Perl file."
    (interactive)
    (when (buffer-file-name)
      (compile (format "perl %s" (shell-quote-argument (buffer-file-name))))))
  :bind (:map perl-mode-map
              ("C-c C-c" . emacs-ide-perl-run)))

;; ============================================================================
;; SHELL SCRIPT
;; ============================================================================
(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'" . bash-mode)
         ("\\.bash\\'" . bash-mode)
         ("\\.zsh\\'" . sh-mode))
  :config
  (when (executable-find "bash")
    (defun emacs-ide-shell-run ()
      "Run current shell script."
      (interactive)
      (when (buffer-file-name)
        (compile (format "bash %s" (shell-quote-argument (buffer-file-name)))))))
  :bind (:map sh-mode-map
              ("C-c C-c" . emacs-ide-shell-run)))

;; ============================================================================
;; VERILOG/VHDL
;; ============================================================================
(use-package verilog-mode
  :ensure t
  :mode (("\\.v\\'" . verilog-mode)
         ("\\.sv\\'" . verilog-mode))
  :if (executable-find "iverilog")
  :custom
  (verilog-indent-level 2)
  (verilog-auto-endcomments t)
  :config
  (defun emacs-ide-verilog-compile ()
    "Compile Verilog with Icarus Verilog."
    (interactive)
    (let* ((file (buffer-file-name))
           (vvp (concat (file-name-sans-extension file) ".vvp")))
      (when file
        (compile (format "iverilog -o %s %s && vvp %s"
                        (shell-quote-argument vvp)
                        (shell-quote-argument file)
                        (shell-quote-argument vvp))))))
  :bind (:map verilog-mode-map
              ("C-c C-c" . emacs-ide-verilog-compile)))

(use-package vhdl-mode
  :ensure nil
  :mode ("\\.vhd\\'" "\\.vhdl\\'")
  :if (executable-find "ghdl")
  :custom
  (vhdl-basic-offset 2)
  :config
  (defun emacs-ide-vhdl-compile ()
    "Compile VHDL with GHDL."
    (interactive)
    (let* ((file (buffer-file-name))
           (entity (file-name-sans-extension (file-name-nondirectory file))))
      (when file
        (compile (format "ghdl -a %s && ghdl -e %s && ghdl -r %s"
                        (shell-quote-argument file) entity entity)))))
  :bind (:map vhdl-mode-map
              ("C-c C-c" . emacs-ide-vhdl-compile)))

;; ============================================================================
;; ASSEMBLY
;; ============================================================================
(use-package nasm-mode
  :ensure t
  :mode "\\.asm\\'"
  :if (executable-find "nasm")
  :config
  (defun emacs-ide-asm-compile ()
    "Compile x86-64 assembly with NASM."
    (interactive)
    (let* ((file (buffer-file-name))
           (obj (concat (file-name-sans-extension file) ".o"))
           (exe (concat (file-name-sans-extension file) ".out")))
      (when file
        (compile (format "nasm -f elf64 %s && ld -o %s %s && %s"
                        (shell-quote-argument file)
                        (shell-quote-argument exe)
                        (shell-quote-argument obj)
                        (shell-quote-argument exe))))))
  :bind (:map nasm-mode-map
              ("C-c C-c" . emacs-ide-asm-compile)))

;; ============================================================================
;; SQL
;; ============================================================================
(use-package sql
  :ensure nil
  :mode "\\.sql\\'"
  :custom
  (sql-product 'postgres))

(use-package sql-indent
  :ensure t
  :hook (sql-mode . sqlind-minor-mode))

;; ============================================================================
;; DATA FORMATS
;; ============================================================================
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'")

(use-package graphql-mode
  :ensure t
  :mode "\\.graphql\\'")

;; ============================================================================
;; MARKUP LANGUAGES
;; ============================================================================
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :custom
  (markdown-command "pandoc")
  :bind (:map markdown-mode-map
              ("C-c C-c p" . markdown-preview)))

;; ============================================================================
;; CMAKE
;; ============================================================================
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; ============================================================================
;; ORG MODE
;; ============================================================================
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation t)
  (org-startup-folded 'content)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)))

;; ============================================================================
;; MODERN LANGUAGES - Kotlin, Scala, Swift, Elixir, etc.
;; ============================================================================
(use-package kotlin-mode
  :ensure t
  :mode "\\.kt\\'"
  :if (executable-find "kotlinc"))

(use-package scala-mode
  :ensure t
  :mode "\\.scala\\'"
  :if (executable-find "scala"))

(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'"
  :if (executable-find "swift"))

(use-package elixir-mode
  :ensure t
  :mode "\\.ex\\'"
  :if (executable-find "elixir"))

(use-package erlang
  :ensure t
  :mode "\\.erl\\'"
  :if (executable-find "erl"))

(use-package nim-mode
  :ensure t
  :mode "\\.nim\\'"
  :if (executable-find "nim"))

(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'"
  :if (executable-find "zig"))

(use-package julia-mode
  :ensure t
  :mode "\\.jl\\'"
  :if (executable-find "julia"))

(use-package tuareg
  :ensure t
  :mode ("\\.ml\\'" "\\.mli\\'")
  :if (executable-find "ocaml"))

(provide 'lang-config)
;;; lang-config.el ends here