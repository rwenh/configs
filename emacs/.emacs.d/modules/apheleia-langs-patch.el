;;; apheleia-langs-patch.el --- Complete formatter map -*- lexical-binding: t -*-
;;; Version: 3.5.1
;;;
;;; Code:

(require 'core-dev)

(with-eval-after-load 'apheleia

  ;;; ── Python ─────────────────────────────────────────────────────────────────

  (when (executable-find "isort")
    (setf (alist-get 'isort apheleia-formatters)
          '("isort" "--profile" "black" "-")))

  (when (executable-find "ruff")
    (setf (alist-get 'ruff apheleia-formatters)
          '("ruff" "format" "-")))

  (let* ((default   (if (executable-find "isort") '(black isort) 'black))
         (formatter (emacs-ide-dev-resolve-formatter "python" default)))
    (setf (alist-get 'python-mode    apheleia-mode-alist) formatter)
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) formatter))

  ;;; ── JavaScript / TypeScript / Web ──────────────────────────────────────────

  (when (executable-find "prettier")
    (dolist (entry '(("javascript" . (js2-mode js-mode js-ts-mode))
                      ("typescript" . (typescript-mode typescript-ts-mode tsx-ts-mode))
                      ("html"       . (web-mode mhtml-mode html-mode))
                      ("css"        . (css-mode css-ts-mode scss-mode less-css-mode))
                      ("json"       . (json-mode json-ts-mode))
                      ("yaml"       . (yaml-mode yaml-ts-mode))
                      ("markdown"   . (markdown-mode gfm-mode))))
      (let ((formatter (emacs-ide-dev-resolve-formatter (car entry) 'prettier)))
        (dolist (mode (cdr entry))
          (setf (alist-get mode apheleia-mode-alist) formatter))))
    (setf (alist-get 'graphql-mode apheleia-mode-alist) 'prettier))

  ;;; ── Rust ───────────────────────────────────────────────────────────────────

  (when (executable-find "rustfmt")
    (let ((formatter (emacs-ide-dev-resolve-formatter "rust" 'rustfmt)))
      (setf (alist-get 'rust-mode    apheleia-mode-alist) formatter)
      (setf (alist-get 'rust-ts-mode apheleia-mode-alist) formatter)))

  ;;; ── Go ─────────────────────────────────────────────────────────────────────

  (when (or (executable-find "goimports") (executable-find "gofmt"))
    (let ((formatter (emacs-ide-dev-resolve-formatter "go" 'gofmt)))
      (setf (alist-get 'go-mode    apheleia-mode-alist) formatter)
      (setf (alist-get 'go-ts-mode apheleia-mode-alist) formatter)))

  ;;; ── C / C++ ────────────────────────────────────────────────────────────────

  (when (executable-find "clang-format")
    (let ((c-formatter   (emacs-ide-dev-resolve-formatter "c"   'clang-format))
          (cpp-formatter (emacs-ide-dev-resolve-formatter "cpp" 'clang-format)))
      (dolist (mode '(c-mode c-ts-mode))
        (setf (alist-get mode apheleia-mode-alist) c-formatter))
      (dolist (mode '(c++-mode c++-ts-mode cuda-mode))
        (setf (alist-get mode apheleia-mode-alist) cpp-formatter))))

  ;;; ── C# ─────────────────────────────────────────────────────────────────────

  (when (executable-find "dotnet-csharpier")
    (setf (alist-get 'csharpier apheleia-formatters)
          '("dotnet-csharpier" "--write-stdout"))
    (let ((formatter (emacs-ide-dev-resolve-formatter "csharp" 'csharpier)))
      (setf (alist-get 'csharp-mode    apheleia-mode-alist) formatter)
      (setf (alist-get 'csharp-ts-mode apheleia-mode-alist) formatter)))

  ;;; ── Java ───────────────────────────────────────────────────────────────────

  (when (executable-find "google-java-format")
    (setf (alist-get 'google-java-format apheleia-formatters)
          '("google-java-format" "-"))
    (let ((formatter (emacs-ide-dev-resolve-formatter "java" 'google-java-format)))
      (setf (alist-get 'java-mode    apheleia-mode-alist) formatter)
      (setf (alist-get 'java-ts-mode apheleia-mode-alist) formatter)))

  ;;; ── Kotlin ─────────────────────────────────────────────────────────────────

  (when (executable-find "ktlint")
    (setf (alist-get 'ktlint apheleia-formatters)
          '("ktlint" "--format" "--stdin" "--log-level=error"))
    (let ((formatter (emacs-ide-dev-resolve-formatter "kotlin" 'ktlint)))
      (setf (alist-get 'kotlin-mode    apheleia-mode-alist) formatter)
      (setf (alist-get 'kotlin-ts-mode apheleia-mode-alist) formatter)))

  ;;; ── Scala ──────────────────────────────────────────────────────────────────

  (when (executable-find "scalafmt")
    (setf (alist-get 'scalafmt apheleia-formatters)
          '("scalafmt" "--stdin"))
    (setf (alist-get 'scala-mode apheleia-mode-alist)
          (emacs-ide-dev-resolve-formatter "scala" 'scalafmt)))

  ;;; ── Lua ────────────────────────────────────────────────────────────────────

  (when (executable-find "stylua")
    (setf (alist-get 'stylua apheleia-formatters)
          '("stylua" "-"))
    (let ((formatter (emacs-ide-dev-resolve-formatter "lua" 'stylua)))
      (setf (alist-get 'lua-mode    apheleia-mode-alist) formatter)
      (setf (alist-get 'lua-ts-mode apheleia-mode-alist) formatter)))

  ;;; ── Shell ──────────────────────────────────────────────────────────────────

  (when (executable-find "shfmt")
    (let ((formatter (emacs-ide-dev-resolve-formatter "shell" 'shfmt)))
      (setf (alist-get 'sh-mode      apheleia-mode-alist) formatter)
      (setf (alist-get 'bash-ts-mode apheleia-mode-alist) formatter)))

  ;;; ── SQL ────────────────────────────────────────────────────────────────────

  (when (executable-find "pg_format")
    (setf (alist-get 'pgformatter apheleia-formatters)
          '("pg_format" "-s2" "-g" "-")))

  (when (executable-find "sqlfluff")
    (setf (alist-get 'sqlfluff apheleia-formatters)
          '("sqlfluff" "format" "--dialect" "postgres"
            "--stdin-filename" filepath "-")))

  (let ((default (cond ((executable-find "pg_format") 'pgformatter)
                        ((executable-find "sqlfluff")  'sqlfluff))))
    (when default
      (let ((formatter (emacs-ide-dev-resolve-formatter "sql" default)))
        (setf (alist-get 'sql-mode    apheleia-mode-alist) formatter)
        (setf (alist-get 'sql-ts-mode apheleia-mode-alist) formatter))))

  ;;; ── Haskell ────────────────────────────────────────────────────────────────

  (when (executable-find "ormolu")
    (setf (alist-get 'ormolu apheleia-formatters)
          '("ormolu" "--stdin-input-file" filepath)))
  (when (executable-find "fourmolu")
    (setf (alist-get 'fourmolu apheleia-formatters)
          '("fourmolu" "--stdin-input-file" filepath)))

  (let ((default (cond ((executable-find "ormolu")   'ormolu)
                        ((executable-find "fourmolu") 'fourmolu))))
    (when default
      (setf (alist-get 'haskell-mode apheleia-mode-alist)
            (emacs-ide-dev-resolve-formatter "haskell" default))))

  ;;; ── Elixir ─────────────────────────────────────────────────────────────────

  (when (executable-find "mix")
    (setf (alist-get 'mix-format apheleia-formatters)
          '("mix" "format" "--stdin-filename" filepath "-"))
    (let ((formatter (emacs-ide-dev-resolve-formatter "elixir" 'mix-format)))
      (setf (alist-get 'elixir-mode    apheleia-mode-alist) formatter)
      (setf (alist-get 'elixir-ts-mode apheleia-mode-alist) formatter)))

  ;;; ── OCaml ──────────────────────────────────────────────────────────────────

  (when (executable-find "ocamlformat")
    (setf (alist-get 'ocamlformat apheleia-formatters)
          '("ocamlformat" "--impl" "-"))
    (setf (alist-get 'tuareg-mode apheleia-mode-alist) 'ocamlformat))

  ;;; ── Zig ────────────────────────────────────────────────────────────────────

  (when (executable-find "zig")
    (setf (alist-get 'zigfmt apheleia-formatters)
          '("zig" "fmt" "--stdin"))
    (setf (alist-get 'zig-mode apheleia-mode-alist)
          (emacs-ide-dev-resolve-formatter "zig" 'zigfmt)))

  ;;; ── Nix ────────────────────────────────────────────────────────────────────

  (when (executable-find "nixpkgs-fmt")
    (setf (alist-get 'nixpkgs-fmt apheleia-formatters)
          '("nixpkgs-fmt"))
    (setf (alist-get 'nix-mode apheleia-mode-alist)
          (emacs-ide-dev-resolve-formatter "nix" 'nixpkgs-fmt)))

  ;;; ── Ruby (hardened) ────────────────────────────────────────────────────────

  (when (executable-find "standardrb")
    (setf (alist-get 'standardrb apheleia-formatters)
          '("standardrb" "--fix" "--stdin" filepath
            "--stderr" "--format" "quiet")))
  (when (executable-find "rubocop")
    (setf (alist-get 'rubocop apheleia-formatters)
          '("rubocop" "--autocorrect" "--stdin" filepath
            "--stderr" "--format" "quiet")))

  (let ((default (cond ((executable-find "standardrb") 'standardrb)
                        ((executable-find "rubocop")    'rubocop))))
    (when default
      (let ((formatter (emacs-ide-dev-resolve-formatter "ruby" default)))
        (setf (alist-get 'ruby-mode    apheleia-mode-alist) formatter)
        (setf (alist-get 'ruby-ts-mode apheleia-mode-alist) formatter))))

  ;;; ── PHP (hardened) ─────────────────────────────────────────────────────────

  (when (executable-find "php-cs-fixer")
    (unless (assq 'php-cs-fixer apheleia-formatters)
      (push '(php-cs-fixer "php-cs-fixer" "fix" "--quiet" filepath)
            apheleia-formatters)))
  (when (executable-find "phpcbf")
    (setf (alist-get 'phpcbf apheleia-formatters)
          '("phpcbf" "--stdin-path" filepath "-")))

  (let ((default (cond ((executable-find "php-cs-fixer") 'php-cs-fixer)
                        ((executable-find "phpcbf")       'phpcbf))))
    (when default
      (let ((formatter (emacs-ide-dev-resolve-formatter "php" default)))
        (setf (alist-get 'php-mode    apheleia-mode-alist) formatter)
        (setf (alist-get 'php-ts-mode apheleia-mode-alist) formatter))))

  ;;; ── Dart ───────────────────────────────────────────────────────────────────

  (when (executable-find "dart")
    (setf (alist-get 'dart-format apheleia-formatters)
          '("dart" "format" "--output=show" "-"))
    (setf (alist-get 'dart-mode apheleia-mode-alist)
          (emacs-ide-dev-resolve-formatter "dart" 'dart-format)))

  ;;; ── Clojure ────────────────────────────────────────────────────────────────

  (when (executable-find "cljfmt")
    (setf (alist-get 'cljfmt apheleia-formatters)
          '("cljfmt" "fix" "-"))
    (let ((formatter (emacs-ide-dev-resolve-formatter "clojure" 'cljfmt)))
      (setf (alist-get 'clojure-mode    apheleia-mode-alist) formatter)
      (setf (alist-get 'clojure-ts-mode apheleia-mode-alist) formatter)))

  ;;; ── Terraform ──────────────────────────────────────────────────────────────

  (when (executable-find "terraform")
    (setf (alist-get 'terraform-fmt apheleia-formatters)
          '("terraform" "fmt" "-"))
    (setf (alist-get 'terraform-mode apheleia-mode-alist)
          (emacs-ide-dev-resolve-formatter "terraform" 'terraform-fmt)))

  ;;; ── TOML ───────────────────────────────────────────────────────────────────

  (when (executable-find "taplo")
    (setf (alist-get 'taplo apheleia-formatters)
          '("taplo" "format" "-"))
    (let ((formatter (emacs-ide-dev-resolve-formatter "toml" 'taplo)))
      (setf (alist-get 'toml-mode    apheleia-mode-alist) formatter)
      (setf (alist-get 'toml-ts-mode apheleia-mode-alist) formatter))))

(provide 'apheleia-langs-patch)
;;; apheleia-langs-patch.el ends here
