;;; apheleia-langs-patch.el --- Complete formatter map -*- lexical-binding: t -*-
;;; Version: 3.4.0
;;;
;;; Code:

(with-eval-after-load 'apheleia

  ;;; ── Python ─────────────────────────────────────────────────────────────────

  (setf (alist-get 'python-mode    apheleia-mode-alist) 'black)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'black)

  (when (executable-find "isort")
    (setf (alist-get 'isort apheleia-formatters)
          '("isort" "--profile" "black" "-"))
    (setf (alist-get 'python-mode    apheleia-mode-alist) '(black isort))
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(black isort)))

  (when (executable-find "ruff")
    (setf (alist-get 'ruff apheleia-formatters)
          '("ruff" "format" "-")))

  ;;; ── JavaScript / TypeScript / Web ──────────────────────────────────────────

  (when (executable-find "prettier")
    (dolist (mode '(js2-mode js-mode js-ts-mode
                    typescript-mode typescript-ts-mode tsx-ts-mode
                    web-mode mhtml-mode html-mode
                    css-mode css-ts-mode scss-mode less-css-mode
                    json-mode json-ts-mode
                    yaml-mode yaml-ts-mode
                    markdown-mode gfm-mode
                    graphql-mode))
      (setf (alist-get mode apheleia-mode-alist) 'prettier)))

  ;;; ── Rust ───────────────────────────────────────────────────────────────────

  (when (executable-find "rustfmt")
    (setf (alist-get 'rust-mode    apheleia-mode-alist) 'rustfmt)
    (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt))

  ;;; ── Go ─────────────────────────────────────────────────────────────────────

  (when (or (executable-find "goimports") (executable-find "gofmt"))
    (setf (alist-get 'go-mode    apheleia-mode-alist) 'gofmt)
    (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'gofmt))

  ;;; ── C / C++ ────────────────────────────────────────────────────────────────

  (when (executable-find "clang-format")
    (dolist (mode '(c-mode c++-mode c-ts-mode c++-ts-mode cuda-mode))
      (setf (alist-get mode apheleia-mode-alist) 'clang-format)))

  ;;; ── C# (NEW) ───────────────────────────────────────────────────────────────

  (when (executable-find "dotnet-csharpier")
    (setf (alist-get 'csharpier apheleia-formatters)
          '("dotnet-csharpier" "--write-stdout"))
    (setf (alist-get 'csharp-mode    apheleia-mode-alist) 'csharpier)
    (setf (alist-get 'csharp-ts-mode apheleia-mode-alist) 'csharpier))

  ;;; ── Java ───────────────────────────────────────────────────────────────────

  (when (executable-find "google-java-format")
    (setf (alist-get 'google-java-format apheleia-formatters)
          '("google-java-format" "-"))
    (setf (alist-get 'java-mode    apheleia-mode-alist) 'google-java-format)
    (setf (alist-get 'java-ts-mode apheleia-mode-alist) 'google-java-format))

  ;;; ── Kotlin ─────────────────────────────────────────────────────────────────

  (when (executable-find "ktlint")
    (setf (alist-get 'ktlint apheleia-formatters)
          '("ktlint" "--format" "--stdin" "--log-level=error"))
    (setf (alist-get 'kotlin-mode    apheleia-mode-alist) 'ktlint)
    (setf (alist-get 'kotlin-ts-mode apheleia-mode-alist) 'ktlint))

  ;;; ── Scala ──────────────────────────────────────────────────────────────────

  (when (executable-find "scalafmt")
    (setf (alist-get 'scalafmt apheleia-formatters)
          '("scalafmt" "--stdin"))
    (setf (alist-get 'scala-mode apheleia-mode-alist) 'scalafmt))

  ;;; ── Lua ────────────────────────────────────────────────────────────────────

  (when (executable-find "stylua")
    (setf (alist-get 'stylua apheleia-formatters)
          '("stylua" "-"))
    (setf (alist-get 'lua-mode    apheleia-mode-alist) 'stylua)
    (setf (alist-get 'lua-ts-mode apheleia-mode-alist) 'stylua))

  ;;; ── Shell ──────────────────────────────────────────────────────────────────

  (when (executable-find "shfmt")
    (setf (alist-get 'sh-mode      apheleia-mode-alist) 'shfmt)
    (setf (alist-get 'bash-ts-mode apheleia-mode-alist) 'shfmt))

  ;;; ── SQL ────────────────────────────────────────────────────────────────────

  (when (executable-find "pg_format")
    (setf (alist-get 'pgformatter apheleia-formatters)
          '("pg_format" "-s2" "-g" "-"))
    (setf (alist-get 'sql-mode    apheleia-mode-alist) 'pgformatter)
    (setf (alist-get 'sql-ts-mode apheleia-mode-alist) 'pgformatter))

  (when (executable-find "sqlfluff")
    (setf (alist-get 'sqlfluff apheleia-formatters)
          '("sqlfluff" "format" "--dialect" "postgres"
            "--stdin-filename" filepath "-")))

  ;;; ── Haskell ────────────────────────────────────────────────────────────────

  (cond
   ((executable-find "ormolu")
    (setf (alist-get 'ormolu apheleia-formatters)
          '("ormolu" "--stdin-input-file" filepath))
    (setf (alist-get 'haskell-mode apheleia-mode-alist) 'ormolu))
   ((executable-find "fourmolu")
    (setf (alist-get 'fourmolu apheleia-formatters)
          '("fourmolu" "--stdin-input-file" filepath))
    (setf (alist-get 'haskell-mode apheleia-mode-alist) 'fourmolu)))

  ;;; ── Elixir ─────────────────────────────────────────────────────────────────

  (when (executable-find "mix")
    (setf (alist-get 'mix-format apheleia-formatters)
          '("mix" "format" "--stdin-filename" filepath "-"))
    (setf (alist-get 'elixir-mode    apheleia-mode-alist) 'mix-format)
    (setf (alist-get 'elixir-ts-mode apheleia-mode-alist) 'mix-format))

  ;;; ── OCaml ──────────────────────────────────────────────────────────────────

  (when (executable-find "ocamlformat")
    (setf (alist-get 'ocamlformat apheleia-formatters)
          '("ocamlformat" "--impl" "-"))
    (setf (alist-get 'tuareg-mode apheleia-mode-alist) 'ocamlformat))

  ;;; ── Zig ────────────────────────────────────────────────────────────────────

  (when (executable-find "zig")
    (setf (alist-get 'zigfmt apheleia-formatters)
          '("zig" "fmt" "--stdin"))
    (setf (alist-get 'zig-mode apheleia-mode-alist) 'zigfmt))

  ;;; ── Nix ────────────────────────────────────────────────────────────────────

  (when (executable-find "nixpkgs-fmt")
    (setf (alist-get 'nixpkgs-fmt apheleia-formatters)
          '("nixpkgs-fmt"))
    (setf (alist-get 'nix-mode apheleia-mode-alist) 'nixpkgs-fmt))

  ;;; ── Ruby (hardened) ────────────────────────────────────────────────────────

  (cond
   ;; standardrb is rubocop-compatible and opinionated — prefer it when present
   ((executable-find "standardrb")
    (setf (alist-get 'standardrb apheleia-formatters)
          '("standardrb" "--fix" "--stdin" filepath
            "--stderr" "--format" "quiet"))
    (setf (alist-get 'ruby-mode    apheleia-mode-alist) 'standardrb)
    (setf (alist-get 'ruby-ts-mode apheleia-mode-alist) 'standardrb))
   ((executable-find "rubocop")
    (setf (alist-get 'rubocop apheleia-formatters)
          '("rubocop" "--autocorrect" "--stdin" filepath
            "--stderr" "--format" "quiet"))
    (setf (alist-get 'ruby-mode    apheleia-mode-alist) 'rubocop)
    (setf (alist-get 'ruby-ts-mode apheleia-mode-alist) 'rubocop)))

  ;;; ── PHP (hardened) ─────────────────────────────────────────────────────────

  (cond
   ((executable-find "php-cs-fixer")
    (unless (assq 'php-cs-fixer apheleia-formatters)
      (push '(php-cs-fixer "php-cs-fixer" "fix" "--quiet" filepath)
            apheleia-formatters))
    (setf (alist-get 'php-mode    apheleia-mode-alist) 'php-cs-fixer)
    (setf (alist-get 'php-ts-mode apheleia-mode-alist) 'php-cs-fixer))
   ((executable-find "phpcbf")
    (setf (alist-get 'phpcbf apheleia-formatters)
          '("phpcbf" "--stdin-path" filepath "-"))
    (setf (alist-get 'php-mode    apheleia-mode-alist) 'phpcbf)
    (setf (alist-get 'php-ts-mode apheleia-mode-alist) 'phpcbf)))

  ;;; ── Dart (NEW) ─────────────────────────────────────────────────────────────

  (when (executable-find "dart")
    (setf (alist-get 'dart-format apheleia-formatters)
          '("dart" "format" "--output=show" "-"))
    (setf (alist-get 'dart-mode apheleia-mode-alist) 'dart-format))

  ;;; ── Clojure ────────────────────────────────────────────────────────────────

  (when (executable-find "cljfmt")
    (setf (alist-get 'cljfmt apheleia-formatters)
          '("cljfmt" "fix" "-"))
    (setf (alist-get 'clojure-mode    apheleia-mode-alist) 'cljfmt)
    (setf (alist-get 'clojure-ts-mode apheleia-mode-alist) 'cljfmt))

  ;;; ── Terraform ──────────────────────────────────────────────────────────────

  (when (executable-find "terraform")
    (setf (alist-get 'terraform-fmt apheleia-formatters)
          '("terraform" "fmt" "-"))
    (setf (alist-get 'terraform-mode apheleia-mode-alist) 'terraform-fmt))

  ;;; ── TOML ───────────────────────────────────────────────────────────────────

  (when (executable-find "taplo")
    (setf (alist-get 'taplo apheleia-formatters)
          '("taplo" "format" "-"))
    (setf (alist-get 'toml-mode    apheleia-mode-alist) 'taplo)
    (setf (alist-get 'toml-ts-mode apheleia-mode-alist) 'taplo)))

(provide 'apheleia-langs-patch)
;;; apheleia-langs-patch.el ends here
