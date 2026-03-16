;;; apheleia-langs-patch.el --- Complete 50-lang apheleia formatter map -*- lexical-binding: t -*-
;;; Commentary:
;;; Extends tools-format.el's apheleia configuration with all 50 languages.
;;; Add "apheleia-langs-patch" to feature-modules AFTER "tools-format".
;;; Each entry checks executable availability before registering.
;;;
;;; Version: 1.0.0
;;; Code:

(with-eval-after-load 'apheleia

  ;; ── Formatters not in apheleia by default ─────────────────────────────────
  ;; Add custom formatter definitions for tools apheleia doesn't ship with

  (when (executable-find "isort")
    (setf (alist-get 'isort apheleia-formatters)
          '("isort" "--profile" "black" "-")))

  (when (executable-find "ruff")
    (setf (alist-get 'ruff apheleia-formatters)
          '("ruff" "format" "-")))

  (when (executable-find "stylua")
    (setf (alist-get 'stylua apheleia-formatters)
          '("stylua" "-")))

  (when (executable-find "nixpkgs-fmt")
    (setf (alist-get 'nixpkgs-fmt aphaleia-formatters)
          '("nixpkgs-fmt")))

  (when (executable-find "ormolu")
    (setf (alist-get 'ormolu apheleia-formatters)
          '("ormolu" "--stdin-input-file" filepath)))

  (when (executable-find "fourmolu")
    (setf (alist-get 'fourmolu apheleia-formatters)
          '("fourmolu" "--stdin-input-file" filepath)))

  (when (executable-find "ktlint")
    (setf (alist-get 'ktlint apheleia-formatters)
          '("ktlint" "--format" "--stdin" "--log-level=error")))

  (when (executable-find "scalafmt")
    (setf (alist-get 'scalafmt apheleia-formatters)
          '("scalafmt" "--stdin")))

  (when (executable-find "mix")
    (setf (alist-get 'mix-format apheleia-formatters)
          '("mix" "format" "-")))

  (when (executable-find "ocamlformat")
    (setf (alist-get 'ocamlformat apheleia-formatters)
          '("ocamlformat" "--impl" "-")))

  (when (executable-find "pg_format")
    (setf (alist-get 'pgformatter apheleia-formatters)
          '("pg_format" "-s2" "-g" "-")))

  (when (executable-find "sqlfluff")
    (setf (alist-get 'sqlfluff apheleia-formatters)
          '("sqlfluff" "fix" "--dialect" "postgres" "-")))

  (when (executable-find "terraform")
    (setf (alist-get 'terraform-fmt apheleia-formatters)
          '("terraform" "fmt" "-")))

  (when (executable-find "zigfmt")
    (setf (alist-get 'zigfmt apheleia-formatters)
          '("zig" "fmt" "--stdin")))

  (when (executable-find "rubocop")
    (setf (alist-get 'rubocop apheleia-formatters)
          '("rubocop" "--auto-correct" "--stdin" filepath "--stderr" "--format" "quiet")))

  (when (executable-find "standardrb")
    (setf (alist-get 'standardrb apheleia-formatters)
          '("standardrb" "--fix" "--stdin" filepath "--stderr" "--format" "quiet")))

  (when (executable-find "phpcbf")
    (setf (alist-get 'phpcbf apheleia-formatters)
          '("phpcbf" "--stdin-path" filepath "-")))

  (when (executable-find "google-java-format")
    (setf (alist-get 'google-java-format apheleia-formatters)
          '("google-java-format" "-")))

  ;; ── Mode → formatter map ───────────────────────────────────────────────────
  ;; Tier 1
  (setf (alist-get 'python-mode       apheleia-mode-alist) 'black)
  (setf (alist-get 'python-ts-mode    apheleia-mode-alist) 'black)
  (setf (alist-get 'js2-mode          apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-mode           apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-ts-mode        apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-mode   apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'tsx-ts-mode       apheleia-mode-alist) 'prettier)
  (setf (alist-get 'web-mode          apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-mode          apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-ts-mode       apheleia-mode-alist) 'prettier)
  (setf (alist-get 'scss-mode         apheleia-mode-alist) 'prettier)
  (setf (alist-get 'rust-mode         apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'rust-ts-mode      apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'go-mode           apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'go-ts-mode        apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'c-mode            apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode          apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c-ts-mode         apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-ts-mode       apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'cuda-mode         apheleia-mode-alist) 'clang-format)
  ;; Tier 2
  (setf (alist-get 'java-mode         apheleia-mode-alist) 'google-java-format)
  (setf (alist-get 'java-ts-mode      apheleia-mode-alist) 'google-java-format)
  (setf (alist-get 'kotlin-mode       apheleia-mode-alist) 'ktlint)
  (setf (alist-get 'scala-mode        apheleia-mode-alist) 'scalafmt)
  (setf (alist-get 'lua-mode          apheleia-mode-alist) 'stylua)
  (setf (alist-get 'sh-mode           apheleia-mode-alist) 'shfmt)
  (setf (alist-get 'bash-ts-mode      apheleia-mode-alist) 'shfmt)
  (setf (alist-get 'sql-mode          apheleia-mode-alist) 'pgformatter)
  ;; Tier 3
  (setf (alist-get 'haskell-mode      apheleia-mode-alist) 'ormolu)
  (setf (alist-get 'elixir-mode       apheleia-mode-alist) 'mix-format)
  (setf (alist-get 'elixir-ts-mode    apheleia-mode-alist) 'mix-format)
  (setf (alist-get 'tuareg-mode       apheleia-mode-alist) 'ocamlformat)
  (setf (alist-get 'zig-mode          apheleia-mode-alist) 'zigfmt)
  (setf (alist-get 'nix-mode          apheleia-mode-alist) 'nixpkgs-fmt)
  (setf (alist-get 'ruby-mode         apheleia-mode-alist) 'rubocop)
  (setf (alist-get 'ruby-ts-mode      apheleia-mode-alist) 'rubocop)
  (setf (alist-get 'php-mode          apheleia-mode-alist) 'phpcbf)
  ;; Tier 4
  (setf (alist-get 'json-mode         apheleia-mode-alist) 'prettier)
  (setf (alist-get 'json-ts-mode      apheleia-mode-alist) 'prettier)
  (setf (alist-get 'yaml-mode         apheleia-mode-alist) 'prettier)
  (setf (alist-get 'yaml-ts-mode      apheleia-mode-alist) 'prettier)
  (setf (alist-get 'toml-mode         apheleia-mode-alist) 'prettier)
  (setf (alist-get 'toml-ts-mode      apheleia-mode-alist) 'prettier)
  (setf (alist-get 'markdown-mode     apheleia-mode-alist) 'prettier)
  (setf (alist-get 'gfm-mode          apheleia-mode-alist) 'prettier)
  (setf (alist-get 'terraform-mode    apheleia-mode-alist) 'terraform-fmt)
  (setf (alist-get 'graphql-mode      apheleia-mode-alist) 'prettier))

(provide 'apheleia-langs-patch)
;;; apheleia-langs-patch.el ends here
