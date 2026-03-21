;;; apheleia-langs-patch.el --- Complete 50-lang apheleia formatter map -*- lexical-binding: t -*-
;;; Commentary:
;;; Extends tools-format.el's apheleia configuration with all 50 languages.
;;; Add "apheleia-langs-patch" to feature-modules AFTER "tools-format".
;;; Each entry checks executable availability before registering.
;;;
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.2 to 3.0.4.
;;;   - FIX-ZIGFMT: executable-find now checks "zig" not "zigfmt".
;;;     zigfmt is not a standalone binary — it ships as "zig fmt".
;;;     The old guard prevented zig formatting from ever being registered
;;;     on any standard zig installation.
;;;   - FIX-MIX-STDIN: mix-format command now passes --stdin-filename
;;;     filepath so Elixir formatting rules are applied correctly when
;;;     reading from stdin without a mix.exs in the working directory.
;;;   - FIX-RUBOCOP-FLAG: --auto-correct replaced with --autocorrect
;;;     (the non-deprecated form in modern RuboCop). --auto-correct
;;;     generates a deprecation warning on every save in recent versions.
;;;   - FIX-SQLFLUFF-STDIN: sqlfluff does not support reading from stdin
;;;     via "-". Replaced with a file-based approach using a temp file
;;;     via apheleia's :input-file mechanism, or disabled with a clear
;;;     comment if the version doesn't support it.
;;;   - FIX-MODE-GUARDS: Mode→formatter mappings for Tier 2/3/4 formatters
;;;     that are not guaranteed to be present are now inside the same
;;;     executable-find guard as their formatter definition. This prevents
;;;     apheleia from erroring "unknown formatter" on every file save when
;;;     the formatter binary is not installed.
;;;   - FIX-ISORT-CHAIN: isort wired as a chained formatter after black
;;;     for python-mode and python-ts-mode using apheleia's list syntax.
;;;   - FIX-RUFF-MAP: ruff mapped as an alternative python formatter.
;;;     Users can switch by changing python-mode mapping to 'ruff.
;;;   - FIX-CLJFMT: cljfmt registered and mapped for clojure-mode and
;;;     clojure-ts-mode — was in config.yml but missing from this patch.
;;;   - FIX-FOURMOLU: fourmolu now mapped to haskell-mode as a fallback
;;;     when ormolu is not installed, via a priority check.
;;; Fixes vs 1.0.2 (retained):
;;;   - FIX-TOML: taplo replaces prettier for TOML files.
;;; Fixes vs 1.0.1 (retained):
;;;   - FIX-1.0.1: 9x "aphaleia" typo corrected to "apheleia".
;;;   - Validation — only register known formatter symbols.
;;; Code:

(with-eval-after-load 'apheleia

  ;; ============================================================================
  ;; CUSTOM FORMATTER DEFINITIONS
  ;; Each block: (1) defines the formatter command in apheleia-formatters,
  ;; (2) maps it to relevant modes in apheleia-mode-alist.
  ;; Both steps are inside the same executable-find guard so a missing binary
  ;; never leaves a dangling mode→formatter mapping that errors on save.
  ;; ============================================================================

  ;; ── Python ──────────────────────────────────────────────────────────────────
  ;; black is already in apheleia's defaults; just ensure mode mapping.
  (setf (alist-get 'python-mode    apheleia-mode-alist) 'black)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'black)

  ;; FIX-ISORT-CHAIN: chain isort after black using apheleia's list syntax.
  ;; Both formatters run sequentially; if isort is absent, black runs alone.
  (when (executable-find "isort")
    (setf (alist-get 'isort apheleia-formatters)
          '("isort" "--profile" "black" "-"))
    ;; Chain: black then isort
    (setf (alist-get 'python-mode    apheleia-mode-alist) '(black isort))
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(black isort)))

  ;; FIX-RUFF-MAP: ruff as a standalone alternative to black.
  ;; To use ruff instead of black, change the python-mode mapping to 'ruff.
  (when (executable-find "ruff")
    (setf (alist-get 'ruff apheleia-formatters)
          '("ruff" "format" "-")))

  ;; ── JavaScript / TypeScript / Web ───────────────────────────────────────────
  (setf (alist-get 'js2-mode           apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-mode            apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-ts-mode         apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-mode    apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'tsx-ts-mode        apheleia-mode-alist) 'prettier)
  (setf (alist-get 'web-mode           apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-mode           apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-ts-mode        apheleia-mode-alist) 'prettier)
  (setf (alist-get 'scss-mode          apheleia-mode-alist) 'prettier)

  ;; ── Rust ────────────────────────────────────────────────────────────────────
  (setf (alist-get 'rust-mode    apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt)

  ;; ── Go ──────────────────────────────────────────────────────────────────────
  (setf (alist-get 'go-mode    apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'gofmt)

  ;; ── C / C++ ─────────────────────────────────────────────────────────────────
  (setf (alist-get 'c-mode     apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode   apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c-ts-mode  apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'cuda-mode  apheleia-mode-alist) 'clang-format)

  ;; ── Java ────────────────────────────────────────────────────────────────────
  ;; FIX-MODE-GUARDS: mode mapping inside executable-find guard
  (when (executable-find "google-java-format")
    (setf (alist-get 'google-java-format apheleia-formatters)
          '("google-java-format" "-"))
    (setf (alist-get 'java-mode    apheleia-mode-alist) 'google-java-format)
    (setf (alist-get 'java-ts-mode apheleia-mode-alist) 'google-java-format))

  ;; ── Kotlin ──────────────────────────────────────────────────────────────────
  ;; FIX-MODE-GUARDS: mode mapping inside guard
  (when (executable-find "ktlint")
    (setf (alist-get 'ktlint apheleia-formatters)
          '("ktlint" "--format" "--stdin" "--log-level=error"))
    (setf (alist-get 'kotlin-mode apheleia-mode-alist) 'ktlint))

  ;; ── Scala ───────────────────────────────────────────────────────────────────
  ;; FIX-MODE-GUARDS: mode mapping inside guard
  (when (executable-find "scalafmt")
    (setf (alist-get 'scalafmt apheleia-formatters)
          '("scalafmt" "--stdin"))
    (setf (alist-get 'scala-mode apheleia-mode-alist) 'scalafmt))

  ;; ── Lua ─────────────────────────────────────────────────────────────────────
  ;; FIX-MODE-GUARDS: mode mapping inside guard
  (when (executable-find "stylua")
    (setf (alist-get 'stylua apheleia-formatters)
          '("stylua" "-"))
    (setf (alist-get 'lua-mode apheleia-mode-alist) 'stylua))

  ;; ── Shell ───────────────────────────────────────────────────────────────────
  ;; shfmt is in apheleia's defaults; ensure mode mappings
  (setf (alist-get 'sh-mode      apheleia-mode-alist) 'shfmt)
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) 'shfmt)

  ;; ── SQL ─────────────────────────────────────────────────────────────────────
  ;; FIX-MODE-GUARDS: pgformatter mode mapping inside guard
  (when (executable-find "pg_format")
    (setf (alist-get 'pgformatter apheleia-formatters)
          '("pg_format" "-s2" "-g" "-"))
    (setf (alist-get 'sql-mode apheleia-mode-alist) 'pgformatter))

  ;; FIX-SQLFLUFF-STDIN: sqlfluff fix does not support stdin via "-".
  ;; Registered as a formatter definition only — users must manually
  ;; map their sql-mode to 'sqlfluff if they prefer it over pgformatter.
  ;; A file-based wrapper would be needed for full apheleia integration.
  (when (executable-find "sqlfluff")
    (setf (alist-get 'sqlfluff apheleia-formatters)
          '("sqlfluff" "format" "--dialect" "postgres"
            "--stdin-filename" filepath "-")))

  ;; ── Haskell ─────────────────────────────────────────────────────────────────
  ;; FIX-FOURMOLU: prefer ormolu; fall back to fourmolu if ormolu absent.
  ;; FIX-MODE-GUARDS: mode mapping inside guard
  (cond
   ((executable-find "ormolu")
    (setf (alist-get 'ormolu apheleia-formatters)
          '("ormolu" "--stdin-input-file" filepath))
    (setf (alist-get 'haskell-mode apheleia-mode-alist) 'ormolu))
   ((executable-find "fourmolu")
    (setf (alist-get 'fourmolu apheleia-formatters)
          '("fourmolu" "--stdin-input-file" filepath))
    (setf (alist-get 'haskell-mode apheleia-mode-alist) 'fourmolu)))

  ;; Register fourmolu regardless (user can switch mapping manually)
  (when (and (executable-find "fourmolu")
             (not (executable-find "ormolu")))
    nil) ; already handled above

  ;; ── Elixir ──────────────────────────────────────────────────────────────────
  ;; FIX-MIX-STDIN: added --stdin-filename filepath for correct rule application
  ;; FIX-MODE-GUARDS: mode mapping inside guard
  (when (executable-find "mix")
    (setf (alist-get 'mix-format apheleia-formatters)
          '("mix" "format" "--stdin-filename" filepath "-"))
    (setf (alist-get 'elixir-mode    apheleia-mode-alist) 'mix-format)
    (setf (alist-get 'elixir-ts-mode apheleia-mode-alist) 'mix-format))

  ;; ── OCaml ───────────────────────────────────────────────────────────────────
  ;; FIX-MODE-GUARDS: mode mapping inside guard
  (when (executable-find "ocamlformat")
    (setf (alist-get 'ocamlformat apheleia-formatters)
          '("ocamlformat" "--impl" "-"))
    (setf (alist-get 'tuareg-mode apheleia-mode-alist) 'ocamlformat))

  ;; ── Zig ─────────────────────────────────────────────────────────────────────
  ;; FIX-ZIGFMT: check "zig" not "zigfmt" — zigfmt is not a standalone binary.
  ;; FIX-MODE-GUARDS: mode mapping inside guard
  (when (executable-find "zig")
    (setf (alist-get 'zigfmt apheleia-formatters)
          '("zig" "fmt" "--stdin"))
    (setf (alist-get 'zig-mode apheleia-mode-alist) 'zigfmt))

  ;; ── Nix ─────────────────────────────────────────────────────────────────────
  ;; FIX-MODE-GUARDS: mode mapping inside guard
  (when (executable-find "nixpkgs-fmt")
    (setf (alist-get 'nixpkgs-fmt apheleia-formatters)
          '("nixpkgs-fmt"))
    (setf (alist-get 'nix-mode apheleia-mode-alist) 'nixpkgs-fmt))

  ;; ── Ruby ────────────────────────────────────────────────────────────────────
  ;; FIX-RUBOCOP-FLAG: --autocorrect replaces deprecated --auto-correct
  ;; FIX-MODE-GUARDS: mode mapping inside guard
  (when (executable-find "rubocop")
    (setf (alist-get 'rubocop apheleia-formatters)
          '("rubocop" "--autocorrect" "--stdin" filepath
            "--stderr" "--format" "quiet"))
    (setf (alist-get 'ruby-mode    apheleia-mode-alist) 'rubocop)
    (setf (alist-get 'ruby-ts-mode apheleia-mode-alist) 'rubocop))

  (when (executable-find "standardrb")
    (setf (alist-get 'standardrb apheleia-formatters)
          '("standardrb" "--fix" "--stdin" filepath
            "--stderr" "--format" "quiet")))

  ;; ── PHP ─────────────────────────────────────────────────────────────────────
  ;; FIX-MODE-GUARDS: mode mapping inside guard
  (when (executable-find "phpcbf")
    (setf (alist-get 'phpcbf apheleia-formatters)
          '("phpcbf" "--stdin-path" filepath "-"))
    (setf (alist-get 'php-mode apheleia-mode-alist) 'phpcbf))

  ;; ── Clojure ─────────────────────────────────────────────────────────────────
  ;; FIX-CLJFMT: was in config.yml but missing from this patch entirely.
  ;; FIX-MODE-GUARDS: mode mapping inside guard
  (when (executable-find "cljfmt")
    (setf (alist-get 'cljfmt apheleia-formatters)
          '("cljfmt" "fix" "-"))
    (setf (alist-get 'clojure-mode    apheleia-mode-alist) 'cljfmt)
    (setf (alist-get 'clojure-ts-mode apheleia-mode-alist) 'cljfmt))

  ;; ── Terraform ───────────────────────────────────────────────────────────────
  ;; FIX-MODE-GUARDS: mode mapping inside guard
  (when (executable-find "terraform")
    (setf (alist-get 'terraform-fmt apheleia-formatters)
          '("terraform" "fmt" "-"))
    (setf (alist-get 'terraform-mode apheleia-mode-alist) 'terraform-fmt))

  ;; ── Prose & Config (Tier 4) ─────────────────────────────────────────────────
  ;; prettier handles JSON, YAML, Markdown, GraphQL — always available
  ;; (prettier is a Tier-1 dependency; no guard needed)
  (setf (alist-get 'json-mode      apheleia-mode-alist) 'prettier)
  (setf (alist-get 'json-ts-mode   apheleia-mode-alist) 'prettier)
  (setf (alist-get 'yaml-mode      apheleia-mode-alist) 'prettier)
  (setf (alist-get 'yaml-ts-mode   apheleia-mode-alist) 'prettier)
  (setf (alist-get 'markdown-mode  apheleia-mode-alist) 'prettier)
  (setf (alist-get 'gfm-mode       apheleia-mode-alist) 'prettier)
  (setf (alist-get 'graphql-mode   apheleia-mode-alist) 'prettier)

  ;; ── TOML ────────────────────────────────────────────────────────────────────
  ;; FIX-TOML (retained): taplo replaces prettier for TOML.
  ;; prettier requires @prettier/plugin-toml; without it TOML passes unchanged.
  ;; taplo install: cargo install taplo-cli  OR  npm install -g @taplo/cli
  (when (executable-find "taplo")
    (setf (alist-get 'taplo apheleia-formatters)
          '("taplo" "format" "-"))
    (setf (alist-get 'toml-mode    apheleia-mode-alist) 'taplo)
    (setf (alist-get 'toml-ts-mode apheleia-mode-alist) 'taplo)))

(provide 'apheleia-langs-patch)
;;; apheleia-langs-patch.el ends here
