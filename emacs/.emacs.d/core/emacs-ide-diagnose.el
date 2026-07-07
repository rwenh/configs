;;; emacs-ide-diagnose.el --- Rapid Diagnostics -*- lexical-binding: t -*-
;;;
;;; Code:

(require 'cl-lib)

(defun emacs-ide-diagnose ()
  "Run comprehensive IDE diagnostics across all modules, modes, and config."
  (interactive)
  (with-output-to-temp-buffer "*IDE Diagnostics*"
    (princ (format "=== EMACS IDE v%s DIAGNOSTICS ===\n"
                   (or (bound-and-true-p emacs-ide-version) "3.2.1")))
    (princ (format "Emacs %s | %s\n\n" emacs-version (format-time-string "%Y-%m-%d %H:%M")))

    ;; ── Core modules ───────────────────────────────────────────────────────────
    (princ "CORE MODULES:\n")
    (dolist (mod '(emacs-ide-config emacs-ide-health emacs-ide-recovery
                   emacs-ide-package emacs-ide-profiler emacs-ide-security
                   emacs-ide-telemetry emacs-ide-test emacs-ide-spot-check
                   emacs-ide-diagnose))
      (princ (format "  %s %s\n" (if (featurep mod) "✓" "✗") mod)))

    ;; ── Feature modules ────────────────────────────────────────────────────────
    (princ "\nFEATURE MODULES:\n")
    (dolist (mod '(ui-core ui-theme ui-modeline ui-dashboard ui-workspace
                   completion-core completion-snippets editing-core core-dev
                   tools-lsp tools-project tools-git tools-terminal tools-format
                   apheleia-langs-patch tools-org tools-spelling tools-notes
                   tools-rest tools-test-runner-registry tools-test debug-core
                   tools-repl tools-project-detect tools-hydra keybindings))
      (princ (format "  %s %s\n" (if (featurep mod) "✓" "✗") mod)))

    ;; ── Lib layer ──────────────────────────────────────────────────────────────
    (princ "\nLIB LAYER (ide-*):\n")
    (dolist (lib '(ide-common ide-simple ide-window
                   ide-search ide-pair ide-register))
      (let* ((path (expand-file-name
                    (concat "lib/" (symbol-name lib) ".el")
                    user-emacs-directory))
             (loaded  (featurep lib))
             (on-disk (file-exists-p path)))
        (princ (format "  %s %-18s %s\n"
                       (cond (loaded "✓") (on-disk "○") (t "✗"))
                       lib
                       (cond (loaded "loaded") (on-disk "on disk") (t "MISSING"))))))

    ;; ── Key functions ──────────────────────────────────────────────────────────
    (princ "\nKEY FUNCTIONS:\n")
    (dolist (fn '(emacs-ide-config-load
                  emacs-ide-config-reload
                  emacs-ide-health-check-all
                  emacs-ide-health-auto-fix
                  emacs-ide-lsp-status
                  emacs-ide-lsp-check-servers
                  emacs-ide-toggle-theme
                  emacs-ide-run-tests
                  emacs-ide-update
                  emacs-ide-freeze-versions
                  emacs-ide-repl-launch
                  emacs-ide-test-run
                  emacs-ide-recovery-report
                  emacs-ide-workspace-status
                  emacs-ide-detect-show-status))
      (princ (format "  %s %s\n" (if (fboundp fn) "✓" "✗") fn)))

    ;; ── Mode status ────────────────────────────────────────────────────────────
    (princ "\nMODE STATUS:\n")
    (dolist (mode '(electric-pair-mode show-paren-mode delete-selection-mode
                    global-display-line-numbers-mode winner-mode))
      (let ((val (and (boundp mode) (symbol-value mode))))
        (princ (format "  %s %s\n" (if val "✓" "✗") mode))))

    ;; ── Performance ────────────────────────────────────────────────────────────
    (princ "\nPERFORMANCE:\n")
    (princ (format "  GC threshold:  %d bytes\n" gc-cons-threshold))
    (princ (format "  GC collections: %d\n" gcs-done))
    (let* ((phases  (and (boundp 'emacs-ide--startup-phases) emacs-ide--startup-phases))
           (elapsed (and phases (cdr (assoc "startup-complete" phases)))))
      (when elapsed
        (princ (format "  Startup time:  %.2fs\n" elapsed))))

    ;; ── Config validation ──────────────────────────────────────────────────────
    (princ "\nCONFIG VALIDATION:\n")
    (princ (format "  Config loaded:  %s\n"
                   (if (bound-and-true-p emacs-ide-config-loaded-p) "✓" "✗")))
    (princ (format "  Environment:    %s\n"
                   (or (bound-and-true-p emacs-ide-config-environment) "unknown")))
    (princ (format "  LSP enabled:    %s\n"
                   (if (bound-and-true-p emacs-ide-lsp-enable) "✓" "✗")))
    (princ (format "  Theme:          %s\n"
                   (or (bound-and-true-p emacs-ide-theme) "none")))
    (princ (format "  GC threshold:   %s\n"
                   (if (= gc-cons-threshold 16777216) "✓ 16MB" "✗ unexpected value")))

    (princ "\nRun M-x emacs-ide-run-tests for the full ERT suite.\n")))

(defun emacs-ide-diagnose-lsp ()
  "Show LSP server availability for all supported languages."
  (interactive)
  (with-output-to-temp-buffer "*LSP Diagnostics*"
    (princ "=== LSP DIAGNOSTICS ===\n\n")
    (princ (format "LSP enabled:  %s\n"
                   (if (bound-and-true-p emacs-ide-lsp-enable) "✓" "✗")))
    (princ (format "lsp-mode loaded: %s\n\n"
                   (if (featurep 'lsp-mode) "✓" "✗ (loads on first file open)")))
    (princ "LSP Servers on PATH:\n")
    (dolist (srv '(("pyright"                   . "Python")
                   ("pylsp"                      . "Python (pylsp)")
                   ("rust-analyzer"              . "Rust")
                   ("gopls"                      . "Go")
                   ("typescript-language-server" . "JS / TypeScript")
                   ("clangd"                     . "C / C++")
                   ("jdtls"                      . "Java")
                   ("kotlin-language-server"     . "Kotlin")
                   ("lua-language-server"         . "Lua")
                   ("bash-language-server"       . "Shell")
                   ("yaml-language-server"       . "YAML")
                   ("sqls"                       . "SQL")
                   ("solargraph"                 . "Ruby")
                   ("elixir-ls"                  . "Elixir")
                   ("clojure-lsp"                . "Clojure")
                   ("haskell-language-server"    . "Haskell")
                   ("zls"                        . "Zig")
                   ("nil"                        . "Nix")
                   ("metals"                     . "Scala")))
      (let ((found (executable-find (car srv))))
        (princ (format "  %s %-36s %s\n"
                       (if found "✓" "✗")
                       (cdr srv)
                       (if found (car srv) "(not on PATH)")))))))

(defun emacs-ide-diagnose-languages ()
  "Show which language modules are enabled in config.yml and their status."
  (interactive)
  (with-output-to-temp-buffer "*Language Diagnostics*"
    (princ "=== LANGUAGE MODULE STATUS ===\n\n")
    (princ (format "%-18s %-10s %-12s %-28s %s\n"
                   "Language" "Enabled" "Loaded" "LSP server" "Formatter"))
    (princ (make-string 80 ?─))
    (princ "\n")
    (dolist (entry
             '(("python"     "lang-python"     "pyright"                   "black")
               ("javascript" "lang-web"        "typescript-language-server" "prettier")
               ("typescript" "lang-web"        "typescript-language-server" "prettier")
               ("rust"       "lang-rust"       "rust-analyzer"             "rustfmt")
               ("go"         "lang-go"         "gopls"                     "gofmt")
               ("c"          "lang-c"          "clangd"                    "clang-format")
               ("java"       "lang-jvm"        "jdtls"                     "google-java-format")
               ("kotlin"     "lang-jvm"        "kotlin-language-server"    "ktlint")
               ("lua"        "lang-lua"        "lua-language-server"        "stylua")
               ("shell"      "lang-shell"      "bash-language-server"      "shfmt")
               ("sql"        "lang-sql"        "sqls"                      "pg_format")
               ("haskell"    "lang-functional" "haskell-language-server"   "ormolu")
               ("clojure"    "lang-functional" "clojure-lsp"               "cljfmt")
               ("elixir"     "lang-functional" "elixir-ls"                 "mix-format")
               ("scala"      "lang-jvm"        "metals"                    "scalafmt")
               ("zig"        "lang-systems"    "zls"                       "zig fmt")
               ("nix"        "lang-systems"    "nil"                       "nixpkgs-fmt")
               ("r"          "lang-data"       "r-languageserver"          "styler")
               ("julia"      "lang-data"       "julia-language-server"     "JuliaFormatter")
               ("prose"      "lang-prose"      "—"                         "prettier")))
      (let* ((lang      (nth 0 entry))
             (module    (nth 1 entry))
             (lsp-bin   (nth 2 entry))
             (fmt-bin   (nth 3 entry))
             (enabled   (if (fboundp 'emacs-ide-dev-lang-enabled-p)
                            (emacs-ide-dev-lang-enabled-p lang)
                          t))
             (loaded    (featurep (intern module)))
             (lsp-ok    (or (string= lsp-bin "—") (executable-find lsp-bin)))
             (fmt-ok    (or (string= fmt-bin "—")
                            (executable-find (car (split-string fmt-bin))))))
        (princ (format "%-18s %-10s %-12s %-28s %s\n"
                       lang
                       (if enabled "yes" "no")
                       (if loaded "✓ loaded" "lazy")
                       (if lsp-ok (concat "✓ " lsp-bin) (concat "✗ " lsp-bin))
                       (if fmt-ok (concat "✓ " fmt-bin) (concat "✗ " fmt-bin))))))
    (princ "\n'lazy' = loads on first file open of that type. This is normal.\n")))

(provide 'emacs-ide-diagnose)
;;; emacs-ide-diagnose.el ends here
