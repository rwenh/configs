;;; tools-format.el --- Formatting with Apheleia -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(use-package apheleia
  :defer t
  :hook (prog-mode . apheleia-mode)
  :config
  ;; TOML via taplo — registered here as the canonical location.
  ;; apheleia-langs-patch.el also sets this; the duplicate is idempotent.
  (when (executable-find "taplo")
    (unless (assq 'taplo apheleia-formatters)
      (push '(taplo "taplo" "format" "-") apheleia-formatters))
    (setf (alist-get 'toml-mode    apheleia-mode-alist) 'taplo)
    (setf (alist-get 'toml-ts-mode apheleia-mode-alist) 'taplo)))

;;;; ── Formatter status command ────────────────────────────────────────────────

(defun emacs-ide-check-formatters ()
  "Display a buffer showing which formatters are installed and which are missing."
  (interactive)
  (let ((fmts '(;; Tier 1 — most users need these
                ("black"        . "Python")
                ("prettier"     . "JS / TS / JSON / YAML / Markdown")
                ("rustfmt"      . "Rust")
                ("gofmt"        . "Go")
                ("clang-format" . "C / C++")
                ;; Tier 2 — language-specific
                ("stylua"       . "Lua")
                ("shfmt"        . "Shell")
                ("taplo"        . "TOML")
                ("ktlint"       . "Kotlin")
                ("google-java-format" . "Java")
                ("scalafmt"     . "Scala")
                ("pg_format"    . "SQL / PostgreSQL")
                ("ormolu"       . "Haskell")
                ("mix"          . "Elixir (mix format)")
                ("ocamlformat"  . "OCaml")
                ("nixpkgs-fmt"  . "Nix")
                ("rubocop"      . "Ruby")
                ("cljfmt"       . "Clojure")
                ("terraform"    . "Terraform")
                ("isort"        . "Python imports")
                ("ruff"         . "Python (ruff, optional)"))))
    (with-output-to-temp-buffer "*Formatter Status*"
      (princ "=== FORMATTER STATUS ===\n\n")
      (let ((found   (cl-remove-if-not
                      (lambda (f) (executable-find (car f))) fmts))
            (missing (cl-remove-if
                      (lambda (f) (executable-find (car f))) fmts)))
        (princ (format "Found:   %d\n" (length found)))
        (dolist (f found)
          (princ (format "  ✓ %-25s %s\n" (car f) (cdr f))))
        (when missing
          (princ (format "\nMissing: %d\n" (length missing)))
          (dolist (f missing)
            (princ (format "  ✗ %-25s %s\n" (car f) (cdr f)))))
        (princ "\nSee README §Tool Installation for per-distro install commands.\n")))))

(provide 'tools-format)
;;; tools-format.el ends here
