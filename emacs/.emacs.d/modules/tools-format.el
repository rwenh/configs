;;; tools-format.el --- Code Formatting with Apheleia -*- lexical-binding: t -*-
;;; Commentary:
;;; Unified code formatting using apheleia, supporting 40+ languages.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (recalibration):
;;;   - FIX-ALIST-MISUSE: Previous version used apheleia-formatters for
;;;     mode→formatter mapping. apheleia-formatters maps formatter-name→command;
;;;     apheleia-mode-alist maps major-mode→formatter-name. Both alists were
;;;     confused throughout the :config block. All mode mappings now correctly
;;;     use apheleia-mode-alist.
;;;   - FIX-TAPLO: Correct: taplo entry added to apheleia-formatters (command)
;;;     AND toml-mode entry added to apheleia-mode-alist (mapping).
;;;   - FIX-FORMATTERS-GUARD: All formatters guarded with (assq) to avoid
;;;     re-adding known entries that apheleia already ships with.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; APHELEIA — unified code formatting
;; ============================================================================
(use-package apheleia
  :defer t
  :config

  ;; ── Taplo (TOML formatter) ────────────────────────────────────────────────
  ;; FIX-ALIST-MISUSE: apheleia-formatters = name→command (the executable spec)
  ;;                   apheleia-mode-alist  = mode→formatter-name (the dispatch)
  ;; Both must be set; setting only one silently does nothing.
  (when (and (boundp 'apheleia-formatters)
             (executable-find "taplo"))
    (unless (assq 'taplo apheleia-formatters)
      (push '(taplo "taplo" "format" "-") apheleia-formatters))
    (when (boundp 'apheleia-mode-alist)
      (setf (alist-get 'toml-mode    apheleia-mode-alist) 'taplo)
      (setf (alist-get 'toml-ts-mode apheleia-mode-alist) 'taplo)))

  ;; ── Additional mode mappings (apheleia already defines the commands) ──────
  (when (boundp 'apheleia-mode-alist)
    (when (executable-find "prettier")
      (setf (alist-get 'json-mode   apheleia-mode-alist) 'prettier)
      (setf (alist-get 'yaml-mode   apheleia-mode-alist) 'prettier))
    (when (executable-find "black")
      (setf (alist-get 'python-mode    apheleia-mode-alist) 'black)
      (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'black))
    (when (executable-find "rustfmt")
      (setf (alist-get 'rust-mode    apheleia-mode-alist) 'rustfmt)
      (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt)))

  ;; ── Format on save ────────────────────────────────────────────────────────
  (add-hook 'prog-mode-hook
            (lambda ()
              (when (and (fboundp 'emacs-ide-config-get)
                         (emacs-ide-config-get 'formatting 'on-save t)
                         (fboundp 'apheleia-mode))
                (apheleia-mode 1))))

  (add-hook 'text-mode-hook
            (lambda ()
              (when (and (fboundp 'emacs-ide-config-get)
                         (emacs-ide-config-get 'formatting 'on-save t)
                         (fboundp 'apheleia-mode)
                         (not (derived-mode-p 'org-mode)))
                (apheleia-mode 1))))

  ;; ── Formatter check ───────────────────────────────────────────────────────
  (defun emacs-ide-check-formatters ()
    "Report which formatters are installed and which are missing."
    (interactive)
    (let ((formatters '(("black"              . "Python")
                        ("prettier"           . "JS/TS/HTML/CSS/JSON/YAML/MD")
                        ("rustfmt"            . "Rust")
                        ("gofmt"              . "Go")
                        ("clang-format"       . "C/C++")
                        ("google-java-format" . "Java")
                        ("ktlint"             . "Kotlin")
                        ("scalafmt"           . "Scala")
                        ("stylua"             . "Lua")
                        ("shfmt"              . "Shell")
                        ("pg_format"          . "SQL")
                        ("taplo"              . "TOML")
                        ("nixpkgs-fmt"        . "Nix")
                        ("ormolu"             . "Haskell")
                        ("cljfmt"             . "Clojure")
                        ("terraform"          . "Terraform")))
          (found '())
          (missing '()))
      (dolist (f formatters)
        (if (executable-find (car f))
            (push f found)
          (push f missing)))
      (with-output-to-temp-buffer "*Formatter Status*"
        (princ "=== FORMATTER STATUS ===\n\n")
        (princ (format "Installed: %d\n" (length found)))
        (dolist (f (nreverse found))
          (princ (format "  ✓ %-25s (%s)\n" (car f) (cdr f))))
        (when missing
          (princ (format "\nMissing: %d\n" (length missing)))
          (dolist (f (nreverse missing))
            (princ (format "  ✗ %-25s (%s)\n" (car f) (cdr f)))))))))

(provide 'tools-format)
;;; tools-format.el ends here
