;;; tools-format.el --- Code Formatting with Apheleia -*- lexical-binding: t -*-
;;; Commentary:
;;; Unified code formatting using apheleia, supporting 40+ languages.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (post-audit calibration):
;;;   - FIX-FORMAT-HOOK-TIMING: format-on-save prog-mode-hook and text-mode-hook
;;;     lambdas were inside (use-package apheleia ... :config ...).  Since
;;;     apheleia is :defer t, its :config only runs when apheleia first loads —
;;;     triggered by the first formatter call, which happens inside a buffer that
;;;     has already activated its major-mode.  That first buffer therefore misses
;;;     the hook entirely.  Hooks are now registered at tools-format.el load time
;;;     (unconditionally), guarding (apheleia-mode 1) with fboundp so they are
;;;     safe to call whether apheleia has loaded yet or not.
;;; Fixes vs 3.0.4 (recalibration, retained):
;;;   - FIX-ALIST-MISUSE, FIX-TAPLO, FIX-FORMATTERS-GUARD.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; FORMAT-ON-SAVE HOOKS
;; FIX-FORMAT-HOOK-TIMING: Registered here at module load time so every
;; prog-mode and text-mode buffer gets the hook, including the very first one
;; that triggers apheleia's deferred load.  The (fboundp 'apheleia-mode) guard
;; makes this safe to evaluate before apheleia loads.
;; ============================================================================
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
