;;; tools-format.el --- Formatting with Apheleia -*- lexical-binding: t -*-
;;; Version: 3.1.1 | PATCH: Added taplo TOML formatter (FIX #10)
;;; Code:

(use-package apheleia
  :defer t
  :hook ((prog-mode text-mode) . apheleia-mode)
  :config
  ;; FIX #10: Register taplo for TOML formatting
  (when (executable-find "taplo")
    (unless (assq 'taplo apheleia-formatters)
      (push '(taplo "taplo" "format" "-") apheleia-formatters))
    (setf (alist-get 'toml-mode apheleia-mode-alist) 'taplo)
    (setf (alist-get 'toml-ts-mode apheleia-mode-alist) 'taplo)
    (message "✓ Taplo TOML formatter registered")))

(defun emacs-ide-check-formatters ()
  (interactive)
  (let ((fmts '(("black" . "Python") ("prettier" . "JS/TS/HTML/CSS/JSON/YAML")
                ("rustfmt" . "Rust") ("gofmt" . "Go") ("clang-format" . "C/C++")
                ("taplo" . "TOML") ("stylua" . "Lua") ("shfmt" . "Shell"))))
    (with-output-to-temp-buffer "*Formatters*"
      (princ "=== FORMATTER STATUS ===\n\n")
      (let ((found (cl-remove-if-not (lambda (f) (executable-find (car f))) fmts))
            (missing (cl-remove-if (lambda (f) (executable-find (car f))) fmts)))
        (princ (format "Found: %d\n" (length found)))
        (dolist (f found) (princ (format "  ✓ %-20s (%s)\n" (car f) (cdr f))))
        (when missing
          (princ (format "\nMissing: %d\n" (length missing)))
          (dolist (f missing) (princ (format "  ✗ %-20s (%s)\n" (car f) (cdr f)))))))))

(provide 'tools-format)
;;; tools-format.el ends here
