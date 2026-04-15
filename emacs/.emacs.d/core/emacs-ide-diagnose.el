;;; emacs-ide-diagnose.el --- Rapid Diagnostics -*- lexical-binding: t -*-
;;; Version: 3.1.0 | NEW: unified diagnostics module
;;; Code:

(require 'cl-lib)

(defun emacs-ide-diagnose ()
  "Run comprehensive IDE diagnostics"
  (interactive)
  (with-output-to-temp-buffer "*IDE Diagnostics*"
    (princ (format "=== EMACS IDE v%s DIAGNOSTICS ===\n"
                   (or (bound-and-true-p emacs-ide-version) "3.1.0")))
    (princ (format "Emacs %s | %s\n\n" emacs-version (format-time-string "%Y-%m-%d %H:%M")))

    (princ "CORE MODULES:\n")
    (dolist (mod '(emacs-ide-config emacs-ide-health emacs-ide-recovery emacs-ide-telemetry))
      (princ (format "  %s %s\n" (if (featurep mod) "✓" "✗") mod)))

    (princ "\nKEY FUNCTIONS:\n")
    (dolist (fn '(emacs-ide-config-load emacs-ide-health-check-all emacs-ide-diagnose))
      (princ (format "  %s %s\n" (if (fboundp fn) "✓" "✗") fn)))

    (princ "\nMODE STATUS:\n")
    (dolist (mode '(electric-pair-mode show-paren-mode delete-selection-mode))
      (let ((sym (symbol-value mode)))
        (princ (format "  %s %s\n" (if sym "✓" "✗") mode))))

    (princ "\nPERFORMANCE:\n")
    (princ (format "  GC threshold: %d bytes\n" gc-cons-threshold))
    (princ (format "  GC collections: %d\n" gcs-done))
    (when (bound-and-true-p after-init-time)
      (princ (format "  Startup: %.2fs\n" (float-time after-init-time))))

    (princ "\nCONFIG VALIDATION:\n")
    (princ (format "  Config loaded: %s\n" (if (bound-and-true-p emacs-ide-config-loaded-p) "✓" "✗")))
    (princ (format "  LSP enabled: %s\n" (if (bound-and-true-p emacs-ide-lsp-enable) "✓" "✗")))
    (princ (format "  Theme: %s\n" (or (bound-and-true-p emacs-ide-theme) "none")))))

(defun emacs-ide-diagnose-lsp ()
  "LSP diagnostics"
  (interactive)
  (with-output-to-temp-buffer "*LSP Diagnostics*"
    (princ "=== LSP DIAGNOSTICS ===\n\n")
    (princ (format "LSP Enable: %s\n" (if (bound-and-true-p emacs-ide-lsp-enable) "✓" "✗")))
    (princ (format "LSP Mode: %s\n\n" (if (bound-and-true-p lsp-mode) "active" "inactive")))
    (princ "LSP Servers:\n")
    (dolist (srv '(("pyright" . "Python")
                   ("rust-analyzer" . "Rust")
                   ("gopls" . "Go")
                   ("typescript-language-server" . "TypeScript")))
      (princ (format "  %s %s (%s)\n" (if (executable-find (car srv)) "✓" "✗") (cdr srv) (car srv))))))

(provide 'emacs-ide-diagnose)
;;; emacs-ide-diagnose.el ends here
