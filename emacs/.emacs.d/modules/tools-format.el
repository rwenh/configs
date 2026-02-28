;;; tools-format.el --- Code Formatting Integration -*- lexical-binding: t -*-
;;; Commentary:
;;; NEW MODULE — provides format-all setup so that the C-c F binding in
;;; keybindings.el (format-all-region-or-buffer) actually works.
;;; Also wires per-language formatters referenced in config.yml.
;;; Add "tools-format" to emacs-ide-feature-modules in init.el,
;;; placed after lang-core and before keybindings.
;;; Code:

;; ============================================================================
;; FORMAT-ALL — universal formatter dispatcher
;; ============================================================================
(use-package format-all
  :commands (format-all-buffer
             format-all-region
             format-all-region-or-buffer)
  :hook (prog-mode . format-all-ensure-formatter)
  :init
  (setq format-all-show-errors 'errors)  ; only pop up on actual errors
  :config
  ;; Per-language formatter preferences matching config.yml
  (setq-default format-all-formatters
                '(("Python"     (black))
                  ("JavaScript" (prettier))
                  ("TypeScript" (prettier))
                  ("HTML"       (prettier))
                  ("CSS"        (prettier))
                  ("JSON"       (prettier))
                  ("Markdown"   (prettier))
                  ("YAML"       (prettier))
                  ("Rust"       (rustfmt))
                  ("Go"         (gofmt))
                  ("C"          (clang-format))
                  ("C++"        (clang-format))
                  ("Shell"      (shfmt)))))

;; ============================================================================
;; APHELEIA — async, non-blocking formatter (alternative / complement)
;; Runs formatters without blocking the editor; falls back gracefully.
;; ============================================================================
(use-package apheleia
  :config
  ;; Override per-mode commands to match installed tooling
  (when (executable-find "black")
    (setf (alist-get 'python-mode apheleia-mode-alist) 'black))
  (when (executable-find "prettier")
    (dolist (mode '(js-mode js2-mode typescript-mode web-mode
                    css-mode json-mode markdown-mode))
      (setf (alist-get mode apheleia-mode-alist) 'prettier)))
  (when (executable-find "rustfmt")
    (setf (alist-get 'rust-mode apheleia-mode-alist) 'rustfmt))
  (when (executable-find "gofmt")
    (setf (alist-get 'go-mode apheleia-mode-alist) 'gofmt))
  ;; Enable globally; per-mode format-on-save from config.yml handled here
  (apheleia-global-mode +1))

;; ============================================================================
;; EDITORCONFIG — respect .editorconfig files in projects
;; ============================================================================
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; ============================================================================
;; FORMAT STATUS CHECKER
;; ============================================================================
(defun emacs-ide-check-formatters ()
  "Report which formatters are available."
  (interactive)
  (let ((formatters '(("black"        . "Python")
                      ("prettier"     . "JS/TS/HTML/CSS/JSON/MD/YAML")
                      ("rustfmt"      . "Rust")
                      ("gofmt"        . "Go")
                      ("clang-format" . "C/C++")
                      ("shfmt"        . "Shell")))
        (found '())
        (missing '()))
    (dolist (f formatters)
      (if (executable-find (car f))
          (push f found)
        (push f missing)))
    (with-output-to-temp-buffer "*Formatters*"
      (princ "=== CODE FORMATTER STATUS ===\n\n")
      (princ "Available:\n")
      (dolist (f found)
        (princ (format "  ✓ %-15s → %s\n" (car f) (cdr f))))
      (when missing
        (princ "\nMissing:\n")
        (dolist (f missing)
          (princ (format "  ✗ %-15s → %s\n" (car f) (cdr f)))))
      (princ "\nInstall via pip/npm/cargo/go install as appropriate.\n"))))

(provide 'tools-format)
;;; tools-format.el ends here
