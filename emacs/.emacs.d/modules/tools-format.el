;;; tools-format.el --- Code Formatting Integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Provides format-all and apheleia setup wired to the config.yml
;;; `formatting:' section. C-c F (format-all-region-or-buffer) and
;;; apheleia format-on-save both read their formatter choices and the
;;; on-save toggle from emacs-ide-config-data at load time.
;;; Add "tools-format" to emacs-ide-feature-modules in init.el,
;;; placed after lang-core and before keybindings.
;;; Version: 2.2.1
;;; Code:

;; ============================================================================
;; CONFIG HELPERS
;; ============================================================================
(defun emacs-ide-format--config-get (key)
  "Get KEY from the `formatting' section of emacs-ide-config-data."
  (when (boundp 'emacs-ide-config-data)
    (let ((fmt (cdr (assoc 'formatting emacs-ide-config-data))))
      (cdr (assoc key fmt)))))

(defun emacs-ide-format--formatter-for (language-key default)
  "Return formatter symbol for LANGUAGE-KEY from config, or DEFAULT.
Returns nil if config explicitly sets the value to false (disabled)."
  (let ((val (emacs-ide-format--config-get language-key)))
    (cond
     ((null val) default)           ; key absent -> use default
     ((eq val t) default)           ; true -> use default
     ((eq val nil) nil)             ; false -> disabled
     (t (intern val)))))            ; string -> symbol

(defun emacs-ide-format--on-save-p ()
  "Return non-nil if format-on-save is enabled in config (default: true)."
  (let ((val (emacs-ide-format--config-get 'on-save)))
    (if (null val) t val)))

;; ============================================================================
;; FORMAT-ALL - universal formatter dispatcher
;; ============================================================================
(use-package format-all
  :commands (format-all-buffer
             format-all-region
             format-all-region-or-buffer)
  :hook (prog-mode . format-all-ensure-formatter)
  :init
  (setq format-all-show-errors
        (or (emacs-ide-format--config-get 'show-errors) 'errors))
  :config
  ;; Build formatter list from config, falling back to sensible defaults.
  ;; A language set to false in config is omitted entirely (formatter disabled).
  (let ((formatters
         (delq nil
               (list
                (when-let ((f (emacs-ide-format--formatter-for 'python     "black")))
                  (list "Python"     (list f)))
                (when-let ((f (emacs-ide-format--formatter-for 'javascript "prettier")))
                  (list "JavaScript" (list f)))
                (when-let ((f (emacs-ide-format--formatter-for 'typescript "prettier")))
                  (list "TypeScript" (list f)))
                (when-let ((f (emacs-ide-format--formatter-for 'html       "prettier")))
                  (list "HTML"       (list f)))
                (when-let ((f (emacs-ide-format--formatter-for 'css        "prettier")))
                  (list "CSS"        (list f)))
                (when-let ((f (emacs-ide-format--formatter-for 'json       "prettier")))
                  (list "JSON"       (list f)))
                (when-let ((f (emacs-ide-format--formatter-for 'markdown   "prettier")))
                  (list "Markdown"   (list f)))
                (when-let ((f (emacs-ide-format--formatter-for 'yaml       "prettier")))
                  (list "YAML"       (list f)))
                (when-let ((f (emacs-ide-format--formatter-for 'rust       "rustfmt")))
                  (list "Rust"       (list f)))
                (when-let ((f (emacs-ide-format--formatter-for 'go         "gofmt")))
                  (list "Go"         (list f)))
                (when-let ((f (emacs-ide-format--formatter-for 'c          "clang-format")))
                  (list "C"          (list f)))
                (when-let ((f (emacs-ide-format--formatter-for 'cpp        "clang-format")))
                  (list "C++"        (list f)))
                (when-let ((f (emacs-ide-format--formatter-for 'shell      "shfmt")))
                  (list "Shell"      (list f)))))))
    (setq-default format-all-formatters formatters)))

;; ============================================================================
;; APHELEIA - async, non-blocking formatter
;; Reads the on-save toggle and per-language choices from config.
;; ============================================================================
(use-package apheleia
  :config
  (let ((py  (emacs-ide-format--formatter-for 'python     "black"))
        (js  (emacs-ide-format--formatter-for 'javascript "prettier"))
        (ts  (emacs-ide-format--formatter-for 'typescript "prettier"))
        (rs  (emacs-ide-format--formatter-for 'rust       "rustfmt"))
        (go  (emacs-ide-format--formatter-for 'go         "gofmt"))
        (c   (emacs-ide-format--formatter-for 'c          "clang-format"))
        (cpp (emacs-ide-format--formatter-for 'cpp        "clang-format")))

    (when (and py (executable-find (symbol-name py)))
      (setf (alist-get 'python-mode apheleia-mode-alist) py))

    (when (and js (executable-find (symbol-name js)))
      (dolist (mode '(js-mode js2-mode web-mode css-mode json-mode markdown-mode))
        (setf (alist-get mode apheleia-mode-alist) js)))

    (when (and ts (executable-find (symbol-name ts)))
      (setf (alist-get 'typescript-mode apheleia-mode-alist) ts))

    (when (and rs (executable-find (symbol-name rs)))
      (setf (alist-get 'rust-mode apheleia-mode-alist) rs))

    (when (and go (executable-find (symbol-name go)))
      (setf (alist-get 'go-mode apheleia-mode-alist) go))

    (when (and c (executable-find (symbol-name c)))
      (setf (alist-get 'c-mode   apheleia-mode-alist) c)
      (setf (alist-get 'c++-mode apheleia-mode-alist) (or cpp c))))

  ;; Respect the on-save toggle from config
  (if (emacs-ide-format--on-save-p)
      (apheleia-global-mode +1)
    (apheleia-global-mode -1)))

;; ============================================================================
;; EDITORCONFIG - respect .editorconfig files in projects
;; ============================================================================
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; ============================================================================
;; FORMAT STATUS CHECKER
;; ============================================================================
(defun emacs-ide-check-formatters ()
  "Report which formatters are available and what config says about them."
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
      (princ (format "Format-on-save: %s\n\n"
                     (if (emacs-ide-format--on-save-p) "ON" "OFF")))
      (princ "Available:\n")
      (dolist (f found)
        (princ (format "  + %-15s -> %s\n" (car f) (cdr f))))
      (when missing
        (princ "\nMissing:\n")
        (dolist (f missing)
          (princ (format "  x %-15s -> %s\n" (car f) (cdr f)))))
      (princ "\nInstall via pip/npm/cargo/go install as appropriate.\n"))))

(provide 'tools-format)
;;; tools-format.el ends here
