;;; tools-format.el --- Code Formatting Integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Provides format-all and apheleia setup wired to the config.yml
;;; `formatting:' section. C-c F (format-all-region-or-buffer) and
;;; apheleia format-on-save both read their formatter choices and the
;;; on-save toggle from emacs-ide-config-data at load time.
;;; Add "tools-format" to emacs-ide-feature-modules in init.el,
;;; placed after lang-core and before keybindings.
;;; Version: 2.2.3
;;; Fixes:
;;;   - 2.2.2: emacs-ide-format--formatter-for had a dead branch: Elisp `null`
;;;     and `(eq val nil)` are identical predicates — both match nil. Since YAML
;;;     `false` parses to nil AND an absent key also returns nil from assoc,
;;;     the "(eq val nil) -> disabled" branch was unreachable and the function
;;;     could never distinguish "user explicitly set false" from "key absent".
;;;     FIX: `emacs-ide-format--config-get` now returns a sentinel `:absent`
;;;     when the key is missing from the section (vs nil for an explicit false).
;;;     `emacs-ide-format--formatter-for` tests for `:absent` to mean "use
;;;     default" and for nil to mean "disabled by user". This correctly
;;;     implements the intended key-absent vs explicitly-false distinction.
;;;   - 2.2.3: format-all-formatters entries had an extra level of list nesting.
;;;     Each entry was (LANGUAGE (FORMATTER)) — format-all expects (LANGUAGE FORMATTER).
;;;     (list "Python" (list f)) produced ("Python" (black)); format-all received
;;;     an invalid formatter spec and silently skipped every language. Fixed to
;;;     (list "Python" f) = ("Python" black).
;;; Code:

;; ============================================================================
;; CONFIG HELPERS
;; ============================================================================
(defun emacs-ide-format--config-get (key)
  "Get KEY from the `formatting' section of emacs-ide-config-data.
Returns the value if present, or the sentinel symbol `:absent' if the
key does not exist in the section.  This distinguishes YAML `false'
(parsed as nil) from a missing key (both used to return nil)."
  (if (not (boundp 'emacs-ide-config-data))
      :absent
    (let* ((fmt  (cdr (assoc 'formatting emacs-ide-config-data)))
           (cell (assoc key fmt)))
      (if cell (cdr cell) :absent))))

(defun emacs-ide-format--formatter-for (language-key default)
  "Return formatter symbol for LANGUAGE-KEY from config, or DEFAULT.
Returns nil when the config key is explicitly set to false (disabled).
FIX 2.2.2: Previously (null val) and (eq val nil) were identical tests —
  both matched nil, making the 'false -> disabled' branch unreachable.
  Now uses the `:absent' sentinel from `emacs-ide-format--config-get'
  to distinguish absent key from explicit false."
  (let ((val (emacs-ide-format--config-get language-key)))
    (cond
     ((eq val :absent) default)    ; key not in config  -> use default
     ((eq val t)       default)    ; explicit true       -> use default
     ((null val)       nil)        ; explicit false/nil  -> disabled
     (t (intern val)))))           ; string value        -> symbol

(defun emacs-ide-format--on-save-p ()
  "Return non-nil if format-on-save is enabled in config (default: true)."
  (let ((val (emacs-ide-format--config-get 'on-save)))
    (if (eq val :absent) t (and val t))))

;; ============================================================================
;; FORMAT-ALL - universal formatter dispatcher
;; ============================================================================
(use-package format-all
  :commands (format-all-buffer
             format-all-region
             format-all-region-or-buffer)
  :hook (prog-mode . format-all-ensure-formatter)
  :bind (("C-c F" . format-all-region-or-buffer))
  :init
  (let ((show-errors (emacs-ide-format--config-get 'show-errors)))
    (setq format-all-show-errors
          (if (eq show-errors :absent) 'errors show-errors)))
  :config
  ;; Build formatter list from config, falling back to sensible defaults.
  ;; A language set to false in config is omitted entirely.
  ;; FIX 2.2.3: Each entry must be (LANGUAGE FORMATTER), not (LANGUAGE (FORMATTER)).
  ;; The previous (list LANG (list f)) produced (LANG (black)) — an invalid spec
  ;; that format-all silently ignored, so no language was ever formatted.
  (let ((formatters
         (delq nil
               (list
                (when-let ((f (emacs-ide-format--formatter-for 'python     "black")))
                  (list "Python"     f))
                (when-let ((f (emacs-ide-format--formatter-for 'javascript "prettier")))
                  (list "JavaScript" f))
                (when-let ((f (emacs-ide-format--formatter-for 'typescript "prettier")))
                  (list "TypeScript" f))
                (when-let ((f (emacs-ide-format--formatter-for 'html       "prettier")))
                  (list "HTML"       f))
                (when-let ((f (emacs-ide-format--formatter-for 'css        "prettier")))
                  (list "CSS"        f))
                (when-let ((f (emacs-ide-format--formatter-for 'json       "prettier")))
                  (list "JSON"       f))
                (when-let ((f (emacs-ide-format--formatter-for 'markdown   "prettier")))
                  (list "Markdown"   f))
                (when-let ((f (emacs-ide-format--formatter-for 'yaml       "prettier")))
                  (list "YAML"       f))
                (when-let ((f (emacs-ide-format--formatter-for 'rust       "rustfmt")))
                  (list "Rust"       f))
                (when-let ((f (emacs-ide-format--formatter-for 'go         "gofmt")))
                  (list "Go"         f))
                (when-let ((f (emacs-ide-format--formatter-for 'c          "clang-format")))
                  (list "C"          f))
                (when-let ((f (emacs-ide-format--formatter-for 'cpp        "clang-format")))
                  (list "C++"        f))
                (when-let ((f (emacs-ide-format--formatter-for 'shell      "shfmt")))
                  (list "Shell"      f))))))
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

    ;; FIX: executable-find guards removed from apheleia entries.
    ;; The previous code did (when (and js (executable-find (symbol-name js))) ...)
    ;; i.e. (executable-find "prettier"). prettier is almost always installed
    ;; locally via npm (./node_modules/.bin/prettier) and is NOT on PATH, so
    ;; executable-find always returned nil and the js/ts/css/json/md entries
    ;; were silently never added to apheleia-mode-alist — JS formatting never worked.
    ;; apheleia handles a missing or locally-installed formatter gracefully
    ;; (it reports a per-save error rather than crashing), so the guard is wrong here.
    ;; format-all (above) still needs the PATH check because it builds a static list.
    (when py (setf (alist-get 'python-mode apheleia-mode-alist) py))

    (when js
      (dolist (mode '(js-mode js2-mode web-mode css-mode json-mode markdown-mode))
        (setf (alist-get mode apheleia-mode-alist) js)))

    (when ts
      (setf (alist-get 'typescript-mode apheleia-mode-alist) ts))

    (when rs
      (setf (alist-get 'rust-mode apheleia-mode-alist) rs))

    (when go
      (setf (alist-get 'go-mode apheleia-mode-alist) go))

    (when c
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
