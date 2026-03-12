;;; tools-format.el --- Code Formatting Integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Provides format-all and apheleia setup wired to the config.yml
;;; `formatting:' section. C-c F (format-all-region-or-buffer) and
;;; apheleia format-on-save both read their formatter choices and the
;;; on-save toggle from emacs-ide-config-data at load time.
;;; Add "tools-format" to emacs-ide-feature-modules in init.el,
;;; placed after lang-core and before keybindings.
;;; Version: 2.2.4
;;; Fixes:
;;;   - 2.2.4: emacs-ide-format--formatter-for previously called `(intern val)'
;;;     unconditionally on any string from config.yml. If the string does not
;;;     match a key in `apheleia-formatters', apheleia silently fails at save
;;;     time. Fix: validate against explicit allowlist
;;;     `emacs-ide-format--known-apheleia-formatters'. Known values intern to
;;;     symbols; unknown values return as strings and emit a :warning.
;;;     apheleia mode-alist entries are now guarded with `symbolp' so
;;;     unrecognised strings never reach apheleia-mode-alist.
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

;; FIX 2.2.4: The set of valid apheleia formatter keys for languages this
;;   config exposes. `emacs-ide-format--formatter-for' previously called
;;   `(intern val)' unconditionally on any string from config.yml, which
;;   works coincidentally when the config string happens to match the key
;;   in `apheleia-formatters' exactly (e.g. "black" -> 'black), but silently
;;   produces an unknown symbol for anything outside this set — apheleia
;;   then reports "no such formatter" at save time with no clear indication
;;   of where the bad value came from.
;;   Fix: validate the interned symbol against this explicit allowlist before
;;   returning it.  An unrecognised string falls through to the string-value
;;   branch (returned as-is) so callers that use the value for executable-find
;;   (format-all) continue to work, while apheleia callers receive nil for
;;   unrecognised values and skip the mode registration rather than register
;;   a broken formatter.
(defconst emacs-ide-format--known-apheleia-formatters
  '(black prettier rustfmt gofmt gofumpt shfmt
    clang-format clang-format-diff autopep8 yapf isort ruff
    eslint eslint_d biome deno
    rubocop standardrb perltidy
    phpcbf phpcs psalm phan
    ktlint google-java-format scalafmt
    terraform nixpkgs-fmt ormolu fourmolu
    swiftformat ocamlformat elm-format
    mix cljfmt zprint pgformatter
    luaformatter stylua cmake-format
    xmllint tidy sqlfluff)
  "Symbols that are valid keys in `apheleia-formatters'.
Used by `emacs-ide-format--formatter-for' to validate config values
before interning them as apheleia formatter keys.")

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
  to distinguish absent key from explicit false.
FIX 2.2.4: `(intern val)' was called unconditionally on any config string.
  If the string does not match an `apheleia-formatters' key the formatter
  is silently unknown at save time.  Now validates against
  `emacs-ide-format--known-apheleia-formatters': known values are interned
  to symbols (for apheleia); unknown values are returned as strings (for
  format-all / executable-find callers) but apheleia mode-alist entries
  for unrecognised formatters are skipped."
  (let ((val (emacs-ide-format--config-get language-key)))
    (cond
     ((eq val :absent) default)    ; key not in config  -> use default
     ((eq val t)       default)    ; explicit true       -> use default
     ((null val)       nil)        ; explicit false/nil  -> disabled
     (t
      ;; Validate before interning: only produce a symbol when it is a
      ;; known apheleia formatter key.  Unknown strings are returned as
      ;; strings so format-all / executable-find callers still work.
      (let ((sym (intern val)))
        (if (memq sym emacs-ide-format--known-apheleia-formatters)
            sym
          (display-warning
           'emacs-ide
           (format "tools-format: unknown formatter %S in config \
(not in emacs-ide-format--known-apheleia-formatters). \
apheleia entry will be skipped; add to the allowlist if this is a valid \
apheleia formatter key."
                   val)
           :warning)
          val))))))           ; string value        -> symbol

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
    ;; FIX 2.2.4: Only register entries whose value is a symbol (i.e. passed
    ;; the allowlist check in emacs-ide-format--formatter-for).  A string
    ;; value means the formatter name was unrecognised — apheleia cannot use
    ;; it and a warning was already emitted above.
    (when (and py (symbolp py))
      (setf (alist-get 'python-mode apheleia-mode-alist) py))

    (when (and js (symbolp js))
      (dolist (mode '(js-mode js2-mode web-mode css-mode json-mode markdown-mode))
        (setf (alist-get mode apheleia-mode-alist) js)))

    (when (and ts (symbolp ts))
      (setf (alist-get 'typescript-mode apheleia-mode-alist) ts))

    (when (and rs (symbolp rs))
      (setf (alist-get 'rust-mode apheleia-mode-alist) rs))

    (when (and go (symbolp go))
      (setf (alist-get 'go-mode apheleia-mode-alist) go))

    (when (and c (symbolp c))
      (setf (alist-get 'c-mode   apheleia-mode-alist) c)
      (setf (alist-get 'c++-mode apheleia-mode-alist) (or (and (symbolp cpp) cpp) c))))

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
