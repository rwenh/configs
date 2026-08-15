;;; emacs-ide-config.el --- Lean Configuration System -*- lexical-binding: t -*-
;;; Version: 3.7.0
;;;
;;; Code:

(require 'cl-lib)

;;;; ── Public variables ────────────────────────────────────────────────────────

(defvar emacs-ide-config-file
  (expand-file-name "config.yml" user-emacs-directory))

(defvar emacs-ide-config-data nil)
(defvar emacs-ide-config-environment nil)
(defvar emacs-ide-config-loaded-p nil)
(defvar emacs-ide-config-reload-hook nil)

;;;; ── Config variable declarations ──────────────────────────────────────────

;; General
(defvar emacs-ide-theme           'ef-dark)
(defvar emacs-ide-font            "JetBrains Mono")
(defvar emacs-ide-font-size       11)

;; LSP
(defvar emacs-ide-lsp-enable                t)
(defvar emacs-ide-lsp-enable-inlay-hints    t)
(defvar emacs-ide-lsp-idle-delay            0.3)
(defvar emacs-ide-lsp-breadcrumb            t)
(defvar emacs-ide-lsp-semantic-tokens       t)
(defvar emacs-ide-lsp-lens                  t)
(defvar emacs-ide-lsp-sideline              t)
(defvar emacs-ide-lsp-signature-help        t)
(defvar emacs-ide-lsp-hover-docs            t)
(defvar emacs-ide-lsp-symbol-highlighting   t)
(defvar emacs-ide-lsp-organize-imports      t)
(defvar emacs-ide-lsp-large-file-threshold  100000)
(defvar emacs-ide-lsp-diagnostics-provider  'flycheck)

;; Completion
(defvar emacs-ide-completion-backend  'corfu)
(defvar emacs-ide-completion-delay    0.15)
(defvar emacs-ide-completion-prefix   1)
(defvar emacs-ide-completion-auto     t)
(defvar emacs-ide-completion-fuzzy    t)
(defvar emacs-ide-completion-preview  t)

;; Features
(defvar emacs-ide-feature-dashboard  t)
(defvar emacs-ide-feature-which-key  t)
(defvar emacs-ide-modeline-height    32)

;; Theme
(defvar emacs-ide-theme-auto-switch  nil)

;; Subsystems
(defvar emacs-ide-git-enable      t)
(defvar emacs-ide-project-enable  t)
(defvar emacs-ide-debug-enable    t)

;;;; ── Defaults ────────────────────────────────────────────────────────────────

(defvar emacs-ide-config-defaults
  '((general
     (theme . ef-dark) (font . "JetBrains Mono") (font-size . 11))
    (lsp
     (enable . t) (inlay-hints . t) (idle-delay . 0.3)
     (headerline-breadcrumb . t) (large-file-threshold . 100000)
     (semantic-tokens . t) (lens . t) (sideline . t)
     (signature-help . t) (hover-docs . t)
     (symbol-highlighting . t) (organize-imports . t)
     (diagnostics-provider . flycheck))
    (completion
     (backend . corfu) (delay . 0.15)
     (auto-prefix . 1) (auto . t) (fuzzy-matching . t) (preview . t))
    (features
     (dashboard . t) (which-key . t) (modeline-height . 32))
    (git
     (enable . t) (auto-revert . t) (gutter . t))
    (theme
     (auto-switch . nil) (dark-hour . 19) (light-hour . 7))
    (telemetry (enabled . t))
    (project (enable . t))
    (debug (enable . t))))

;;;; ── Environment ─────────────────────────────────────────────────────────────

(defun emacs-ide-config-detect-environment ()
  (or (getenv "EMACS_ENVIRONMENT")
      (let ((h (system-name)))
        (cond ((string-match-p "work\\|corp" h) "work")
              ((string-match-p "home\\|personal" h) "home")
              (t "default")))))

(setq emacs-ide-config-environment (emacs-ide-config-detect-environment))

;;;; ── Simple & Safe YAML Parser ──────────────────────────────────────────────

(defun emacs-ide-config-parse-value (str &optional as-string)
  "Parse STR into null/nil, t, a number, or otherwise either an
interned symbol (default — several settings, e.g. lsp.diagnostics-provider
and repl.side, are compared with `eq' against a symbol and rely on this)
or the trimmed string itself when AS-STRING is non-nil.  Sequence items
are parsed with AS-STRING so paths and directory names come back as
plain strings, matching what every consumer of a list value expects."
  (let ((s (string-trim (replace-regexp-in-string "[ \t]+#.*$" "" (or str "")))))
    (cond
     ((or (string-empty-p s) (member s '("null" "~"))) nil)
     ((string= s "true") t)
     ((string= s "false") nil)
     ((string-match-p "^-?[0-9]+\\.?[0-9]*$" s) (string-to-number s))
     (as-string s)
     (t (intern s)))))

(defun emacs-ide-config--line-indent (raw-line)
  "Return the number of leading space/tab characters on RAW-LINE."
  (let ((i 0) (len (length raw-line)))
    (while (and (< i len) (memq (aref raw-line i) '(?\s ?\t)))
      (setq i (1+ i)))
    i))

(defun emacs-ide-config--read-lines (file)
  "Return FILE's meaningful content as a list of (INDENT . TEXT),
skipping blank lines and full-line comments.  TEXT is the line with
surrounding whitespace stripped; inline comments on scalar values are
still stripped later, exactly as before, only by `emacs-ide-config-parse-value'."
  (with-temp-buffer
    (insert-file-contents file)
    (let (result)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((raw (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
               (trimmed (string-trim raw)))
          (unless (or (string-empty-p trimmed) (string-prefix-p "#" trimmed))
            (push (cons (emacs-ide-config--line-indent raw) trimmed) result)))
        (forward-line 1))
      (nreverse result))))

(defun emacs-ide-config-parse-yaml (file)
  "Simple, dependency-free parser for config.yml."
  (when (file-exists-p file)
    (let ((lines (emacs-ide-config--read-lines file)))
      (cl-labels
          ((peek ()        (car lines))
           (peek-indent ()  (and lines (caar lines)))
           (advance ()      (pop lines))
           (parse-seq (indent)
             "Consume '- item' lines at exactly INDENT; return a list."
             (let (items)
               (while (and lines (= (peek-indent) indent)
                           (string-prefix-p "- " (cdr (peek))))
                 (push (emacs-ide-config-parse-value
                        (string-trim (substring (cdr (advance)) 2))
                        t)
                       items))
               (nreverse items)))
           (parse-map (min-indent)
             "Consume `key: value'/`key:' lines at indent >= MIN-INDENT,
recursing into nested mappings and sequences; return an alist."
             (let (result)
               (while (and lines (>= (peek-indent) min-indent))
                 (let* ((entry   (advance))
                        (indent  (car entry))
                        (text    (cdr entry)))
                   (when (string-match "^\\([a-z0-9_-]+\\):" text)
                     (let ((key     (intern (match-string 1 text)))
                           (val-str (string-trim
                                     (replace-regexp-in-string
                                      "\\(^\\|[ \t]\\)#.*$" ""
                                      (substring text (match-end 0))))))
                       (push
                        (cons key
                              (cond
                               ;; `key: value' on the same line
                               ((not (string-empty-p val-str))
                                (emacs-ide-config-parse-value val-str))
                               ;; `key:' with nested content below it
                               ((and lines (> (peek-indent) indent))
                                (if (string-prefix-p "- " (cdr (peek)))
                                    (parse-seq (peek-indent))
                                  (parse-map (peek-indent))))
                               ;; `key:' with nothing following at all
                               (t nil)))
                        result)))))
               (nreverse result))))
        (parse-map 0)))))

;;;; ── Apply ──────────────────────────────────────────────────────────────────

(defun emacs-ide-config--set (var alist key default)
  (let ((cell (assoc key alist)))
    (set var (if cell (cdr cell) default))))

(defun emacs-ide-config-apply (cfg)
  (when-let ((g (alist-get 'general cfg)))
    (emacs-ide-config--set 'emacs-ide-theme     g 'theme 'ef-dark)
    (emacs-ide-config--set 'emacs-ide-font      g 'font "JetBrains Mono")
    (emacs-ide-config--set 'emacs-ide-font-size g 'font-size 11))

  (when-let ((l (alist-get 'lsp cfg)))
    (emacs-ide-config--set 'emacs-ide-lsp-enable                l 'enable t)
    (emacs-ide-config--set 'emacs-ide-lsp-enable-inlay-hints    l 'inlay-hints t)
    (emacs-ide-config--set 'emacs-ide-lsp-idle-delay            l 'idle-delay 0.3)
    (emacs-ide-config--set 'emacs-ide-lsp-breadcrumb            l 'headerline-breadcrumb t)
    (emacs-ide-config--set 'emacs-ide-lsp-semantic-tokens       l 'semantic-tokens t)
    (emacs-ide-config--set 'emacs-ide-lsp-lens                  l 'lens t)
    (emacs-ide-config--set 'emacs-ide-lsp-sideline              l 'sideline t)
    (emacs-ide-config--set 'emacs-ide-lsp-signature-help        l 'signature-help t)
    (emacs-ide-config--set 'emacs-ide-lsp-hover-docs            l 'hover-docs t)
    (emacs-ide-config--set 'emacs-ide-lsp-symbol-highlighting   l 'symbol-highlighting t)
    (emacs-ide-config--set 'emacs-ide-lsp-organize-imports      l 'organize-imports t)
    (emacs-ide-config--set 'emacs-ide-lsp-large-file-threshold  l 'large-file-threshold 100000)
    (emacs-ide-config--set 'emacs-ide-lsp-diagnostics-provider  l 'diagnostics-provider 'flycheck))

  (when-let ((c (alist-get 'completion cfg)))
    (emacs-ide-config--set 'emacs-ide-completion-backend c 'backend 'corfu)
    (emacs-ide-config--set 'emacs-ide-completion-delay   c 'delay 0.15)
    (emacs-ide-config--set 'emacs-ide-completion-prefix  c 'auto-prefix 1)
    (emacs-ide-config--set 'emacs-ide-completion-auto    c 'auto t)
    (emacs-ide-config--set 'emacs-ide-completion-fuzzy   c 'fuzzy-matching t)
    (emacs-ide-config--set 'emacs-ide-completion-preview c 'preview t))

  (when-let ((f (alist-get 'features cfg)))
    (emacs-ide-config--set 'emacs-ide-feature-dashboard f 'dashboard t)
    (emacs-ide-config--set 'emacs-ide-feature-which-key f 'which-key t)
    (emacs-ide-config--set 'emacs-ide-modeline-height   f 'modeline-height 32))

  (when-let ((tcfg (alist-get 'theme cfg)))
    (emacs-ide-config--set 'emacs-ide-theme-auto-switch tcfg 'auto-switch nil))

  (when-let ((g (alist-get 'git cfg)))
    (emacs-ide-config--set 'emacs-ide-git-enable g 'enable t))

  (when-let ((tel (alist-get 'telemetry cfg)))
    (emacs-ide-config--set 'emacs-ide-telemetry-enabled tel 'enabled t))

  (when-let ((p (alist-get 'project cfg)))
    (emacs-ide-config--set 'emacs-ide-project-enable p 'enable t))

  (when-let ((perf (alist-get 'performance cfg)))
    (emacs-ide-config--set 'emacs-ide-startup-time-target perf 'startup-time-target 3.0))

  (when-let ((d (alist-get 'debug cfg)))
    (emacs-ide-config--set 'emacs-ide-debug-enable d 'enable t)))

;;;; ── Public accessor ────────────────────────────────────────────────────────

(defun emacs-ide-config-get (section key &optional default)
  "Return KEY's value from SECTION in `emacs-ide-config-data', or DEFAULT.
SECTION and KEY are symbols matching config.yml keys, e.g.:
  (emacs-ide-config-get \\='lsp \\='idle-delay 0.3)"
  (if-let* ((data emacs-ide-config-data)
            (sec  (alist-get section data))
            (cell (assoc key sec)))
      (cdr cell)
    default))

;;;; ── Config show ─────────────────────────────────────────────────────────────

(defun emacs-ide-config-show ()
  "Display active config variable values derived from config.yml."
  (interactive)
  (with-output-to-temp-buffer "*Config Values*"
    (princ (format "=== ACTIVE CONFIG (env: %s) ===\n\n"
                   (or emacs-ide-config-environment "unknown")))
    (princ (format "Config loaded:  %s\n\n"
                   (if emacs-ide-config-loaded-p "yes" "no")))
    (dolist (entry
             `(("General"
                ("theme"              . ,emacs-ide-theme)
                ("font"               . ,emacs-ide-font)
                ("font-size"          . ,emacs-ide-font-size))
               ("LSP"
                ("enable"             . ,emacs-ide-lsp-enable)
                ("inlay-hints"        . ,emacs-ide-lsp-enable-inlay-hints)
                ("idle-delay"         . ,emacs-ide-lsp-idle-delay)
                ("breadcrumb"         . ,emacs-ide-lsp-breadcrumb)
                ("semantic-tokens"    . ,emacs-ide-lsp-semantic-tokens)
                ("lens"               . ,emacs-ide-lsp-lens)
                ("sideline"           . ,emacs-ide-lsp-sideline)
                ("diagnostics"        . ,emacs-ide-lsp-diagnostics-provider)
                ("large-file"         . ,emacs-ide-lsp-large-file-threshold))
               ("Completion"
                ("backend"            . ,emacs-ide-completion-backend)
                ("delay"              . ,emacs-ide-completion-delay)
                ("auto-prefix"        . ,emacs-ide-completion-prefix)
                ("auto"               . ,emacs-ide-completion-auto)
                ("fuzzy"              . ,emacs-ide-completion-fuzzy))
               ("Features"
                ("dashboard"          . ,emacs-ide-feature-dashboard)
                ("which-key"          . ,emacs-ide-feature-which-key)
                ("modeline-height"    . ,emacs-ide-modeline-height))
               ("Subsystems"
                ("git.enable"         . ,emacs-ide-git-enable)
                ("project.enable"     . ,emacs-ide-project-enable)
                ("debug.enable"       . ,emacs-ide-debug-enable)
                ("theme.auto-switch"  . ,emacs-ide-theme-auto-switch))))
      (princ (format "%s:\n" (car entry)))
      (dolist (kv (cdr entry))
        (princ (format "  %-22s %s\n" (car kv) (cdr kv))))
      (princ "\n"))))

;;;; ── Load / Reload ──────────────────────────────────────────────────────────

(defun emacs-ide-config-load ()
  (interactive)
  (setq emacs-ide-config-data
        (or (emacs-ide-config-parse-yaml emacs-ide-config-file)
            emacs-ide-config-defaults))
  (emacs-ide-config-apply emacs-ide-config-data)
  (setq emacs-ide-config-loaded-p t)
  (message "✓ Config loaded (env: %s)" emacs-ide-config-environment))

(defun emacs-ide-config-reload ()
  (interactive)
  (emacs-ide-config-load)
  (run-hooks 'emacs-ide-config-reload-hook))

(defalias 'emacs-ide-reload-config #'emacs-ide-config-reload)

(defun emacs-ide-config-edit ()
  (interactive)
  (find-file emacs-ide-config-file))

(provide 'emacs-ide-config)
;;; emacs-ide-config.el ends here
