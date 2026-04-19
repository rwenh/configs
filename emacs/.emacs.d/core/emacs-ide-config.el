;;; emacs-ide-config.el --- Lean Configuration System -*- lexical-binding: t -*-
;;; Version: 3.2.0 | FIX: config-apply uses alist-get not let-alist, reload hook
;;;           fires only on reload (not initial load), project/debug/feature
;;;           enable variables now set, nth file-size fixed.
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-config-file (expand-file-name "config.yml" user-emacs-directory))
(defvar emacs-ide-config-data nil)
(defvar emacs-ide-config-environment nil)
(defvar emacs-ide-config-loaded-p nil)

;; Declared before everything so modules can safely add-hook at load time.
(defvar emacs-ide-config-reload-hook nil
  "Hook run only after an explicit config reload (M-x emacs-ide-config-reload).
NOT run on the initial startup load. All modules that react to config
changes at runtime should register handlers here.")

;; Cache / pre-warm invalidation on explicit reload only.
(add-hook 'emacs-ide-config-reload-hook
          (lambda ()
            (when (boundp 'emacs-ide-dev--config-languages)
              (setq emacs-ide-dev--config-languages nil))
            (when (and (boundp 'emacs-ide-detect--pre-warmed)
                       (hash-table-p emacs-ide-detect--pre-warmed))
              (clrhash emacs-ide-detect--pre-warmed))))

(defvar emacs-ide-config-defaults
  '((general
     (theme . ef-dark) (font . "JetBrains Mono") (font-size . 11) (safe-mode . nil))
    (lsp    (enable . t) (inlay-hints . t) (large-file-threshold . 100000) (semantic-tokens . t))
    (completion (backend . corfu) (delay . 0.1) (snippet-expansion . t))
    (performance (gc-threshold . 16777216) (startup-time-target . 3.0) (native-comp-jobs . 4))
    (features (dashboard . t) (which-key . t) (beacon . t))
    (security (tls-verify . t) (package-signatures . allow-unsigned))
    (telemetry (enabled . t) (usage-stats . t))
    (project  (enable . t))
    (debug    (enable . t))))

(defun emacs-ide-config-detect-environment ()
  (or (getenv "EMACS_ENVIRONMENT")
      (let ((hostname (system-name)))
        (cond ((string-match-p "work\\|corp"     hostname) "work")
              ((string-match-p "home\\|personal" hostname) "home")
              (t "default")))))

(setq emacs-ide-config-environment (emacs-ide-config-detect-environment))

(defun emacs-ide-config-parse-value (str)
  "Parse STR to Elisp type: null / bool / number / symbol / string."
  (let ((s (string-trim (replace-regexp-in-string "[ \t]+#.*$" "" str))))
    (cond
     ((or (string-empty-p s) (string= s "null") (string= s "~")) nil)
     ((string= s "true")  t)
     ((string= s "false") nil)
     ((string-match-p "^-?[0-9]+\\(\\.[0-9]+\\)?$" s) (string-to-number s))
     ((member s '("allow-unsigned" "high" "medium" "low" "errors" "warnings" "always"
                  "bottom" "right" "left" "top" "created" "alphabetic" "deferred"))
      (intern s))
     ((and (string-match-p "^[a-z][a-z0-9_-]*$" s)
           (not (member s '("black" "prettier" "rustfmt" "gofmt" "rg" "ripgrep" "grep"))))
      (intern s))
     (t (replace-regexp-in-string "^['\"]\\|['\"]$" "" s)))))

(defun emacs-ide-config-parse-yaml (file)
  "Parse YAML with 3-level nesting (indents 0 / 2 / 4 / 6)."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((data '()) (sec nil) (sub nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line   (buffer-substring (line-beginning-position) (line-end-position)))
                 (indent (- (length line) (length (string-trim-left line))))
                 (trim   (string-trim line)))
            (unless (or (string-empty-p trim) (string-prefix-p "#" trim))
              (cond
               ;; Level 0 — top-level section header
               ((and (= indent 0)
                     (string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*$" trim))
                (setq sec (intern (match-string 1 trim))
                      sub nil)
                (unless (assoc sec data)
                  (push (cons sec '()) data)))

               ;; Level 2 — subsection header OR flat key-value
               ((and (= indent 2) sec)
                (cond
                 ;; Subsection header: "key:"
                 ((string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*$" trim)
                  (setq sub (intern (match-string 1 trim)))
                  (when-let ((e (assoc sec data)))
                    (unless (assoc sub (cdr e))
                      (setcdr e (append (cdr e) (list (cons sub '())))))))
                 ;; Flat key: "key: value"  — resets sub so indent-4 lines
                 ;; that belong to a PREVIOUS subsection are not misattributed.
                 ((string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*\\(.*\\)$" trim)
                  (let* ((k (intern (match-string 1 trim)))
                         (v (emacs-ide-config-parse-value (match-string 2 trim)))
                         (e (assoc sec data)))
                    (when e
                      (setq sub nil)          ;; explicit: flat kv ends any subsection
                      (setcdr e (append (cdr e) (list (cons k v)))))))))

               ;; Level 4 — nested key-value under a subsection
               ((and (= indent 4) sec sub)
                (when (string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*\\(.*\\)$" trim)
                  (let* ((k   (intern (match-string 1 trim)))
                         (v   (emacs-ide-config-parse-value (match-string 2 trim)))
                         (se  (assoc sec data))
                         (sbe (and se (assoc sub (cdr se)))))
                    (when sbe
                      (setcdr sbe (append (cdr sbe) (list (cons k v))))))))

               ;; Level 6 — list items under a subsection key
               ((and (= indent 6) sec sub)
                (when (string-prefix-p "- " trim)
                  (let* ((v   (emacs-ide-config-parse-value
                               (string-trim (substring trim 2))))
                         (se  (assoc sec data))
                         (sbe (and se (assoc sub (cdr se)))))
                    (when sbe
                      (setcdr sbe (append (cdr sbe) (list v))))))))))
          (forward-line 1))
        (nreverse data)))))

;;; ─── Variable declarations (before apply) ───────────────────────────────────
;; Declare all variables that emacs-ide-config-apply sets so other modules can
;; (boundp ...) / (bound-and-true-p ...) safely even before config loads.

(defvar emacs-ide-theme              'ef-dark)
(defvar emacs-ide-font               "JetBrains Mono")
(defvar emacs-ide-font-size          11)
(defvar emacs-ide-lsp-enable         t)
(defvar emacs-ide-lsp-enable-inlay-hints t)
(defvar emacs-ide-completion-backend 'corfu)
(defvar emacs-ide-completion-delay   0.1)
(defvar emacs-ide-git-enable         t)
(defvar emacs-ide-telemetry-enabled  t)
(defvar emacs-ide-project-enable     t)   ;; guards tools-project.el
(defvar emacs-ide-debug-enable       t)   ;; guards debug-core.el
(defvar emacs-ide-feature-dashboard  t)   ;; guards ui-dashboard.el

;;; ─── Apply ──────────────────────────────────────────────────────────────────

(defun emacs-ide-config-apply (cfg)
  "Apply CFG alist to well-known IDE variables.
Uses alist-get throughout — avoids the let-alist misuse that was
silently misbehaving with nested alist structures."
  (when-let ((general (alist-get 'general cfg)))
    (set-with-default 'emacs-ide-theme
                      (alist-get 'theme    general) 'ef-dark)
    (set-with-default 'emacs-ide-font
                      (alist-get 'font     general) "JetBrains Mono")
    (set-with-default 'emacs-ide-font-size
                      (alist-get 'font-size general) 11))

  (when-let ((lsp (alist-get 'lsp cfg)))
    (set-with-default 'emacs-ide-lsp-enable
                      (alist-get 'enable      lsp) t)
    (set-with-default 'emacs-ide-lsp-enable-inlay-hints
                      (alist-get 'inlay-hints lsp) t))

  (when-let ((completion (alist-get 'completion cfg)))
    (set-with-default 'emacs-ide-completion-backend
                      (alist-get 'backend completion) 'corfu)
    (set-with-default 'emacs-ide-completion-delay
                      (alist-get 'delay   completion) 0.1))

  (when-let ((performance (alist-get 'performance cfg)))
    (set-with-default 'gc-cons-threshold
                      (alist-get 'gc-threshold performance) 16777216))

  (when-let ((git (alist-get 'git cfg)))
    (set-with-default 'emacs-ide-git-enable
                      (alist-get 'enable git) t))

  (when-let ((telemetry (alist-get 'telemetry cfg)))
    (set-with-default 'emacs-ide-telemetry-enabled
                      (alist-get 'enabled telemetry) t))

  ;; Project section — sets the guard variable used by tools-project.el
  (when-let ((project (alist-get 'project cfg)))
    (set-with-default 'emacs-ide-project-enable
                      (alist-get 'enable project) t))

  ;; Debug section — sets the guard variable used by debug-core.el
  (when-let ((debug (alist-get 'debug cfg)))
    (set-with-default 'emacs-ide-debug-enable
                      (alist-get 'enable debug) t))

  ;; Features section — dashboard guard for ui-dashboard.el
  (when-let ((features (alist-get 'features cfg)))
    (set-with-default 'emacs-ide-feature-dashboard
                      (alist-get 'dashboard features) t)))

(defun set-with-default (var val default)
  "Set VAR to VAL when VAL is non-nil, otherwise set it to DEFAULT."
  (set var (if (not (null val)) val default)))

;;; ─── Load / reload ──────────────────────────────────────────────────────────

(defun emacs-ide-config-load ()
  "Load config.yml and apply values. Does NOT fire the reload hook.
The reload hook is intentionally reserved for explicit reloads only so that
modules registered on it do not run during the initial startup sequence
before those modules have themselves finished loading."
  (interactive)
  (condition-case err
      (progn
        (setq emacs-ide-config-data
              (or (and (file-exists-p emacs-ide-config-file)
                       (emacs-ide-config-parse-yaml emacs-ide-config-file))
                  emacs-ide-config-defaults))
        (emacs-ide-config-apply emacs-ide-config-data)
        (setq emacs-ide-config-loaded-p t)
        (message "✓ Config loaded (env: %s)" emacs-ide-config-environment)
        t)
    (error
     (warn "⚠ Config load failed: %s — using defaults" (error-message-string err))
     (setq emacs-ide-config-data  emacs-ide-config-defaults
           emacs-ide-config-loaded-p t)
     nil)))

(defun emacs-ide-config-reload ()
  "Reload config.yml, re-apply values, then fire `emacs-ide-config-reload-hook'.
This is the only code path that fires the hook."
  (interactive)
  (when (emacs-ide-config-load)
    (run-hooks 'emacs-ide-config-reload-hook))
  (message "✓ Config reloaded (cache cleared, hooks fired)"))

(defun emacs-ide-config-get (section key &optional default)
  "Return the value of KEY inside SECTION from the loaded config, or DEFAULT."
  (let ((s (cdr (assoc section emacs-ide-config-data))))
    (if (and s (assoc key s))
        (cdr (assoc key s))
      default)))

(defun emacs-ide-config-show ()
  "Display all currently active configuration values."
  (interactive)
  (with-output-to-temp-buffer "*IDE Config*"
    (princ "=== EMACS IDE ACTIVE CONFIG ===\n\n")
    (princ (format "File:        %s\n" emacs-ide-config-file))
    (princ (format "Environment: %s\n" emacs-ide-config-environment))
    (princ (format "Loaded:      %s\n\n" (if emacs-ide-config-loaded-p "yes" "no")))
    (princ "Key Variables:\n")
    (dolist (entry `(("emacs-ide-theme"              . ,emacs-ide-theme)
                     ("emacs-ide-font"               . ,emacs-ide-font)
                     ("emacs-ide-font-size"           . ,emacs-ide-font-size)
                     ("emacs-ide-lsp-enable"          . ,emacs-ide-lsp-enable)
                     ("emacs-ide-completion-backend"  . ,emacs-ide-completion-backend)
                     ("emacs-ide-completion-delay"    . ,emacs-ide-completion-delay)
                     ("emacs-ide-project-enable"      . ,emacs-ide-project-enable)
                     ("emacs-ide-debug-enable"        . ,emacs-ide-debug-enable)
                     ("emacs-ide-feature-dashboard"   . ,emacs-ide-feature-dashboard)
                     ("gc-cons-threshold"             . ,gc-cons-threshold)))
      (princ (format "  %-36s %s\n" (car entry) (cdr entry))))))

(defun emacs-ide-config-edit ()
  "Open config.yml for editing."
  (interactive)
  (find-file emacs-ide-config-file))

(provide 'emacs-ide-config)
;;; emacs-ide-config.el ends here
