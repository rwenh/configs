;;; emacs-ide-config.el --- Lean Configuration System -*- lexical-binding: t -*-
;;; Version: 3.1.0 | Fixes: level-3 YAML, hook declaration, reload
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-config-file (expand-file-name "config.yml" user-emacs-directory))
(defvar emacs-ide-config-data nil)
(defvar emacs-ide-config-environment nil)
(defvar emacs-ide-config-loaded-p nil)
(defvar emacs-ide-config-reload-hook nil "Hook: run after config reload via M-x emacs-ide-config-reload")

(defvar emacs-ide-config-defaults
  '((general
     (theme . ef-dark) (font . "JetBrains Mono") (font-size . 11) (safe-mode . nil))
    (lsp (enable . t) (inlay-hints . t) (large-file-threshold . 100000) (semantic-tokens . t))
    (completion (backend . corfu) (delay . 0.1) (snippet-expansion . t))
    (performance (gc-threshold . 16777216) (startup-time-target . 3.0) (native-comp-jobs . 4))
    (features (dashboard . t) (which-key . t) (beacon . t))
    (security (tls-verify . t) (package-signatures . allow-unsigned))
    (telemetry (enabled . t) (usage-stats . t))))

(defun emacs-ide-config-detect-environment ()
  (or (getenv "EMACS_ENVIRONMENT")
      (let ((hostname (system-name)))
        (cond ((string-match-p "work\\|corp" hostname) "work")
              ((string-match-p "home\\|personal" hostname) "home")
              (t "default")))))

(setq emacs-ide-config-environment (emacs-ide-config-detect-environment))

(defun emacs-ide-config-parse-value (str)
  "Parse STR to Elisp type: null/bool/number/symbol/string"
  (let ((s (string-trim (replace-regexp-in-string "[ \t]+#.*$" "" str))))
    (cond
     ((or (string-empty-p s) (string= s "null") (string= s "~")) nil)
     ((string= s "true") t)
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
  "Parse YAML with 3-level nesting (indent 0/2/4/6)"
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((data '()) (sec nil) (sub nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
                 (indent (- (length line) (length (string-trim-left line))))
                 (trim (string-trim line)))
            (unless (or (string-empty-p trim) (string-prefix-p "#" trim))
              (cond
               ;; Level 0: section
               ((and (= indent 0) (string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*$" trim))
                (setq sec (intern (match-string 1 trim)) sub nil)
                (unless (assoc sec data) (push (cons sec '()) data)))
               ;; Level 2: subsection or key-value
               ((and (= indent 2) sec)
                (cond
                 ((string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*$" trim)
                  (setq sub (intern (match-string 1 trim)))
                  (when-let ((e (assoc sec data)))
                    (unless (assoc sub (cdr e))
                      (setcdr e (append (cdr e) (list (cons sub '())))))))
                 ((string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*\\(.*\\)$" trim)
                  (let* ((k (intern (match-string 1 trim)))
                         (v (emacs-ide-config-parse-value (match-string 2 trim)))
                         (e (assoc sec data)))
                    (when e
                      (if sub (setq sub nil))
                      (setcdr e (append (cdr e) (list (cons k v)))))))))
               ;; Level 4: nested key-value
               ((and (= indent 4) sec sub)
                (when (string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*\\(.*\\)$" trim)
                  (let* ((k (intern (match-string 1 trim)))
                         (v (emacs-ide-config-parse-value (match-string 2 trim)))
                         (se (assoc sec data))
                         (sbe (and se (assoc sub (cdr se)))))
                    (when sbe (setcdr sbe (append (cdr sbe) (list (cons k v))))))))
               ;; Level 6: deep list items (fixed v3.0.4)
               ((and (= indent 6) sec sub)
                (when (string-prefix-p "- " trim)
                  (let* ((v (emacs-ide-config-parse-value (string-trim (substring trim 2))))
                         (se (assoc sec data))
                         (sbe (and se (assoc sub (cdr se)))))
                    (when sbe (setcdr sbe (append (cdr sbe) (list v)))))))))
            (forward-line 1)))
        (nreverse data)))))

(defun emacs-ide-config-load ()
  (interactive)
  (condition-case err
      (progn
        (setq emacs-ide-config-data
              (or (and (file-exists-p emacs-ide-config-file)
                       (emacs-ide-config-parse-yaml emacs-ide-config-file))
                  emacs-ide-config-defaults))
        (emacs-ide-config-apply emacs-ide-config-data)
        (setq emacs-ide-config-loaded-p t)
        (run-hooks 'emacs-ide-config-reload-hook)
        (message "✓ Config loaded (env: %s)" emacs-ide-config-environment)
        t)
    (error
     (warn "⚠ Config load failed: %s — using defaults" (error-message-string err))
     (setq emacs-ide-config-data emacs-ide-config-defaults emacs-ide-config-loaded-p t)
     nil)))

(defun emacs-ide-config-apply (cfg)
  "Apply CFG plist to variables (inline — no cl-flet overhead)"
  (let-alist cfg
    (when .general
      (set-with-default 'emacs-ide-theme (alist-get 'theme .general) 'ef-dark)
      (set-with-default 'emacs-ide-font (alist-get 'font .general) "JetBrains Mono")
      (set-with-default 'emacs-ide-font-size (alist-get 'font-size .general) 11))
    (when .lsp
      (set-with-default 'emacs-ide-lsp-enable (alist-get 'enable .lsp) t)
      (set-with-default 'emacs-ide-lsp-enable-inlay-hints (alist-get 'inlay-hints .lsp) t))
    (when .completion
      (set-with-default 'emacs-ide-completion-backend (alist-get 'backend .completion) 'corfu)
      (set-with-default 'emacs-ide-completion-delay (alist-get 'delay .completion) 0.1))
    (when .performance
      (set-with-default 'gc-cons-threshold (alist-get 'gc-threshold .performance) 16777216))
    (when .git
      (set-with-default 'emacs-ide-git-enable (alist-get 'enable .git) t))
    (when .telemetry
      (set-with-default 'emacs-ide-telemetry-enabled (alist-get 'enabled .telemetry) t))))

(defun set-with-default (var val default)
  "Set VAR to VAL if not nil, else DEFAULT"
  (set var (or val default)))

(defun emacs-ide-config-get (section key &optional default)
  "Get value for SECTION:KEY or DEFAULT"
  (let ((s (cdr (assoc section emacs-ide-config-data))))
    (or (cdr (assoc key s)) default)))

(defun emacs-ide-config-reload ()
  (interactive)
  (emacs-ide-config-load)
  (message "✓ Config reloaded"))

(provide 'emacs-ide-config)
;;; emacs-ide-config.el ends here
