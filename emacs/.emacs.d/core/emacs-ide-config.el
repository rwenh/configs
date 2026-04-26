;;; emacs-ide-config.el --- Lean Configuration System -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;; Code:

(require 'cl-lib)

;;;; ── Public variables ────────────────────────────────────────────────────────

(defvar emacs-ide-config-file
  (expand-file-name "config.yml" user-emacs-directory)
  "Path to the YAML configuration file.")

(defvar emacs-ide-config-data nil
  "Parsed configuration alist, populated by `emacs-ide-config-load'.")

(defvar emacs-ide-config-environment nil
  "Current environment string: \"work\", \"home\", or \"default\".")

(defvar emacs-ide-config-loaded-p nil
  "Non-nil after a successful `emacs-ide-config-load'.")

;; Declared here so every module can safely `add-hook' at load time, even
;; before this file finishes evaluating.
(defvar emacs-ide-config-reload-hook nil
  "Hook run ONLY after an explicit `emacs-ide-config-reload'.
NOT fired on the initial startup load.  All modules that need to react to
runtime config changes should register handlers here.")

;;;; ── Reload-hook: built-in cache invalidation ───────────────────────────────

(add-hook 'emacs-ide-config-reload-hook
          (lambda ()
            ;; core-dev language cache
            (when (boundp 'emacs-ide-dev--config-languages)
              (setq emacs-ide-dev--config-languages nil))
            ;; project-detect pre-warm cache
            (when (and (boundp 'emacs-ide-detect--pre-warmed)
                       (hash-table-p emacs-ide-detect--pre-warmed))
              (clrhash emacs-ide-detect--pre-warmed))))

;;;; ── Defaults ────────────────────────────────────────────────────────────────

(defvar emacs-ide-config-defaults
  '((general
     (theme . ef-dark) (font . "JetBrains Mono") (font-size . 11)
     (safe-mode . nil) (show-dashboard . t) (restore-session . nil))

    (lsp
     (enable . t) (inlay-hints . t) (idle-delay . 0.3)
     (large-file-threshold . 100000) (semantic-tokens . t)
     (lens . t) (sideline . t) (breadcrumb . t))

    (completion
     (backend . corfu) (delay . 0.15) (snippet-expansion . t)
     (auto-prefix . 1) (popup-height . 16))

    (performance
     (gc-threshold . 16777216) (startup-time-target . 3.0)
     (native-comp-jobs . 4) (read-process-output-max . 4194304))

    (features
     (dashboard . t) (which-key . t) (beacon . t)
     (line-numbers . t) (relative-line-numbers . nil)
     (modeline . doom-modeline) (modeline-height . 32))

    (security
     (tls-verify . t) (package-signatures . allow-unsigned)
     (network-security-level . high))

    ;; NOTE: uses `enabled' (with -d) for backward compat; both spellings
    ;; are accepted in emacs-ide-config-apply.
    (telemetry (enabled . t) (usage-stats . t))

    (project  (enable . t) (caching . t) (indexing . alien))

    (debug    (enable . t))

    ;; FIX #28: sections previously absent from defaults
    (git
     (enable . t) (auto-revert . t) (gutter . t)
     (gutter-update-interval . 0.3) (fill-column . 72)
     (summary-max-length . 50))

    (repl
     (window-height . 0.35) (side . bottom) (auto-focus . t))

    (terminal
     (enable . t) (max-scrollback . 100000)
     (timer-delay . 0.01) (kill-buffer-on-exit . t))

    (theme
     (auto-switch . nil) (dark-hour . 19) (light-hour . 7))

    (workspace
     (enable . t) (auto-switch . t) (save-on-exit . t)
     (state-file . "var/persp-state")))
  "Fallback configuration used when config.yml is absent or unreadable.")

;;;; ── Environment detection ───────────────────────────────────────────────────

(defun emacs-ide-config-detect-environment ()
  "Detect current environment from $EMACS_ENVIRONMENT or hostname."
  (or (getenv "EMACS_ENVIRONMENT")
      (let ((hostname (system-name)))
        (cond
         ((string-match-p "work\\|corp"     hostname) "work")
         ((string-match-p "home\\|personal" hostname) "home")
         (t "default")))))

(setq emacs-ide-config-environment (emacs-ide-config-detect-environment))

;;;; ── YAML value parser ───────────────────────────────────────────────────────

(defun emacs-ide-config-parse-value (str)
  "Parse STR to an Elisp value: nil / t / number / symbol / string.

Conversion rules (in order):
  null / ~ / empty  → nil
  true              → t
  false             → nil
  integer / float   → number
  known enum words  → interned symbol
  simple identifier → interned symbol (unless in exclusion list)
  anything else     → string (single/double quotes stripped)"
  (let ((s (string-trim
             (replace-regexp-in-string "[ \t]+#.*$" "" str))))
    (cond
     ;; Null / empty
     ((or (string-empty-p s)
          (string= s "null")
          (string= s "~"))
      nil)
     ;; Booleans
     ((string= s "true")  t)
     ((string= s "false") nil)
     ;; Numbers (integer and float)
     ((string-match-p "^-?[0-9]+\\(\\.[0-9]+\\)?$" s)
      (string-to-number s))
     ;; Known enumeration values that should always be symbols
     ((member s '("allow-unsigned" "require-signatures"
                  "high" "medium" "low"
                  "errors" "warnings" "always" "never"
                  "bottom" "right" "left" "top"
                  "created" "alphabetic" "deferred"
                  "corfu" "company"
                  "doom-modeline" "powerline"
                  "alien" "native" "hybrid"
                  "ripgrep" "git-grep"))
      (intern s))
     ;; Simple lowercase identifiers — intern unless in the exclusion list.
     ((and (string-match-p "^[a-z][a-z0-9_-]*$" s)
           (not (member s '("black"     "prettier"  "rustfmt"
                            "gofmt"     "rg"        "grep"
                            "shfmt"     "stylua"    "ktlint"
                            "ormolu"    "cljfmt"    "rubocop"
                            "autopep8"  "isort"     "ruff"))))
      (intern s))
     ;; Everything else: plain string, strip surrounding quotes
     (t
      (replace-regexp-in-string "^['\"]\\|['\"]$" "" s)))))

;;;; ── YAML parser ────────────────────────────────────────────────────────────
;;
;; Supports 4 indent levels (0 / 2 / 4 / 6) and three tracking variables:
;;
;;   sec    — current top-level section  (indent 0 header)
;;   sub    — current sub-section        (indent 2 empty-value key)
;;   subsub — current nested sub-section (indent 4 empty-value key)
;;
;; Resulting alist structure:
;;   ((sec (sub (subsub val …) (key . val) …) (key . val) …) …)
;;
;; List items (`- value') are supported at indent 4 (appended to sub's list)
;; and at indent 6 (appended to subsub's list).  This covers all real cases
;; in config.yml:
;;   workspace.defaults          — indent-4 list under `defaults'
;;   security.auth-sources       — indent-4 list under `auth-sources'
;;   project.search-paths        — indent-4 list under `search-paths'
;;   project.globally-ignored-*  — indent-4 list
;;   environments.*.org-agenda-files — indent-6 list under `org-agenda-files'

(defun emacs-ide-config-parse-yaml (file)
  "Parse FILE (a YAML path) into a nested alist.  Returns nil if FILE is absent."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((data   '())
            (sec    nil)
            (sub    nil)
            (subsub nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((raw    (buffer-substring (line-beginning-position)
                                           (line-end-position)))
                 (indent (- (length raw) (length (string-trim-left raw))))
                 (trim   (string-trim raw)))

            (unless (or (string-empty-p trim)
                        (string-prefix-p "#" trim))
              (cond

               ;; ── Indent 0: top-level section header ──────────────────────
               ;; Only matches "word:" with nothing after the colon.
               ((and (= indent 0)
                     (string-match
                      "^\\([a-z][a-z0-9_-]*\\):[ \t]*$" trim))
                (setq sec    (intern (match-string 1 trim))
                      sub    nil
                      subsub nil)
                (unless (assoc sec data)
                  (push (cons sec '()) data)))

               ;; ── Indent 2: subsection header OR flat key-value ────────────
               ((and (= indent 2) sec)
                (cond
                 ;; "key:" with nothing after → new subsection
                 ((string-match
                   "^\\([a-z][a-z0-9_-]*\\):[ \t]*$" trim)
                  (setq sub    (intern (match-string 1 trim))
                        subsub nil)
                  (when-let ((e (assoc sec data)))
                    (unless (assoc sub (cdr e))
                      (setcdr e (append (cdr e)
                                        (list (cons sub '())))))))

                 ;; "key: value" — flat entry under sec; ends any sub-context
                 ((string-match
                   "^\\([a-z][a-z0-9_-]*\\):[ \t]+\\(.*\\)$" trim)
                  (let* ((k (intern (match-string 1 trim)))
                         (v (emacs-ide-config-parse-value
                             (match-string 2 trim)))
                         (e (assoc sec data)))
                    (when e
                      (setq sub    nil
                            subsub nil)
                      (setcdr e (append (cdr e)
                                        (list (cons k v)))))))))

               ;; ── Indent 4: under a sub-section ───────────────────────────
               ((and (= indent 4) sec sub)
                (cond

                 ;; FIX #1A ── "- value": list item appended to sub's list.
                 ;; This was the missing branch that caused workspace.defaults,
                 ;; security.auth-sources, and project.search-paths to be
                 ;; silently dropped on every startup.
                 ((string-prefix-p "- " trim)
                  (let* ((v   (emacs-ide-config-parse-value
                               (string-trim (substring trim 2))))
                         (se  (assoc sec data))
                         (sbe (and se (assoc sub (cdr se)))))
                    (when sbe
                      (setcdr sbe (append (cdr sbe) (list v))))))

                 ;; "key:" with nothing after → 3rd-level subsection (subsub).
                 ;; FIX #1B: track subsub so indent-6 items land correctly.
                 ((string-match
                   "^\\([a-z][a-z0-9_-]*\\):[ \t]*$" trim)
                  (setq subsub (intern (match-string 1 trim)))
                  (let* ((se  (assoc sec data))
                         (sbe (and se (assoc sub (cdr se)))))
                    (when sbe
                      (unless (assoc subsub (cdr sbe))
                        (setcdr sbe
                                (append (cdr sbe)
                                        (list (cons subsub '()))))))))

                 ;; "key: value" — entry under sub; resets subsub.
                 ((string-match
                   "^\\([a-z][a-z0-9_-]*\\):[ \t]+\\(.*\\)$" trim)
                  (let* ((k   (intern (match-string 1 trim)))
                         (v   (emacs-ide-config-parse-value
                               (match-string 2 trim)))
                         (se  (assoc sec data))
                         (sbe (and se (assoc sub (cdr se)))))
                    (when sbe
                      (setq subsub nil)
                      (setcdr sbe (append (cdr sbe)
                                          (list (cons k v)))))))))

               ;; ── Indent 6: under subsub or as list items ──────────────────
               ((and (= indent 6) sec sub)
                (cond

                 ;; FIX #1B ── "- value" under subsub.
                 ;; Old code appended directly to sub, orphaning the values.
                 ;; Now resolves the correct subsub alist entry.
                 ((and subsub (string-prefix-p "- " trim))
                  (let* ((v    (emacs-ide-config-parse-value
                                (string-trim (substring trim 2))))
                         (se   (assoc sec data))
                         (sbe  (and se (assoc sub (cdr se))))
                         (ssbe (and sbe (assoc subsub (cdr sbe)))))
                    (when ssbe
                      (setcdr ssbe (append (cdr ssbe) (list v))))))

                 ;; "key: value" under subsub.
                 ((and subsub
                       (string-match
                        "^\\([a-z][a-z0-9_-]*\\):[ \t]+\\(.*\\)$" trim))
                  (let* ((k    (intern (match-string 1 trim)))
                         (v    (emacs-ide-config-parse-value
                                (match-string 2 trim)))
                         (se   (assoc sec data))
                         (sbe  (and se (assoc sub (cdr se))))
                         (ssbe (and sbe (assoc subsub (cdr sbe)))))
                    (when ssbe
                      (setcdr ssbe
                              (append (cdr ssbe)
                                      (list (cons k v)))))))))))

          (forward-line 1))
        (nreverse data)))))

;;;; ── Variable declarations ───────────────────────────────────────────────────

;; General
(defvar emacs-ide-theme              'ef-dark)
(defvar emacs-ide-font               "JetBrains Mono")
(defvar emacs-ide-font-size          11)

;; LSP
(defvar emacs-ide-lsp-enable              t)
(defvar emacs-ide-lsp-enable-inlay-hints  t)
(defvar emacs-ide-lsp-idle-delay          0.3)
(defvar emacs-ide-lsp-semantic-tokens     t)
(defvar emacs-ide-lsp-lens                t)
(defvar emacs-ide-lsp-sideline            t)
(defvar emacs-ide-lsp-breadcrumb          t)

;; Completion
(defvar emacs-ide-completion-backend  'corfu)
(defvar emacs-ide-completion-delay    0.15)
(defvar emacs-ide-completion-prefix   1)

;; Features
(defvar emacs-ide-feature-dashboard   t)
(defvar emacs-ide-feature-which-key   t)
(defvar emacs-ide-modeline-height     32)

;; Theme auto-switch
(defvar emacs-ide-theme-auto-switch    nil)
(defvar emacs-ide-theme-auto-dark-hour  19)
(defvar emacs-ide-theme-auto-light-hour  7)

;; Git
(defvar emacs-ide-git-enable          t)
(defvar emacs-ide-git-gutter          t)
(defvar emacs-ide-git-auto-revert     t)

;; Telemetry
(defvar emacs-ide-telemetry-enabled   t)

;; Project / debug guards (read by tools-project.el, debug-core.el)
(defvar emacs-ide-project-enable      t)
(defvar emacs-ide-debug-enable        t)

;;;; ── Core helper: emacs-ide-config--set ─────────────────────────────────────

(defun emacs-ide-config--set (var alist key default)
  "Set VAR from KEY in ALIST, using DEFAULT only when KEY is absent.
Unlike the old set-with-default, this correctly propagates nil/false
values from config.yml instead of silently falling back to DEFAULT."
  (let ((cell (assoc key alist)))
    (set var (if cell (cdr cell) default))))

(defun set-with-default (var val default)
  "Deprecated.  Use `emacs-ide-config--set' instead.
Cannot correctly express nil/false values; kept for compatibility only."
  (set var (if (not (null val)) val default)))

;;;; ── Apply ──────────────────────────────────────────────────────────────────

(defun emacs-ide-config-apply (cfg)
  "Apply parsed config alist CFG to all well-known IDE variables.

Uses `emacs-ide-config--set' throughout, which correctly handles
nil/false values from config.yml (FIX #31).  Previously this used
set-with-default which could not represent false, so lsp.enable: false,
git.enable: false, etc. had no effect."

  ;; ── general ──────────────────────────────────────────────────────────────
  (when-let ((general (alist-get 'general cfg)))
    (emacs-ide-config--set 'emacs-ide-theme
                            general 'theme    'ef-dark)
    (emacs-ide-config--set 'emacs-ide-font
                            general 'font     "JetBrains Mono")
    (emacs-ide-config--set 'emacs-ide-font-size
                            general 'font-size 11))

  ;; ── lsp ──────────────────────────────────────────────────────────────────
  (when-let ((lsp (alist-get 'lsp cfg)))
    (emacs-ide-config--set 'emacs-ide-lsp-enable
                            lsp 'enable       t)
    (emacs-ide-config--set 'emacs-ide-lsp-enable-inlay-hints
                            lsp 'inlay-hints  t)
    ;; FIX #6 (partial): idle-delay, semantic-tokens, lens, sideline,
    ;; breadcrumb were parsed but never applied before this version.
    (emacs-ide-config--set 'emacs-ide-lsp-idle-delay
                            lsp 'idle-delay   0.3)
    (emacs-ide-config--set 'emacs-ide-lsp-semantic-tokens
                            lsp 'semantic-tokens t)
    (emacs-ide-config--set 'emacs-ide-lsp-lens
                            lsp 'lens         t)
    (emacs-ide-config--set 'emacs-ide-lsp-sideline
                            lsp 'sideline     t)
    (emacs-ide-config--set 'emacs-ide-lsp-breadcrumb
                            lsp 'breadcrumb   t))

  ;; ── completion ───────────────────────────────────────────────────────────
  (when-let ((completion (alist-get 'completion cfg)))
    (emacs-ide-config--set 'emacs-ide-completion-backend
                            completion 'backend     'corfu)
    (emacs-ide-config--set 'emacs-ide-completion-delay
                            completion 'delay       0.15)
    (emacs-ide-config--set 'emacs-ide-completion-prefix
                            completion 'auto-prefix 1))

  ;; ── performance ──────────────────────────────────────────────────────────
  (when-let ((perf (alist-get 'performance cfg)))
    (emacs-ide-config--set 'gc-cons-threshold
                            perf 'gc-threshold 16777216))

  ;; ── features ─────────────────────────────────────────────────────────────
  ;; FIX #6 (partial): which-key and modeline-height added.
  (when-let ((features (alist-get 'features cfg)))
    (emacs-ide-config--set 'emacs-ide-feature-dashboard
                            features 'dashboard      t)
    (emacs-ide-config--set 'emacs-ide-feature-which-key
                            features 'which-key      t)
    (emacs-ide-config--set 'emacs-ide-modeline-height
                            features 'modeline-height 32))

  ;; ── theme auto-switch ────────────────────────────────────────────────────
  (when-let ((theme (alist-get 'theme cfg)))
    (emacs-ide-config--set 'emacs-ide-theme-auto-switch
                            theme 'auto-switch nil)
    (emacs-ide-config--set 'emacs-ide-theme-auto-dark-hour
                            theme 'dark-hour   19)
    (emacs-ide-config--set 'emacs-ide-theme-auto-light-hour
                            theme 'light-hour  7))

  ;; ── git ──────────────────────────────────────────────────────────────────
  ;; FIX #6 (partial): gutter and auto-revert added.
  (when-let ((git (alist-get 'git cfg)))
    (emacs-ide-config--set 'emacs-ide-git-enable
                            git 'enable      t)
    (emacs-ide-config--set 'emacs-ide-git-gutter
                            git 'gutter      t)
    (emacs-ide-config--set 'emacs-ide-git-auto-revert
                            git 'auto-revert t))

  ;; ── telemetry ────────────────────────────────────────────────────────────
  ;; FIX #45: config.yml uses `enabled' (with -d) while every other section
  ;; uses `enable'.  Both are accepted here for backward compatibility.
  ;; `enabled' takes precedence when both are present.
  (when-let ((telemetry (alist-get 'telemetry cfg)))
    (let* ((with-d    (assoc 'enabled telemetry))  ;; enabled (legacy)
           (without-d (assoc 'enable  telemetry))  ;; enable  (preferred)
           (cell      (or with-d without-d)))
      (set 'emacs-ide-telemetry-enabled
           (if cell (cdr cell) t))))

  ;; ── project ──────────────────────────────────────────────────────────────
  (when-let ((project (alist-get 'project cfg)))
    (emacs-ide-config--set 'emacs-ide-project-enable
                            project 'enable t))

  ;; ── debug ────────────────────────────────────────────────────────────────
  (when-let ((debug (alist-get 'debug cfg)))
    (emacs-ide-config--set 'emacs-ide-debug-enable
                            debug 'enable t)))

;;;; ── Load / reload ──────────────────────────────────────────────────────────

(defun emacs-ide-config-load ()
  "Parse config.yml and apply all values to IDE variables.
Does NOT run `emacs-ide-config-reload-hook' — that hook is reserved for
explicit user-initiated reloads so that modules registered on it do not
fire during the initial startup sequence before they have finished loading."
  (interactive)
  (condition-case err
      (progn
        (setq emacs-ide-config-data
              (or (and (file-exists-p emacs-ide-config-file)
                       (emacs-ide-config-parse-yaml emacs-ide-config-file))
                  emacs-ide-config-defaults))
        (emacs-ide-config-apply emacs-ide-config-data)
        ;; Activate theme auto-switch if requested in config
        (when emacs-ide-theme-auto-switch
          (with-eval-after-load 'ui-theme
            (when (fboundp 'emacs-ide-theme-enable-auto)
              (emacs-ide-theme-enable-auto))))
        (setq emacs-ide-config-loaded-p t)
        (message "✓ Config loaded (env: %s)" emacs-ide-config-environment)
        t)
    (error
     (warn "⚠ Config load failed: %s — using defaults"
           (error-message-string err))
     (setq emacs-ide-config-data    emacs-ide-config-defaults
           emacs-ide-config-loaded-p t)
     (emacs-ide-config-apply emacs-ide-config-defaults)
     nil)))

(defun emacs-ide-config-reload ()
  "Re-parse config.yml, re-apply values, then run `emacs-ide-config-reload-hook'.
This is the ONLY code path that fires the reload hook."
  (interactive)
  (when (emacs-ide-config-load)
    (run-hooks 'emacs-ide-config-reload-hook))
  (message "✓ Config reloaded (cache cleared, hooks fired)"))

;;;; ── Public accessors ────────────────────────────────────────────────────────

(defun emacs-ide-config-get (section key &optional default)
  "Return the value of KEY in SECTION from loaded config, or DEFAULT.

Correctly returns nil when KEY is present with a false/nil value, as
opposed to when KEY is absent (in which case DEFAULT is returned).
This makes it safe to use for boolean config flags like lsp.enable."
  (let* ((s    (cdr (assoc section emacs-ide-config-data)))
         (cell (and s (assoc key s))))
    (if cell (cdr cell) default)))

(defun emacs-ide-config-get-list (section key)
  "Return value of KEY in SECTION as a list, or nil.
Handles both a single value and a proper list."
  (let ((val (emacs-ide-config-get section key nil)))
    (cond
     ((null val)    nil)
     ((listp val)   val)
     (t             (list val)))))

;;;; ── Display helpers ────────────────────────────────────────────────────────

(defun emacs-ide-config-show ()
  "Display all currently active IDE configuration values."
  (interactive)
  (with-output-to-temp-buffer "*IDE Config*"
    (princ "=== EMACS IDE ACTIVE CONFIG ===\n\n")
    (princ (format "File:        %s\n" emacs-ide-config-file))
    (princ (format "Environment: %s\n" emacs-ide-config-environment))
    (princ (format "Loaded:      %s\n\n"
                   (if emacs-ide-config-loaded-p "yes" "no")))
    (princ "Key Variables:\n")
    (dolist (entry
             `(;; General
               ("emacs-ide-theme"              . ,emacs-ide-theme)
               ("emacs-ide-font"               . ,emacs-ide-font)
               ("emacs-ide-font-size"           . ,emacs-ide-font-size)
               ;; LSP
               ("emacs-ide-lsp-enable"          . ,emacs-ide-lsp-enable)
               ("emacs-ide-lsp-enable-inlay-hints" . ,emacs-ide-lsp-enable-inlay-hints)
               ("emacs-ide-lsp-idle-delay"      . ,emacs-ide-lsp-idle-delay)
               ("emacs-ide-lsp-semantic-tokens" . ,emacs-ide-lsp-semantic-tokens)
               ("emacs-ide-lsp-lens"            . ,emacs-ide-lsp-lens)
               ;; Completion
               ("emacs-ide-completion-backend"  . ,emacs-ide-completion-backend)
               ("emacs-ide-completion-delay"    . ,emacs-ide-completion-delay)
               ("emacs-ide-completion-prefix"   . ,emacs-ide-completion-prefix)
               ;; Feature guards
               ("emacs-ide-feature-dashboard"   . ,emacs-ide-feature-dashboard)
               ("emacs-ide-feature-which-key"   . ,emacs-ide-feature-which-key)
               ("emacs-ide-modeline-height"     . ,emacs-ide-modeline-height)
               ;; Theme auto-switch
               ("emacs-ide-theme-auto-switch"   . ,emacs-ide-theme-auto-switch)
               ("emacs-ide-theme-auto-dark-hour" . ,emacs-ide-theme-auto-dark-hour)
               ("emacs-ide-theme-auto-light-hour". ,emacs-ide-theme-auto-light-hour)
               ;; Git
               ("emacs-ide-git-enable"          . ,emacs-ide-git-enable)
               ("emacs-ide-git-gutter"          . ,emacs-ide-git-gutter)
               ("emacs-ide-git-auto-revert"     . ,emacs-ide-git-auto-revert)
               ;; System
               ("emacs-ide-project-enable"      . ,emacs-ide-project-enable)
               ("emacs-ide-debug-enable"        . ,emacs-ide-debug-enable)
               ("emacs-ide-telemetry-enabled"   . ,emacs-ide-telemetry-enabled)
               ("gc-cons-threshold"             . ,gc-cons-threshold)))
      (princ (format "  %-42s %s\n" (car entry) (cdr entry))))))

(defun emacs-ide-config-edit ()
  "Open config.yml for editing."
  (interactive)
  (find-file emacs-ide-config-file))

(provide 'emacs-ide-config)
;;; emacs-ide-config.el ends here
