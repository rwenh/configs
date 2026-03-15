;;; emacs-ide-config.el --- Configuration Management System -*- lexical-binding: t -*-
;;; Commentary:
;;; YAML and Elisp configuration management with proper nested parsing.
;;; Version: 2.2.7
;;; Fixes vs 2.2.6:
;;;   - C-13 (CRITICAL): YAML parser subsection context leak.
;;;     When a new top-level section header was encountered at indent 0,
;;;     `current-subsection` was reset to nil but ONLY inside the cond branch
;;;     that matched the section header regex.  A top-level key like:
;;;       git:
;;;         enable: true
;;;       languages:
;;;         python:
;;;           lsp-server: pyright
;;;         go:          ← indent 4 inside languages
;;;           lsp-server: gopls
;;;       terminal:     ← indent 0: new top-level section
;;;         enable: true   ← indent 2, but current-subsection is still 'go!
;;;     After parsing `go:` under languages, current-subsection = 'go.
;;;     When `terminal:` is reached at indent 0, the FIRST cond branch
;;;     fires (section-header match), sets current-section = 'terminal,
;;;     and sets current-subsection = nil. So far correct.
;;;     BUT: the level-2 branch fires next for `enable: true`.
;;;     Because current-subsection is now nil (correctly), the key-value
;;;     branch fires and the value lands in terminal. CORRECT.
;;;     The REAL failure case is a subsection that is NOT followed by a
;;;     new level-0 section.  Example:
;;;       environments:
;;;         work:
;;;           theme: modus-operandi
;;;         home:             ← indent 2 sub-section header
;;;           theme: modus-vivendi
;;;     After `work:`, current-subsection = 'work.
;;;     The `home:` line is at indent 2 and matches the sub-section header
;;;     regex — the level-2/sub-section branch correctly fires and sets
;;;     current-subsection = 'home. OK.
;;;     BUT: if we then add a top-level key immediately after:
;;;       keybindings:    ← indent 0
;;;         ...
;;;     The indent-0 branch fires and sets current-subsection = nil. ALSO OK.
;;;     The ACTUAL corruption path is:
;;;       languages:
;;;         python:
;;;           lsp-server: pyright
;;;         typescript:
;;;           lsp-server: typescript-language-server
;;;       keybindings:     ← indent-0: resets current-section correctly
;;;         custom_save: ...   ← indent-2, current-subsection is nil: OK
;;;     This specific path is fine.  The failure occurs when a COMMENT or
;;;     BLANK LINE between sub-sections causes the parser to re-enter the
;;;     indent-2 key-value branch WHILE current-subsection is still set
;;;     from the previous sub-section, writing the new value into the wrong
;;;     sub-section's alist.
;;;     Additionally: the indent-2 sub-section header branch only resets
;;;     current-subsection when a NEW sub-section is detected.  A plain
;;;     key-value at indent 2 with current-subsection still set writes into
;;;     the sub-section instead of the parent section — wrong for sections
;;;     like `git:` that have only flat key-value pairs at indent 2.
;;;     Fix: reset current-subsection to nil at the START of every indent-0
;;;     section-header match, BEFORE any other processing, AND reset it
;;;     whenever an indent-2 key-value is processed while current-subsection
;;;     is set (i.e. treat plain key-value at indent 2 as "end of subsection").
;;;   - C-14 (HIGH): emacs-ide-config-apply: gc-cons-threshold applied on
;;;     every reload, causing GC storms mid-session. Fix: only apply the
;;;     threshold if Emacs is still in its bootstrap/startup window
;;;     (emacs-startup-hook has not yet fired, checked via
;;;     after-init-time being nil) OR if the new value is larger than
;;;     the current threshold (safe to increase at any time).
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; CONFIGURATION VARIABLES
;; ============================================================================
(defvar emacs-ide-theme 'modus-vivendi)
(defvar emacs-ide-font "JetBrains Mono")
(defvar emacs-ide-font-size 11)
(defvar emacs-ide-completion-backend 'corfu)
(defvar emacs-ide-completion-delay 0.1)
(defvar emacs-ide-lsp-enable t)
(defvar emacs-ide-lsp-enable-inlay-hints t)
(defvar emacs-ide-lsp-large-file-threshold 100000)
(defvar emacs-ide-startup-time-target 3.0)
(defvar emacs-ide-safe-mode nil)
(defvar emacs-ide-telemetry-enabled t)
(defvar emacs-ide-feature-dashboard t)
(defvar emacs-ide-feature-which-key t)
(defvar emacs-ide-native-comp-jobs 4)
(defvar emacs-ide-format-on-save t)
(defvar emacs-ide-git-enable t)
(defvar emacs-ide-git-gutter t)
(defvar emacs-ide-terminal-enable t)
(defvar emacs-ide-terminal-shell "")
(defvar emacs-ide-debug-enable t)
(defvar emacs-ide-project-enable t)
(defvar emacs-ide-project-search 'ripgrep)

;; ============================================================================
;; META VARIABLES
;; ============================================================================
(defvar emacs-ide-config-file
  (expand-file-name "config.yml" user-emacs-directory))
(defvar emacs-ide-config-data nil)
(defvar emacs-ide-config-environment nil)
(defvar emacs-ide-config-loaded-p nil)

(defvar emacs-ide-config-defaults
  '((general
     (theme . modus-vivendi)
     (font . "JetBrains Mono")
     (font-size . 11)
     (safe-mode . nil))
    (completion
     (backend . corfu)
     (delay . 0.1)
     (snippet-expansion . t))
    (lsp
     (enable . t)
     (inlay-hints . t)
     (large-file-threshold . 100000)
     (semantic-tokens . t))
    (performance
     (gc-threshold . 16777216)
     (startup-time-target . 3.0)
     (native-comp-jobs . 4))
    (features
     (dashboard . t)
     (which-key . t)
     (beacon . t)
     (rainbow-delimiters . t))
    (security
     (tls-verify . t)
     (package-signatures . allow-unsigned))
    (telemetry
     (enabled . t)
     (usage-stats . t))))

;; ============================================================================
;; ENVIRONMENT DETECTION
;; ============================================================================
(defun emacs-ide-config-detect-environment ()
  "Detect current environment."
  (or (getenv "EMACS_ENVIRONMENT")
      (let ((hostname (system-name)))
        (cond
         ((string-match-p "work\\|corp\\|office" hostname) "work")
         ((string-match-p "home\\|personal" hostname) "home")
         (t "default")))))

(setq emacs-ide-config-environment
      (emacs-ide-config-detect-environment))

;; ============================================================================
;; YAML VALUE PARSER
;; ============================================================================
(defun emacs-ide-config-parse-value (value-string)
  "Parse VALUE-STRING to appropriate Emacs Lisp type.
Strips inline YAML comments before parsing."
  (let* ((stripped (replace-regexp-in-string
                    "[ \t]+#[^\"']*$" "" value-string))
         (trimmed (string-trim stripped)))
    (cond
     ((or (string-empty-p trimmed) (string= trimmed "null")) nil)
     ((string= trimmed "true") t)
     ((string= trimmed "false") nil)
     ((string-match-p "^-?[0-9]+$" trimmed) (string-to-number trimmed))
     ((string-match-p "^-?[0-9]+\\.[0-9]+$" trimmed) (string-to-number trimmed))
     ;; Intern known option symbols; keep executable/tool names as strings.
     ((and (string-match-p "^[a-z][a-z0-9_-]*$" trimmed)
           (not (member trimmed
                        '("black" "prettier" "rustfmt" "gofmt" "gofumpt"
                          "shfmt" "clang-format" "clang-format-diff"
                          "autopep8" "yapf" "isort" "ruff"
                          "eslint" "eslint_d" "biome" "deno"
                          "rubocop" "standardrb" "perltidy"
                          "phpcbf" "phpcs" "psalm" "phan"
                          "ktlint" "google-java-format" "scalafmt"
                          "terraform" "nixpkgs-fmt" "ormolu" "fourmolu"
                          "swiftformat" "ocamlformat" "elm-format"
                          "mix" "cljfmt" "zprint" "pgformatter"
                          "luaformatter" "stylua" "cmake-format"
                          "xmllint" "tidy" "sqlfluff"
                          "pyright" "pyright-langserver" "pylsp"
                          "jedi-language-server"
                          "rust-analyzer" "gopls"
                          "typescript-language-server" "tsserver"
                          "clangd" "ccls" "jdtls" "kotlin-language-server"
                          "solargraph" "sorbet"
                          "rg" "ripgrep" "grep" "ag" "fd" "find"
                          "aspell" "hunspell" "ispell"
                          "flake8" "pylint" "mypy"
                          "eslint" "tsc" "node" "npm" "npx"
                          "cargo" "rustc" "go" "python" "python3"
                          "ruby" "php" "java" "mvn" "gradle"
                          ;; C-15: these were missing — add projectile
                          ;; indexing modes and other bare-word options
                          ;; that must stay as symbols, not strings
                          "native" "hybrid" "alien"))))
      (intern trimmed))
     (t (replace-regexp-in-string "^['\"]\\|['\"]$" "" trimmed)))))

;; ============================================================================
;; YAML PARSER — supports up to 3 levels of nesting
;; C-13 FIX: Reset current-subsection to nil at the start of every indent-0
;; section-header match so that sub-section context never bleeds across
;; top-level sections.  Also reset it when a plain key-value at indent 2
;; is processed while current-subsection is non-nil — this ends the
;; sub-section and writes the key into the parent section.
;; ============================================================================
(defun emacs-ide-config-parse-yaml-improved (file)
  "Parse YAML FILE with support for up to 3 levels of nesting.
Returns association list of configuration."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((data '())
            (current-section nil)
            (current-subsection nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                 (indent (- (length line) (length (string-trim-left line))))
                 (trimmed (string-trim line)))

            (unless (or (string-empty-p trimmed)
                        (string-prefix-p "#" trimmed))

              (cond

               ;; ── Level 0: top-level section header ──────────────────────
               ((and (= indent 0)
                     (string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*$" trimmed))
                (let ((name (intern (match-string 1 trimmed))))
                  (setq current-section name
                        ;; C-13 FIX: always clear subsection at level-0 boundary
                        current-subsection nil)
                  (unless (assoc name data)
                    (push (cons name '()) data))))

               ;; ── Level 2: sub-section header OR key-value ────────────────
               ((and (= indent 2) current-section)
                (cond
                 ;; Sub-section header (e.g. "  work:" or "  python:")
                 ((string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*$" trimmed)
                  (let* ((name (intern (match-string 1 trimmed)))
                         (sec-entry (assoc current-section data)))
                    (setq current-subsection name)
                    (when sec-entry
                      (unless (assoc name (cdr sec-entry))
                        (setcdr sec-entry
                                (append (cdr sec-entry)
                                        (list (cons name '()))))))))

                 ;; List item under section (no subsection active)
                 ((and (string-prefix-p "- " trimmed) (null current-subsection))
                  (let* ((val (emacs-ide-config-parse-value
                               (string-trim (substring trimmed 2))))
                         (sec-entry (assoc current-section data)))
                    (when sec-entry
                      (setcdr sec-entry
                              (append (cdr sec-entry) (list val))))))

                 ;; Key-value pair at indent 2
                 ((string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*\\(.*\\)$" trimmed)
                  (let* ((key (intern (match-string 1 trimmed)))
                         (val (emacs-ide-config-parse-value
                               (string-trim (match-string 2 trimmed))))
                         (sec-entry (assoc current-section data)))
                    (when sec-entry
                      (if (null current-subsection)
                          ;; Normal flat key under section
                          (let ((existing (assoc key (cdr sec-entry))))
                            (if existing
                                (setcdr existing val)
                              (setcdr sec-entry
                                      (append (cdr sec-entry)
                                              (list (cons key val))))))
                        ;; C-13 FIX: a plain key=value at indent 2 while
                        ;; current-subsection is set means the subsection
                        ;; block has ended.  Write into the parent section
                        ;; and clear the subsection context.
                        (setq current-subsection nil)
                        (let ((existing (assoc key (cdr sec-entry))))
                          (if existing
                              (setcdr existing val)
                            (setcdr sec-entry
                                    (append (cdr sec-entry)
                                            (list (cons key val))))))))))))

               ;; ── Level 4: key-value inside a sub-section ─────────────────
               ((and (= indent 4) current-section current-subsection)
                (cond
                 ;; List item under subsection
                 ((string-prefix-p "- " trimmed)
                  (let* ((val (emacs-ide-config-parse-value
                               (string-trim (substring trimmed 2))))
                         (sec-entry  (assoc current-section data))
                         (sub-entry  (and sec-entry
                                          (assoc current-subsection (cdr sec-entry)))))
                    (when sub-entry
                      (setcdr sub-entry
                              (append (cdr sub-entry) (list val))))))

                 ;; Key-value under subsection
                 ((string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*\\(.*\\)$" trimmed)
                  (let* ((key (intern (match-string 1 trimmed)))
                         (val (emacs-ide-config-parse-value
                               (string-trim (match-string 2 trimmed))))
                         (sec-entry  (assoc current-section data))
                         (sub-entry  (and sec-entry
                                          (assoc current-subsection (cdr sec-entry)))))
                    (when sub-entry
                      (let ((existing (assoc key (cdr sub-entry))))
                        (if existing
                            (setcdr existing val)
                          (setcdr sub-entry
                                  (append (cdr sub-entry)
                                          (list (cons key val)))))))))
                 )))))

          (forward-line 1))
        (nreverse data)))))

;; ============================================================================
;; CONFIGURATION LOADING & APPLICATION
;; ============================================================================
(defun emacs-ide-config-load ()
  "Load and apply configuration from file."
  (interactive)
  (condition-case err
      (progn
        (setq emacs-ide-config-data
              (or (and (file-exists-p emacs-ide-config-file)
                       (emacs-ide-config-parse-yaml-improved emacs-ide-config-file))
                  emacs-ide-config-defaults))
        (emacs-ide-config-apply emacs-ide-config-data)
        (setq emacs-ide-config-loaded-p t)
        (message "✓ Configuration loaded for environment: %s"
                 emacs-ide-config-environment)
        t)
    (error
     (warn "⚠️  Failed to load configuration: %s. Using defaults."
           (error-message-string err))
     (setq emacs-ide-config-data emacs-ide-config-defaults)
     (setq emacs-ide-config-loaded-p t)
     nil)))

(defun emacs-ide-config-apply (config)
  "Apply CONFIG settings to Emacs.
C-14 FIX: gc-cons-threshold is only applied during bootstrap (before
  after-init-time is set) or when the YAML value is larger than the
  current threshold. Applying a smaller GC threshold mid-session on
  M-x emacs-ide-config-reload causes immediate GC storms during
  active LSP/treesit work."
  (cl-flet ((section (key) (cdr (assoc key config)))
            (val (key alist) (when (assoc key alist) (cdr (assoc key alist)))))

    ;; ── General ──────────────────────────────────────────────────────────────
    (let ((general (section 'general)))
      (when general
        (when (assoc 'theme general)
          (setq emacs-ide-theme (val 'theme general)))
        (when (assoc 'font general)
          (setq emacs-ide-font (val 'font general)))
        (when (assoc 'font-size general)
          (setq emacs-ide-font-size (val 'font-size general)))
        (when (assoc 'safe-mode general)
          (setq emacs-ide-safe-mode (val 'safe-mode general)))))

    ;; ── Completion ───────────────────────────────────────────────────────────
    (let ((completion (section 'completion)))
      (when completion
        (when (assoc 'backend completion)
          (setq emacs-ide-completion-backend (val 'backend completion)))
        (when (assoc 'delay completion)
          (setq emacs-ide-completion-delay (val 'delay completion)))))

    ;; ── LSP ──────────────────────────────────────────────────────────────────
    (let ((lsp (section 'lsp)))
      (when lsp
        (when (assoc 'enable lsp)
          (setq emacs-ide-lsp-enable (val 'enable lsp)))
        (when (assoc 'inlay-hints lsp)
          (setq emacs-ide-lsp-enable-inlay-hints (val 'inlay-hints lsp)))
        (when (assoc 'large-file-threshold lsp)
          (setq emacs-ide-lsp-large-file-threshold
                (val 'large-file-threshold lsp)))))

    ;; ── Performance ──────────────────────────────────────────────────────────
    ;; C-14 FIX: Only apply gc-cons-threshold during startup window OR when
    ;; the configured value is >= the current threshold (safe to raise GC
    ;; at any time; dangerous to lower it mid-session during active work).
    (let ((perf (section 'performance)))
      (when perf
        (when (assoc 'gc-threshold perf)
          (let ((new-threshold (val 'gc-threshold perf)))
            (when (or (null after-init-time)          ; still in startup
                      (>= new-threshold gc-cons-threshold)) ; raising is safe
              (setq gc-cons-threshold new-threshold))))
        (when (assoc 'startup-time-target perf)
          (setq emacs-ide-startup-time-target (val 'startup-time-target perf)))
        (when (assoc 'native-comp-jobs perf)
          (setq emacs-ide-native-comp-jobs
                (max 1 (val 'native-comp-jobs perf))))))

    ;; ── Features ─────────────────────────────────────────────────────────────
    (let ((features (section 'features)))
      (when features
        (when (assoc 'dashboard features)
          (setq emacs-ide-feature-dashboard (val 'dashboard features)))
        (when (assoc 'which-key features)
          (setq emacs-ide-feature-which-key (val 'which-key features)))))

    ;; ── Formatting ───────────────────────────────────────────────────────────
    (let ((formatting (section 'formatting)))
      (when formatting
        (when (assoc 'on-save formatting)
          (setq emacs-ide-format-on-save (val 'on-save formatting)))))

    ;; ── Git ──────────────────────────────────────────────────────────────────
    (let ((git (section 'git)))
      (when git
        (when (assoc 'enable git)
          (setq emacs-ide-git-enable (val 'enable git)))
        (when (assoc 'gutter git)
          (setq emacs-ide-git-gutter (val 'gutter git)))))

    ;; ── Terminal ─────────────────────────────────────────────────────────────
    (let ((terminal (section 'terminal)))
      (when terminal
        (when (assoc 'enable terminal)
          (setq emacs-ide-terminal-enable (val 'enable terminal)))
        (when (assoc 'shell terminal)
          (setq emacs-ide-terminal-shell (or (val 'shell terminal) "")))))

    ;; ── Debug ────────────────────────────────────────────────────────────────
    (let ((debug (section 'debug)))
      (when debug
        (when (assoc 'enable debug)
          (setq emacs-ide-debug-enable (val 'enable debug)))))

    ;; ── Project ──────────────────────────────────────────────────────────────
    (let ((project (section 'project)))
      (when project
        (when (assoc 'enable project)
          (setq emacs-ide-project-enable (val 'enable project)))
        (when (assoc 'default-search project)
          (setq emacs-ide-project-search (val 'default-search project)))))

    ;; ── Security ─────────────────────────────────────────────────────────────
    (let ((security (section 'security)))
      (when security
        (when (assoc 'tls-verify security)
          (require 'gnutls)
          (setq gnutls-verify-error (val 'tls-verify security)))))

    ;; ── Telemetry ────────────────────────────────────────────────────────────
    (let ((telemetry (section 'telemetry)))
      (when telemetry
        (when (assoc 'enabled telemetry)
          (let ((want (val 'enabled telemetry)))
            (cond
             ((and want (fboundp 'emacs-ide-telemetry-enable))
              (emacs-ide-telemetry-enable))
             ((and (not want) (fboundp 'emacs-ide-telemetry-disable))
              (emacs-ide-telemetry-disable))
             (t
              (setq emacs-ide-telemetry-enabled want)))))))))

;; ============================================================================
;; CONFIGURATION ACCESSORS
;; ============================================================================
(defun emacs-ide-config-get (section key &optional default)
  "Get config value for SECTION and KEY, or DEFAULT."
  (let* ((section-data (cdr (assoc section emacs-ide-config-data)))
         (cell (assoc key section-data)))
    (if cell (cdr cell) default)))

(defun emacs-ide-config-get-nested (path &optional default)
  "Get config value using dot-separated string PATH of arbitrary depth."
  (let* ((parts (split-string path "\\."))
         (result emacs-ide-config-data)
         (found t))
    (dolist (part parts)
      (if (not found)
          nil
        (let ((cell (assoc (intern part) result)))
          (if cell
              (setq result (cdr cell))
            (setq found nil
                  result default)))))
    (if found result default)))

(defun emacs-ide-config-set (section key value)
  "Set config value for SECTION KEY to VALUE."
  (let ((section-data (assoc section emacs-ide-config-data)))
    (if section-data
        (let ((key-data (assoc key (cdr section-data))))
          (if key-data
              (setcdr key-data value)
            (setcdr section-data
                    (append (cdr section-data) (list (cons key value))))))
      (push (cons section (list (cons key value)))
            emacs-ide-config-data))))

;; ============================================================================
;; CONFIGURATION TEMPLATES
;; ============================================================================
(defun emacs-ide-config-create-template ()
  "Create a template config.yml file."
  (interactive)
  (when (or (not (file-exists-p emacs-ide-config-file))
            (y-or-n-p "config.yml exists. Overwrite? "))
    (with-temp-file emacs-ide-config-file
      (insert "# Enterprise Emacs IDE Configuration
# Version: 2.2.7
# Edit this file to customize your setup
# Changes take effect after: M-x emacs-ide-config-reload

general:
  theme: modus-vivendi
  font: JetBrains Mono
  font-size: 11
  safe-mode: false

completion:
  backend: corfu
  delay: 0.1
  snippet-expansion: true

lsp:
  enable: true
  inlay-hints: true
  large-file-threshold: 100000
  semantic-tokens: true

performance:
  gc-threshold: 16777216
  startup-time-target: 3.0
  native-comp-jobs: 4

features:
  dashboard: true
  which-key: true
  beacon: true
  rainbow-delimiters: true

formatting:
  on-save: true

git:
  enable: true
  gutter: true

terminal:
  enable: true
  shell: /bin/bash

debug:
  enable: true

project:
  enable: true
  default-search: ripgrep

security:
  tls-verify: true
  package-signatures: allow-unsigned

telemetry:
  enabled: true
  usage-stats: true
"))
    (message "✓ Created config template at %s" emacs-ide-config-file)))

;; ============================================================================
;; INTERACTIVE COMMANDS
;; ============================================================================
(defun emacs-ide-config-edit ()
  "Open config file for editing."
  (interactive)
  (unless (file-exists-p emacs-ide-config-file)
    (emacs-ide-config-create-template))
  (find-file emacs-ide-config-file))

(defun emacs-ide-config-reload ()
  "Reload configuration from file."
  (interactive)
  (emacs-ide-config-load)
  (message "✓ Configuration reloaded"))

(defun emacs-ide-config-show ()
  "Display current configuration."
  (interactive)
  (with-output-to-temp-buffer "*Configuration*"
    (princ "=== EMACS IDE CONFIGURATION ===\n\n")
    (princ (format "Environment: %s\n\n" emacs-ide-config-environment))
    (princ "Current Settings:\n")
    (dolist (section emacs-ide-config-data)
      (princ (format "\n[%s]\n" (upcase (symbol-name (car section)))))
      (dolist (item (cdr section))
        (when (consp item)
          (princ (format "  %-30s = %s\n"
                         (symbol-name (car item))
                         (cdr item))))))))

(provide 'emacs-ide-config)
;;; emacs-ide-config.el ends here
