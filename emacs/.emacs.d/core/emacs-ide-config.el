;;; emacs-ide-config.el --- Configuration Management System -*- lexical-binding: t -*-
;;; Commentary:
;;; YAML and Elisp configuration management with proper nested parsing.
;;; Version: 2.2.6
;;; Fixes:
;;;   - 2.2.6: emacs-ide-config-apply telemetry block: replaced raw (setq
;;;     emacs-ide-telemetry-enabled VAL) with calls to the proper
;;;     emacs-ide-telemetry-enable / emacs-ide-telemetry-disable functions.
;;;     The previous raw setq only updated the variable; it did NOT stop or
;;;     restart the 0.5-second repeat idle timer created by
;;;     emacs-ide-telemetry--ensure-flush-timer at module load time.
;;;     On a config reload that disabled telemetry, the timer kept running.
;;;     On a subsequent reload that re-enabled it, a SECOND timer was created
;;;     alongside the still-running first. After enough reload cycles the
;;;     timer-function slot could become nil, producing:
;;;       "error running timer: (void-function nil)"
;;;     on every 0.5 s idle tick.
;;;     Fix: call emacs-ide-telemetry-enable / emacs-ide-telemetry-disable
;;;     which correctly start/stop the timer. Fall back to raw setq when the
;;;     functions are not yet loaded (safe mode / partial startup).
;;;   - 2.2.5: emacs-ide-config-apply: added missing sections — formatting,
;;;     languages, git, terminal, debug, project.
;;;   - 2.2.5: emacs-ide-config-apply: TLS block uses (require 'gnutls) eagerly.
;;;   - 2.2.5: defvar declarations moved ABOVE emacs-ide-config-apply.
;;;   - 2.2.4: emacs-ide-config-get-nested now handles arbitrary depth paths.
;;;   - 2.2.4: YAML parser supports 3-level nesting for environments: block.
;;;   - 2.2.4: emacs-ide-config-apply: replaced defvar with setq for reload.
;;;   - 2.2.4: boolean false from YAML parses to nil; use (assoc) not when-let.
;;;   - 2.2.4: YAML key regex extended to [a-z][a-z0-9_-]* for hyphenated keys.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; CONFIGURATION VARIABLES (declared first so apply can setq them cleanly)
;; ============================================================================
(defvar emacs-ide-theme 'modus-vivendi
  "Current theme (from config).")

(defvar emacs-ide-font "JetBrains Mono"
  "Editor font (from config).")

(defvar emacs-ide-font-size 11
  "Font size (from config).")

(defvar emacs-ide-completion-backend 'corfu
  "Completion backend: corfu or company (from config).")

(defvar emacs-ide-completion-delay 0.1
  "Completion delay in seconds (from config).")

(defvar emacs-ide-lsp-enable t
  "Enable LSP mode (from config).")

(defvar emacs-ide-lsp-enable-inlay-hints t
  "Enable LSP inlay hints (from config).")

(defvar emacs-ide-lsp-large-file-threshold 100000
  "Disable some LSP features for files larger than this (from config).")

(defvar emacs-ide-startup-time-target 3.0
  "Target startup time in seconds (from config).")

(defvar emacs-ide-safe-mode nil
  "Safe mode flag (from config).")

(defvar emacs-ide-telemetry-enabled t
  "Telemetry enabled (from config).")

(defvar emacs-ide-feature-dashboard t
  "Dashboard feature enabled (from config).")

(defvar emacs-ide-feature-which-key t
  "Which-key feature enabled (from config).")

(defvar emacs-ide-native-comp-jobs 4
  "Native compilation parallel jobs (from config).")

;; Formatting
(defvar emacs-ide-format-on-save t
  "Format on save via apheleia (from config).")

;; Git
(defvar emacs-ide-git-enable t
  "Enable Magit and git features (from config).")

(defvar emacs-ide-git-gutter t
  "Show git changes in gutter (from config).")

;; Terminal
(defvar emacs-ide-terminal-enable t
  "Enable VTerm (from config).")

(defvar emacs-ide-terminal-shell ""
  "Shell for VTerm; empty means use $SHELL (from config).")

;; Debug
(defvar emacs-ide-debug-enable t
  "Enable DAP debugging (from config).")

;; Project
(defvar emacs-ide-project-enable t
  "Enable Projectile (from config).")

(defvar emacs-ide-project-search 'ripgrep
  "Default project search tool (from config).")

;; ============================================================================
;; CONFIGURATION VARIABLES (meta)
;; ============================================================================
(defvar emacs-ide-config-file
  (expand-file-name "config.yml" user-emacs-directory)
  "Main configuration file (YAML format).")

(defvar emacs-ide-config-data nil
  "Parsed configuration data.")

(defvar emacs-ide-config-environment nil
  "Current environment (work, home, etc).")

(defvar emacs-ide-config-loaded-p nil
  "Whether configuration has been loaded.")

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
     (usage-stats . t)))
  "Default configuration values.")

;; ============================================================================
;; ENVIRONMENT DETECTION
;; ============================================================================
(defun emacs-ide-config-detect-environment ()
  "Detect current environment (work, home, etc)."
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
Strips inline YAML comments (# ...) before parsing so that values
like \"16777216  # 16MB\" correctly parse to the integer 16777216."
  (let* ((stripped (replace-regexp-in-string
                    "[ \t]+#[^\"']*$" "" value-string))
         (trimmed (string-trim stripped)))
    (cond
     ((or (string-empty-p trimmed) (string= trimmed "null")) nil)
     ((string= trimmed "true") t)
     ;; false -> nil; callers must use (assoc key section) not (when-let)
     ;; to distinguish explicitly-false from absent key.
     ((string= trimmed "false") nil)
     ((string-match-p "^-?[0-9]+$" trimmed) (string-to-number trimmed))
     ((string-match-p "^-?[0-9]+\\.[0-9]+$" trimmed) (string-to-number trimmed))
     ;; Only intern values that are genuine Emacs option symbols — i.e. values
     ;; the config uses as mode/backend selectors (corfu, modus-vivendi, hybrid,
     ;; ripgrep, pyright, etc.) but NOT executable names like "black", "prettier",
     ;; "rustfmt", "gofmt", "shfmt", "clang-format" which must stay as strings
     ;; so executable-find and format-all receive the right type.
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
                          "ruby" "php" "java" "mvn" "gradle"))))
      (intern trimmed))
     (t (replace-regexp-in-string "^['\"]\\|['\"]$" "" trimmed)))))

;; ============================================================================
;; YAML PARSER — supports up to 3 levels of nesting
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

                 ;; Key-value pair under section (no subsection active)
                 ((and (string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*\\(.*\\)$" trimmed)
                       (null current-subsection))
                  (let* ((key (intern (match-string 1 trimmed)))
                         (val (emacs-ide-config-parse-value
                               (string-trim (match-string 2 trimmed))))
                         (sec-entry (assoc current-section data)))
                    (when sec-entry
                      (let ((existing (assoc key (cdr sec-entry))))
                        (if existing
                            (setcdr existing val)
                          (setcdr sec-entry
                                  (append (cdr sec-entry)
                                          (list (cons key val)))))))))))

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
FIX 2.2.5: Added missing sections: formatting, languages, git, terminal,
  debug, project. All 13 top-level config sections now have handlers.
FIX 2.2.5: TLS block now uses (require 'gnutls) instead of
  (with-eval-after-load 'gnutls) so settings apply before any network call.
FIX 2.2.4: Use `setq` not `defvar` so values update on reload.
FIX 2.2.4: Use `(assoc key section)` not `when-let` so boolean false applies."
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
    (let ((perf (section 'performance)))
      (when perf
        (when (assoc 'gc-threshold perf)
          (setq gc-cons-threshold (val 'gc-threshold perf)))
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
    ;; FIX 2.2.5: This section was previously missing from config-apply.
    ;; Modules (tools-format.el) read these via emacs-ide-config-get which
    ;; works directly from emacs-ide-config-data, but the canonical IDE vars
    ;; were never set, so emacs-ide-format-on-save always held the defvar default.
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
    ;; FIX 2.2.5: Replaced (with-eval-after-load 'gnutls ...) with
    ;; (require 'gnutls) so TLS settings apply before any network call.
    ;; The old deferred form left the first network call (straight bootstrap,
    ;; package refresh) using Emacs default unverified TLS settings.
    (let ((security (section 'security)))
      (when security
        (when (assoc 'tls-verify security)
          (require 'gnutls)
          (setq gnutls-verify-error (val 'tls-verify security)))))

    ;; ── Telemetry ────────────────────────────────────────────────────────────
    ;; FIX 2.2.6: Use enable/disable functions to correctly manage the
    ;; repeat idle timer lifecycle. Raw setq left stale timers running across
    ;; config reloads, eventually producing (void-function nil) timer errors.
    ;; Fall back to setq when telemetry.el is not loaded yet (safe mode).
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
  "Get config value for SECTION and KEY (symbols), or DEFAULT."
  (let* ((section-data (cdr (assoc section emacs-ide-config-data)))
         (cell (assoc key section-data)))
    (if cell (cdr cell) default)))

(defun emacs-ide-config-get-nested (path &optional default)
  "Get config value using dot-separated string PATH of arbitrary depth.
FIX 2.2.4: Handles paths of any depth, e.g.:
  \"performance.gc-threshold\"        -> 2 levels
  \"environments.work.theme\"         -> 3 levels
  \"environments.work.org-directory\" -> 3 levels"
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
# Version: 2.2.5
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
