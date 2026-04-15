;;; emacs-ide-config.el --- Configuration Management System -*- lexical-binding: t -*-
;;; Commentary:
;;; YAML and Elisp configuration management with proper nested parsing.
;;; Version: 3.0.4
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-theme 'ef-dark)
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

(defvar emacs-ide-config-file
  (expand-file-name "config.yml" user-emacs-directory))
(defvar emacs-ide-config-data nil)
(defvar emacs-ide-config-environment nil)
(defvar emacs-ide-config-loaded-p nil)

(defvar emacs-ide-config-reload-hook nil
  "Hook run after configuration is reloaded via M-x emacs-ide-config-reload.")

(defvar emacs-ide-config-defaults
  '((general
     (theme . ef-dark)
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
     (native-comp-jobs . 4)
     (slow-package-threshold . 0.1))
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

(defun emacs-ide-config-detect-environment ()
  "Detect current environment from EMACS_ENVIRONMENT or hostname."
  (or (getenv "EMACS_ENVIRONMENT")
      (let ((hostname (system-name)))
        (cond
         ((string-match-p "work\\|corp\\|office" hostname) "work")
         ((string-match-p "home\\|personal" hostname) "home")
         (t "default")))))

(setq emacs-ide-config-environment
      (emacs-ide-config-detect-environment))

(defun emacs-ide-config-parse-value (value-string)
  "Parse VALUE-STRING to appropriate Emacs Lisp type."
  (let* ((stripped (replace-regexp-in-string
                    "[ \t]+#[^\"']*$" "" value-string))
         (trimmed (string-trim stripped)))
    (cond
     ((or (string-empty-p trimmed)
          (string= trimmed "null")
          (string= trimmed "~")) nil)
     ((string= trimmed "true") t)
     ((string= trimmed "false") nil)
     ((string-match-p "^-?[0-9]+$" trimmed) (string-to-number trimmed))
     ((string-match-p "^-?[0-9]+\\.[0-9]+$" trimmed) (string-to-number trimmed))
     ((member trimmed
              '("allow-unsigned" "high" "medium" "low"
                "errors" "warnings" "always"
                "bottom" "right" "left" "top"
                "created" "alphabetic"
                "deferred" "always"
                "content" "overview"
                "week" "day" "month"
                "first-error" "yes"))
      (intern trimmed))
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
                          "tsc" "node" "npm" "npx"
                          "cargo" "rustc" "go" "python" "python3"
                          "ruby" "php" "java" "mvn" "gradle"
                          "native" "hybrid" "alien"))))
      (intern trimmed))
     (t (replace-regexp-in-string "^['\"]\\|['\"]$" "" trimmed)))))

(defun emacs-ide-config-parse-yaml-improved (file)
  "Parse YAML FILE with support for up to 3 levels of nesting."
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

               ((and (= indent 0)
                     (string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*$" trimmed))
                (let ((name (intern (match-string 1 trimmed))))
                  (setq current-section name
                        current-subsection nil)
                  (unless (assoc name data)
                    (push (cons name '()) data))))

               ((and (= indent 2) current-section)
                (cond
                 ((string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*$" trimmed)
                  (let* ((name (intern (match-string 1 trimmed)))
                         (sec-entry (assoc current-section data)))
                    (setq current-subsection name)
                    (when sec-entry
                      (unless (assoc name (cdr sec-entry))
                        (setcdr sec-entry
                                (append (cdr sec-entry)
                                        (list (cons name '()))))))))
                 ((and (string-prefix-p "- " trimmed) (null current-subsection))
                  (let* ((val (emacs-ide-config-parse-value
                               (string-trim (substring trimmed 2))))
                         (sec-entry (assoc current-section data)))
                    (when sec-entry
                      (setcdr sec-entry
                              (append (cdr sec-entry) (list val))))))
                 ((string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*\\(.*\\)$" trimmed)
                  (let* ((key (intern (match-string 1 trimmed)))
                         (val (emacs-ide-config-parse-value
                               (string-trim (match-string 2 trimmed))))
                         (sec-entry (assoc current-section data)))
                    (when sec-entry
                      (if (null current-subsection)
                          (let ((existing (assoc key (cdr sec-entry))))
                            (if existing
                                (setcdr existing val)
                              (setcdr sec-entry
                                      (append (cdr sec-entry)
                                              (list (cons key val))))))
                        (setq current-subsection nil)
                        (let ((existing (assoc key (cdr sec-entry))))
                          (if existing
                              (setcdr existing val)
                            (setcdr sec-entry
                                    (append (cdr sec-entry)
                                            (list (cons key val)))))))))))

               ((and (= indent 4) current-section current-subsection)
                (cond
                 ((string-prefix-p "- " trimmed)
                  (let* ((val (emacs-ide-config-parse-value
                               (string-trim (substring trimmed 2))))
                         (sec-entry (assoc current-section data))
                         (sub-entry (and sec-entry
                                         (assoc current-subsection
                                                (cdr sec-entry)))))
                    (when sub-entry
                      (setcdr sub-entry
                              (append (cdr sub-entry) (list val))))))
                 ((string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*\\(.*\\)$" trimmed)
                  (let* ((key (intern (match-string 1 trimmed)))
                         (val (emacs-ide-config-parse-value
                               (string-trim (match-string 2 trimmed))))
                         (sec-entry (assoc current-section data))
                         (sub-entry (and sec-entry
                                         (assoc current-subsection
                                                (cdr sec-entry)))))
                    (when sub-entry
                      (let ((existing (assoc key (cdr sub-entry))))
                        (if existing
                            (setcdr existing val)
                          (setcdr sub-entry
                                  (append (cdr sub-entry)
                                          (list (cons key val)))))))))))

               ((and (= indent 4) current-section (null current-subsection))
                (when (string-prefix-p "- " trimmed)
                  (let* ((val (emacs-ide-config-parse-value
                               (string-trim (substring trimmed 2))))
                         (sec-entry (assoc current-section data))
                         (children  (and sec-entry (cdr sec-entry)))
                         (last-list (and children
                                         (cl-find-if
                                          (lambda (c)
                                            (and (consp c) (listp (cdr c))))
                                          (reverse children)))))
                    (when last-list
                      (setcdr last-list (append (cdr last-list) (list val)))))))

               ((and (= indent 6) current-section current-subsection)
                (cond
                 ((string-prefix-p "- " trimmed)
                  (let* ((val (emacs-ide-config-parse-value
                               (string-trim (substring trimmed 2))))
                         (sec-entry (assoc current-section data))
                         (sub-entry (and sec-entry
                                         (assoc current-subsection
                                                (cdr sec-entry)))))
                    (when sub-entry
                      (setcdr sub-entry
                              (append (cdr sub-entry) (list val))))))
                 ((string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*\\(.*\\)$" trimmed)
                  (let* ((key (intern (match-string 1 trimmed)))
                         (val (emacs-ide-config-parse-value
                               (string-trim (match-string 2 trimmed))))
                         (sec-entry (assoc current-section data))
                         (sub-entry (and sec-entry
                                         (assoc current-subsection
                                                (cdr sec-entry)))))
                    (when sub-entry
                      (let ((existing (assoc key (cdr sub-entry))))
                        (if existing
                            (setcdr existing val)
                          (setcdr sub-entry
                                  (append (cdr sub-entry)
                                          (list (cons key val))))))))))))

              ))

          (forward-line 1))
        (nreverse data)))))


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
  "Apply CONFIG settings to Emacs."
  (cl-flet ((section (key) (cdr (assoc key config)))
            (val (key alist) (when (assoc key alist) (cdr (assoc key alist)))))

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

    (let ((completion (section 'completion)))
      (when completion
        (when (assoc 'backend completion)
          (setq emacs-ide-completion-backend (val 'backend completion)))
        (when (assoc 'delay completion)
          (setq emacs-ide-completion-delay (val 'delay completion)))))

    (let ((lsp (section 'lsp)))
      (when lsp
        (when (assoc 'enable lsp)
          (setq emacs-ide-lsp-enable (val 'enable lsp)))
        (when (assoc 'inlay-hints lsp)
          (setq emacs-ide-lsp-enable-inlay-hints (val 'inlay-hints lsp)))
        (when (assoc 'large-file-threshold lsp)
          (setq emacs-ide-lsp-large-file-threshold
                (val 'large-file-threshold lsp)))))

    (let ((perf (section 'performance)))
      (when perf
        (when (assoc 'gc-threshold perf)
          (let ((new-threshold (val 'gc-threshold perf)))
            (when (or (null after-init-time)
                      (>= new-threshold gc-cons-threshold))
              (setq gc-cons-threshold new-threshold))))
        (when (assoc 'startup-time-target perf)
          (setq emacs-ide-startup-time-target (val 'startup-time-target perf)))
        (when (assoc 'native-comp-jobs perf)
          (setq emacs-ide-native-comp-jobs
                (max 1 (val 'native-comp-jobs perf))))
        (when (assoc 'slow-package-threshold perf)
          (let ((threshold (val 'slow-package-threshold perf)))
            (when (and (numberp threshold) (> threshold 0)
                       (boundp 'emacs-ide-package-slow-threshold))
              (setq emacs-ide-package-slow-threshold threshold))))))

    (let ((features (section 'features)))
      (when features
        (when (assoc 'dashboard features)
          (setq emacs-ide-feature-dashboard (val 'dashboard features)))
        (when (assoc 'which-key features)
          (setq emacs-ide-feature-which-key (val 'which-key features)))))

    (let ((formatting (section 'formatting)))
      (when formatting
        (when (assoc 'on-save formatting)
          (setq emacs-ide-format-on-save (val 'on-save formatting)))))

    (let ((git (section 'git)))
      (when git
        (when (assoc 'enable git)
          (setq emacs-ide-git-enable (val 'enable git)))
        (when (assoc 'gutter git)
          (setq emacs-ide-git-gutter (val 'gutter git)))))

    (let ((terminal (section 'terminal)))
      (when terminal
        (when (assoc 'enable terminal)
          (setq emacs-ide-terminal-enable (val 'enable terminal)))
        (when (assoc 'shell terminal)
          (setq emacs-ide-terminal-shell (or (val 'shell terminal) "")))))

    (let ((debug (section 'debug)))
      (when debug
        (when (assoc 'enable debug)
          (setq emacs-ide-debug-enable (val 'enable debug)))))

    (let ((project (section 'project)))
      (when project
        (when (assoc 'enable project)
          (setq emacs-ide-project-enable (val 'enable project)))
        (when (assoc 'default-search project)
          (setq emacs-ide-project-search (val 'default-search project)))
        (when (assoc 'indexing project)
          (let ((method (val 'indexing project)))
            (when (memq method '(native hybrid alien))
              (with-eval-after-load 'projectile
                (setq projectile-indexing-method method)))))))

    (let ((editing (section 'editing)))
      (when (and editing (assoc 'meow editing))
        (let ((want-meow (val 'meow editing)))
          (when (and (boundp 'emacs-ide-meow-enabled)
                     (fboundp 'emacs-ide-toggle-meow))
            (cond
             ((and want-meow (not emacs-ide-meow-enabled))
              (emacs-ide-toggle-meow))
             ((and (not want-meow) emacs-ide-meow-enabled)
              (emacs-ide-toggle-meow)))))))

    (let ((workspace (section 'workspace)))
      (when workspace
        (when (assoc 'auto-switch workspace)
          (when (boundp 'emacs-ide-workspace-auto-switch)
            (setq emacs-ide-workspace-auto-switch
                  (val 'auto-switch workspace))))
        (when (assoc 'save-on-exit workspace)
          nil)))

    (let ((security (section 'security)))
      (when security
        (when (assoc 'tls-verify security)
          (require 'gnutls)
          (setq gnutls-verify-error (val 'tls-verify security)))))

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
              (setq emacs-ide-telemetry-enabled want)))))
        (when (assoc 'max-log-size telemetry)
          (let ((size (val 'max-log-size telemetry)))
            (when (and (numberp size) (> size 0)
                       (boundp 'emacs-ide-telemetry-max-log-size))
              (setq emacs-ide-telemetry-max-log-size size))))))))

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

(defun emacs-ide-config-create-template ()
  "Create a template config.yml file."
  (interactive)
  (when (or (not (file-exists-p emacs-ide-config-file))
            (y-or-n-p "config.yml exists. Overwrite? "))
    (with-temp-file emacs-ide-config-file
      (insert "# Enterprise Emacs IDE Configuration
# Version: 3.0.4
# Edit this file to customize your setup.
# Changes take effect after: M-x emacs-ide-config-reload

general:
  theme: ef-dark
  font: JetBrains Mono
  font-size: 11
  safe-mode: false
  show-dashboard: true
  restore-session: false

completion:
  backend: corfu
  delay: 0.1
  snippet-expansion: true
  fuzzy-matching: true

lsp:
  enable: true
  inlay-hints: true
  large-file-threshold: 100000
  semantic-tokens: true
  idle-delay: 0.3
  diagnostics-provider: flycheck

performance:
  gc-threshold: 16777216
  startup-time-target: 3.0
  native-comp-jobs: 4
  read-process-output-max: 4194304
  slow-package-threshold: 0.1

features:
  dashboard: true
  which-key: true
  beacon: true
  rainbow-delimiters: true
  line-numbers: true
  modeline: doom-modeline

formatting:
  on-save: true
  show-errors: errors
  python: black
  javascript: prettier
  typescript: prettier
  rust: rustfmt
  go: gofmt
  c: clang-format
  cpp: clang-format

editing:
  meow: false
  hydra: true
  surround: true

workspace:
  enable: true
  auto-switch: true
  save-on-exit: true
  state-file: var/persp-state
  defaults:
    - main
    - debug
    - scratch

git:
  enable: true
  gutter: true
  auto-revert: true
  fill-column: 72

terminal:
  enable: true
  shell:
  max-scrollback: 100000
  kill-buffer-on-exit: true

debug:
  enable: true
  python: true
  node: true
  go: true
  rust: true
  cpp: true

project:
  enable: true
  indexing: alien
  default-search: ripgrep
  search-paths:
    - ~/projects
    - ~/code

languages:
  python: true
  javascript: true
  typescript: true
  rust: true
  go: true
  c: true
  java: true
  lua: true
  shell: true
  sql: true
  prose: true

lang-settings:
  python:
    lsp-server: pyright
    formatter: black
  javascript:
    lsp-server: typescript-language-server
    formatter: prettier
  typescript:
    lsp-server: typescript-language-server
    formatter: prettier
  rust:
    lsp-server: rust-analyzer
    formatter: rustfmt
  go:
    lsp-server: gopls
    formatter: gofmt

repl:
  window-height: 0.35
  side: bottom
  auto-focus: true

security:
  tls-verify: true
  package-signatures: allow-unsigned

telemetry:
  enabled: true
  usage-stats: true
  local-only: true

environments:
  work:
    git-user-name: Your Name
    git-user-email: you@company.com
    theme: ef-light
  home:
    git-user-name: Your Name
    git-user-email: you@personal.com
    theme: ef-dark

keybindings:
"))
    (message "✓ Created config template at %s" emacs-ide-config-file)))

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
  (run-hooks 'emacs-ide-config-reload-hook)
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
