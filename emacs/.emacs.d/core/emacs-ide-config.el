;;; emacs-ide-config.el --- Configuration Management System -*- lexical-binding: t -*-
;;; Commentary:
;;; YAML and Elisp configuration management with proper nested parsing.
;;; Version: 2.2.1
;;; Fixes:
;;;   - emacs-ide-config-apply: replaced `defvar` with `setq` so config values
;;;     actually update on reload (defvar is a no-op if var is already bound)
;;;   - emacs-ide-config-apply: boolean `false` from YAML parses to nil;
;;;     when-let skipped nil values entirely — use `when (assoc ...)` instead
;;;   - YAML key regex `[a-z_]+` missed hyphenated keys (font-size, gc-threshold)
;;;     → extended to `[a-z][a-z0-9_-]*`
;;;   - emacs-ide-config-get-nested: accepts a string path now, not a symbol,
;;;     to avoid symbol-name mangling and support arbitrary depth safely
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; CONFIGURATION VARIABLES
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
;; YAML PARSING
;; FIX: key regex extended from [a-z_]+ to [a-z][a-z0-9_-]* so hyphenated
;;      keys like font-size, gc-threshold, large-file-threshold are captured.
;; ============================================================================
(defun emacs-ide-config-parse-yaml-improved (file)
  "Parse YAML FILE with support for nested structures.
Returns association list of configuration."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((data '())
            (current-section nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                 (indent (- (length line) (length (string-trim-left line))))
                 (trimmed (string-trim line)))

            (unless (or (string-empty-p trimmed)
                        (string-prefix-p "#" trimmed))

              ;; Section header: zero indent, ends with colon, no value after colon
              (if (and (= indent 0)
                       (string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*$" trimmed))
                  (let ((section-name (intern (match-string 1 trimmed))))
                    (setq current-section section-name)
                    (unless (assoc section-name data)
                      (push (cons section-name '()) data)))

                ;; Key-value pair inside a section
                (when (and (> indent 0) current-section)
                  (cond
                   ;; List item
                   ((string-prefix-p "- " trimmed)
                    (let* ((value (string-trim (substring trimmed 2)))
                           (parsed-value (emacs-ide-config-parse-value value))
                           (section-data (assoc current-section data)))
                      (when section-data
                        (setcdr section-data
                                (append (cdr section-data) (list parsed-value))))))

                   ;; FIX: key regex now includes hyphens [a-z][a-z0-9_-]*
                   ((string-match "^\\([a-z][a-z0-9_-]*\\):[ \t]*\\(.*\\)$" trimmed)
                    (let* ((key (intern (match-string 1 trimmed)))
                           (value-str (string-trim (match-string 2 trimmed)))
                           (parsed-value (emacs-ide-config-parse-value value-str))
                           (section-data (assoc current-section data)))
                      (when section-data
                        (let ((existing (assoc key (cdr section-data))))
                          (if existing
                              (setcdr existing parsed-value)
                            (setcdr section-data
                                    (append (cdr section-data)
                                            (list (cons key parsed-value)))))))))))))

          (forward-line 1))
        (nreverse data))))))


(defun emacs-ide-config-parse-value (value-string)
  "Parse VALUE-STRING to appropriate Emacs Lisp type."
  (let ((trimmed (string-trim value-string)))
    (cond
     ((or (string-empty-p trimmed) (string= trimmed "null")) nil)
     ((string= trimmed "true") t)
     ;; FIX: explicit 'false' → must return nil but be distinguishable from
     ;;      "missing key". We use nil; callers must use (assoc key section)
     ;;      not (when-let (val ...)) to detect false booleans.
     ((string= trimmed "false") nil)
     ((string-match-p "^-?[0-9]+$" trimmed) (string-to-number trimmed))
     ((string-match-p "^-?[0-9]+\\.[0-9]+$" trimmed) (string-to-number trimmed))
     ;; FIX: symbol pattern now includes hyphens
     ((string-match-p "^[a-z][a-z0-9_-]*$" trimmed) (intern trimmed))
     (t (replace-regexp-in-string "^['\"]\\|['\"]$" "" trimmed)))))

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
FIX: Use `setq` not `defvar` so values update on reload.
FIX: Use `(assoc key section)` instead of `when-let (val ...)` so that
     boolean `false` values (parsed as nil) are still applied."
  ;; General settings
  (when-let ((general (cdr (assoc 'general config))))
    (when (assoc 'theme general)
      (setq emacs-ide-theme (cdr (assoc 'theme general))))
    (when (assoc 'font general)
      (setq emacs-ide-font (cdr (assoc 'font general))))
    (when (assoc 'font-size general)
      (setq emacs-ide-font-size (cdr (assoc 'font-size general))))
    ;; FIX: safe-mode can legitimately be nil (false); detect by key presence
    (when (assoc 'safe-mode general)
      (setq emacs-ide-safe-mode (cdr (assoc 'safe-mode general)))))

  ;; Completion settings
  (when-let ((completion (cdr (assoc 'completion config))))
    (when (assoc 'backend completion)
      (setq emacs-ide-completion-backend (cdr (assoc 'backend completion))))
    (when (assoc 'delay completion)
      (setq emacs-ide-completion-delay (cdr (assoc 'delay completion)))))

  ;; LSP settings
  (when-let ((lsp (cdr (assoc 'lsp config))))
    (when (assoc 'enable lsp)
      (setq emacs-ide-lsp-enable (cdr (assoc 'enable lsp))))
    (when (assoc 'inlay-hints lsp)
      (setq emacs-ide-lsp-enable-inlay-hints (cdr (assoc 'inlay-hints lsp))))
    (when (assoc 'large-file-threshold lsp)
      (setq emacs-ide-lsp-large-file-threshold
            (cdr (assoc 'large-file-threshold lsp)))))

  ;; Performance settings
  (when-let ((performance (cdr (assoc 'performance config))))
    (when (assoc 'gc-threshold performance)
      (setq gc-cons-threshold (cdr (assoc 'gc-threshold performance))))
    (when (assoc 'startup-time-target performance)
      (setq emacs-ide-startup-time-target
            (cdr (assoc 'startup-time-target performance))))
    (when (assoc 'native-comp-jobs performance)
      (setq emacs-ide-native-comp-jobs
            (max 1 (cdr (assoc 'native-comp-jobs performance))))))

  ;; Features settings
  (when-let ((features (cdr (assoc 'features config))))
    (when (assoc 'dashboard features)
      (setq emacs-ide-feature-dashboard (cdr (assoc 'dashboard features))))
    (when (assoc 'which-key features)
      (setq emacs-ide-feature-which-key (cdr (assoc 'which-key features)))))

  ;; Security settings
  (when-let ((security (cdr (assoc 'security config))))
    (when (assoc 'tls-verify security)
      (with-eval-after-load 'gnutls
        (setq gnutls-verify-error (cdr (assoc 'tls-verify security))))))

  ;; Telemetry settings
  (when-let ((telemetry (cdr (assoc 'telemetry config))))
    (when (assoc 'enabled telemetry)
      (setq emacs-ide-telemetry-enabled (cdr (assoc 'enabled telemetry))))))

;; ============================================================================
;; CONFIGURATION ACCESSORS
;; ============================================================================
(defun emacs-ide-config-get (section key &optional default)
  "Get config value for SECTION and KEY (symbols), or DEFAULT."
  (let* ((section-data (cdr (assoc section emacs-ide-config-data)))
         (cell (assoc key section-data)))
    (if cell (cdr cell) default)))

(defun emacs-ide-config-get-nested (path &optional default)
  "Get config value using dot-separated string PATH (e.g., \"performance.gc-threshold\").
FIX: PATH is now a string, not a symbol, to avoid symbol-name round-trip issues."
  (let* ((parts (split-string path "\\."))
         (section (intern (car parts)))
         (key (intern (cadr parts)))
         (cell (assoc key (cdr (assoc section emacs-ide-config-data)))))
    (if cell (cdr cell) default)))

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
;; CONFIGURATION VARIABLES (with defaults)
;; These are set here as initial defaults; emacs-ide-config-apply will
;; override them with values from config.yml via `setq`.
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
# Version: 2.2.1
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
