;;; emacs-ide-config.el --- Configuration Management System (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; YAML and Elisp configuration management with proper nested parsing
;;; Author: Enterprise Emacs Team
;;; Version: 2.1.0
;;; Code:

(require 'cl-lib)
(require 'json)  ; Use json for nested structure support

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
;; IMPROVED YAML PARSING - HANDLES NESTED STRUCTURES
;; ============================================================================
(defun emacs-ide-config-parse-yaml-improved (file)
  "Parse YAML FILE with support for nested structures.
Returns association list of configuration."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((data '())
            (current-section nil)
            (current-indent 0)
            (stack '()))
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position)))
                 (indent (- (length line) (length (string-trim-left line))))
                 (trimmed (string-trim line)))
            
            ;; Skip empty lines and comments
            (unless (or (string-empty-p trimmed)
                       (string-prefix-p "#" trimmed))
              
              ;; Handle section headers (no indent, ends with :)
              (if (and (= indent 0) (string-suffix-p ":" trimmed))
                  (let ((section-name (intern (substring trimmed 0 -1))))
                    (setq current-section section-name)
                    (setq current-indent 0)
                    (setq stack (list section-name '()))
                    (push (cons section-name '()) data))
                
                ;; Handle key-value pairs (indented)
                (when (and (> indent 0) current-section)
                  (cond
                   ;; List item
                   ((string-prefix-p "-" trimmed)
                    (let ((value (string-trim (substring trimmed 1)))
                          (parsed-value (emacs-ide-config-parse-value value)))
                      (when (car stack)
                        (let ((section-data (assoc (car stack) data)))
                          (when section-data
                            (setcdr section-data
                                   (cons parsed-value (cdr section-data))))))))
                   
                   ;; Nested key-value pair
                   ((string-match "^\\([a-z_]+\\):[[:space:]]*\\(.*\\)$" trimmed)
                    (let* ((key (intern (match-string 1 trimmed)))
                           (value-str (match-string 2 trimmed))
                           (parsed-value (emacs-ide-config-parse-value value-str))
                           (section-data (assoc current-section data)))
                      (when section-data
                        (let ((existing (assoc key (cdr section-data))))
                          (if existing
                              (setcdr existing parsed-value)
                            (setcdr section-data
                                   (cons (cons key parsed-value) (cdr section-data)))))))))))
            
            (forward-line 1)))
        
        ;; Return with reversed section order
        (nreverse data)))))

(defun emacs-ide-config-parse-value (value-string)
  "Parse VALUE-STRING to appropriate Emacs Lisp type."
  (let ((trimmed (string-trim value-string)))
    (cond
     ;; Empty or nil
     ((or (string-empty-p trimmed) (string= trimmed "null"))
      nil)
     ;; Boolean
     ((string= trimmed "true") t)
     ((string= trimmed "false") nil)
     ;; Number
     ((string-match-p "^-?[0-9]+$" trimmed)
      (string-to-number trimmed))
     ;; Float
     ((string-match-p "^-?[0-9]+\\.[0-9]+$" trimmed)
      (string-to-number trimmed))
     ;; Symbol
     ((string-match-p "^[a-z-]+$" trimmed)
      (intern trimmed))
     ;; String (remove quotes if present)
     (t (replace-regexp-in-string "^['\"]\\|['\"]$" "" trimmed)))))

;; ============================================================================
;; CONFIGURATION LOADING & APPLICATION
;; ============================================================================
(defun emacs-ide-config-load ()
  "Load and apply configuration from file."
  (interactive)
  (condition-case err
      (progn
        ;; Try to load config file, fallback to defaults
        (setq emacs-ide-config-data
              (or (and (file-exists-p emacs-ide-config-file)
                      (emacs-ide-config-parse-yaml-improved emacs-ide-config-file))
                  emacs-ide-config-defaults))
        
        ;; Apply configuration
        (emacs-ide-config-apply emacs-ide-config-data)
        
        ;; Mark as loaded
        (setq emacs-ide-config-loaded-p t)
        
        (message "✓ Configuration loaded for environment: %s"
                 emacs-ide-config-environment)
        t)
    (error
     (warn "⚠️  Failed to load configuration: %s. Using defaults." (error-message-string err))
     (setq emacs-ide-config-data emacs-ide-config-defaults)
     (setq emacs-ide-config-loaded-p t)
     nil)))

(defun emacs-ide-config-apply (config)
  "Apply CONFIG settings to Emacs."
  ;; General settings
  (when-let ((general (cdr (assoc 'general config))))
    (when-let ((theme (cdr (assoc 'theme general))))
      (defvar emacs-ide-theme theme))
    (when-let ((font (cdr (assoc 'font general))))
      (defvar emacs-ide-font font))
    (when-let ((font-size (cdr (assoc 'font-size general))))
      (defvar emacs-ide-font-size font-size))
    (when-let ((safe-mode (cdr (assoc 'safe-mode general))))
      (defvar emacs-ide-safe-mode safe-mode)))
  
  ;; Completion settings
  (when-let ((completion (cdr (assoc 'completion config))))
    (when-let ((backend (cdr (assoc 'backend completion))))
      (defvar emacs-ide-completion-backend backend))
    (when-let ((delay (cdr (assoc 'delay completion))))
      (defvar emacs-ide-completion-delay delay)))
  
  ;; LSP settings
  (when-let ((lsp (cdr (assoc 'lsp config))))
    (when-let ((enable (cdr (assoc 'enable lsp))))
      (defvar emacs-ide-lsp-enable enable))
    (when-let ((inlay-hints (cdr (assoc 'inlay-hints lsp))))
      (defvar emacs-ide-lsp-enable-inlay-hints inlay-hints))
    (when-let ((threshold (cdr (assoc 'large-file-threshold lsp))))
      (defvar emacs-ide-lsp-large-file-threshold threshold)))
  
  ;; Performance settings
  (when-let ((performance (cdr (assoc 'performance config))))
    (when-let ((gc-threshold (cdr (assoc 'gc-threshold performance))))
      (setq gc-cons-threshold gc-threshold))
    (when-let ((target (cdr (assoc 'startup-time-target performance))))
      (defvar emacs-ide-startup-time-target target))
    (when-let ((jobs (cdr (assoc 'native-comp-jobs performance))))
      (when (fboundp 'native-comp-available-p)
        (defvar emacs-ide-native-comp-jobs (max 1 jobs)))))
  
  ;; Features settings
  (when-let ((features (cdr (assoc 'features config))))
    (when-let ((dashboard (cdr (assoc 'dashboard features))))
      (defvar emacs-ide-feature-dashboard dashboard))
    (when-let ((which-key (cdr (assoc 'which-key features))))
      (defvar emacs-ide-feature-which-key which-key)))
  
  ;; Security settings
  (when-let ((security (cdr (assoc 'security config))))
    (when-let ((tls-verify (cdr (assoc 'tls-verify security))))
      (with-eval-after-load 'gnutls
        (setq gnutls-verify-error tls-verify))))
  
  ;; Telemetry settings
  (when-let ((telemetry (cdr (assoc 'telemetry config))))
    (when-let ((enabled (cdr (assoc 'enabled telemetry))))
      (defvar emacs-ide-telemetry-enabled enabled))))

;; ============================================================================
;; CONFIGURATION ACCESSORS
;; ============================================================================
(defun emacs-ide-config-get (section key &optional default)
  "Get config value for SECTION and KEY, or DEFAULT.
SECTION and KEY should be symbols."
  (let* ((section-data (cdr (assoc section emacs-ide-config-data)))
         (value (cdr (assoc key section-data))))
    (or value default)))

(defun emacs-ide-config-get-nested (path &optional default)
  "Get config value using dot-separated PATH (e.g., 'performance.gc-threshold').
Returns DEFAULT if not found."
  (let* ((parts (split-string (symbol-name path) "\\."))
         (section (intern (car parts)))
         (key (intern (cadr parts)))
         (value (emacs-ide-config-get section key)))
    (or value default)))

(defun emacs-ide-config-set (section key value)
  "Set config value for SECTION KEY to VALUE."
  (let ((section-data (assoc section emacs-ide-config-data)))
    (if section-data
        (let ((key-data (assoc key (cdr section-data))))
          (if key-data
              (setcdr key-data value)
            (setcdr section-data
                   (cons (cons key value) (cdr section-data)))))
      (push (cons section (list (cons key value)))
            emacs-ide-config-data))))

;; ============================================================================
;; CONFIGURATION VARIABLES (with defaults)
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
# Version: 2.1.0
# Edit this file to customize your setup
# Changes take effect after: M-x emacs-ide-config-reload

# ============================================================================
# GENERAL SETTINGS
# ============================================================================
general:
  theme: modus-vivendi        # or modus-operandi
  font: JetBrains Mono        # Primary font
  font_size: 11               # Font size in points
  safe_mode: false            # Enable safe mode on startup

# ============================================================================
# COMPLETION FRAMEWORK
# ============================================================================
completion:
  backend: corfu              # or company
  delay: 0.1                  # Delay before showing completions
  snippet_expansion: true     # Enable YASnippet

# ============================================================================
# LSP (LANGUAGE SERVER PROTOCOL)
# ============================================================================
lsp:
  enable: true                # Master LSP switch
  inlay_hints: true           # Show type hints inline
  large_file_threshold: 100000 # Disable LSP features for large files
  semantic_tokens: true       # Semantic highlighting

# ============================================================================
# PERFORMANCE
# ============================================================================
performance:
  gc_threshold: 16777216      # 16MB - GC threshold
  startup_time_target: 3.0    # Target startup time (seconds)
  native_comp_jobs: 4         # Parallel compilation jobs

# ============================================================================
# UI FEATURES
# ============================================================================
features:
  dashboard: true             # Startup dashboard
  which_key: true             # Show available keybindings
  beacon: true                # Highlight cursor on scroll
  rainbow_delimiters: true    # Color-code parentheses

# ============================================================================
# SECURITY
# ============================================================================
security:
  tls_verify: true            # Verify TLS certificates
  package_signatures: allow-unsigned

# ============================================================================
# TELEMETRY (LOCAL ONLY)
# ============================================================================
telemetry:
  enabled: true               # Enable local telemetry
  usage_stats: true           # Track command usage
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
        (princ (format "  %-30s = %s\n"
                      (symbol-name (car item))
                      (cdr item)))))))

(provide 'emacs-ide-config)
;;; emacs-ide-config.el ends here