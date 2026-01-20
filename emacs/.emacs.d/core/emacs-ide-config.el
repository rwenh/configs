;;; emacs-ide-config.el --- Configuration Management System -*- lexical-binding: t -*-
;;; Commentary:
;;; YAML and Elisp configuration management with environment detection
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
     (startup-time-target . 2.0)
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
;; YAML PARSING (Simple implementation)
;; ============================================================================
(defun emacs-ide-config-parse-yaml-simple (file)
  "Simple YAML parser for basic key-value configs in FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((data '())
            (current-section nil)
            (indent-level 0))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
            ;; Skip comments and empty lines
            (unless (or (string-match-p "^#" line)
                       (string-match-p "^[[:space:]]*$" line))
              (cond
               ;; Section header (no indent, ends with :)
               ((string-match "^\\([a-z_]+\\):[[:space:]]*$" line)
                (setq current-section (intern (match-string 1 line)))
                (push (cons current-section '()) data))
               
               ;; Key-value pair (indented)
               ((string-match "^[[:space:]]+\\([a-z_]+\\):[[:space:]]*\\(.+\\)$" line)
                (when current-section
                  (let* ((key (intern (match-string 1 line)))
                         (value (match-string 2 line))
                         (parsed-value (emacs-ide-config-parse-value value))
                         (section-data (assoc current-section data)))
                    (setcdr section-data
                           (cons (cons key parsed-value)
                                 (cdr section-data)))))))))
          (forward-line 1))
        (nreverse data)))))

(defun emacs-ide-config-parse-value (value-string)
  "Parse VALUE-STRING to appropriate Emacs Lisp type."
  (let ((trimmed (string-trim value-string)))
    (cond
     ;; Boolean
     ((string= trimmed "true") t)
     ((string= trimmed "false") nil)
     ;; Number
     ((string-match-p "^[0-9]+$" trimmed)
      (string-to-number trimmed))
     ;; Float
     ((string-match-p "^[0-9]+\\.[0-9]+$" trimmed)
      (string-to-number trimmed))
     ;; Symbol
     ((string-match-p "^[a-z-]+$" trimmed)
      (intern trimmed))
     ;; String (remove quotes if present)
     (t (replace-regexp-in-string "^['\"]\\|['\"]$" "" trimmed)))))

;; ============================================================================
;; CONFIGURATION LOADING
;; ============================================================================
(defun emacs-ide-config-load ()
  "Load configuration from file."
  (interactive)
  (setq emacs-ide-config-data
        (if (file-exists-p emacs-ide-config-file)
            (progn
              (message "Loading configuration from %s" emacs-ide-config-file)
              (emacs-ide-config-parse-yaml-simple emacs-ide-config-file))
          (message "No config file found, using defaults")
          emacs-ide-config-defaults))
  
  ;; Apply configuration
  (emacs-ide-config-apply emacs-ide-config-data)
  
  (message "Configuration loaded for environment: %s"
           emacs-ide-config-environment))

(defun emacs-ide-config-apply (config)
  "Apply CONFIG settings to Emacs."
  ;; General settings
  (when-let ((general (cdr (assoc 'general config))))
    (when-let ((theme (cdr (assoc 'theme general))))
      (setq emacs-ide-theme theme))
    (when-let ((font (cdr (assoc 'font general))))
      (setq emacs-ide-font font))
    (when-let ((font-size (cdr (assoc 'font-size general))))
      (setq emacs-ide-font-size font-size)))
  
  ;; Completion settings
  (when-let ((completion (cdr (assoc 'completion config))))
    (when-let ((backend (cdr (assoc 'backend completion))))
      (setq emacs-ide-completion-backend backend))
    (when-let ((delay (cdr (assoc 'delay completion))))
      (setq emacs-ide-completion-delay delay)))
  
  ;; LSP settings
  (when-let ((lsp (cdr (assoc 'lsp config))))
    (when-let ((enable (cdr (assoc 'enable lsp))))
      (setq emacs-ide-lsp-enable enable))
    (when-let ((inlay-hints (cdr (assoc 'inlay-hints lsp))))
      (setq emacs-ide-lsp-enable-inlay-hints inlay-hints))
    (when-let ((threshold (cdr (assoc 'large-file-threshold lsp))))
      (setq emacs-ide-lsp-large-file-threshold threshold)))
  
  ;; Performance settings
  (when-let ((performance (cdr (assoc 'performance config))))
    (when-let ((gc-threshold (cdr (assoc 'gc-threshold performance))))
      (setq gc-cons-threshold gc-threshold))
    (when-let ((target (cdr (assoc 'startup-time-target performance))))
      (setq emacs-ide-startup-time-target target))))

;; ============================================================================
;; CONFIGURATION ACCESSORS
;; ============================================================================
(defun emacs-ide-config-get (section key &optional default)
  "Get config value for SECTION and KEY, or DEFAULT."
  (let* ((section-data (cdr (assoc section emacs-ide-config-data)))
         (value (cdr (assoc key section-data))))
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
  "Current theme.")

(defvar emacs-ide-font "JetBrains Mono"
  "Editor font.")

(defvar emacs-ide-font-size 11
  "Font size.")

(defvar emacs-ide-completion-backend 'corfu
  "Completion backend: corfu or company.")

(defvar emacs-ide-completion-delay 0.1
  "Completion delay in seconds.")

(defvar emacs-ide-lsp-enable t
  "Enable LSP mode.")

(defvar emacs-ide-lsp-enable-inlay-hints t
  "Enable LSP inlay hints.")

(defvar emacs-ide-lsp-large-file-threshold 100000
  "Disable some LSP features for files larger than this.")

(defvar emacs-ide-startup-time-target 2.0
  "Target startup time in seconds.")

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
# Edit this file to customize your setup

general:
  theme: modus-vivendi        # or modus-operandi
  font: JetBrains Mono
  font_size: 11
  safe_mode: false

completion:
  backend: corfu              # or company
  delay: 0.1
  snippet_expansion: true

lsp:
  enable: true
  inlay_hints: true
  large_file_threshold: 100000
  semantic_tokens: true

performance:
  gc_threshold: 16777216      # 16MB
  startup_time_target: 2.0
  native_comp_jobs: 4

features:
  dashboard: true
  which_key: true
  beacon: true
  rainbow_delimiters: true

security:
  tls_verify: true
  package_signatures: allow-unsigned

telemetry:
  enabled: true               # Local only, never sent
  usage_stats: true

# Environment-specific settings
environments:
  work:
    org_directory: ~/work/org
    project_paths:
      - ~/work/projects
  home:
    org_directory: ~/personal/org
    project_paths:
      - ~/code
      - ~/projects
"))
    (message "Created config template at %s" emacs-ide-config-file)))

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
  (message "Configuration reloaded"))

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
        (princ (format "  %-20s = %s\n"
                      (symbol-name (car item))
                      (cdr item)))))))

(provide 'emacs-ide-config)
;;; emacs-ide-config.el ends here
