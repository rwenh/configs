;;; emacs-ide-diagnose.el --- IDE Diagnostics & Health Reporting -*- lexical-binding: t -*-
;;; Commentary:
;;; Comprehensive diagnostics and health checking for the Emacs IDE.
;;; Provides detailed reports on loaded modules, LSP status, configuration,
;;; and system performance.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; CONFIGURATION
;; ============================================================================

(defvar emacs-ide-diagnose-buffer-name "*IDE Diagnostics*"
  "Buffer name for diagnostic reports.")

(defvar emacs-ide-diagnose-show-timestamps t
  "Whether to include timestamps in diagnostic output.")

;; ============================================================================
;; CORE DIAGNOSTICS
;; ============================================================================

(defun emacs-ide-diagnose ()
  "Run comprehensive IDE diagnostics and display full report."
  (interactive)
  (let ((buffer (get-buffer-create emacs-ide-diagnose-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== EMACS IDE v3.0.4 DIAGNOSTICS ===\n")
      (insert (format "Emacs %s | %s\n\n"
                      emacs-version
                      (format-time-string "%Y-%m-%d %H:%M")))
      
      ;; Core modules
      (emacs-ide-diagnose--core-modules)
      (insert "\n")
      
      ;; Feature modules
      (emacs-ide-diagnose--feature-modules)
      (insert "\n")
      
      ;; Key functions
      (emacs-ide-diagnose--key-functions)
      (insert "\n")
      
      ;; Mode status
      (emacs-ide-diagnose--mode-status)
      (insert "\n")
      
      ;; Performance
      (emacs-ide-diagnose--performance-report)
      (insert "\n")
      
      ;; Configuration validation
      (emacs-ide-diagnose--config-validation)
      
      (goto-char (point-min)))
    (display-buffer buffer)))

;; ============================================================================
;; CORE MODULES DIAGNOSTICS
;; ============================================================================

(defun emacs-ide-diagnose--core-modules ()
  "Diagnose core module status."
  (insert "CORE MODULES:\n")
  
  (let ((core-modules
         '((emacs-ide-config           . emacs-ide-config-load)
           (emacs-ide-health           . emacs-ide-health-run-checks)
           (emacs-ide-recovery         . emacs-ide-recovery-log)
           (emacs-ide-package          . emacs-ide-package-report)
           (emacs-ide-profiler         . emacs-ide-profile-start)
           (emacs-ide-security         . emacs-ide-security-check)
           (emacs-ide-telemetry        . emacs-ide-telemetry-report)
           (emacs-ide-test             . emacs-ide-run-tests)
           (emacs-ide-spot-check       . emacs-ide-spot-check))))
    
    (dolist (module core-modules)
      (let ((feature (car module))
            (cmd (cdr module)))
        (insert (format "  %s emacs-ide-%-30s "
                        (if (featurep feature) "✓" "✗")
                        (symbol-name feature)))
        (insert (format "feature:%s    "
                        (if (featurep feature) "yes" "no")))
        (insert (format "cmd %s: %s\n"
                        (symbol-name cmd)
                        (if (fboundp cmd) "ok" "MISSING")))))))

;; ============================================================================
;; FEATURE MODULES DIAGNOSTICS
;; ============================================================================

(defun emacs-ide-diagnose--feature-modules ()
  "Diagnose feature module status."
  (insert "FEATURE MODULES:\n")
  
  (let ((feature-modules
         '(ui-core ui-theme ui-modeline ui-dashboard ui-workspace
           completion-core completion-snippets editing-core core-dev
           tools-lsp tools-project tools-git tools-terminal tools-format
           apheleia-langs-patch tools-org tools-spelling tools-notes
           tools-rest tools-test-runner-registry tools-test debug-core
           tools-repl tools-project-detect tools-hydra keybindings)))
    
    (dolist (feature feature-modules)
      (let* ((loaded (featurep feature))
             (file (locate-library (symbol-name feature)))
             (status (cond
                      ((and loaded file) "ok")
                      (loaded "ok")
                      (file "loadable")
                      (t "MISSING"))))
        (insert (format "  %s %-40s file:%s    feature:%s    %s\n"
                        (if loaded "✓" "✗")
                        (symbol-name feature)
                        (if file "yes" "no")
                        (if loaded "yes" "no")
                        status))))))

;; ============================================================================
;; KEY FUNCTION SPOT-CHECK
;; ============================================================================

(defun emacs-ide-diagnose--key-functions ()
  "Check critical IDE functions are defined."
  (insert "KEY FUNCTION SPOT-CHECK:\n")
  
  (let ((key-functions
         '(emacs-ide-health-check-all
           emacs-ide-run-tests
           emacs-ide-toggle-theme
           emacs-ide-detect-show-status
           emacs-ide-repl-status
           emacs-ide-test-runner-status
           emacs-ide-workspace-status
           emacs-ide-lsp-status
           emacs-ide-startup-report
           emacs-ide-show-version
           emacs-ide-config-reload
           emacs-ide-diagnose)))
    
    (dolist (func key-functions)
      (insert (format "  %s %s\n"
                      (if (fboundp func) "✓" "✗")
                      (symbol-name func))))))

;; ============================================================================
;; MODE STATUS
;; ============================================================================

(defun emacs-ide-diagnose--mode-status ()
  "Check critical minor modes are enabled."
  (insert "MODE STATUS:\n")
  
  (let ((modes '(electric-pair-mode
                 show-paren-mode
                 delete-selection-mode
                 display-line-numbers-mode
                 winner-mode)))
    
    (dolist (mode modes)
      (insert (format "  %s %s\n"
                      (if (and (boundp mode) (symbol-value mode)) "✓" "✗")
                      (symbol-name mode))))))

;; ============================================================================
;; PERFORMANCE REPORT
;; ============================================================================

(defun emacs-ide-diagnose--performance-report ()
  "Generate performance diagnostics."
  (insert "PERFORMANCE METRICS:\n")
  
  (insert (format "  GC threshold: %d bytes\n" gc-cons-threshold))
  (insert (format "  GC collections: %d\n" gcs-done))
  (insert (format "  Memory used: %s\n" (emacs-ide-diagnose--format-memory)))
  
  (when (boundp 'emacs-ide-startup-time-target)
    (insert (format "  Startup target: %.1fs\n" emacs-ide-startup-time-target)))
  
  (when (boundp 'after-init-time)
    (let ((startup-time (float-time after-init-time)))
      (insert (format "  Actual startup: %.2fs\n" startup-time))))
  
  (when (boundp 'emacs-ide-native-comp-jobs)
    (insert (format "  Native comp jobs: %d\n" emacs-ide-native-comp-jobs))))

(defun emacs-ide-diagnose--format-memory ()
  "Format memory usage in human-readable format.
FIX-MEMORY-INFO: memory-info does not exist in Emacs. Use garbage-collect
return value or process-attributes as a safe cross-platform alternative."
  (condition-case nil
      (let* ((mem-info (garbage-collect))
             ;; mem-info is a list of (TYPE USED FREE) — sum the USED fields
             ;; Each element: (TYPE SIZE-USED SIZE-FREE) where size is in bytes for
             ;; conses/strings etc. Use memory-limit as a coarse total.
             (limit-kb (memory-limit))
             (used-kb  (if (fboundp 'emacs-pid)
                           ;; Rough estimate via process memory on Linux
                           (let ((statm (format "/proc/%d/statm" (emacs-pid))))
                             (if (file-readable-p statm)
                                 (with-temp-buffer
                                   (insert-file-contents statm)
                                   (* (string-to-number (car (split-string
                                                               (buffer-string))))
                                      4))   ; pages → KB (approx, 4KB pages)
                               0))
                           0)))
        (if (> used-kb 0)
            (format "~%d MB (limit %d MB)"
                    (/ used-kb 1024)
                    (/ limit-kb 1024))
          (format "limit %d MB" (/ limit-kb 1024))))
    (error "N/A")))

;; ============================================================================
;; CONFIGURATION VALIDATION
;; ============================================================================

(defun emacs-ide-diagnose--config-validation ()
  "Validate configuration integrity."
  (insert "CONFIGURATION VALIDATION:\n")
  
  (let ((config-checks
         '((emacs-ide-config-data . "Config data loaded")
           (emacs-ide-config-environment . "Environment detected")
           (emacs-ide-config-loaded-p . "Config marked loaded")
           (emacs-ide-lsp-enable . "LSP enabled")
           (emacs-ide-theme . "Theme variable set"))))
    
    (dolist (check config-checks)
      (let ((var (car check))
            (desc (cdr check)))
        (insert (format "  %s %s\n"
                        (if (boundp var) "✓" "✗")
                        desc))))))

;; ============================================================================
;; SUMMARY REPORT
;; ============================================================================

(defun emacs-ide-diagnose--generate-summary ()
  "Generate diagnostic summary."
  (let ((core-ok (cl-count-if #'featurep
                              '(emacs-ide-config
                                emacs-ide-health
                                emacs-ide-recovery)))
        (features-ok (cl-count-if #'featurep
                                  '(ui-core completion-core editing-core
                                    core-dev tools-lsp tools-hydra keybindings)))
        (functions-ok (cl-count-if #'fboundp
                                   '(emacs-ide-run-tests
                                     emacs-ide-toggle-theme
                                     emacs-ide-lsp-status))))
    
    (format "\n✅ SUMMARY: %d core modules, %d feature modules, %d key functions"
            core-ok features-ok functions-ok)))

;; ============================================================================
;; LSP DIAGNOSTICS
;; ============================================================================

(defun emacs-ide-diagnose-lsp ()
  "Run LSP-specific diagnostics."
  (interactive)
  (let ((buffer (get-buffer-create "*LSP Diagnostics*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== LSP DIAGNOSTICS ===\n\n")
      
      (insert (format "LSP Enable Status: %s\n"
                      (if (bound-and-true-p emacs-ide-lsp-enable) "ENABLED" "DISABLED")))
      (insert (format "LSP Mode Active: %s\n\n"
                      (if (bound-and-true-p lsp-mode) "YES" "NO")))
      
      (insert "Registered LSP Servers:\n")
      (if (boundp 'lsp-enabled-clients)
          (dolist (client lsp-enabled-clients)
            (insert (format "  • %s\n" client)))
        (insert "  (No clients registered)\n"))
      
      (insert "\nLSP Configuration:\n")
      (insert (format "  lsp-idle-delay: %s\n" (or (bound-and-true-p lsp-idle-delay) "default")))
      (insert (format "  lsp-enable-folding: %s\n" (or (bound-and-true-p lsp-enable-folding) "default")))
      (insert (format "  lsp-enable-snippet: %s\n" (or (bound-and-true-p lsp-enable-snippet) "default")))
      (insert (format "  lsp-enable-symbol-highlighting: %s\n" 
                      (or (bound-and-true-p lsp-enable-symbol-highlighting) "default")))
      
      (insert "\nLSP UI Status:\n")
      (insert (format "  lsp-ui enabled: %s\n" (and (featurep 'lsp-ui) "YES")))
      (insert (format "  lsp-treemacs enabled: %s\n" (and (featurep 'lsp-treemacs) "YES")))
      
      (goto-char (point-min)))
    (display-buffer buffer)))

;; ============================================================================
;; LANGUAGE SUPPORT DIAGNOSTICS
;; ============================================================================

(defun emacs-ide-diagnose-languages ()
  "Run language support diagnostics."
  (interactive)
  (let ((buffer (get-buffer-create "*Language Diagnostics*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== LANGUAGE SUPPORT DIAGNOSTICS ===\n\n")
      
      (let ((languages '((python "pyright")
                         (rust "rust-analyzer")
                         (go "gopls")
                         (typescript "typescript-language-server")
                         (javascript "typescript-language-server")
                         (c "clangd")
                         (cpp "clangd")
                         (java "jdtls"))))
        
        (dolist (lang languages)
          (let ((name (car lang))
                (server (cadr lang)))
            (insert (format "%s:\n" (upcase (symbol-name name))))
            (insert (format "  LSP Server: %s\n" server))
            (insert (format "  Available: %s\n" 
                            (if (executable-find server) "YES ✓" "NO ✗")))
            
            (when (fboundp 'emacs-ide-dev-lang-enabled-p)
              (insert (format "  Enabled in config: %s\n"
                              (if (emacs-ide-dev-lang-enabled-p (symbol-name name)) 
                                  "YES" "NO"))))
            (insert "\n")))))
    
    (display-buffer buffer)))

;; ============================================================================
;; QUICK STATUS
;; ============================================================================

(defun emacs-ide-diagnose-quick ()
  "Print quick diagnostic status to minibuffer."
  (interactive)
  (let* ((core-loaded (featurep 'emacs-ide-config))
         (lsp-enabled (bound-and-true-p emacs-ide-lsp-enable))
         (config-loaded (bound-and-true-p emacs-ide-config-loaded-p))
         ;; FIX-PKG-COUNT: package.el is disabled (package-enable-at-startup nil).
         ;; package-activated-list is always empty. Use straight.el recipe cache.
         (pkg-count (condition-case nil
                        (cond
                         ((and (boundp 'straight--recipe-cache)
                               (hash-table-p straight--recipe-cache))
                          (hash-table-count straight--recipe-cache))
                         ((file-directory-p (expand-file-name
                                             "straight/build" user-emacs-directory))
                          (length (directory-files
                                   (expand-file-name "straight/build"
                                                     user-emacs-directory)
                                   nil "^[^.]")))
                         (t 0))
                      (error 0))))
    (message "IDE Status: %s | LSP: %s | Config: %s | Packages: %d"
             (if core-loaded "✓" "✗")
             (if lsp-enabled "✓" "✗")
             (if config-loaded "✓" "✗")
             pkg-count)))

(provide 'emacs-ide-diagnose)
;;; emacs-ide-diagnose.el ends here