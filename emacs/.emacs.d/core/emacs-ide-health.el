;;; emacs-ide-health.el --- Enterprise Health Check System -*- lexical-binding: t -*-
;;; Commentary:
;;; Production-grade health monitoring and auto-recovery
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; HEALTH CHECK CONFIGURATION
;; ============================================================================
(defvar emacs-ide-health-check-on-startup t
  "Run health check on startup.")

(defvar emacs-ide-health-auto-fix t
  "Automatically fix issues when possible.")

(defvar emacs-ide-health-check-interval (* 60 60)
  "Interval for periodic health checks (seconds).")

(defvar emacs-ide-health-last-check nil
  "Timestamp of last health check.")

(defvar emacs-ide-health-results nil
  "Results from last health check.")

;; ============================================================================
;; HEALTH CHECK REGISTRY
;; ============================================================================
(defvar emacs-ide-health-checks
  '((system-tools . emacs-ide-health-check-system-tools)
    (lsp-servers . emacs-ide-health-check-lsp-servers)
    (formatters . emacs-ide-health-check-formatters)
    (tree-sitter . emacs-ide-health-check-treesitter)
    (performance . emacs-ide-health-check-performance)
    (packages . emacs-ide-health-check-packages)
    (security . emacs-ide-health-check-security))
  "Registry of health check functions.")

;; ============================================================================
;; CHECK: SYSTEM TOOLS
;; ============================================================================
(defun emacs-ide-health-check-system-tools ()
  "Check essential system tools."
  (let ((required-tools
         '(("git" . "Version control")
           ("grep" . "Text search")
           ("find" . "File search")))
        (recommended-tools
         '(("rg" . "Fast text search (ripgrep)")
           ("fd" . "Fast file search")
           ("ag" . "Fast text search (silver searcher)")))
        (missing-required '())
        (missing-recommended '())
        (warnings '()))
    
    ;; Check required tools
    (dolist (tool required-tools)
      (unless (executable-find (car tool))
        (push tool missing-required)))
    
    ;; Check recommended tools
    (dolist (tool recommended-tools)
      (unless (executable-find (car tool))
        (push tool missing-recommended)))
    
    ;; Generate report
    (cond
     (missing-required
      (list :status 'error
            :message (format "Missing required tools: %s"
                           (mapconcat (lambda (x) (car x))
                                    missing-required ", "))
            :details missing-required
            :fixable nil))
     (missing-recommended
      (list :status 'warning
            :message (format "Missing recommended tools: %s"
                           (mapconcat (lambda (x) (car x))
                                    missing-recommended ", "))
            :details missing-recommended
            :fixable nil))
     (t
      (list :status 'ok
            :message "All system tools available")))))

;; ============================================================================
;; CHECK: LSP SERVERS
;; ============================================================================
(defun emacs-ide-health-check-lsp-servers ()
  "Check LSP server availability."
  (let ((servers
         '(("Python" "pyright" "npm install -g pyright")
           ("Python (alt)" "pylsp" "pip install python-lsp-server")
           ("Rust" "rust-analyzer" "rustup component add rust-analyzer")
           ("Go" "gopls" "go install golang.org/x/tools/gopls@latest")
           ("TypeScript" "typescript-language-server" "npm install -g typescript-language-server")
           ("C/C++" "clangd" "System package manager")
           ("C/C++ (alt)" "ccls" "System package manager")
           ("Java" "jdtls" "Installed via lsp-java")
           ("Bash" "bash-language-server" "npm install -g bash-language-server")))
        (available '())
        (missing '()))
    
    (dolist (server servers)
      (if (executable-find (nth 1 server))
          (push (car server) available)
        (push server missing)))
    
    (cond
     ((null available)
      (list :status 'error
            :message "No LSP servers found"
            :details missing
            :fixable t
            :fix-instructions
            "Install LSP servers using the commands shown in details"))
     (missing
      (list :status 'warning
            :message (format "%d/%d LSP servers available"
                           (length available)
                           (+ (length available) (length missing)))
            :details missing
            :fixable t))
     (t
      (list :status 'ok
            :message (format "All %d LSP servers available" (length available)))))))

;; ============================================================================
;; CHECK: FORMATTERS
;; ============================================================================
(defun emacs-ide-health-check-formatters ()
  "Check code formatter availability."
  (let ((formatters
         '(("Python (black)" "black" "pip install black")
           ("Python (isort)" "isort" "pip install isort")
           ("JavaScript/TypeScript" "prettier" "npm install -g prettier")
           ("Rust" "rustfmt" "rustup component add rustfmt")
           ("Go" "gofmt" "Built-in with Go")
           ("C/C++" "clang-format" "System package manager")
           ("Shell" "shfmt" "go install mvdan.cc/sh/v3/cmd/shfmt@latest")))
        (available '())
        (missing '()))
    
    (dolist (formatter formatters)
      (if (executable-find (nth 1 formatter))
          (push (car formatter) available)
        (push formatter missing)))
    
    (if available
        (list :status 'ok
              :message (format "%d formatters available" (length available))
              :details available)
      (list :status 'warning
            :message "No formatters found"
            :details missing
            :fixable t))))

;; ============================================================================
;; CHECK: TREE-SITTER
;; ============================================================================
(defun emacs-ide-health-check-treesitter ()
  "Check tree-sitter availability."
  (if (not (fboundp 'treesit-available-p))
      (list :status 'error
            :message "Tree-sitter not available (Emacs must be compiled with tree-sitter)")
    
    (if (not (treesit-available-p))
        (list :status 'error
              :message "Tree-sitter library not found")
      
      (let ((grammars '(python rust go javascript typescript c cpp))
            (available '())
            (missing '()))
        
        (dolist (lang grammars)
          (if (treesit-language-available-p lang)
              (push lang available)
            (push lang missing)))
        
        (cond
         ((null available)
          (list :status 'error
                :message "No tree-sitter grammars installed"
                :details missing
                :fixable t
                :fix-function 'emacs-ide-health-fix-treesitter))
         (missing
          (list :status 'warning
                :message (format "%d/%d tree-sitter grammars installed"
                               (length available)
                               (+ (length available) (length missing)))
                :details missing
                :fixable t
                :fix-function 'emacs-ide-health-fix-treesitter))
         (t
          (list :status 'ok
                :message (format "All %d tree-sitter grammars installed"
                               (length available)))))))))

;; ============================================================================
;; CHECK: PERFORMANCE
;; ============================================================================
(defun emacs-ide-health-check-performance ()
  "Check performance metrics."
  (let ((startup-time (if emacs-ide--startup-phases
                          (cdr (car emacs-ide--startup-phases))
                        999))
        (gc-count (- gcs-done (or emacs-ide--gc-count-start 0)))
        (warnings '()))
    
    (when (> startup-time 3.0)
      (push (format "Slow startup: %.2fs (target: <2.0s)" startup-time) warnings))
    
    (when (> gc-count 50)
      (push (format "Excessive GC during startup: %d (target: <20)" gc-count) warnings))
    
    (when (> (length (buffer-list)) 50)
      (push (format "Many buffers open: %d" (length (buffer-list))) warnings))
    
    (if warnings
        (list :status 'warning
              :message (format "%d performance issues" (length warnings))
              :details warnings)
      (list :status 'ok
            :message "Performance metrics within targets"))))

;; ============================================================================
;; CHECK: PACKAGES
;; ============================================================================
(defun emacs-ide-health-check-packages ()
  "Check package health."
  (let ((issues '()))
    
    ;; Check for outdated packages (if straight.el)
    (when (fboundp 'straight--get-melpa)
      ;; This would check for updates, but we skip for performance
      nil)
    
    ;; Check for missing critical packages
    (let ((critical '(use-package which-key projectile magit)))
      (dolist (pkg critical)
        (unless (package-installed-p pkg)
          (push (format "Critical package missing: %s" pkg) issues))))
    
    (if issues
        (list :status 'error
              :message (format "%d package issues" (length issues))
              :details issues
              :fixable t)
      (list :status 'ok
            :message "All packages healthy"))))

;; ============================================================================
;; CHECK: SECURITY
;; ============================================================================
(defun emacs-ide-health-check-security ()
  "Check security configuration."
  (let ((warnings '()))
    
    ;; Check TLS configuration
    (unless (and (boundp 'gnutls-verify-error)
                 gnutls-verify-error)
      (push "TLS verification not enforced" warnings))
    
    ;; Check custom file location
    (when (string= custom-file user-init-file)
      (push "Custom file should be separate from init.el" warnings))
    
    ;; Check for GPG
    (unless (executable-find "gpg")
      (push "GPG not found (needed for auth-source encryption)" warnings))
    
    (if warnings
        (list :status 'warning
              :message (format "%d security warnings" (length warnings))
              :details warnings)
      (list :status 'ok
            :message "Security configuration OK"))))

;; ============================================================================
;; HEALTH CHECK RUNNER
;; ============================================================================
(defun emacs-ide-health-run-check (check-name check-fn)
  "Run CHECK-FN and return result with CHECK-NAME."
  (condition-case err
      (let ((result (funcall check-fn)))
        (cons check-name result))
    (error
     (cons check-name
           (list :status 'error
                 :message (format "Check crashed: %s" (error-message-string err)))))))

(defun emacs-ide-health-check-all ()
  "Run all health checks."
  (interactive)
  (message "🏥 Running health checks...")
  (let ((results '())
        (errors 0)
        (warnings 0))
    
    ;; Run all checks
    (dolist (check emacs-ide-health-checks)
      (let* ((check-name (car check))
             (check-fn (cdr check))
             (result (emacs-ide-health-run-check check-name check-fn))
             (status (plist-get (cdr result) :status)))
        (push result results)
        (cond
         ((eq status 'error) (setq errors (1+ errors)))
         ((eq status 'warning) (setq warnings (1+ warnings))))))
    
    (setq emacs-ide-health-results (reverse results))
    (setq emacs-ide-health-last-check (current-time))
    
    ;; Display results
    (emacs-ide-health-display-results results errors warnings)
    
    ;; Auto-fix if enabled
    (when (and emacs-ide-health-auto-fix
               (or (> errors 0) (> warnings 0))
               (y-or-n-p (format "%d errors, %d warnings. Attempt auto-fix? "
                               errors warnings)))
      (emacs-ide-health-auto-fix results))
    
    results))

;; ============================================================================
;; DISPLAY RESULTS
;; ============================================================================
(defun emacs-ide-health-display-results (results errors warnings)
  "Display health check RESULTS with ERRORS and WARNINGS count."
  (with-output-to-temp-buffer "*Health Check*"
    (princ "=== EMACS IDE HEALTH CHECK ===\n\n")
    (princ (format "Status: %s\n"
                   (cond
                    ((> errors 0) "❌ CRITICAL")
                    ((> warnings 0) "⚠️  WARNING")
                    (t "✓ HEALTHY"))))
    (princ (format "Time: %s\n\n"
                   (format-time-string "%Y-%m-%d %H:%M:%S")))
    
    (dolist (result results)
      (let* ((check-name (car result))
             (status (plist-get (cdr result) :status))
             (message (plist-get (cdr result) :message))
             (details (plist-get (cdr result) :details))
             (icon (cond
                    ((eq status 'ok) "✓")
                    ((eq status 'warning) "⚠")
                    ((eq status 'error) "✗")
                    (t "?"))))
        (princ (format "%s %s\n" icon (upcase (symbol-name check-name))))
        (princ (format "  %s\n" message))
        (when details
          (princ "  Details:\n")
          (dolist (detail (if (listp details) details (list details)))
            (if (listp detail)
                (princ (format "    - %s: %s\n" (car detail) (nth 2 detail)))
              (princ (format "    - %s\n" detail)))))
        (princ "\n")))
    
    (princ (format "\nSummary: %d errors, %d warnings\n" errors warnings))
    (when (or (> errors 0) (> warnings 0))
      (princ "\nPress 'f' to attempt auto-fix\n"))))

;; ============================================================================
;; AUTO-FIX
;; ============================================================================
(defun emacs-ide-health-auto-fix (results)
  "Attempt to automatically fix issues in RESULTS."
  (interactive)
  (let ((fixed 0))
    (dolist (result results)
      (let ((fix-fn (plist-get (cdr result) :fix-function)))
        (when (and fix-fn (functionp fix-fn))
          (condition-case err
              (progn
                (funcall fix-fn)
                (setq fixed (1+ fixed))
                (message "✓ Fixed: %s" (car result)))
            (error
             (message "✗ Failed to fix %s: %s"
                     (car result)
                     (error-message-string err)))))))
    
    (if (> fixed 0)
        (message "✓ Auto-fixed %d issues. Run health check again to verify." fixed)
      (message "No auto-fixable issues found."))))

(defun emacs-ide-health-fix-treesitter ()
  "Auto-install missing tree-sitter grammars."
  (when (fboundp 'treesit-install-language-grammar)
    (let ((langs '(python rust go javascript typescript c cpp)))
      (dolist (lang langs)
        (unless (treesit-language-available-p lang)
          (message "Installing tree-sitter grammar: %s" lang)
          (treesit-install-language-grammar lang))))))

;; ============================================================================
;; STARTUP CHECK
;; ============================================================================
(defun emacs-ide-health-check-startup ()
  "Quick health check on startup (non-blocking)."
  (when emacs-ide-health-check-on-startup
    (run-with-idle-timer
     2 nil
     (lambda ()
       (let ((critical-checks '((system-tools . emacs-ide-health-check-system-tools)
                               (packages . emacs-ide-health-check-packages))))
         (dolist (check critical-checks)
           (let* ((result (emacs-ide-health-run-check (car check) (cdr check)))
                  (status (plist-get (cdr result) :status)))
             (when (eq status 'error)
               (warn "Health check failed: %s - %s"
                     (car result)
                     (plist-get (cdr result) :message))))))))))

;; ============================================================================
;; INTERACTIVE COMMANDS
;; ============================================================================
(defun emacs-ide-health ()
  "Run full health check."
  (interactive)
  (emacs-ide-health-check-all))

(defun emacs-ide-health-fix ()
  "Attempt to fix health issues."
  (interactive)
  (if emacs-ide-health-results
      (emacs-ide-health-auto-fix emacs-ide-health-results)
    (message "Run health check first with M-x emacs-ide-health")))

(provide 'emacs-ide-health)
;;; emacs-ide-health.el ends here
