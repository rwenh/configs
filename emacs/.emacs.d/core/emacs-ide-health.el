;;; emacs-ide-health.el --- Enterprise Health Check System -*- lexical-binding: t -*-
;;; Commentary:
;;; Health monitoring and auto-recovery.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.3 to 3.0.4.
;;;   - FIX-STARTUP-REGISTRY: emacs-ide-health-check-startup now iterates
;;;     emacs-ide-health-checks (the live registry) instead of a hardcoded
;;;     parallel list. New checks registered by future modules are included
;;;     in the startup scan automatically.
;;;   - FIX-CUSTOM-FILE: emacs-ide-health-check-security replaced
;;;     (boundp 'custom-file) with (stringp custom-file) — custom-file is
;;;     always bound (built-in, defaults to nil), so (string= nil ...) threw
;;;     wrong-type-argument. Mirrors FIX-CUSTOM in init.el v3.0.4.
;;;   - FIX-RESULT-ORDER: emacs-ide-health-check-all now passes the
;;;     already-reversed results list to emacs-ide-health-display-results so
;;;     the report buffer and emacs-ide-health-results share the same order.
;;;   - FIX-FIXABLE: Removed :fixable t from checks that have no
;;;     :fix-function — the flag was a false promise; auto-fix silently did
;;;     nothing for these checks.
;;;   - FIX-AUTOFIX-NAME: emacs-ide-health-auto-fix variable renamed to
;;;     emacs-ide-health-enable-auto-fix to avoid name collision with the
;;;     interactive command emacs-ide-health-auto-fix.
;;;   - FIX-GC-THRESHOLD: gc-count > 50 threshold is now a configurable
;;;     defvar emacs-ide-health-gc-startup-threshold with an explaining
;;;     comment.
;;;   - FIX-AG: Removed "ag" from recommended tools — project uses
;;;     ripgrep/fd exclusively per config.yml; ag is never expected.
;;;   - FIX-PKG-BACKEND: emacs-ide-health-check-packages now reads
;;;     emacs-ide-completion-backend to check the right completion package
;;;     instead of always checking corfu unconditionally.
;;;   - FIX-INTERVAL: emacs-ide-health-check-interval now wired to a
;;;     repeating idle timer so periodic health checks actually fire.
;;;   - FIX-LSP-CONFIG: emacs-ide-health-check-lsp-servers now reads
;;;     enabled languages and their lsp-server from config, covering all
;;;     enabled langs rather than only 5 hardcoded Tier-1 servers.
;;;   - FIX-FMT-CONFIG: emacs-ide-health-check-formatters now reads enabled
;;;     languages and their formatter from config similarly.
;;; Fixes vs 2.2.2:
;;;   - C-16 (HIGH): Health check registry accumulated duplicate entries on
;;;     every config reload. Each top-level (emacs-ide-health-register-check ...)
;;;     call pushed onto `emacs-ide-health-checks` unconditionally. On a
;;;     second load of this file (e.g. via M-x emacs-ide-config-reload or
;;;     any module re-require), every check was registered twice.
;;;     emacs-ide-health-check-all then ran each check twice, doubling the
;;;     error/warning counts and corrupting the summary string shown in the
;;;     modeline and dashboard.
;;;     Fix: emacs-ide-health-register-check now checks for an existing
;;;     entry with the same name before pushing. This makes registration
;;;     idempotent: a second load is a no-op for already-registered checks.
;;;   - C-17 (MEDIUM): emacs-ide-health-auto-fix was declared (interactive)
;;;     but takes a `results` argument. When called via M-x it received nil
;;;     for results, looped over nothing, and printed "No auto-fixable issues
;;;     found" even when fixable issues existed. This was confusing because
;;;     the function IS the right thing to call from the health report buffer.
;;;     Fix: split into a worker function `emacs-ide-health--auto-fix-results`
;;;     (takes results, not interactive) and an interactive command
;;;     `emacs-ide-health-auto-fix` that uses the last saved results.
;;; Code:

(require 'cl-lib)

(defvar emacs-ide-health-check-on-startup t)
;; FIX-AUTOFIX-NAME: renamed from emacs-ide-health-auto-fix to avoid
;; collision with the interactive command emacs-ide-health-auto-fix.
(defvar emacs-ide-health-enable-auto-fix t)
(defvar emacs-ide-health-check-interval (* 60 60)
  "Interval in seconds for periodic idle health checks. Default: 1 hour.")
;; FIX-GC-THRESHOLD: Configurable threshold for startup GC count warning.
;; 50 is a reasonable baseline for a config with ~150 packages; raise if
;; you see spurious warnings on a clean install.
(defvar emacs-ide-health-gc-startup-threshold 50
  "GC count during startup above which a performance warning is issued.")
(defvar emacs-ide-health-last-check nil)
(defvar emacs-ide-health-results nil)
(defvar emacs-ide-health--last-errors 0)
(defvar emacs-ide-health--last-warnings 0)
;; FIX-INTERVAL: Timer handle for the periodic health check.
(defvar emacs-ide-health--periodic-timer nil
  "Timer for periodic idle health checks.")

(defun emacs-ide-health--summary-string ()
  "Return a short status string for modeline/dashboard use."
  (cond
   ((null emacs-ide-health-last-check) "? unchecked")
   ((> emacs-ide-health--last-errors 0)
    (format "✗ %d error%s"
            emacs-ide-health--last-errors
            (if (= emacs-ide-health--last-errors 1) "" "s")))
   ((> emacs-ide-health--last-warnings 0)
    (format "⚠ %d warning%s"
            emacs-ide-health--last-warnings
            (if (= emacs-ide-health--last-warnings 1) "" "s")))
   (t "✓ Healthy")))

(defun emacs-ide-health--update-counts (errors warnings)
  "Store ERRORS and WARNINGS and refresh modeline."
  (setq emacs-ide-health--last-errors   errors
        emacs-ide-health--last-warnings warnings)
  (force-mode-line-update t))

(defvar emacs-ide-health-checks nil
  "Registry of health check functions.")

(defun emacs-ide-health-register-check (name fn)
  "Register health check FN under NAME.
C-16 FIX: Idempotent — if a check with NAME is already registered
it is replaced in-place rather than pushed again. This prevents
duplicate registrations when the module is reloaded."
  (let ((existing (assoc name emacs-ide-health-checks)))
    (if existing
        (setcdr existing fn)
      (push (cons name fn) emacs-ide-health-checks))))

;; ============================================================================
;; INTERNAL HELPERS
;; ============================================================================
(defun emacs-ide-health--find-phase (phase-name)
  "Return elapsed time for PHASE-NAME, or nil."
  (when (and (boundp 'emacs-ide--startup-phases)
             emacs-ide--startup-phases)
    (cdr (assoc phase-name emacs-ide--startup-phases))))

;; ============================================================================
;; SYSTEM TOOLS CHECK
;; ============================================================================
(defun emacs-ide-health-check-system-tools ()
  "Check essential system tools."
  (let ((required '("git" "grep" "find"))
        ;; FIX-AG: removed "ag" — project uses ripgrep/fd per config.yml
        (recommended '("rg" "fd"))
        (missing-required '())
        (missing-recommended '()))
    (dolist (tool required)
      (unless (executable-find tool)
        (push tool missing-required)))
    (dolist (tool recommended)
      (unless (executable-find tool)
        (push tool missing-recommended)))
    (cond
     (missing-required
      (list :status 'error
            :message (format "Missing required tools: %s"
                             (string-join (nreverse missing-required) ", "))
            :details missing-required))
     (missing-recommended
      (list :status 'warning
            :message (format "Missing recommended tools: %s"
                             (string-join (nreverse missing-recommended) ", "))
            :details missing-recommended))
     (t (list :status 'ok :message "All system tools available")))))

(emacs-ide-health-register-check 'system-tools #'emacs-ide-health-check-system-tools)

;; ============================================================================
;; LSP SERVERS CHECK
;; ============================================================================
(defun emacs-ide-health--find-executable (candidates)
  "Return first executable in CANDIDATES found on PATH, or nil."
  (cl-some #'executable-find candidates))

(defun emacs-ide-health-check-lsp-servers ()
  "Check LSP server availability for all enabled languages.
FIX-LSP-CONFIG: Reads enabled languages and their configured lsp-server
from emacs-ide-config-data (lang-settings section) rather than checking
only 5 hardcoded Tier-1 servers. Falls back to the hardcoded list if
config is not yet loaded."
  (let* (;; Build server list from config if available, else use Tier-1 fallback
         (servers
          (if (and (boundp 'emacs-ide-config-data) emacs-ide-config-data
                   (fboundp 'emacs-ide-config-get-nested))
              (let ((lang-settings (cdr (assoc 'lang-settings emacs-ide-config-data)))
                    (languages     (cdr (assoc 'languages     emacs-ide-config-data)))
                    (result '()))
                (dolist (lang-entry lang-settings)
                  (let* ((lang    (car lang-entry))
                         (enabled (cdr (assoc lang languages)))
                         ;; absent key defaults to enabled per config.yml spec
                         (active  (if (assoc lang languages) enabled t))
                         (server  (cdr (assoc 'lsp-server (cdr lang-entry)))))
                    (when (and active server (stringp server))
                      (push (cons (symbol-name lang) (list server)) result))))
                (or result
                    '(("Python"     . ("pyright-langserver" "pyright"))
                      ("Rust"       . ("rust-analyzer"))
                      ("Go"         . ("gopls"))
                      ("TypeScript" . ("typescript-language-server"))
                      ("C/C++"      . ("clangd" "ccls")))))
            ;; Fallback when config not loaded
            '(("Python"     . ("pyright-langserver" "pyright"))
              ("Rust"       . ("rust-analyzer"))
              ("Go"         . ("gopls"))
              ("TypeScript" . ("typescript-language-server"))
              ("C/C++"      . ("clangd" "ccls")))))
         (available '())
         (missing '())
         (secondary-warnings '()))

    (dolist (s servers)
      (if (emacs-ide-health--find-executable (cdr s))
          (push (car s) available)
        (push (car s) missing)))

    (when (and (executable-find "typescript-language-server")
               (not (executable-find "tsserver")))
      (push "typescript-language-server present but `tsserver` (npm package `typescript`) missing — TS LSP will crash"
            secondary-warnings))

    (let ((base-status (cond ((null available) 'error)
                             (missing 'warning)
                             (t 'ok)))
          (base-msg (format "%d/%d LSP servers available"
                            (length available) (length servers))))
      (if secondary-warnings
          (list :status 'warning
                :message (concat base-msg " (runtime dependency issues)")
                :details (list :available available
                               :missing missing
                               :warnings secondary-warnings))
        (list :status base-status
              :message base-msg
              :details (list :available available :missing missing))))))

(emacs-ide-health-register-check 'lsp-servers #'emacs-ide-health-check-lsp-servers)

;; ============================================================================
;; FORMATTERS CHECK
;; ============================================================================
(defun emacs-ide-health-check-formatters ()
  "Check code formatter availability for all enabled languages.
FIX-FMT-CONFIG: Reads enabled languages and their configured formatter
from emacs-ide-config-data rather than checking only 4 hardcoded
Tier-1 formatters. Falls back to hardcoded list if config not loaded."
  (let* ((formatters
          (if (and (boundp 'emacs-ide-config-data) emacs-ide-config-data)
              (let ((lang-settings (cdr (assoc 'lang-settings emacs-ide-config-data)))
                    (languages     (cdr (assoc 'languages     emacs-ide-config-data)))
                    (result '()))
                (dolist (lang-entry lang-settings)
                  (let* ((lang      (car lang-entry))
                         (enabled   (cdr (assoc lang languages)))
                         (active    (if (assoc lang languages) enabled t))
                         (formatter (cdr (assoc 'formatter (cdr lang-entry)))))
                    (when (and active formatter (stringp formatter))
                      (push (cons formatter (symbol-name lang)) result))))
                (or result
                    '(("black"    . "Python")
                      ("prettier" . "JavaScript/TypeScript")
                      ("rustfmt"  . "Rust")
                      ("gofmt"    . "Go"))))
            '(("black"    . "Python")
              ("prettier" . "JavaScript/TypeScript")
              ("rustfmt"  . "Rust")
              ("gofmt"    . "Go"))))
         (missing '()))
    (dolist (f formatters)
      (unless (executable-find (car f))
        (push (format "%s (%s)" (car f) (cdr f)) missing)))
    (if missing
        (list :status 'warning
              :message (format "%d formatters missing" (length missing))
              :details missing)
      (list :status 'ok :message "All formatters available"))))

(emacs-ide-health-register-check 'formatters #'emacs-ide-health-check-formatters)

;; ============================================================================
;; PERFORMANCE CHECK
;; ============================================================================
(defun emacs-ide-health-check-performance ()
  "Check performance metrics."
  (let* ((startup-time (or (emacs-ide-health--find-phase "startup-complete")
                           (emacs-ide-health--find-phase "feature-modules")))
         (gc-count (- gcs-done
                      (if (boundp 'emacs-ide--gc-count-start)
                          emacs-ide--gc-count-start 0)))
         (warnings '()))
    (when (and startup-time
               (> startup-time (or (and (boundp 'emacs-ide-startup-time-target)
                                        emacs-ide-startup-time-target)
                                   3.0)))
      (push (format "Slow startup: %.2fs" startup-time) warnings))
    ;; FIX-GC-THRESHOLD: Use configurable threshold defvar rather than
    ;; magic number 50. Adjust emacs-ide-health-gc-startup-threshold if
    ;; you see spurious warnings on a clean install with many packages.
    (when (> gc-count emacs-ide-health-gc-startup-threshold)
      (push (format "Excessive GC during startup: %d (threshold: %d)"
                    gc-count emacs-ide-health-gc-startup-threshold)
            warnings))
    (if warnings
        (list :status 'warning
              :message (format "%d performance issues" (length warnings))
              :details warnings)
      (list :status 'ok :message "Performance within targets"))))

(emacs-ide-health-register-check 'performance #'emacs-ide-health-check-performance)

;; ============================================================================
;; PACKAGES CHECK
;; ============================================================================
(defun emacs-ide-health-check-packages ()
  "Check package health.
FIX-PKG-BACKEND: Checks the configured completion backend
(emacs-ide-completion-backend) rather than always checking corfu,
so switching to company in config.yml does not produce false warnings."
  (let* ((backend (if (boundp 'emacs-ide-completion-backend)
                      emacs-ide-completion-backend
                    'corfu))
         (base-pkgs '(use-package which-key projectile magit vertico))
         (pkgs (append base-pkgs (list backend)))
         (issues '()))
    (dolist (pkg pkgs)
      (unless (or (featurep pkg)
                  (locate-library (symbol-name pkg)))
        (push (format "Package not found: %s" pkg) issues)))
    (if issues
        (list :status 'warning
              :message (format "%d package warnings" (length issues))
              :details issues)
      (list :status 'ok :message "All packages healthy"))))

(emacs-ide-health-register-check 'packages #'emacs-ide-health-check-packages)

;; ============================================================================
;; SECURITY CHECK
;; ============================================================================
(defun emacs-ide-health-check-security ()
  "Check security configuration."
  (let ((warnings '()))
    (unless (and (boundp 'gnutls-verify-error) gnutls-verify-error)
      (push "TLS verification not enforced" warnings))
    ;; FIX-CUSTOM-FILE: (boundp 'custom-file) is always t — custom-file is a
    ;; built-in variable that defaults to nil. (string= nil ...) throws
    ;; wrong-type-argument. Use (stringp custom-file) to safely handle nil.
    (when (and (stringp custom-file)
               (string= custom-file user-init-file))
      (push "Custom file should be separate from init.el" warnings))
    (unless (executable-find "gpg")
      (push "GPG not found (needed for auth-source encryption)" warnings))
    (if warnings
        (list :status 'warning
              :message (format "%d security warnings" (length warnings))
              :details warnings)
      (list :status 'ok :message "Security configuration OK"))))

(emacs-ide-health-register-check 'security #'emacs-ide-health-check-security)

;; ============================================================================
;; HEALTH RUNNER
;; ============================================================================
(defun emacs-ide-health-run-check (check-name check-fn)
  "Run CHECK-FN and return cons of CHECK-NAME and result."
  (condition-case err
      (cons check-name (funcall check-fn))
    (error
     (cons check-name
           (list :status 'error
                 :message (format "Check crashed: %s"
                                  (error-message-string err)))))))

(defun emacs-ide-health-check-all ()
  "Run all registered health checks and display results."
  (interactive)
  (message "🏥 Running health checks...")
  (let ((results '()) (errors 0) (warnings 0))
    (dolist (check emacs-ide-health-checks)
      (let* ((res (emacs-ide-health-run-check (car check) (cdr check)))
             (status (plist-get (cdr res) :status)))
        (push res results)
        (cond ((eq status 'error)   (cl-incf errors))
              ((eq status 'warning) (cl-incf warnings)))))
    ;; FIX-RESULT-ORDER: reverse once here and reuse for both display and save
    ;; so the report buffer and emacs-ide-health-results are in the same order.
    (let ((ordered (reverse results)))
      (setq emacs-ide-health-results ordered
            emacs-ide-health-last-check (current-time))
      (emacs-ide-health--update-counts errors warnings)
      (emacs-ide-health-display-results ordered errors warnings)
      ;; FIX-AUTOFIX-NAME: variable renamed to emacs-ide-health-enable-auto-fix
      (when (and emacs-ide-health-enable-auto-fix
                 (or (> errors 0) (> warnings 0))
                 (y-or-n-p (format "%d errors, %d warnings. Attempt auto-fix? "
                                   errors warnings)))
        (emacs-ide-health--auto-fix-results ordered))
      ordered)))

(defun emacs-ide-health--format-details (details)
  "Format DETAILS for display."
  (cond
   ((and (listp details) (keywordp (car details)))
    (let ((available (plist-get details :available))
          (missing   (plist-get details :missing))
          (warnings  (plist-get details :warnings)))
      (concat
       (when available
         (format "      Available: %s\n"
                 (mapconcat (lambda (x) (if (symbolp x) (symbol-name x) x))
                            available ", ")))
       (when missing
         (format "      Missing:   %s\n"
                 (mapconcat (lambda (x) (if (symbolp x) (symbol-name x) x))
                            missing ", ")))
       (when warnings
         (mapconcat (lambda (w) (format "      ⚠ %s\n" w)) warnings "")))))
   ((listp details)
    (mapconcat (lambda (d) (format "    - %s\n" d)) details ""))
   (t (format "    - %s\n" details))))

(defun emacs-ide-health-display-results (results errors warnings)
  "Display RESULTS with ERRORS and WARNINGS counts."
  (with-output-to-temp-buffer "*Health Check*"
    (princ "=== EMACS IDE HEALTH CHECK ===\n\n")
    (princ (format "Status: %s\n"
                   (cond ((> errors 0)   "❌ CRITICAL")
                         ((> warnings 0) "⚠️  WARNING")
                         (t              "✓ HEALTHY"))))
    (princ (format "Time: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
    (dolist (r results)
      (let* ((name    (car r))
             (res     (cdr r))
             (status  (plist-get res :status))
             (msg     (plist-get res :message))
             (details (plist-get res :details))
             (icon    (cond ((eq status 'ok)      "✓")
                            ((eq status 'warning) "⚠")
                            ((eq status 'error)   "✗")
                            (t "?"))))
        (princ (format "%s %s\n" icon (upcase (symbol-name name))))
        (princ (format "  %s\n" msg))
        (when details
          (princ "  Details:\n")
          (princ (emacs-ide-health--format-details details)))
        (princ "\n")))))

;; ============================================================================
;; AUTO-FIX
;; C-17 FIX: Split into worker (takes results) and interactive command
;; (uses last saved results). Previously the single (interactive) function
;; received nil from M-x and silently did nothing.
;; ============================================================================
(defun emacs-ide-health--auto-fix-results (results)
  "Worker: attempt auto-fix for each entry in RESULTS that has :fix-function."
  (let ((fixed 0))
    (dolist (res results)
      (let ((fix-fn (plist-get (cdr res) :fix-function)))
        (when (and fix-fn (functionp fix-fn))
          (condition-case err
              (progn (funcall fix-fn) (cl-incf fixed))
            (error (message "Auto-fix failed for %s: %s"
                            (car res) (error-message-string err)))))))
    (if (> fixed 0)
        (message "✓ Auto-fixed %d issues" fixed)
      (message "No auto-fixable issues found"))))

(defun emacs-ide-health-auto-fix ()
  "Attempt auto-fix using the results from the last health check.
C-17 FIX: This is now a zero-argument interactive command. It uses
`emacs-ide-health-results` (saved by the last emacs-ide-health-check-all
run) so M-x emacs-ide-health-auto-fix works correctly."
  (interactive)
  (if (null emacs-ide-health-results)
      (message "No health check results available. Run M-x emacs-ide-health-check-all first.")
    (emacs-ide-health--auto-fix-results emacs-ide-health-results)))

;; ============================================================================
;; STARTUP QUICK CHECK
;; ============================================================================
(defun emacs-ide-health-check-startup ()
  "Quick health check on startup (non-blocking).
FIX-STARTUP-REGISTRY: Now iterates emacs-ide-health-checks (the live
registry) instead of a hardcoded parallel list, so any check registered
by future modules is automatically included in the startup scan."
  (when emacs-ide-health-check-on-startup
    (run-with-idle-timer
     3 nil
     (lambda ()
       (let ((results '())
             (errors 0)
             (warnings 0))
         (dolist (chk emacs-ide-health-checks)
           (let* ((res    (emacs-ide-health-run-check (car chk) (cdr chk)))
                  (status (plist-get (cdr res) :status)))
             (push res results)
             (cond ((eq status 'error)   (cl-incf errors))
                   ((eq status 'warning) (cl-incf warnings)))))
         (let ((ordered (reverse results)))
           (setq emacs-ide-health-results    ordered
                 emacs-ide-health-last-check (current-time))
           (emacs-ide-health--update-counts errors warnings)
           (cond
            ((> errors 0)
             (message "🏥 Health: ✗ %d error%s found — M-x emacs-ide-health-check-all"
                      errors (if (= errors 1) "" "s")))
            ((> warnings 0)
             (message "🏥 Health: ⚠ %d warning%s — M-x emacs-ide-health-check-all"
                      warnings (if (= warnings 1) "" "s")))
            (t
             (message "🏥 Health: ✓ All checks passed")))))))))

;; ============================================================================
;; PERIODIC HEALTH CHECK TIMER
;; FIX-INTERVAL: Wire emacs-ide-health-check-interval to an actual repeating
;; idle timer. Previously the variable was defined but never used.
;; ============================================================================
(defun emacs-ide-health--start-periodic-timer ()
  "Start the periodic idle health check timer.
Cancels any existing timer first to avoid duplicates on reload."
  (when emacs-ide-health--periodic-timer
    (cancel-timer emacs-ide-health--periodic-timer)
    (setq emacs-ide-health--periodic-timer nil))
  (setq emacs-ide-health--periodic-timer
        (run-with-idle-timer emacs-ide-health-check-interval
                             :repeat
                             #'emacs-ide-health-check-startup)))

;; Start the timer after startup is complete so it doesn't compete
;; with the initial startup check.
(add-hook 'emacs-startup-hook #'emacs-ide-health--start-periodic-timer 110)

(provide 'emacs-ide-health)
;;; emacs-ide-health.el ends here
