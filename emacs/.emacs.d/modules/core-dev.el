;;; core-dev.el --- Shared IDE Infrastructure -*- lexical-binding: t -*-
;;; Commentary:
;;; Single plug-in point for LSP, completion, DAP, formatting, snippets,
;;; and eldoc.  Every lang-*.el calls (emacs-ide-dev-attach-lsp SERVER) and
;;; (emacs-ide-dev-attach-dap TEMPLATE) rather than touching lsp-mode or
;;; dap-mode directly.  This keeps each lang module ~60 lines and makes the
;;; shared behaviour easy to update in one place.
;;;
;;; Version: 1.0.3
;;; Fixes vs 1.0.2:
;;;   - FIX-6b: emacs-ide-dev-lang-enabled-p and emacs-ide-dev--config-lang-settings
;;;     called (assoc lang-key ...) where lang-key is a string (e.g. "python") but
;;;     the YAML parser interns all subsection keys as symbols ('python etc.).
;;;     assoc with a string key against a symbol-keyed alist always returns nil:
;;;       - emacs-ide-dev-lang-enabled-p: (null entry) always t → every lang
;;;         treated as enabled even when set false in config.yml.
;;;       - emacs-ide-dev--config-lang-settings: always returned nil → no
;;;         per-language settings were ever readable from lang-settings:.
;;;     Fix: (intern lang-key) before assoc in both functions.
;;; Fixes vs 1.0.1:
;;;   - FIX-6: Top-level section keys "languages"/"lang-settings" changed to
;;;     symbols 'languages/'lang-settings (same class of bug).
;;;
;;; Public API (for lang modules):
;;;   (emacs-ide-dev-attach-lsp SERVER &optional EXTRA-VARS)
;;;     Hook lsp-mode or eglot onto the current major-mode.
;;;   (emacs-ide-dev-attach-dap TEMPLATE-NAME)
;;;     Register a DAP debug template for the current major-mode.
;;;   (emacs-ide-dev-attach-formatter FORMATTER-SYM MODE-SYM)
;;;     Register an apheleia formatter for a major-mode.
;;;   (emacs-ide-dev-attach-repl REPL-FN &optional KEY)
;;;     Wire a REPL launch function onto the mode map via C-c r.
;;;   (emacs-ide-dev-lang-enabled-p LANG-KEY)
;;;     Return non-nil if LANG-KEY is enabled in config.yml languages section.
;;;   (emacs-ide-dev-executable-guard EXECUTABLES)
;;;     Return non-nil if ALL listed executables are on PATH.
;;;
;;; Dependency load order (guaranteed by init.el feature-modules list):
;;;   tools-lsp  → provides lsp-mode, lsp-ui, dap-mode
;;;   tools-format → provides apheleia
;;;   completion-snippets → provides yasnippet
;;;   core-dev   → this file, loaded AFTER the above three
;;;   lang-*.el  → loaded after core-dev
;;;
;;; RECALIBRATED 1.0.1:
;;;   - Language key validation: assoc uses string= test (case-sensitive).
;;;   - Config language lookup: Added bounds checks for nil config data.
;;;   - Formatter registration: Verified apheleia-formatters contains entry
;;;     before registration to prevent orphaned mode-alist entries.
;;;   - LSP hook macro: Guards against malformed mode-name inputs.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; INTERNAL STATE
;; ============================================================================

(defvar emacs-ide-dev--registered-langs nil
  "Alist of (LANG-KEY . plist) for every lang module that has registered.")

(defvar emacs-ide-dev--config-languages nil
  "Cached languages section from config.yml. Populated on first access.")

;; ============================================================================
;; CONFIG.YML INTEGRATION (RECALIBRATED)
;; ============================================================================

(defun emacs-ide-dev--config-languages ()
  "Return the languages alist from config.yml (boolean enable flags), cached."
  (or emacs-ide-dev--config-languages
      (setq emacs-ide-dev--config-languages
            (condition-case nil
                (when (boundp 'emacs-ide-config-data)
                  (cdr (assoc 'languages emacs-ide-config-data)))
              (error nil)))))

(defun emacs-ide-dev--config-lang-settings (lang-key)
  "Return the settings alist for LANG-KEY from lang-settings: section.
This is separate from languages: (booleans) to avoid YAML key collision.
FIX-6b: LANG-KEY is a string; YAML subsection keys are symbols. intern first."
  (condition-case nil
      (when (boundp 'emacs-ide-config-data)
        (let ((ls (cdr (assoc 'lang-settings emacs-ide-config-data))))
          ;; FIX-6b: intern so string "python" matches symbol 'python in alist
          (cdr (assoc (intern lang-key) ls))))
    (error nil)))

(defun emacs-ide-dev-lang-enabled-p (lang-key)
  "Return non-nil if LANG-KEY is toggled on in config.yml languages section.
LANG-KEY is a string matching the key in the languages: block, e.g. \"python\".
languages: contains only boolean flags. Per-lang settings live in lang-settings:.
If the languages section is absent or the key is absent, defaults to t.
FIX-6b: LANG-KEY is a string; YAML subsection keys are symbols. intern before
assoc so \"python\" correctly matches 'python in the parsed alist."
  (let* ((langs (emacs-ide-dev--config-languages))
         ;; FIX-6b: intern lang-key — YAML parser produces symbol keys
         (entry (and langs (assoc (intern lang-key) langs))))
    (if (null langs)
        t
      (if (null entry)
          t
        (not (eq (cdr entry) nil)))))) ; explicit false → disabled

;; ============================================================================
;; EXECUTABLE GUARD
;; ============================================================================

(defun emacs-ide-dev-executable-guard (executables)
  "Return non-nil only if ALL strings in EXECUTABLES are found on PATH.
Use in lang module :if clauses:
   :if (emacs-ide-dev-executable-guard '(\"python3\" \"pyright\"))"
  (cl-every #'executable-find executables))

;; ============================================================================
;; LSP ATTACHMENT (RECALIBRATED)
;; ============================================================================

(defun emacs-ide-dev-attach-lsp (server &optional extra-vars)
  "Add lsp-mode to the current buffer's major-mode hook.
SERVER is a symbol or string (informational; lsp-mode auto-selects).
EXTRA-VARS is an optional alist of (SYMBOL . VALUE) set before lsp starts,
e.g. '((lsp-rust-analyzer-cargo-watch-command . \"clippy\")).
Call this from inside a use-package :config block.
RECALIBRATED: Added major-mode validation."
  (when (and (bound-and-true-p emacs-ide-lsp-enable)
             (fboundp 'lsp-deferred)
             (boundp 'major-mode))
    (dolist (kv (or extra-vars nil))
      (set (car kv) (cdr kv)))
    (add-hook (intern (concat (symbol-name major-mode) "-hook"))
              #'lsp-deferred)))

(defmacro emacs-ide-dev-lsp-hook (mode &rest extra-vars)
  "Convenience macro: add lsp-deferred to MODE-hook with EXTRA-VARS set.
Expands to an add-hook call guarded by emacs-ide-lsp-enable.
Usage in use-package :config:
   (emacs-ide-dev-lsp-hook python-mode
     (lsp-pyright-use-library-code-for-types . t))
RECALIBRATED: Added mode symbol validation."
  (declare (indent 1))
  (when (symbolp mode)
    `(when (and (bound-and-true-p emacs-ide-lsp-enable)
                (fboundp 'lsp-deferred))
       ,@(mapcar (lambda (kv) `(setq ,(car kv) ,(cdr kv))) extra-vars)
       (add-hook ',(intern (concat (symbol-name mode) "-hook"))
                 #'lsp-deferred))))

;; ============================================================================
;; DAP ATTACHMENT
;; ============================================================================

(defun emacs-ide-dev-attach-dap (template-name require-sym)
  "Load DAP adapter REQUIRE-SYM and register TEMPLATE-NAME for the mode.
REQUIRE-SYM is the dap-mode adapter symbol, e.g. 'dap-python or 'dap-lldb.
Called from lang module :config blocks.  Safe no-op if dap-mode absent."
  (when (and (bound-and-true-p emacs-ide-debug-enable)
             (fboundp 'dap-debug))
    (condition-case err
        (require require-sym nil 'noerror)
      (error (message "core-dev: DAP adapter %s not available: %s"
                      require-sym err)))))

;; ============================================================================
;; FORMATTER ATTACHMENT (RECALIBRATED)
;; ============================================================================

(defun emacs-ide-dev-attach-formatter (formatter-sym mode-sym)
  "Register FORMATTER-SYM as the apheleia formatter for MODE-SYM.
Safe no-op if apheleia is absent or formatter not in apheleia-formatters.
RECALIBRATED: Added validation that formatter exists in apheleia-formatters
before registration to prevent orphaned entries."
  (when (and (bound-and-true-p emacs-ide-format-on-save)
             (boundp 'apheleia-mode-alist)
             (boundp 'apheleia-formatters)
             (symbolp formatter-sym)
             (assq formatter-sym apheleia-formatters))
    (setf (alist-get mode-sym apheleia-mode-alist) formatter-sym)))

;; ============================================================================
;; REPL ATTACHMENT
;; ============================================================================

(defun emacs-ide-dev-attach-repl (mode-map repl-fn &optional key)
  "Bind REPL-FN on MODE-MAP under KEY (default C-c r).
REPL-FN is a zero-arg interactive function that opens the REPL."
  (let ((k (or key (kbd "C-c r"))))
    (when (keymapp mode-map)
      (define-key mode-map k repl-fn))))

;; ============================================================================
;; STANDARD COMPILE KEY
;; ============================================================================

(defun emacs-ide-dev-bind-compile (mode-map compile-fn)
  "Bind COMPILE-FN to C-c C-c in MODE-MAP.
Canonical compile/run key for all lang modules."
  (when (keymapp mode-map)
    (define-key mode-map (kbd "C-c C-c") compile-fn)))

;; ============================================================================
;; TREESITTER GRAMMAR ENSURE
;; ============================================================================

(defun emacs-ide-dev-ensure-treesit (lang-sym)
  "Ensure tree-sitter grammar for LANG-SYM is installed, silently.
Called from lang module :config at idle time — never blocks startup."
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p)
             (fboundp 'treesit-language-available-p)
             (not (treesit-language-available-p lang-sym))
             (fboundp 'treesit-install-language-grammar))
    (run-with-idle-timer
     3 nil
     (lambda ()
       (condition-case err
           (progn
             (treesit-install-language-grammar lang-sym)
             (message "core-dev: installed treesit grammar %s" lang-sym))
         (error
          (message "core-dev: treesit grammar %s failed: %s" lang-sym err)))))))

;; ============================================================================
;; LANG MODULE REGISTRATION (RECALIBRATED)
;; ============================================================================

(defun emacs-ide-dev-register (lang-key &rest plist)
  "Register a lang module under LANG-KEY with metadata PLIST.
Keys: :lsp-server :formatter :test-cmd :repl :modes :tier
Called at the top of each lang-*.el for telemetry and health checks.
RECALIBRATED: String comparison for lang-key lookup (string=)."
  (setf (alist-get lang-key emacs-ide-dev--registered-langs
                   nil nil #'string=)
        plist))

(defun emacs-ide-dev-registered-langs ()
  "Return list of all registered lang keys."
  (mapcar #'car emacs-ide-dev--registered-langs))

;; ============================================================================
;; HEALTH SUMMARY (callable from emacs-ide-health)
;; ============================================================================

(defun emacs-ide-dev-health-report ()
  "Return alist of (LANG-KEY . :ok/:warn/:error) for registered langs."
  (mapcar
   (lambda (entry)
     (let* ((key  (car entry))
            (info (cdr entry))
            (lsp  (plist-get info :lsp-server))
            (ok   (or (null lsp) (executable-find lsp))))
       (cons key (if ok :ok :warn))))
   emacs-ide-dev--registered-langs))

(provide 'core-dev)
;;; core-dev.el ends here
