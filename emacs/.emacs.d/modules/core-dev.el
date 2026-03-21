;;; core-dev.el --- Shared IDE Infrastructure -*- lexical-binding: t -*-
;;; Commentary:
;;; Single plug-in point for LSP, completion, DAP, formatting, snippets,
;;; and eldoc.  Every lang-*.el calls (emacs-ide-dev-attach-lsp SERVER) and
;;; (emacs-ide-dev-attach-dap TEMPLATE) rather than touching lsp-mode or
;;; dap-mode directly.  This keeps each lang module ~60 lines and makes the
;;; shared behaviour easy to update in one place.
;;;
;;; Version: 1.0.6
;;; Fixes vs 1.0.5:
;;;   - FIX-TREESIT-TIMER: The add-to-list call to populate
;;;     treesit-language-source-alist was running OUTSIDE the idle timer
;;;     lambda, at the moment emacs-ide-dev-ensure-treesit was called.
;;;     But treesit-install-language-grammar runs INSIDE the idle timer
;;;     3 seconds later. By then the alist entry is present — but the
;;;     issue is that the idle timer lambda is a closure that does NOT
;;;     re-read treesit-language-source-alist at call time in all Emacs
;;;     builds; some builds signal "Cannot find recipe" if the alist was
;;;     modified after the grammar install function was byte-compiled.
;;;     The definitive fix: move the add-to-list call INSIDE the idle
;;;     timer lambda so alist population and grammar install happen in
;;;     the exact same execution context with no timing gap.
;;; Fixes vs 1.0.4:
;;;   - FIX-TREESIT-SOURCE: emacs-ide-dev-ensure-treesit called
;;;     treesit-install-language-grammar without first ensuring the language
;;;     exists in treesit-language-source-alist. Emacs cannot install a grammar
;;;     without a source URL — it throws "Cannot find recipe for this language"
;;;     regardless of the symbol name used. This is why both 'cpp and 'c++
;;;     failed identically.
;;;     Fix: added emacs-ide-dev--treesit-sources, a complete alist mapping
;;;     every language symbol used in lang-*.el to its upstream git URL.
;;;     emacs-ide-dev-ensure-treesit now merges the entry into
;;;     treesit-language-source-alist before calling install, so Emacs
;;;     always has the URL it needs.
;;;   - FIX-TREESIT-CPP-NAME: The treesit recipe name for C++ is 'cpp
;;;     (matching treesit-language-source-alist convention and the shared
;;;     library libtree-sitter-cpp.so). Using 'c++ caused "no recipe" because
;;;     Emacs looks up the symbol name directly in treesit-language-source-alist.
;;;     lang-c.el v1.0.3 reverts back to (emacs-ide-dev-ensure-treesit 'cpp).
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
;; TREESITTER GRAMMAR SOURCES
;; FIX-TREESIT-SOURCE: treesit-install-language-grammar requires a URL entry
;; in treesit-language-source-alist — without it Emacs throws "Cannot find
;; recipe for this language" regardless of symbol name. We populate this alist
;; for every language used across all lang-*.el modules before attempting install.
;; NOTE: C++ recipe key is 'cpp (not 'c++) — matches libtree-sitter-cpp.so
;; ============================================================================
(defconst emacs-ide-dev--treesit-sources
  '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
    (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
    (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
    (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
    (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
    (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
    (java       . ("https://github.com/tree-sitter/tree-sitter-java"))
    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
    (lua        . ("https://github.com/MunifTanjim/tree-sitter-lua"))
    (markdown   . ("https://github.com/ikatyang/tree-sitter-markdown" "master" "tree-sitter-markdown/src"))
    (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
    (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
    (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
    (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml")))
  "Source URLs for tree-sitter grammars used across all lang-*.el modules.
Each entry is (LANG-SYMBOL . (URL &optional REVISION SOURCE-DIR)).
These are merged into `treesit-language-source-alist' before install so
`treesit-install-language-grammar' always has the URL it needs.")

;; ============================================================================
;; TREESITTER GRAMMAR ENSURE
;; ============================================================================

(defun emacs-ide-dev-ensure-treesit (lang-sym)
  "Ensure tree-sitter grammar for LANG-SYM is installed, silently.
FIX-TREESIT-SOURCE: Merges LANG-SYM into `treesit-language-source-alist'
from `emacs-ide-dev--treesit-sources' before calling install, so Emacs
always has the source URL it needs. Without this, install always throws
\\='Cannot find recipe for this language\\=' regardless of symbol name.
Called from lang module top level via idle timer — never blocks startup."
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p)
             (fboundp 'treesit-language-available-p)
             (not (treesit-language-available-p lang-sym))
             (fboundp 'treesit-install-language-grammar))
    ;; FIX-TREESIT-TIMER: source registration and grammar install must happen
    ;; in the same execution context. Moving add-to-list inside the lambda
    ;; ensures the alist entry exists at the exact moment install runs.
    (run-with-idle-timer
     3 nil
     (lambda ()
       (condition-case err
           (progn
             ;; Register source URL immediately before install call
             (when (and (boundp 'treesit-language-source-alist)
                        (not (assq lang-sym treesit-language-source-alist)))
               (when-let ((src (assq lang-sym emacs-ide-dev--treesit-sources)))
                 (add-to-list 'treesit-language-source-alist
                              (cons lang-sym (cdr src)))))
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
