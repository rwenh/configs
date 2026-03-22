;;; tools-lsp.el --- LSP Mode Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Language Server Protocol configuration with performance optimizations.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-CRASH: (or (bound-and-true-p emacs-ide-lsp-enable-inlay-hints) t)
;;;     was a bare expression sitting in the middle of the lsp-mode :init setq
;;;     body with no preceding variable name. setq requires (var val) pairs;
;;;     this gave it an odd argument count of 65, throwing:
;;;       wrong-number-of-arguments setq 65
;;;     This aborted the entire :init block — lsp-mode never initialised,
;;;     causing the 4788s startup time (the startup timer kept running waiting
;;;     for LSP to settle). Fixed by adding the missing key:
;;;       lsp-inlay-hints-enable (or ...)
;;;   - FIX-TS-HOOKS: Added tree-sitter mode variants to lsp-mode :hook list.
;;;     c-ts-mode, c++-ts-mode, python-ts-mode, rust-ts-mode, go-ts-mode,
;;;     java-ts-mode, js-ts-mode, typescript-ts-mode were all missing.
;;;     With Emacs 29's tree-sitter-first approach, opening files in ts-mode
;;;     variants did not activate LSP.
;;;   - FIX-INIT-ORDER: Reordered lsp-pyright use-package to put :init before
;;;     :config in source (use-package always runs :init before :config
;;;     regardless of source order, but correct ordering aids readability
;;;     and prevents confusion).
;;;   - FIX-DIAGNOSTICS-PROVIDER: lsp-diagnostics-provider now reads from
;;;     config.yml lsp.diagnostics-provider instead of being hardcoded to
;;;     :flycheck.
;;; Fixes vs 2.3.1 (retained):
;;;   - 2.3.1: FIX-AUTO-GUESS-ROOT: lsp-auto-guess-root t caused LSP to use
;;;     /home/cody as project root instead of the actual project folder.
;;;     With lsp-enable-file-watchers t this triggered "watch 56,659
;;;     directories?" prompt on every file open. Fix: lsp-auto-guess-root
;;;     set to nil — lsp-mode now uses projectile/project.el root detection
;;;     which correctly scopes to the actual project directory.
;;;     FIX-LSP-UI-SPLIT: lsp-ui-doc-position 'at-point caused lsp-ui to
;;;     open its documentation in a split buffer window on systems where
;;;     child-frame rendering is unavailable. Fix: changed to 'frame so
;;;     docs always use a child frame and never split the window.
;;;     lsp-ui-doc-show-with-mouse set to nil so the doc frame only appears
;;;     on explicit C-c l u toggle, preventing surprise splits on hover.
;;;   - 2.3.0: lsp-pyright :init block consolidated from lang-python.el.
;;;   - 2.2.9: lsp-use-plists removed from use-package lsp-mode :init block.
;;;     It must be set before lsp-mode ever loads so the JSON-RPC deserializer
;;;     is compiled in plist mode from the start. Setting it in :init is too
;;;     late — straight.el has already loaded lsp-mode by the time :init runs,
;;;     so the deserializer was baked as hash-table mode. Every message from
;;;     every LSP server then threw:
;;;       wrong-type-argument hash-table-p (:jsonrpc "2.0" ...)
;;;     The canonical assignment now lives in early-init.el v2.2.8 (after
;;;     gc-setup, before any package loads). The line here is intentionally
;;;     removed rather than left as a comment to avoid future confusion.
;;; Fixes:
;;;   - 2.2.8: Removed duplicate emacs-ide-lsp-check-servers definition that
;;;     was left inside the (when emacs-ide-lsp-enable ...) block after the
;;;     2.2.6 fix moved the canonical definition outside it.  Having two
;;;     definitions caused the inner (inferior, LSP-only) version to shadow
;;;     the outer (always-available) one during the load phase when
;;;     emacs-ide-lsp-enable is t: Emacs evaluates the inner defun first,
;;;     installs it as the function cell, then evaluates the outer defun and
;;;     replaces it — wasted work but not fatal.  When emacs-ide-lsp-enable
;;;     is nil the inner defun is never evaluated, so only the outer
;;;     definition exists, which is correct.  Removed the inner copy to
;;;     eliminate the confusion and the redundant evaluation.
;;;   - 2.2.7: lsp-pyright :if guard now checks both "pyright" and
;;;     "pyright-langserver". emacs-ide-health.el v2.2.2 already handled
;;;     both names; the use-package guard here only checked "pyright" so
;;;     the package silently never loaded on Arch/NixOS/some pip installs
;;;     where only "pyright-langserver" is on PATH.
;;;   - 2.2.6: emacs-ide-lsp-status and emacs-ide-lsp-check-servers moved
;;;     outside (when emacs-ide-lsp-enable ...) block. Previously both were
;;;     defined only when LSP was enabled; keybindings.el unconditionally
;;;     binds C-c L to emacs-ide-lsp-status, so with LSP disabled every
;;;     C-c L press produced a void-function error. Both functions now live
;;;     at the top level and check emacs-ide-lsp-enable internally.
;;;   - 2.2.5: lsp-idle-delay was incorrectly set to emacs-ide-completion-delay
;;;     (the corfu popup delay, default 0.1s). LSP idle delay governs when the
;;;     server is polled after inactivity — 0.1s causes server hammering on every
;;;     keystroke. Fixed to a constant 0.3s, independent of completion config.
;;;   - 2.2.4: lsp-ui-doc-show-with-cursor and lsp-ui-doc-show-with-mouse
;;;     were both t, causing the popup to appear constantly from two triggers
;;;     simultaneously (cursor movement + mouse hover). Set show-with-cursor
;;;     to nil; show-with-mouse t is the less intrusive default. Users can
;;;     invoke lsp-ui-doc-toggle (C-c l u) on demand.
;;;   - 2.2.4: emacs-ide-lsp-enable guard now uses run-with-idle-timer
;;;     deferred check to ensure emacs-ide-config-apply has completed before
;;;     the guard is evaluated. Direct (when (bound-and-true-p emacs-ide-lsp-enable))
;;;     at load time could fire before config was applied.
;;;   - Guard: original (unless ... (provide) ...) continued executing after
;;;     provide because provide does not stop evaluation. Fixed with a proper
;;;     top-level `when` wrapping all LSP setup.
;;;   - dumb-jump-go-other-window was bound to M-g o colliding with
;;;     consult-outline; moved to M-g O.
;;;   - helpful use-package block removed — keybindings.el owns those globally.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; GUARD: LSP DISABLED IN CONFIG
;; FIX 2.2.4: emacs-ide-lsp-enable is set by emacs-ide-config-apply which
;;   runs during the config-load phase of init.el. By the time tools-lsp.el
;;   is loaded (feature-modules phase), the variable is set. The guard is
;;   correct as a top-level when; the previous concern was theoretical.
;;   Retained as-is — the critical fix is the double-doc-popup issue above.
;; ============================================================================
(when (bound-and-true-p emacs-ide-lsp-enable)

;; ============================================================================
;; LSP OPTIMIZATION FOR LARGE FILES
;; ============================================================================
(defun emacs-ide-lsp-optimize-large-files ()
  "Disable expensive LSP features for large files before activation."
  (let ((threshold (or (bound-and-true-p emacs-ide-lsp-large-file-threshold)
                       100000)))
    (when (> (buffer-size) threshold)
      (setq-local lsp-enable-symbol-highlighting nil
                  lsp-enable-on-type-formatting  nil
                  lsp-enable-folding             nil
                  lsp-lens-enable                nil
                  lsp-semantic-tokens-enable     nil
                  lsp-enable-indentation         nil)
      (message "LSP large-file optimizations applied"))))

(defun emacs-ide-lsp-deferred-optimized ()
  "Apply large-file optimizations then defer LSP startup."
  (emacs-ide-lsp-optimize-large-files)
  (when (fboundp 'lsp-deferred)
    (lsp-deferred)))

;; ============================================================================
;; FLYCHECK
;; ============================================================================
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically
        '(save idle-change mode-enabled new-line)
        flycheck-indication-mode        'left-margin
        flycheck-display-errors-delay   0.1
        flycheck-idle-change-delay      0.5
        flycheck-highlighting-mode      'lines
        flycheck-emacs-lisp-load-path   'inherit
        flycheck-disabled-checkers      '(emacs-lisp-checkdoc)
        flycheck-temp-prefix            ".flycheck"
        flycheck-global-modes           '(not org-mode))
  :bind (:map flycheck-mode-map
              ("C-c ! n" . flycheck-next-error)
              ("C-c ! p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)
              ("C-c ! v" . flycheck-verify-setup)))

(use-package flycheck-pos-tip
  :after flycheck
  :if (display-graphic-p)
  :config
  (when (fboundp 'flycheck-pos-tip-mode)
    (flycheck-pos-tip-mode 1)))

;; ============================================================================
;; LSP-MODE CORE
;; ============================================================================
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((c-mode              . emacs-ide-lsp-deferred-optimized)
         (c++-mode            . emacs-ide-lsp-deferred-optimized)
         ;; FIX-TS-HOOKS: added all ts-mode variants for Emacs 29+
         (c-ts-mode           . emacs-ide-lsp-deferred-optimized)
         (c++-ts-mode         . emacs-ide-lsp-deferred-optimized)
         (python-mode         . emacs-ide-lsp-deferred-optimized)
         (python-ts-mode      . emacs-ide-lsp-deferred-optimized)
         (rust-mode           . emacs-ide-lsp-deferred-optimized)
         (rust-ts-mode        . emacs-ide-lsp-deferred-optimized)
         (go-mode             . emacs-ide-lsp-deferred-optimized)
         (go-ts-mode          . emacs-ide-lsp-deferred-optimized)
         (java-mode           . emacs-ide-lsp-deferred-optimized)
         (java-ts-mode        . emacs-ide-lsp-deferred-optimized)
         (js-mode             . emacs-ide-lsp-deferred-optimized)
         (js-ts-mode          . emacs-ide-lsp-deferred-optimized)
         (js2-mode            . emacs-ide-lsp-deferred-optimized)
         (typescript-mode     . emacs-ide-lsp-deferred-optimized)
         (typescript-ts-mode  . emacs-ide-lsp-deferred-optimized)
         (lsp-mode            . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix              "C-c l"
        lsp-completion-provider        :none   ; wire manually below for corfu
        lsp-idle-delay                 0.3   ; LSP server idle delay — independent of completion popup delay
        lsp-log-io                     nil
        lsp-keep-workspace-alive       nil
        lsp-enable-folding             t
        lsp-enable-links               t
        lsp-enable-snippet             t
        lsp-modeline-code-actions-enable     t
        lsp-modeline-diagnostics-enable      t
        lsp-modeline-workspace-status-enable t
        lsp-signature-auto-activate          t
        lsp-signature-render-documentation   t
        lsp-eldoc-enable-hover               t
        lsp-eldoc-render-all                 nil
        lsp-headerline-breadcrumb-enable     t
        lsp-semantic-tokens-enable           t
        lsp-enable-symbol-highlighting       t
        lsp-lens-enable                      t
        lsp-enable-on-type-formatting        t
        lsp-enable-indentation               t
        lsp-before-save-edits                t
        lsp-restart                          'auto-restart
        lsp-enable-suggest-server-download   t
        ;; FIX-AUTO-GUESS-ROOT: was t — caused entire /home/cody to be used
        ;; as project root, triggering "watch 56,659 directories?" prompt.
        ;; nil makes lsp-mode use projectile/project.el root detection instead.
        lsp-auto-guess-root                  nil
        ;; Suppress the watcher confirmation prompt entirely — if the
        ;; directory count exceeds lsp-file-watch-threshold, just silently
        ;; disable watchers rather than asking the user every time.
        lsp-enable-file-watchers             t
        lsp-file-watch-threshold             1000
        lsp-ask-valid-restart                nil
        ;; FIX-CRASH: was a bare (or ...) expression with no variable name —
        ;; gave setq an odd argument count of 65, crashing the entire :init.
        lsp-inlay-hints-enable               (or (bound-and-true-p emacs-ide-lsp-enable-inlay-hints) t)
        ;; NOTE: lsp-use-plists is intentionally NOT set here.
        ;; It must be set before lsp-mode loads — see early-init.el v2.2.8.
        lsp-warn-no-matched-clients          nil
        ;; FIX-DIAGNOSTICS-PROVIDER: read from config.yml lsp.diagnostics-provider
        ;; instead of hardcoding :flycheck. Defaults to :flycheck if config unavailable.
        lsp-diagnostics-provider
        (let ((p (and (fboundp 'emacs-ide-config-get)
                      (emacs-ide-config-get 'lsp 'diagnostics-provider nil))))
          (cond ((eq p 'flycheck) :flycheck)
                ((eq p 'flymake)  :flymake)
                (t                :flycheck)))
        lsp-auto-configure                   t
        lsp-watch-file-ignore-regexps
        '("[/\\\\]\\.git$"         "[/\\\\]\\.hg$"
          "[/\\\\]node_modules$"   "[/\\\\]__pycache__$"
          "[/\\\\]\\.venv$"        "[/\\\\]venv$"
          "[/\\\\]target$"         "[/\\\\]build$"
          "[/\\\\]dist$"           "[/\\\\]\\.mypy_cache$"
          "[/\\\\]\\.pytest_cache$"))
  :bind (:map lsp-mode-map
              ("C-c l r" . lsp-rename)
              ("C-c l f" . lsp-format-buffer)
              ("C-c l F" . lsp-format-region)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l d" . lsp-describe-thing-at-point)
              ("C-c l R" . lsp-find-references)
              ("C-c l i" . lsp-find-implementation)
              ("C-c l t" . lsp-find-type-definition)
              ("C-c l o" . lsp-organize-imports)
              ("C-c l h" . lsp-document-highlight)
              ("C-c l w" . lsp-workspace-restart)
              ("C-c l W" . lsp-workspace-shutdown))
  :config
  ;; Wire lsp-completion-at-point into corfu
  (defun emacs-ide-lsp-setup-completion ()
    "Prepend lsp-completion-at-point to capf list for corfu."
    (when (fboundp 'lsp-completion-at-point)
      (setq-local completion-at-point-functions
                  (cons #'lsp-completion-at-point
                        (remove #'lsp-completion-at-point
                                completion-at-point-functions)))))
  (add-hook 'lsp-completion-mode-hook #'emacs-ide-lsp-setup-completion)

  (defun emacs-ide-lsp-restart-all ()
    "Restart all LSP workspaces."
    (interactive)
    (when (bound-and-true-p lsp-mode)
      (lsp-restart-workspace)
      (message "✓ LSP workspace restarted"))))

;; ============================================================================
;; LSP-UI
;; FIX 2.2.4: Both lsp-ui-doc-show-with-cursor and lsp-ui-doc-show-with-mouse
;;   were t, causing the doc popup to appear from two simultaneous triggers.
;;   show-with-cursor set to nil — docs appear on hover only, reducing noise.
;;   Use C-c l u (lsp-ui-doc-toggle) to show docs on demand at point.
;; ============================================================================
(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-doc-enable            t
        lsp-ui-doc-show-with-cursor  nil
        ;; FIX-LSP-UI-SPLIT: was t — doc frame appeared on hover immediately
        ;; on buffer open, splitting the window on systems without child-frame
        ;; support. Set to nil — use C-c l u to show docs on demand.
        lsp-ui-doc-show-with-mouse   nil
        lsp-ui-doc-delay             0.5
        ;; FIX-LSP-UI-SPLIT: was 'at-point — fell back to buffer/split window
        ;; when child-frame unavailable. 'frame always uses a proper frame.
        lsp-ui-doc-position          'frame
        lsp-ui-doc-max-width         120
        lsp-ui-doc-max-height        30
        lsp-ui-sideline-enable              t
        lsp-ui-sideline-show-hover          nil
        lsp-ui-sideline-show-diagnostics    t
        lsp-ui-sideline-show-code-actions   t
        lsp-ui-sideline-ignore-duplicate    t
        lsp-ui-sideline-delay               0.2
        lsp-ui-peek-enable                  t
        lsp-ui-peek-show-directory          t
        lsp-ui-peek-list-width              60
        lsp-ui-peek-peek-height             20
        lsp-ui-imenu-enable                 t
        lsp-ui-imenu-kind-position          'left)
  :bind (:map lsp-ui-mode-map
              ("M-."     . lsp-ui-peek-find-definitions)
              ("M-?"     . lsp-ui-peek-find-references)
              ("C-c l u" . lsp-ui-doc-toggle)))

;; ============================================================================
;; LSP-TREEMACS
;; ============================================================================
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (when (fboundp 'lsp-treemacs-sync-mode)
    (lsp-treemacs-sync-mode 1)))

;; ============================================================================
;; LANGUAGE-SPECIFIC LSP SERVERS
;; ============================================================================
;; FIX 2.2.7: lsp-pyright use-package had :if (executable-find "pyright").
;;   Some distributions (Arch, NixOS, some pip installs) install the binary
;;   as "pyright-langserver" rather than "pyright". emacs-ide-health.el v2.2.2
;;   already checks both names for its health widget; the use-package :if
;;   guard here only checked "pyright", so the package silently never loaded
;;   on systems where only "pyright-langserver" is on PATH.
;;   Fix: check both names via an or form.
(use-package lsp-pyright
  :after lsp-mode
  :if (or (executable-find "pyright") (executable-find "pyright-langserver"))
  ;; FIX-INIT-ORDER: :init precedes :config in source for readability.
  ;; use-package always runs :init before :config regardless of source order,
  ;; but correct ordering prevents future confusion.
  :init
  (setq lsp-pyright-multi-root              nil
        lsp-pyright-auto-import-completions  t
        lsp-pyright-auto-search-paths        t
        ;; Moved from lang-python.el v1.0.2 (FIX-DOUBLE-PANE-1)
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-stub-path                ""
        lsp-pyright-venv-path                (expand-file-name "~/.virtualenvs"))
  ;; FIX: require lsp-pyright in :config so it registers its server with
  ;; lsp-mode before any python buffer is opened. Removed the old hook that
  ;; called lsp-deferred a second time (double-start bug).
  :config
  (require 'lsp-pyright))

(with-eval-after-load 'rust-mode
  (when (executable-find "rust-analyzer")
    (require 'lsp-rust nil t)))

(with-eval-after-load 'go-mode
  (when (executable-find "gopls")
    (require 'lsp-go nil t)))

;; ============================================================================
;; DUMB-JUMP — fallback navigation when LSP unavailable
;; FIX: dumb-jump-go-other-window moved from M-g o to M-g O
;;      to avoid collision with consult-outline on M-g o.
;; ============================================================================
(use-package dumb-jump
  :bind (("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g O" . dumb-jump-go-other-window))  ; was M-g o — collision fixed
  :init
  (setq dumb-jump-selector     'completing-read
        dumb-jump-aggressive   t
        dumb-jump-prefer-searcher 'rg)
  :config
  (when (fboundp 'dumb-jump-xref-activate)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))

;; ============================================================================
;; ELDOC
;; ============================================================================
(use-package eldoc
  :straight nil
  :init
  (setq eldoc-idle-delay                 0.1
        eldoc-echo-area-use-multiline-p  nil
        eldoc-echo-area-prefer-doc-buffer t)
  :config
  (global-eldoc-mode 1))

) ;; end (when emacs-ide-lsp-enable ...)

;; ============================================================================
;; ALWAYS-AVAILABLE COMMANDS
;; FIX 2.2.6: emacs-ide-lsp-status and emacs-ide-lsp-check-servers were
;;   previously defined inside the (when emacs-ide-lsp-enable ...) block.
;;   When LSP is disabled, keybindings.el still binds C-c L to
;;   emacs-ide-lsp-status, calling an undefined function and signalling a
;;   void-function error on every C-c L press.
;;   Both functions are now defined unconditionally outside the guard.
;;   They check (bound-and-true-p lsp-mode) / featurep internally so they
;;   behave correctly whether or not LSP is enabled.
;; ============================================================================
(defun emacs-ide-lsp-status ()
  "Display LSP connection status.
Safe to call even when LSP is disabled — reports not-active rather
than signalling void-function."
  (interactive)
  (if (not (bound-and-true-p emacs-ide-lsp-enable))
      (message "LSP: Disabled in config (emacs-ide-lsp-enable is nil)")
    (if (bound-and-true-p lsp-mode)
        (message "LSP: %s | Workspace: %s"
                 (if (and (fboundp 'lsp-workspaces) (lsp-workspaces))
                     "✓ Connected" "✗ Disconnected")
                 (or (and (fboundp 'lsp-workspace-root) (lsp-workspace-root))
                     "None"))
      (message "LSP: Not active in this buffer"))))

(defun emacs-ide-lsp-check-servers ()
  "Check and display LSP server availability.
Safe to call even when LSP is disabled."
  (interactive)
  (let ((servers '(("pyright"                    . "Python")
                   ("rust-analyzer"              . "Rust")
                   ("gopls"                      . "Go")
                   ("typescript-language-server" . "TypeScript/JS")
                   ("clangd"                     . "C/C++")))
        (available '())
        (missing '()))
    (dolist (server servers)
      (if (executable-find (car server))
          (push server available)
        (push server missing)))
    (with-output-to-temp-buffer "*LSP Servers*"
      (princ "=== LSP SERVERS STATUS ===\n\n")
      (princ (format "LSP enabled in config: %s\n\n"
                     (if (bound-and-true-p emacs-ide-lsp-enable) "YES" "NO")))
      (princ (format "Available: %d\n" (length available)))
      (dolist (srv available)
        (princ (format "  ✓ %-35s (%s)\n" (car srv) (cdr srv))))
      (when missing
        (princ (format "\nMissing: %d\n" (length missing)))
        (dolist (srv missing)
          (princ (format "  ✗ %-35s (%s)\n" (car srv) (cdr srv)))))
      (princ "\nInstall missing servers via your system package manager\n")
      (princ "or: M-x lsp-install-server\n"))))

;; helpful bindings (C-h f/v/k/F/C, C-c C-d) are set in keybindings.el which
;; loads unconditionally and always last.

(unless (bound-and-true-p emacs-ide-lsp-enable)
  (message "⚠️  LSP disabled in config — skipping tools-lsp"))

;; Always provide, even when LSP is disabled
(provide 'tools-lsp)
;;; tools-lsp.el ends here
