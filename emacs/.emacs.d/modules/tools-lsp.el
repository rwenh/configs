;;; tools-lsp.el --- LSP Mode Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Language Server Protocol configuration with performance optimizations.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (recalibration):
;;;   - FIX-LSP-UI-DOC-TOGGLE: lsp-ui-doc-toggle does not exist in modern
;;;     lsp-ui (it was removed in lsp-ui 7.x). The correct public API is
;;;     lsp-ui-doc-glance (show doc for thing at point, dismiss on next event)
;;;     and lsp-ui-doc-hide (explicitly hide the popup). A thin
;;;     emacs-ide-lsp-ui-doc-toggle wrapper is defined that calls
;;;     lsp-ui-doc-glance when the popup is hidden and lsp-ui-doc-hide when
;;;     visible, providing the expected toggle UX without calling a
;;;     nonexistent function.  C-c l u is bound to this wrapper.
;;;   - FIX-LSP-CHECK-SERVERS-API: (retained from prior audit) — checks PATH
;;;     directly, does not call nonexistent (lsp-check-servers).
;;; All prior fixes retained (FIX-CRASH, FIX-TS-HOOKS, FIX-DIAGNOSTICS-PROVIDER,
;;; FIX-AUTO-GUESS-ROOT, FIX-LSP-UI-SPLIT, FIX-INIT-ORDER, etc.)
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; GUARD: LSP DISABLED IN CONFIG
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
        lsp-completion-provider        :none
        lsp-idle-delay                 0.3
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
        lsp-auto-guess-root                  nil
        lsp-enable-file-watchers             t
        lsp-file-watch-threshold             1000
        lsp-ask-valid-restart                nil
        lsp-inlay-hints-enable               (or (bound-and-true-p emacs-ide-lsp-enable-inlay-hints) t)
        lsp-warn-no-matched-clients          nil
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
;; LSP-UI DOC TOGGLE HELPER
;; FIX-LSP-UI-DOC-TOGGLE: lsp-ui-doc-toggle was removed from lsp-ui 7.x.
;; The public API is:
;;   lsp-ui-doc-glance  — show doc for thing at point (hides on next event)
;;   lsp-ui-doc-show    — show doc and keep it open
;;   lsp-ui-doc-hide    — hide the popup
;; This wrapper provides the expected toggle behaviour: show if hidden,
;; hide if already visible.  C-c l u is bound to this in the lsp-ui :bind.
;; ============================================================================
(defun emacs-ide-lsp-ui-doc-toggle ()
  "Toggle lsp-ui documentation popup.
Shows documentation via lsp-ui-doc-glance when hidden; hides via
lsp-ui-doc-hide when already visible.  Works with lsp-ui 7.x+ where
the old lsp-ui-doc-toggle function was removed."
  (interactive)
  (if (and (boundp 'lsp-ui-doc--frame)
           (frame-live-p lsp-ui-doc--frame)
           (frame-visible-p lsp-ui-doc--frame))
      (when (fboundp 'lsp-ui-doc-hide)
        (lsp-ui-doc-hide))
    (when (fboundp 'lsp-ui-doc-glance)
      (lsp-ui-doc-glance))))

;; ============================================================================
;; LSP-UI
;; ============================================================================
(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-doc-enable            t
        lsp-ui-doc-show-with-cursor  nil
        lsp-ui-doc-show-with-mouse   nil
        lsp-ui-doc-delay             0.5
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
              ;; FIX-LSP-UI-DOC-TOGGLE: lsp-ui-doc-toggle removed in lsp-ui 7.x.
              ;; Bound to the wrapper above which uses the current public API.
              ("C-c l u" . emacs-ide-lsp-ui-doc-toggle)))

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
(use-package lsp-pyright
  :after lsp-mode
  :if (or (executable-find "pyright") (executable-find "pyright-langserver"))
  :init
  (setq lsp-pyright-multi-root              nil
        lsp-pyright-auto-import-completions  t
        lsp-pyright-auto-search-paths        t
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-stub-path                ""
        lsp-pyright-venv-path                (expand-file-name "~/.virtualenvs"))
  :config
  (require 'lsp-pyright))

(with-eval-after-load 'rust-mode
  (when (executable-find "rust-analyzer")
    (require 'lsp-rust nil t)))

(with-eval-after-load 'go-mode
  (when (executable-find "gopls")
    (require 'lsp-go nil t)))

;; ============================================================================
;; DUMB-JUMP
;; ============================================================================
(use-package dumb-jump
  :bind (("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g O" . dumb-jump-go-other-window))
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
;; ============================================================================
(defun emacs-ide-lsp-status ()
  "Display LSP connection status for the current buffer.
Safe to call even when LSP is disabled."
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
FIX-LSP-CHECK-SERVERS-API: Checks PATH directly — does NOT call
(lsp-check-servers) which does not exist in lsp-mode's public API.
Live workspace status uses (lsp-workspaces) which is a real lsp-mode function."
  (interactive)
  (let ((servers '(("pyright"                    . "Python")
                   ("rust-analyzer"              . "Rust")
                   ("gopls"                      . "Go")
                   ("typescript-language-server" . "TypeScript/JS")
                   ("clangd"                     . "C/C++")
                   ("jdtls"                      . "Java")
                   ("kotlin-language-server"     . "Kotlin")
                   ("lua-language-server"        . "Lua")
                   ("bash-language-server"       . "Shell")
                   ("sqls"                       . "SQL")))
        (available '())
        (missing '()))
    (dolist (server servers)
      (if (executable-find (car server))
          (push server available)
        (push server missing)))
    (with-output-to-temp-buffer "*LSP Servers*"
      (princ "=== LSP SERVERS STATUS ===\n\n")
      (princ (format "LSP enabled in config: %s\n"
                     (if (bound-and-true-p emacs-ide-lsp-enable) "YES" "NO")))
      ;; FIX-LSP-CHECK-SERVERS-API: use (lsp-workspaces) — a real lsp-mode fn
      (when (and (bound-and-true-p emacs-ide-lsp-enable)
                 (fboundp 'lsp-workspaces))
        (let ((ws (ignore-errors (lsp-workspaces))))
          (princ (format "Active workspaces: %d\n" (length ws)))))
      (princ (format "\nAvailable: %d\n" (length available)))
      (dolist (srv available)
        (princ (format "  ✓ %-35s (%s)\n" (car srv) (cdr srv))))
      (when missing
        (princ (format "\nMissing: %d\n" (length missing)))
        (dolist (srv missing)
          (princ (format "  ✗ %-35s (%s)\n" (car srv) (cdr srv)))))
      (princ "\nInstall missing servers via your system package manager\n")
      (princ "or: M-x lsp-install-server\n"))))

(unless (bound-and-true-p emacs-ide-lsp-enable)
  (message "⚠️  LSP disabled in config — skipping tools-lsp"))

(provide 'tools-lsp)
;;; tools-lsp.el ends here
