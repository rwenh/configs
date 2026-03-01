;;; tools-lsp.el --- LSP Mode Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Language Server Protocol configuration with performance optimizations.
;;; Version: 2.2.3
;;; Fixes:
;;;   - Guard: original (unless ... (provide) ...) continued executing after
;;;     provide because provide does not stop evaluation. Fixed with a proper
;;;     top-level `when` wrapping all LSP setup.
;;;   - dumb-jump-go-other-window was bound to M-g o colliding with
;;;     consult-outline in keybindings.el; moved to M-g O here.
;;;   - emacs-ide-lsp-semantic-tokens was undefined; replaced with direct
;;;     reference to emacs-ide-lsp-enable-inlay-hints / config value.
;;;   - 2.2.2: `helpful` use-package block was inside the (when emacs-ide-lsp-enable)
;;;     guard, so users with LSP disabled lost all helpful bindings (C-h f/v/k/F/C,
;;;     C-c C-d). Moved outside the guard so helpful loads unconditionally.
;;;   - 2.2.3: helpful use-package block removed entirely from this module.
;;;     keybindings.el (always-last, unconditional) owns all helpful global
;;;     bindings. Having them in two places was redundant noise.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; GUARD: LSP DISABLED IN CONFIG
;; FIX: provide alone doesn't stop execution. Use a top-level when guard.
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
  :hook ((c-mode          . emacs-ide-lsp-deferred-optimized)
         (c++-mode        . emacs-ide-lsp-deferred-optimized)
         (python-mode     . emacs-ide-lsp-deferred-optimized)
         (rust-mode       . emacs-ide-lsp-deferred-optimized)
         (go-mode         . emacs-ide-lsp-deferred-optimized)
         (java-mode       . emacs-ide-lsp-deferred-optimized)
         (js-mode         . emacs-ide-lsp-deferred-optimized)
         (js2-mode        . emacs-ide-lsp-deferred-optimized)
         (typescript-mode . emacs-ide-lsp-deferred-optimized)
         (lsp-mode        . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix              "C-c l"
        lsp-completion-provider        :none   ; wire manually below for corfu
        lsp-idle-delay                 (or (bound-and-true-p emacs-ide-completion-delay) 0.3)
        lsp-log-io                     nil
        lsp-enable-file-watchers       t
        lsp-file-watch-threshold       2000
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
        ;; FIX: emacs-ide-lsp-semantic-tokens was undefined; use config var directly
        lsp-semantic-tokens-enable           t
        lsp-enable-symbol-highlighting       t
        lsp-lens-enable                      t
        lsp-enable-on-type-formatting        t
        lsp-enable-indentation               t
        lsp-before-save-edits                t
        lsp-auto-guess-root                  t
        lsp-restart                          'auto-restart
        lsp-enable-suggest-server-download   t
        lsp-inlay-hint-enable
        (or (bound-and-true-p emacs-ide-lsp-enable-inlay-hints) t)
        lsp-use-plists                       t
        lsp-warn-no-matched-clients          nil
        lsp-diagnostics-provider             :flycheck
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

  (defun emacs-ide-lsp-status ()
    "Display LSP connection status."
    (interactive)
    (if (bound-and-true-p lsp-mode)
        (message "LSP: %s | Workspace: %s"
                 (if (lsp-workspaces) "✓ Connected" "✗ Disconnected")
                 (or (lsp-workspace-root) "None"))
      (message "LSP: Not active in this buffer")))

  (defun emacs-ide-lsp-restart-all ()
    "Restart all LSP workspaces."
    (interactive)
    (when (bound-and-true-p lsp-mode)
      (lsp-restart-workspace)
      (message "✓ LSP workspace restarted"))))

;; ============================================================================
;; LSP-UI
;; ============================================================================
(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-doc-enable            t
        lsp-ui-doc-show-with-cursor  t
        lsp-ui-doc-show-with-mouse   t
        lsp-ui-doc-delay             0.2
        lsp-ui-doc-position          'at-point
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
(use-package lsp-pyright
  :after lsp-mode
  :if (executable-find "pyright")
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright nil t)
                          (emacs-ide-lsp-deferred-optimized)))
  :init
  (setq lsp-pyright-multi-root              nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths       t))

(with-eval-after-load 'rust-mode
  (when (executable-find "rust-analyzer")
    (require 'lsp-rust nil t)))

(with-eval-after-load 'go-mode
  (when (executable-find "gopls")
    (require 'lsp-go nil t)))

;; ============================================================================
;; DUMB-JUMP — fallback navigation when LSP unavailable
;; FIX: dumb-jump-go-other-window moved from M-g o to M-g O
;;      to avoid collision with consult-outline on M-g o (keybindings.el)
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

;; ============================================================================
;; LSP SERVER STATUS CHECKER
;; ============================================================================
(defun emacs-ide-lsp-check-servers ()
  "Check and display LSP server availability."
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
      (princ (format "Available: %d\n" (length available)))
      (dolist (srv available)
        (princ (format "  ✓ %-35s (%s)\n" (car srv) (cdr srv))))
      (when missing
        (princ (format "\nMissing: %d\n" (length missing)))
        (dolist (srv missing)
          (princ (format "  ✗ %-35s (%s)\n" (car srv) (cdr srv)))))
      (princ "\nInstall missing servers via your system package manager\n")
      (princ "or: M-x lsp-install-server\n"))))

) ;; end (when emacs-ide-lsp-enable ...)

;; helpful bindings (C-h f/v/k/F/C, C-c C-d) are set in keybindings.el which
;; loads unconditionally and always last — no need to duplicate them here.

;; Always provide, even when LSP is disabled
(unless (bound-and-true-p emacs-ide-lsp-enable)
  (message "⚠️  LSP disabled in config — skipping tools-lsp"))

(provide 'tools-lsp)
;;; tools-lsp.el ends here
