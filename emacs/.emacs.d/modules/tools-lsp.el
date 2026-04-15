;;; tools-lsp.el --- LSP Mode Configuration -*- lexical-binding: t -*-
;;; Version: 3.1.0
;;; Code:

(require 'cl-lib)

(when (bound-and-true-p emacs-ide-lsp-enable)

;;; ─── Large-file optimizations ───────────────────────────────────────────────

(defun emacs-ide-lsp-optimize-large-files ()
  (let ((threshold (or (bound-and-true-p emacs-ide-lsp-large-file-threshold)
                       100000)))
    (when (> (buffer-size) threshold)
      (setq-local lsp-enable-symbol-highlighting   nil
                  lsp-enable-on-type-formatting    nil
                  lsp-enable-folding               nil
                  lsp-lens-enable                  nil
                  lsp-semantic-tokens-enable       nil
                  lsp-enable-indentation           nil)
      (message "LSP: large-file mode — expensive features disabled"))))

(defun emacs-ide-lsp-deferred-optimized ()
  (emacs-ide-lsp-optimize-large-files)
  (when (fboundp 'lsp-deferred)
    (lsp-deferred)))

;;; ─── Flycheck ───────────────────────────────────────────────────────────────

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically
        '(save idle-change mode-enabled new-line)
        flycheck-indication-mode        'left-margin
        flycheck-display-errors-delay   0.2
        flycheck-idle-change-delay      0.5
        flycheck-highlighting-mode      'symbols
        flycheck-emacs-lisp-load-path   'inherit
        flycheck-disabled-checkers      '(emacs-lisp-checkdoc)
        flycheck-temp-prefix            ".flycheck"
        flycheck-global-modes           '(not org-mode)
        flycheck-error-list-minimum-level 'warning)
  :bind (:map flycheck-mode-map
              ("C-c ! n" . flycheck-next-error)
              ("C-c ! p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)
              ("C-c ! v" . flycheck-verify-setup)
              ("C-c ! e" . flycheck-explain-error-at-point)))

(use-package flycheck-pos-tip
  :after flycheck
  :if (display-graphic-p)
  :config
  (when (fboundp 'flycheck-pos-tip-mode)
    (flycheck-pos-tip-mode 1)))

;;; ─── lsp-mode ───────────────────────────────────────────────────────────────

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
        lsp-headerline-breadcrumb-segments   '(project file symbols)
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
        lsp-file-watch-threshold             2000
        lsp-ask-valid-restart                nil
        lsp-inlay-hints-enable               (or (bound-and-true-p
                                                   emacs-ide-lsp-enable-inlay-hints)
                                                  t)
        lsp-warn-no-matched-clients          nil
        lsp-diagnostics-provider
        (let ((p (and (fboundp 'emacs-ide-config-get)
                      (emacs-ide-config-get 'lsp 'diagnostics-provider nil))))
          (cond ((eq p 'flymake) :flymake)
                (t               :flycheck)))
        lsp-auto-configure                   t
        lsp-completion-show-kind             t
        lsp-completion-show-detail           t
        lsp-watch-file-ignore-regexps
        '("[/\\\\]\\.git$"         "[/\\\\]\\.hg$"
          "[/\\\\]node_modules$"   "[/\\\\]__pycache__$"
          "[/\\\\]\\.venv$"        "[/\\\\]venv$"
          "[/\\\\]target$"         "[/\\\\]build$"
          "[/\\\\]dist$"           "[/\\\\]\\.mypy_cache$"
          "[/\\\\]\\.pytest_cache$" "[/\\\\]\\.next$"))
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
              ("C-c l W" . lsp-workspace-shutdown)
              ("C-c l I" . lsp-inlay-hints-mode))
  :config
  (defun emacs-ide-lsp-setup-completion ()
    (when (fboundp 'lsp-completion-at-point)
      (setq-local completion-at-point-functions
                  (cons #'lsp-completion-at-point
                        (remove #'lsp-completion-at-point
                                completion-at-point-functions)))))
  (add-hook 'lsp-completion-mode-hook #'emacs-ide-lsp-setup-completion)

  ;; Show a spinner while LSP is initialising
  (when (fboundp 'lsp-modeline-workspace-status-mode)
    (lsp-modeline-workspace-status-mode 1))

  (defun emacs-ide-lsp-restart-all ()
    (interactive)
    (when (bound-and-true-p lsp-mode)
      (lsp-restart-workspace)
      (message "✓ LSP workspace restarted"))))

;;; ─── lsp-ui ─────────────────────────────────────────────────────────────────

(defun emacs-ide-lsp-ui-doc-toggle ()
  (interactive)
  (if (and (boundp 'lsp-ui-doc--frame)
           (frame-live-p lsp-ui-doc--frame)
           (frame-visible-p lsp-ui-doc--frame))
      (when (fboundp 'lsp-ui-doc-hide)  (lsp-ui-doc-hide))
    (when (fboundp 'lsp-ui-doc-glance) (lsp-ui-doc-glance))))

(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-doc-enable              t
        lsp-ui-doc-show-with-cursor    nil
        lsp-ui-doc-show-with-mouse     t
        lsp-ui-doc-delay               0.6
        lsp-ui-doc-position            'at-point
        lsp-ui-doc-max-width           110
        lsp-ui-doc-max-height          30
        lsp-ui-doc-use-webkit          nil
        lsp-ui-sideline-enable              t
        lsp-ui-sideline-show-hover          nil
        lsp-ui-sideline-show-diagnostics    t
        lsp-ui-sideline-show-code-actions   t
        lsp-ui-sideline-ignore-duplicate    t
        lsp-ui-sideline-delay               0.3
        lsp-ui-peek-enable                  t
        lsp-ui-peek-show-directory          t
        lsp-ui-peek-list-width              60
        lsp-ui-peek-peek-height             25
        lsp-ui-imenu-enable                 t
        lsp-ui-imenu-kind-position          'left
        lsp-ui-imenu-auto-refresh           t
        lsp-ui-imenu-auto-refresh-delay     1.0)
  :bind (:map lsp-ui-mode-map
              ("M-."     . lsp-ui-peek-find-definitions)
              ("M-?"     . lsp-ui-peek-find-references)
              ("C-c l u" . emacs-ide-lsp-ui-doc-toggle)))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (when (fboundp 'lsp-treemacs-sync-mode)
    (lsp-treemacs-sync-mode 1)))

;;; ─── lsp-pyright ────────────────────────────────────────────────────────────

(use-package lsp-pyright
  :after lsp-mode
  :if (or (executable-find "pyright") (executable-find "pyright-langserver"))
  :init
  (setq lsp-pyright-multi-root               nil
        lsp-pyright-auto-import-completions   t
        lsp-pyright-auto-search-paths         t
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-venv-path                 (expand-file-name "~/.virtualenvs"))
  :config
  (require 'lsp-pyright))

(with-eval-after-load 'rust-mode
  (when (executable-find "rust-analyzer") (require 'lsp-rust nil t)))

(with-eval-after-load 'go-mode
  (when (executable-find "gopls") (require 'lsp-go nil t)))

;;; ─── dumb-jump (fallback xref) ──────────────────────────────────────────────

(use-package dumb-jump
  :bind (("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g O" . dumb-jump-go-other-window))
  :init
  (setq dumb-jump-selector           'completing-read
        dumb-jump-aggressive         t
        dumb-jump-prefer-searcher    'rg
        dumb-jump-rg-search-args     "--pcre2")
  :config
  (when (fboundp 'dumb-jump-xref-activate)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))

;;; ─── eldoc ──────────────────────────────────────────────────────────────────

(use-package eldoc
  :straight nil
  :init
  (setq eldoc-idle-delay                  0.1
        eldoc-echo-area-use-multiline-p   'truncate-sym-name-if-fit
        eldoc-echo-area-prefer-doc-buffer  t
        eldoc-documentation-strategy      'eldoc-documentation-compose-eagerly)
  :config
  (global-eldoc-mode 1))

) ;; end (when emacs-ide-lsp-enable ...)

;;; ─── Status commands ─────────────────────────────────────────────────────────

(defun emacs-ide-lsp-status ()
  (interactive)
  (if (not (bound-and-true-p emacs-ide-lsp-enable))
      (message "LSP: disabled in config")
    (if (bound-and-true-p lsp-mode)
        (message "LSP: %s | workspace: %s | inlay-hints: %s"
                 (if (and (fboundp 'lsp-workspaces) (lsp-workspaces))
                     "✓ connected" "✗ disconnected")
                 (or (and (fboundp 'lsp-workspace-root)
                          (lsp-workspace-root)) "none")
                 (if (bound-and-true-p lsp-inlay-hints-mode) "on" "off"))
      (message "LSP: not active in this buffer"))))

(defun emacs-ide-lsp-check-servers ()
  (interactive)
  (let* ((servers '(("pyright"                    . "Python")
                    ("rust-analyzer"              . "Rust")
                    ("gopls"                      . "Go")
                    ("typescript-language-server" . "TypeScript/JS")
                    ("clangd"                     . "C/C++")
                    ("jdtls"                      . "Java")
                    ("kotlin-language-server"     . "Kotlin")
                    ("lua-language-server"        . "Lua")
                    ("bash-language-server"       . "Shell")
                    ("sqls"                       . "SQL")
                    ("zls"                        . "Zig")
                    ("nil"                        . "Nix")))
         (available (cl-remove-if-not (lambda (s) (executable-find (car s))) servers))
         (missing   (cl-remove-if     (lambda (s) (executable-find (car s))) servers)))
    (with-output-to-temp-buffer "*LSP Servers*"
      (princ (format "=== LSP SERVERS  (enabled: %s) ===\n\n"
                     (if (bound-and-true-p emacs-ide-lsp-enable) "YES" "NO")))
      (princ (format "Available: %d\n" (length available)))
      (dolist (s available) (princ (format "  ✓ %-35s (%s)\n" (car s) (cdr s))))
      (when missing
        (princ (format "\nMissing: %d\n" (length missing)))
        (dolist (s missing) (princ (format "  ✗ %-35s (%s)\n" (car s) (cdr s)))))
      (princ "\nInstall: M-x lsp-install-server\n"))))

(unless (bound-and-true-p emacs-ide-lsp-enable)
  (message "⚠  LSP disabled in config — tools-lsp loaded (status cmds available)"))

(provide 'tools-lsp)
;;; tools-lsp.el ends here
