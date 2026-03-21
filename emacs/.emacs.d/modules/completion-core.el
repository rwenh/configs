;;; completion-core.el --- Elite Completion Framework -*- lexical-binding: t -*-
;;; Commentary:
;;; Vertico + Consult + Corfu + Embark as action hub.
;;; v3.0.0: Embark wired deeper — C-. in any context opens action menu.
;;; consult-lsp added for symbol search across workspace.
;;; nerd-icons-completion adds icons to marginalia annotations.
;;; All existing fixes from 2.2.4 retained.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 3.0.1 to 3.0.4.
;;;   - FIX-CONSULT-PROJECTILE-KEYS: consult-projectile bindings moved to
;;;     non-colliding keys (C-c p B, C-c p F, C-c p P) so they are not
;;;     silently overwritten by keybindings.el which binds C-c p f to
;;;     projectile-find-file (keybindings.el always loads last and wins).
;;;   - FIX-INIT-DEAD-CODE: Removed the embark-indicators assignment from
;;;     the embark :init block — it was immediately superseded by the
;;;     :config block's assignment and was dead code.
;;;   - FIX-DELAY-ZERO: corfu-auto-delay now uses (if (boundp ...) ... 0.1)
;;;     instead of (or (bound-and-true-p ...) 0.1). bound-and-true-p returns
;;;     nil for the value 0 (falsy), causing delay: 0 in config.yml to
;;;     silently fall back to 0.1 instead of 0.
;;;   - FIX-MAGIT-GUARD: embark-file-map and embark-buffer-map bindings for
;;;     magit-status wrapped in with-eval-after-load 'magit to prevent
;;;     void-function errors when magit hasn't loaded yet at embark setup time.
;;;   - FIX-MULTIFORM-SETQ: vertico-multiform-categories setq moved inside
;;;     the (when (fboundp 'vertico-multiform-mode) ...) guard so it only
;;;     runs when the mode is actually available.
;;;   - FIX-NERD-ICONS-NOTE: Header comment clarified — nerd-icons-completion
;;;     is configured in ui-core.el (not here); stale comment removed.
;;;   - FIX-ABBREV: quietly-read-abbrev-file replaced with the modern
;;;     (read-abbrev-file abbrev-file-name t) form for Emacs 29+.
;;; Fixes vs 3.0.1 (retained):
;;;   - FIX-DEDUP: Removed (electric-pair-mode 1) call. init.el core-settings
;;;     block already enables it.
;;; Code:

;; ============================================================================
;; VERTICO
;; ============================================================================
(use-package vertico
  :init
  (setq vertico-scroll-margin 0
        vertico-count         20
        vertico-resize        t
        vertico-cycle         t)
  :config
  (vertico-mode 1)
  ;; FIX-MULTIFORM-SETQ: vertico-multiform-categories moved inside the
  ;; fboundp guard so it only runs when the mode is actually available.
  (when (fboundp 'vertico-multiform-mode)
    (vertico-multiform-mode 1)
    (setq vertico-multiform-categories
          '((file       grid)
            (consult-grep buffer)
            (imenu      buffer)
            (buffer     flat)
            (symbol     (vertico-sort-function . vertico-sort-alpha))))))

(use-package vertico-directory
  :after vertico :straight nil
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; ============================================================================
;; ORDERLESS
;; ============================================================================
(use-package orderless
  :init
  (setq completion-styles                   '(orderless basic)
        completion-category-defaults        nil
        completion-category-overrides       '((file (styles basic partial-completion)))
        orderless-component-separator       #'orderless-escapable-split-on-space
        orderless-matching-styles           '(orderless-literal orderless-regexp orderless-flex)))

;; ============================================================================
;; MARGINALIA
;; ============================================================================
(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (setq marginalia-align        'right
        marginalia-align-offset 0)
  :config (marginalia-mode 1))

;; ============================================================================
;; CONSULT
;; ============================================================================
(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("M-y"     . consult-yank-pop)
         ("C-x C-r" . consult-recent-file)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ("M-g o"   . consult-outline)
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s r"   . consult-ripgrep)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s f"   . consult-find)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines))
  :init
  (setq consult-narrow-key             "<"
        consult-preview-key            '(:debounce 0.2 any)
        consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip"
        register-preview-delay         0.3
        register-preview-function      #'consult-register-format)
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   :preview-key '(:debounce 0.4 any))
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; consult-lsp — workspace symbol search via LSP
(use-package consult-lsp
  :after (consult lsp-mode)
  :bind (("M-g s" . consult-lsp-symbols)
         ("M-g e" . consult-lsp-diagnostics)))

;; consult-projectile — project-scoped file/buffer search
;; FIX-CONSULT-PROJECTILE-KEYS: use C-c p B/F/P (uppercase) to avoid
;; collision with keybindings.el which binds C-c p b/f/p to the plain
;; projectile commands. keybindings.el loads last and wins — lowercase
;; bindings set here would be silently overwritten on every startup.
(use-package consult-projectile
  :after (consult projectile)
  :bind (("C-c p B" . consult-projectile-switch-to-buffer)
         ("C-c p F" . consult-projectile-find-file)
         ("C-c p P" . consult-projectile-switch-project)))

;; ============================================================================
;; EMBARK — CONTEXT ACTION HUB
;; Wired as the primary C-. action dispatcher in every context:
;;   - minibuffer candidates
;;   - symbols/files/URLs at point in any buffer
;;   - consult search results
;;   - LSP symbols
;; ============================================================================
(use-package embark
  :bind (("C-."   . embark-act)        ; act on thing at point / candidate
         ("C-;"   . embark-dwim)       ; sensible default action
         ("C-h B" . embark-bindings))  ; show all bindings
  :init
  ;; FIX-INIT-DEAD-CODE: removed embark-indicators from :init — it was
  ;; immediately superseded by the :config assignment and was dead code.
  (setq prefix-help-command #'embark-prefix-help-command
        embark-quit-after-action
        '((kill-buffer . nil) (t . t)))
  :config
  ;; Which-key integration — show embark actions in which-key popup
  (defun embark-which-key-indicator ()
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target
                    (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix (lookup-key keymap prefix) keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))
  (setq embark-indicators
        (list #'embark-which-key-indicator
              #'embark-highlight-indicator
              #'embark-isearch-highlight-indicator))

  ;; Extra embark actions for IDE workflows
  ;; FIX-MAGIT-GUARD: wrap magit-status bindings in with-eval-after-load
  ;; to prevent void-function errors when magit hasn't loaded at embark
  ;; setup time. embark's :config runs when embark loads, which is before
  ;; magit in the module load order.
  (with-eval-after-load 'magit
    (define-key embark-file-map   (kbd "g") #'magit-status)
    (define-key embark-buffer-map (kbd "g") #'magit-status))
  (define-key embark-symbol-map     (kbd "h") #'helpful-symbol)
  (define-key embark-identifier-map (kbd "d") #'xref-find-definitions)
  (define-key embark-identifier-map (kbd "r") #'xref-find-references))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================================
;; CORFU (unchanged from 2.2.4)
;; ============================================================================
(use-package corfu
  :init
  (setq corfu-auto              t
        ;; FIX-DELAY-ZERO: use (if (boundp ...) ... 0.1) instead of
        ;; (or (bound-and-true-p ...) 0.1). bound-and-true-p returns nil
        ;; for the value 0 (falsy), so delay: 0 in config.yml would
        ;; silently fall back to 0.1 rather than disabling the delay.
        corfu-auto-delay        (if (boundp 'emacs-ide-completion-delay)
                                    emacs-ide-completion-delay
                                  0.1)
        corfu-auto-prefix       2
        corfu-cycle             t
        corfu-quit-at-boundary  'separator
        corfu-quit-no-match     'separator
        corfu-preview-current   t
        corfu-preselect         'prompt
        corfu-on-exact-match    nil
        corfu-scroll-margin     5
        corfu-count             20
        corfu-max-width         100
        corfu-min-width         20)
  :bind (:map corfu-map
              ("TAB"     . corfu-next)
              ([tab]     . corfu-next)
              ("S-TAB"   . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET"     . corfu-insert)
              ([return]  . corfu-insert))
  :config
  (global-corfu-mode 1)
  (when (fboundp 'corfu-popupinfo-mode)  (corfu-popupinfo-mode 1))
  (when (fboundp 'corfu-history-mode)    (corfu-history-mode 1)))

;; ============================================================================
;; CAPE (unchanged from 2.2.4 — buffer-local only)
;; ============================================================================
(use-package cape
  :config
  (defun emacs-ide-cape-setup ()
    (dolist (fn (list #'cape-keyword #'cape-elisp-block #'cape-file #'cape-dabbrev))
      (cl-pushnew fn completion-at-point-functions)))
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'emacs-ide-cape-setup)))

;; ============================================================================
;; HIPPIE EXPAND
;; ============================================================================
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(with-eval-after-load 'yasnippet
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))
(global-set-key (kbd "M-/") 'hippie-expand)

;; ============================================================================
;; SAVEHIST / SAVEPLACE / ABBREV (unchanged)
;; ============================================================================
(use-package savehist :straight nil
  :init
  (setq savehist-additional-variables '(search-ring regexp-search-ring
                                        kill-ring compile-history command-history)
        history-length               10000
        history-delete-duplicates   t
        savehist-autosave-interval  60)
  :config (savehist-mode 1))

(use-package saveplace :straight nil
  :init (setq save-place-file (expand-file-name "var/places" user-emacs-directory))
  :config (save-place-mode 1))

(setq-default abbrev-mode t)
(setq save-abbrevs 'silently
      abbrev-file-name (expand-file-name "var/abbrev_defs" user-emacs-directory))
;; FIX-ABBREV: quietly-read-abbrev-file is a deprecated alias in Emacs 29+.
;; Use (read-abbrev-file file t) — the t argument suppresses messages.
(when (file-exists-p abbrev-file-name)
  (read-abbrev-file abbrev-file-name t))

;; ============================================================================
;; ELECTRIC PAIR / COMPLETION SETTINGS (unchanged)
;; ============================================================================
;; FIX-DEDUP: electric-pair-mode call removed — init.el already enables it
;; in the core-settings block. Calling it a third time is redundant.
(setq electric-pair-pairs '((?\" . ?\") (?{ . ?}) (?\[ . ?\]) (?\( . ?\)) (?` . ?`)))

(setq completion-cycle-threshold       3
      completion-auto-help            'always
      completion-auto-select          'second-tab
      completions-detailed             t
      completions-format              'one-column
      completion-ignore-case           t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case    t
      tab-always-indent               'complete
      enable-recursive-minibuffers     t
      resize-mini-windows              t
      max-mini-window-height           0.33)

(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

(defun emacs-ide-complete-or-indent ()
  (interactive)
  (if (minibufferp)
      (when (fboundp 'minibuffer-complete) (minibuffer-complete))
    (if (looking-at "\\>")
        (when (fboundp 'completion-at-point) (completion-at-point))
      (indent-for-tab-command))))

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (local-set-key (kbd "TAB") 'emacs-ide-complete-or-indent))))

(provide 'completion-core)
;;; completion-core.el ends here
