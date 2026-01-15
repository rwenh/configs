;;; completion-config.el --- Elite Completion Framework -*- lexical-binding: t -*-
;;; Commentary:
;;; Vertico + Consult + Company + Corfu - Maximum efficiency
;;; Code:

;; ============================================================================
;; VERTICO - VERTICAL COMPLETION
;; ============================================================================
(use-package vertico
  :demand t
  :init
  (setq vertico-scroll-margin 0
        vertico-count 20
        vertico-resize t
        vertico-cycle t)
  :config
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (setq vertico-multiform-categories
        '((file grid)
          (consult-grep buffer)
          (imenu buffer)
          (buffer flat))))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; ============================================================================
;; ORDERLESS - FLEXIBLE MATCHING
;; ============================================================================
(use-package orderless
  :demand t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space
        orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

;; ============================================================================
;; MARGINALIA - RICH ANNOTATIONS
;; ============================================================================
(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (setq marginalia-align 'right
        marginalia-align-offset 0
        marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode 1))

;; ============================================================================
;; CONSULT - ENHANCED SEARCH
;; ============================================================================
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s r" . consult-ripgrep)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("C-c f" . consult-recent-file)
         :map isearch-mode-map
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :init
  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-project-function #'projectile-project-root
        consult-preview-key 'any)
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

;; ============================================================================
;; EMBARK - CONTEXTUAL ACTIONS
;; ============================================================================
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command
        embark-indicators '(embark-minimal-indicator
                           embark-highlight-indicator
                           embark-isearch-highlight-indicator)
        embark-quit-after-action '((kill-buffer . nil)
                                  (t . t))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================================
;; COMPANY - CODE COMPLETION
;; ============================================================================
(use-package company
  :hook ((prog-mode text-mode org-mode) . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)
              ("<tab>" . company-complete-selection)
              ("TAB" . company-complete-selection)
              ("C-h" . company-show-doc-buffer)
              ("C-w" . nil)
              ("RET" . nil)
              ("<return>" . nil))
  :init
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0
        company-tooltip-limit 20
        company-tooltip-align-annotations t
        company-require-match nil
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-show-numbers t
        company-tooltip-flip-when-above t
        company-tooltip-offset-display 'lines
        company-transformers '(company-sort-by-occurrence)
        company-backends '((company-capf :with company-yasnippet)
                          (company-dabbrev-code company-keywords company-files)
                          company-dabbrev)))

(use-package company-box
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-max-candidates 50
        company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-backends-colors nil
        company-box-show-single-candidate t
        company-box-frame-behavior 'point
        company-box-doc-enable t
        company-box-doc-delay 0.3))

;; ============================================================================
;; CORFU - INLINE COMPLETION
;; ============================================================================
(use-package corfu
  :demand t
  :init
  (setq corfu-auto t
        corfu-auto-delay 0.0
        corfu-auto-prefix 1
        corfu-cycle t
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preview-current 'insert
        corfu-preselect 'prompt
        corfu-on-exact-match nil
        corfu-scroll-margin 5
        corfu-count 20
        corfu-max-width 100
        corfu-min-width 20)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET" . corfu-insert)
              ([return] . corfu-insert))
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; ============================================================================
;; YASNIPPET
;; ============================================================================
(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :bind (("C-c y e" . yas-expand)
         ("C-c y n" . yas-new-snippet)
         ("C-c y v" . yas-visit-snippet-file)
         ("C-c y i" . yas-insert-snippet))
  :init
  (setq yas-verbosity 1
        yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory))
        yas-triggers-in-field t
        yas-wrap-around-region t)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package yasnippet-capf
  :after (yasnippet cape)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

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
        try-complete-lisp-symbol
        yas-hippie-try-expand))

(global-set-key (kbd "M-/") 'hippie-expand)

;; ============================================================================
;; RECENTF
;; ============================================================================
(recentf-mode 1)
(setq recentf-max-saved-items 500
      recentf-max-menu-items 50
      recentf-auto-cleanup 'never
      recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$"
                        "\\.revive$" "/TAGS$" "^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'" "^/sudo:" "^/scp:"
                        "\\.emacs\\.d/.*" "\\.cache/.*"))

(run-at-time nil (* 5 60) 'recentf-save-list)

;; ============================================================================
;; SAVEHIST
;; ============================================================================
(savehist-mode 1)
(setq savehist-additional-variables '(search-ring regexp-search-ring
                                      kill-ring compile-history
                                      command-history register-alist)
      history-length 10000
      history-delete-duplicates t
      savehist-autosave-interval 60
      savehist-save-minibuffer-history t)

;; ============================================================================
;; SAVE PLACE
;; ============================================================================
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil
      save-place-file (expand-file-name "places" user-emacs-directory))

;; ============================================================================
;; ABBREV MODE
;; ============================================================================
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently
      abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

;; ============================================================================
;; BOOKMARKS
;; ============================================================================
(setq bookmark-save-flag 1
      bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)
      bookmark-version-control t)

;; ============================================================================
;; AUTO-COMPLETE PAIRS
;; ============================================================================
(electric-pair-mode 1)
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\' . ?\')
        (?\{ . ?\})
        (?\[ . ?\])
        (?\( . ?\))
        (?` . ?`)))

;; ============================================================================
;; COMPLETION STYLES
;; ============================================================================
(setq completion-cycle-threshold 3
      completion-auto-help 'always
      completion-auto-select 'second-tab
      completion-show-help nil
      completions-detailed t
      completions-format 'one-column
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; ============================================================================
;; TAB COMPLETION
;; ============================================================================
(setq tab-always-indent 'complete
      tab-first-completion 'word-or-paren-or-punct)

;; ============================================================================
;; MINIBUFFER SETTINGS
;; ============================================================================
(setq enable-recursive-minibuffers t
      minibuffer-depth-indicate-mode t
      minibuffer-eldef-shorten-default t
      resize-mini-windows t
      max-mini-window-height 0.33)

(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

;; ============================================================================
;; FILE NAME COMPLETION
;; ============================================================================
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; ============================================================================
;; ICOMPLETE (fallback)
;; ============================================================================
(fido-vertical-mode -1)
(icomplete-mode -1)

;; ============================================================================
;; CUSTOM COMPLETION FUNCTIONS
;; ============================================================================
(defun emacs-ide-complete-or-indent ()
  "Complete or indent."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (and (fboundp 'company-manual-begin)
             (company-manual-begin))
        (company-complete-common)
      (indent-for-tab-command))))

(global-set-key (kbd "TAB") 'emacs-ide-complete-or-indent)

(provide 'completion-config)
;;; completion-config.el ends here