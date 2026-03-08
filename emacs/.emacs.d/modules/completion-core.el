;;; completion-core.el --- Elite Completion Framework -*- lexical-binding: t -*-
;;; Commentary:
;;; Vertico + Consult + Corfu - Maximum efficiency with proper config integration
;;; Version: 2.2.3
;;; Fixes:
;;;   - 2.2.2: corfu-preview-current changed from 'insert to t.
;;;     'insert auto-inserts the top candidate as you type, which breaks
;;;     normal typing whenever a wrong completion is preselected. 't means
;;;     preview-only (shown but not committed until TAB/RET).
;;;   - Removed single-quote from electric-pair-pairs (breaks Elisp/most langs)
;;;   - save-place-file moved to var/ subdirectory
;;;   - abbrev-file-name moved to var/ subdirectory
;;;   - Removed duplicate recentf/bookmark configs (canonical in tools-project.el)
;;;   - TAB global rebind scoped to prog-mode/text-mode only
;;;   - 2.2.3: marginalia-annotators removed — dropped in marginalia 1.0 (Nov 2022),
;;;     replaced by marginalia-annotator-registry. Setting it caused a void-variable
;;;     warning on every startup.
;;; Code:

;; ============================================================================
;; VERTICO - VERTICAL COMPLETION (DEFERRED)
;; ============================================================================
(use-package vertico
  :init
  (setq vertico-scroll-margin 0
        vertico-count 20
        vertico-resize t
        vertico-cycle t)
  :config
  (vertico-mode 1)
  (when (fboundp 'vertico-multiform-mode)
    (vertico-multiform-mode 1))
  (setq vertico-multiform-categories
        '((file grid)
          (consult-grep buffer)
          (imenu buffer)
          (buffer flat))))

(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; ============================================================================
;; ORDERLESS - FLEXIBLE MATCHING (DEFERRED)
;; ============================================================================
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space
        orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

;; ============================================================================
;; MARGINALIA - RICH ANNOTATIONS (DEFERRED)
;; ============================================================================
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (setq marginalia-align 'right
        marginalia-align-offset 0)
        ;; FIX 2.2.3: marginalia-annotators removed — the variable was dropped in
        ;; marginalia 1.0 (Nov 2022) and replaced by marginalia-annotator-registry.
        ;; Setting it produced a void-variable warning on every startup.
  :config
  (marginalia-mode 1))

;; ============================================================================
;; CONSULT - ENHANCED SEARCH (DEFERRED)
;; ============================================================================
(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("M-y"     . consult-yank-pop)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o"   . consult-outline)
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         ("M-s r"   . consult-ripgrep)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s f"   . consult-find)
         ("M-s F"   . consult-locate)
         ("C-c f"   . consult-recent-file)
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
;; EMBARK - CONTEXTUAL ACTIONS (DEFERRED)
;; ============================================================================
(use-package embark
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
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
;; CORFU - INLINE COMPLETION (MAIN SYSTEM - DEFERRED)
;; FIX 2.2.2: corfu-preview-current changed from 'insert to t.
;;   'insert commits the top candidate into the buffer as you type —
;;   this breaks normal typing (e.g. typing "foobar" auto-inserts a
;;   wrong completion after every character). 't shows a preview
;;   highlight only; the candidate is not inserted until TAB/RET.
;; ============================================================================
(use-package corfu
  :init
  (setq corfu-auto t
        corfu-auto-delay (or (bound-and-true-p emacs-ide-completion-delay) 0.1)
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preview-current t          ; FIX: was 'insert (aggressive auto-insert)
        corfu-preselect 'prompt
        corfu-on-exact-match nil
        corfu-scroll-margin 5
        corfu-count 20
        corfu-max-width 100
        corfu-min-width 20)
  :bind (:map corfu-map
              ("TAB"     . corfu-next)
              ([tab]     . corfu-next)
              ("S-TAB"   . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET"     . corfu-insert)
              ([return]  . corfu-insert))
  :config
  (global-corfu-mode 1)
  (when (fboundp 'corfu-popupinfo-mode)
    (corfu-popupinfo-mode 1))
  (when (fboundp 'corfu-history-mode)
    (corfu-history-mode 1)))

;; ============================================================================
;; CAPE - COMPLETION AT POINT EXTENSION (DEFERRED)
;; ============================================================================
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; ============================================================================
;; HIPPIE EXPAND WITH YASNIPPET INTEGRATION
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
;; SAVEHIST - MINIBUFFER HISTORY (BUILTIN)
;; ============================================================================
(use-package savehist
  :straight nil
  :init
  (setq savehist-additional-variables '(search-ring regexp-search-ring
                                        kill-ring compile-history
                                        command-history register-alist)
        history-length 10000
        history-delete-duplicates t
        savehist-autosave-interval 60
        savehist-save-minibuffer-history t)
  :config
  (savehist-mode 1))

;; ============================================================================
;; SAVE PLACE (BUILTIN)
;; ============================================================================
(use-package saveplace
  :straight nil
  :init
  (setq save-place-forget-unreadable-files nil
        save-place-file (expand-file-name "var/places" user-emacs-directory))
  :config
  (save-place-mode 1))

;; ============================================================================
;; ABBREV MODE (BUILTIN)
;; ============================================================================
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently
      abbrev-file-name (expand-file-name "var/abbrev_defs" user-emacs-directory))

(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

;; ============================================================================
;; AUTO-COMPLETE PAIRS (BUILTIN)
;; FIX: Removed (?'  . ?') from electric-pair-pairs — single-quote pairing
;;      is harmful in Emacs Lisp (where ' is a reader prefix) and many other
;;      languages.
;; ============================================================================
(electric-pair-mode 1)
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\{ . ?\})
        (?\[ . ?\])
        (?\( . ?\))
        (?` . ?`)))

;; ============================================================================
;; COMPLETION STYLES (BUILTIN)
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
;; TAB COMPLETION (BUILTIN)
;; ============================================================================
(setq tab-always-indent 'complete
      tab-first-completion 'word-or-paren-or-punct)

;; ============================================================================
;; MINIBUFFER SETTINGS (BUILTIN)
;; ============================================================================
(setq enable-recursive-minibuffers t
      minibuffer-depth-indicate-mode t
      minibuffer-eldef-shorten-default t
      resize-mini-windows t
      max-mini-window-height 0.33)

(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

;; ============================================================================
;; CUSTOM COMPLETION FUNCTIONS
;; FIX: TAB rebind scoped to prog-mode and text-mode only via hooks.
;;      Global TAB rebind was breaking org-mode, vterm, and other modes.
;; ============================================================================
(defun emacs-ide-complete-or-indent ()
  "Complete or indent intelligently."
  (interactive)
  (if (minibufferp)
      (when (fboundp 'minibuffer-complete)
        (minibuffer-complete))
    (if (looking-at "\\>")
        (when (fboundp 'completion-at-point)
          (completion-at-point))
      (indent-for-tab-command))))

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook
            (lambda ()
              (local-set-key (kbd "TAB") 'emacs-ide-complete-or-indent))))

(provide 'completion-core)
;;; completion-core.el ends here
