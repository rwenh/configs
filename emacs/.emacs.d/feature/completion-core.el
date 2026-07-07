;;; completion-core.el --- Elite Completion Framework -*- lexical-binding: t -*-
;;; Version: 3.4.0
;;;
;;; Code:

;;;; ── Vertico ──────────────────────────────────────────────────────────────────

(use-package vertico
  :init
  (setq vertico-scroll-margin 0
        vertico-count         20
        vertico-resize        t
        vertico-cycle         t)
  :config
  (vertico-mode 1)
  (when (fboundp 'vertico-multiform-mode)
    (vertico-multiform-mode 1)
    (require 'vertico-sort nil t)
    (setq vertico-multiform-categories
          `((file        grid)
            (consult-grep buffer)
            (imenu        buffer)
            (buffer       flat)
            (symbol       (vertico-sort-function . vertico-sort-alpha))
            (command      (vertico-sort-function
                           . ,(if (fboundp 'vertico-sort-history-length-alpha)
                                  'vertico-sort-history-length-alpha
                                'vertico-sort-alpha)))))))

(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;;; ── Orderless ───────────────────────────────────────────────────────────────

(use-package orderless
  :init
  (setq completion-styles             '(orderless basic)
        completion-category-defaults  nil
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space
        ;; Respect emacs-ide-completion-fuzzy from config
        orderless-matching-styles
        (if (bound-and-true-p emacs-ide-completion-fuzzy)
            '(orderless-literal orderless-regexp orderless-flex)
          '(orderless-literal orderless-regexp))))

;;;; ── Marginalia ──────────────────────────────────────────────────────────────

(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (setq marginalia-align         'right
        marginalia-align-offset   0
        marginalia-truncate-width 120)
  :config (marginalia-mode 1))

;;;; ── Consult ─────────────────────────────────────────────────────────────────

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
  (setq consult-narrow-key           "<"
        consult-preview-key          '(:debounce 0.2 any)
        consult-ripgrep-args
        (concat "rg --null --line-buffered --color=never --max-columns=1000 "
                "--path-separator / --smart-case --no-heading --with-filename "
                "--line-number --search-zip --hidden --glob '!.git'")
        register-preview-delay       0.3
        register-preview-function    #'consult-register-format
        consult-async-min-input      2
        consult-async-refresh-delay  0.15
        consult-async-input-debounce 0.1
        consult-async-input-throttle 0.2)
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   :preview-key '(:debounce 0.4 any))
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-find-args
        "find . -not ( -wholename */.git* -prune )"))

(use-package consult-lsp
  :after (consult lsp-mode)
  :bind (("M-g s" . consult-lsp-symbols)
         ("M-g e" . consult-lsp-diagnostics)))

(use-package consult-projectile
  :after (consult projectile)
  :bind (("C-c p B" . consult-projectile-switch-to-buffer)
         ("C-c p F" . consult-projectile-find-file)
         ("C-c p P" . consult-projectile-switch-project)))

;;;; ── Embark ──────────────────────────────────────────────────────────────────

(use-package embark
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command  #'embark-prefix-help-command
        embark-quit-after-action
        '((kill-buffer . nil) (t . t)))
  :config
  (defun embark-which-key-indicator (&optional keymap targets prefix)
    "Show the Embark action menu using which-key."
    (if (null keymap)
        (when (fboundp 'which-key--hide-popup-ignore-command)
          (which-key--hide-popup-ignore-command))
      (when (fboundp 'which-key--show-keymap)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target
                    (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix (lookup-key keymap prefix) keymap)
         nil nil t
         (lambda (binding)
           (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        (list #'embark-which-key-indicator
              #'embark-highlight-indicator
              #'embark-isearch-highlight-indicator))

  (with-eval-after-load 'magit
    (define-key embark-file-map   (kbd "g") #'magit-status)
    (define-key embark-buffer-map (kbd "g") #'magit-status))
  (define-key embark-symbol-map     (kbd "h") #'helpful-symbol)
  (define-key embark-identifier-map (kbd "d") #'xref-find-definitions)
  (define-key embark-identifier-map (kbd "r") #'xref-find-references))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;; ── Corfu (in-buffer completion) ───────────────────────────────────────────

(use-package corfu
  :demand t
  :init
  (setq corfu-auto             (if (boundp 'emacs-ide-completion-auto)
                                   emacs-ide-completion-auto
                                 t)
        corfu-auto-delay       (if (and (fboundp 'emacs-ide-config-get)
                                        (boundp 'emacs-ide-config-data))
                                   (or (emacs-ide-config-get
                                        'completion 'delay nil)
                                       0.15)
                                 0.15)
        corfu-auto-prefix      (if (and (fboundp 'emacs-ide-config-get)
                                        (boundp 'emacs-ide-config-data))
                                   (or (emacs-ide-config-get
                                        'completion 'auto-prefix nil)
                                       1)
                                 1)
        corfu-count            (if (and (fboundp 'emacs-ide-config-get)
                                        (boundp 'emacs-ide-config-data))
                                   (or (emacs-ide-config-get
                                        'completion 'popup-height nil)
                                       16)
                                 16)
        corfu-cycle            t
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match    'separator
        corfu-preview-current  (if (boundp 'emacs-ide-completion-preview)
                                   emacs-ide-completion-preview
                                 t)
        corfu-preselect        'prompt
        corfu-on-exact-match   nil
        corfu-scroll-margin    4
        corfu-max-width        90
        corfu-min-width        20
        corfu-bar-width        0.5
        corfu-left-margin-width 0.5)
  :bind (:map corfu-map
              ("TAB"     . corfu-next)
              ([tab]     . corfu-next)
              ("S-TAB"   . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET"     . corfu-insert)
              ([return]  . corfu-insert)
              ("M-SPC"   . corfu-insert-separator)
              ("M-d"     . corfu-popupinfo-toggle))
  :config
  (global-corfu-mode 1)
  (when (fboundp 'corfu-popupinfo-mode)
    (corfu-popupinfo-mode 1)
    (setq corfu-popupinfo-delay      '(0.5 . 0.2)
          corfu-popupinfo-max-height  20
          corfu-popupinfo-max-width   80))
  (when (fboundp 'corfu-history-mode)
    (corfu-history-mode 1))
  (add-hook 'emacs-ide-config-reload-hook
            (lambda ()
              (when (fboundp 'emacs-ide-config-get)
                (when-let ((delay (emacs-ide-config-get 'completion 'delay nil)))
                  (setq corfu-auto-delay delay))
                (when-let ((pfx (emacs-ide-config-get 'completion 'auto-prefix nil)))
                  (setq corfu-auto-prefix pfx))
                (when-let ((h (emacs-ide-config-get 'completion 'popup-height nil)))
                  (setq corfu-count h))
                (when (boundp 'emacs-ide-completion-auto)
                  (setq corfu-auto emacs-ide-completion-auto))
                (when (boundp 'emacs-ide-completion-preview)
                  (setq corfu-preview-current emacs-ide-completion-preview))))))

;;;; ── Cape (completion extensions) ──────────────────────────────────────────

(use-package cape
  :config
  (defun emacs-ide-cape-setup ()
    "Add Cape completion sources to the local capf list."
    (dolist (fn (list #'cape-keyword
                      #'cape-elisp-block
                      #'cape-file
                      #'cape-dabbrev))
      (cl-pushnew fn completion-at-point-functions)))
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'emacs-ide-cape-setup))
  (add-to-list 'completion-at-point-functions #'cape-file))

;;;; ── Hippie expand ───────────────────────────────────────────────────────────

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
  (add-to-list 'hippie-expand-try-functions-list
               'yas-hippie-try-expand))

;;;; ── History persistence ─────────────────────────────────────────────────────

(use-package savehist
  :straight nil
  :init
  (setq savehist-additional-variables
        '(search-ring
          regexp-search-ring
          compile-history
          command-history
          extended-command-history)
        history-length            25000
        history-delete-duplicates t
        savehist-autosave-interval 60)
  :config (savehist-mode 1))

(use-package saveplace
  :straight nil
  :init
  (setq save-place-file
        (expand-file-name "var/places" user-emacs-directory))
  :config (save-place-mode 1))

;;;; ── Abbreviations ───────────────────────────────────────────────────────────

(setq-default abbrev-mode t)
(setq save-abbrevs    'silently
      abbrev-file-name (expand-file-name "var/abbrev_defs" user-emacs-directory))
(when (file-exists-p abbrev-file-name)
  (read-abbrev-file abbrev-file-name t))

;;;; ── Minibuffer polish ───────────────────────────────────────────────────────

(setq completion-cycle-threshold            3
      completion-auto-help                 'always
      completion-auto-select               'second-tab
      completions-detailed                  t
      completions-format                   'one-column
      completion-ignore-case                t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case    t
      tab-always-indent                    'complete
      enable-recursive-minibuffers          t
      resize-mini-windows                   t
      max-mini-window-height                0.4)

(minibuffer-depth-indicate-mode  1)
(minibuffer-electric-default-mode 1)

(provide 'completion-core)
;;; completion-core.el ends here
