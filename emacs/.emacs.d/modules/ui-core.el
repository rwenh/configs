;;; ui-core.el --- Office-Grade Visual Configuration -*- lexical-binding: t -*-
;;; Version: 3.4.0
;;; Code:

(when (fboundp 'menu-bar-mode)     (menu-bar-mode   -1))
(when (fboundp 'tool-bar-mode)     (tool-bar-mode   -1))
(when (fboundp 'scroll-bar-mode)   (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode)      (tooltip-mode    -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

(setq ring-bell-function              'ignore
      visible-bell                    nil
      use-dialog-box                  nil
      use-file-dialog                 nil
      echo-keystrokes                 0.01
      inhibit-splash-screen           t
      inhibit-startup-echo-area-message t)

;;; ─── Display buffer rules ────────────────────────────────────────────────────

(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

(add-to-list 'display-buffer-alist
             '("^\\*Dictionary\\*"
               (display-buffer-in-side-window)
               (side . left)
               (window-width . 70)))

(setq Man-notify-method 'aggressive)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; ─── ef-themes ────────────────────────────────────────────────────────────────

(use-package ef-themes
  :demand t
  :init
  (setq ef-themes-to-toggle        '(ef-dark ef-light)
        ef-themes-mixed-fonts       t
        ef-themes-variable-pitch-ui t
        ef-themes-headings
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.5))
          (3 . (variable-pitch regular 1.3))
          (4 . (variable-pitch regular 1.1))
          (t . (variable-pitch regular 1.0))))
  :config
  (let* ((raw   (bound-and-true-p emacs-ide-theme))
         (theme (cond
                 ((stringp raw) (intern raw))
                 ((symbolp raw) raw)
                 (t             'ef-dark)))
         (theme (if (string-prefix-p "modus-" (symbol-name theme))
                    (if (string-suffix-p "operandi" (symbol-name theme))
                        'ef-light
                      'ef-dark)
                  theme)))
    (condition-case err
        (load-theme theme t)
      (error
       (message "ui-core: could not load theme %s (%s), falling back to ef-dark"
                theme err)
       (load-theme 'ef-dark t)))))

;;; ─── nerd-icons ───────────────────────────────────────────────────────────────

(use-package nerd-icons
  :demand t
  :init
  (setq nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  (when (display-graphic-p)
    (unless (member "Symbols Nerd Font Mono" (font-family-list))
      (message "⚠️  nerd-icons: run M-x nerd-icons-install-fonts"))))

(use-package nerd-icons-dired
  :if (display-graphic-p)
  :defer t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :if (display-graphic-p)
  :after (nerd-icons marginalia)
  :defer t
  :config (nerd-icons-completion-mode 1))

(use-package nerd-icons-corfu
  :if (display-graphic-p)
  :after (nerd-icons corfu)
  :defer t
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; ─── Font & frame (GUI only) ──────────────────────────────────────────────────

(when (display-graphic-p)
  (setq frame-resize-pixelwise  t
        window-resize-pixelwise t)

  (defun emacs-ide-set-font (font-name size &optional weight)
    (when (member font-name (font-family-list))
      (set-face-attribute 'default nil
                          :font   font-name
                          :height (* size 10)
                          :weight (or weight 'normal))
      t))

  (let ((font (or (bound-and-true-p emacs-ide-font) "JetBrains Mono"))
        (size (or (bound-and-true-p emacs-ide-font-size) 11)))
    (or (emacs-ide-set-font font size 'medium)
        (emacs-ide-set-font "Cascadia Code"   size)
        (emacs-ide-set-font "Fira Code"       size)
        (emacs-ide-set-font "Iosevka"         size)
        (emacs-ide-set-font "Source Code Pro" size)))

  (condition-case nil
      (or (set-face-attribute 'variable-pitch nil :font "Inter-11")
          (set-face-attribute 'variable-pitch nil :font "Cantarell-11"))
    (error nil))

  (use-package ligature :defer t)
  (add-hook 'after-init-hook
            (lambda ()
              (when (fboundp 'ligature-set-ligatures)
                (ligature-set-ligatures
                 'prog-mode
                 '("->" "<-" "=>" "<=" ">=" "!=" "==" "//" "/*" "*/"
                   "..." "::" "&&" "||" "++" "--" "<<" ">>" "<>" "|>"
                   "<|" "##" "###" "####" ";;"))
                (global-ligature-mode t))))

  (when (fboundp 'set-fontset-font)
    (when (member "Noto Color Emoji" (font-family-list))
      (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend))))

;;; ─── Smooth scrolling ────────────────────────────────────────────────────────

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-use-momentum         t
        pixel-scroll-precision-large-scroll-height  40.0
        pixel-scroll-precision-interpolation-factor 1.0))

;;; ─── Line numbers ─────────────────────────────────────────────────────────────

(let ((line-nums-on (if (fboundp 'emacs-ide-config-get)
                        (emacs-ide-config-get 'features 'line-numbers t)
                      t))
      (relative-on (if (fboundp 'emacs-ide-config-get)
                       (emacs-ide-config-get 'features 'relative-line-numbers nil)
                     nil)))
  (when line-nums-on
    (global-display-line-numbers-mode 1)
    (setq display-line-numbers-type        (if relative-on 'relative t)
          display-line-numbers-width       3
          display-line-numbers-grow-only   t
          display-line-numbers-width-start t)))

(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook
                eshell-mode-hook vterm-mode-hook dashboard-mode-hook
                neotree-mode-hook image-mode-hook pdf-view-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; ─── Core visual modes ────────────────────────────────────────────────────────

(column-number-mode 1)
(size-indication-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)

(setq show-paren-delay                   0
      show-paren-style                   'mixed
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;; ─── doom-modeline ────────────────────────────────────────────────────────────

(use-package doom-modeline
  :defer t
  :init
  (setq doom-modeline-height                  30
        doom-modeline-bar-width               4
        doom-modeline-icon                    t
        doom-modeline-major-mode-icon         t
        doom-modeline-major-mode-color-icon   t
        doom-modeline-buffer-file-name-style  'truncate-upto-project
        doom-modeline-buffer-state-icon       t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes             nil
        doom-modeline-enable-word-count       t
        doom-modeline-buffer-encoding         nil
        doom-modeline-checker-simple-format   t
        doom-modeline-number-limit            99
        doom-modeline-vcs-max-length          20
        doom-modeline-workspace-name          t
        doom-modeline-lsp                     t
        doom-modeline-env-version             t)
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (let ((modeline (if (fboundp 'emacs-ide-config-get)
                                  (emacs-ide-config-get 'features 'modeline 'doom-modeline)
                                'doom-modeline)))
                (when (eq modeline 'doom-modeline)
                  (when (fboundp 'doom-modeline-mode)
                    (doom-modeline-mode 1)))))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " — Emacs IDE"))

;;; ─── Visual enhancement packages ─────────────────────────────────────────────

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :hook (prog-mode . rainbow-mode))

(use-package highlight-numbers
  :defer t
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :init
  (setq hl-todo-keyword-faces
        '(("TODO"       . "#ff6c6b") ("FIXME"      . "#ff6c6b")
          ("HACK"       . "#c678dd") ("NOTE"       . "#98be65")
          ("DEPRECATED" . "#ECBE7B") ("XXX"        . "#ff6c6b")
          ("BUG"        . "#ff6c6b") ("OPTIMIZE"   . "#51afef")
          ("PERF"       . "#51afef") ("REVIEW"     . "#c678dd"))))

(use-package beacon
  :defer t
  :init (setq beacon-blink-when-focused t beacon-size 40))

(use-package dimmer
  :defer t
  :init (setq dimmer-fraction 0.4 dimmer-adjustment-mode :foreground))

(use-package pulsar
  :defer t
  :init (setq pulsar-pulse t pulsar-delay 0.055 pulsar-iterations 10
              pulsar-face 'pulsar-magenta pulsar-highlight-face 'pulsar-yellow))

(add-hook 'after-init-hook
          (lambda ()
            (let ((cfg (lambda (key)
                         (if (fboundp 'emacs-ide-config-get)
                             (emacs-ide-config-get 'features key t)
                           t))))
              (when (and (funcall cfg 'beacon) (fboundp 'beacon-mode))
                (beacon-mode 1))
              (when (and (funcall cfg 'dimmer) (fboundp 'dimmer-mode))
                (dimmer-mode 1))
              (when (and (funcall cfg 'pulsar) (fboundp 'pulsar-global-mode))
                (pulsar-global-mode 1)))))

;;; ─── highlight-indent-guides ─────────────────────────────────────────────────

(use-package highlight-indent-guides
  :defer t
  :init
  (setq highlight-indent-guides-method        'character
        highlight-indent-guides-responsive    'top
        highlight-indent-guides-delay         0)
  :hook (prog-mode . emacs-ide--maybe-enable-indent-guides))

(defun emacs-ide--maybe-enable-indent-guides ()
  "Enable highlight-indent-guides if the feature flag is set."
  (when (if (fboundp 'emacs-ide-config-get)
            (emacs-ide-config-get 'features 'highlight-indent-guides t)
          t)
    (highlight-indent-guides-mode 1)))

;;; ─── which-key ───────────────────────────────────────────────────────────────

(use-package which-key
  :defer t
  :init
  (setq which-key-idle-delay           0.3
        which-key-idle-secondary-delay 0.05
        which-key-separator            " → "
        which-key-prefix-prefix        "+"
        which-key-sort-order           'which-key-key-order-alpha
        which-key-max-display-columns  4
        which-key-min-display-lines    6
        which-key-max-description-length 32))

(add-hook 'after-init-hook
          (lambda ()
            (when (and (fboundp 'which-key-mode)
                       (or (not (fboundp 'emacs-ide-config-get))
                           (emacs-ide-config-get 'features 'which-key t)))
              (which-key-mode 1))))

;;; ─── Window management ───────────────────────────────────────────────────────

(use-package ace-window
  :defer t
  :init
  (setq aw-keys            '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope           'frame
        aw-background      t
        aw-dispatch-always t))

(winner-mode 1)

(use-package transpose-frame
  :defer t
  :bind (("C-c w t" . transpose-frame)
         ("C-c w f" . flip-frame)
         ("C-c w r" . rotate-frame-clockwise)))

;;; ─── Neotree ─────────────────────────────────────────────────────────────────

(use-package neotree
  :defer t
  :commands (neotree-toggle neotree)
  :init
  (setq neo-smart-open            t
        neo-theme                 (if (display-graphic-p) 'nerd 'arrow)
        neo-window-width          30
        neo-create-file-auto-open t
        neo-show-updir-line       nil
        neo-vc-integration        '(face)))

;;; ─── Dired enhancements ──────────────────────────────────────────────────────

(use-package diredfl
  :defer t
  :hook (dired-mode . diredfl-mode))

;;; ─── Writing / focus ─────────────────────────────────────────────────────────

(use-package visual-fill-column
  :defer t
  :init
  (setq visual-fill-column-width       120
        visual-fill-column-center-text nil)
  :hook ((org-mode markdown-mode) . visual-fill-column-mode))

;;; ─── tab-bar ─────────────────────────────────────────────────────────────────

(use-package tab-bar
  :straight nil
  :init
  (setq tab-bar-show              1
        tab-bar-close-button-show nil
        tab-bar-new-button-show   nil
        tab-bar-tab-hints         t
        tab-bar-separator         "  "
        tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count))

(add-hook 'after-init-hook
          (lambda ()
            (when (fboundp 'tab-bar-mode)
              (tab-bar-mode 1)))
          ;; run early in after-init so ui-workspace's tab-bar-format
          ;; customization applies before first display
          5)

;;; ─── editorconfig ────────────────────────────────────────────────────────────

(if (version<= "30" emacs-version)
    ;; Built-in since Emacs 30 — enable globally after init
    (add-hook 'after-init-hook
              (lambda ()
                (when (fboundp 'editorconfig-mode)
                  (editorconfig-mode 1))))
  ;; Emacs 29 and earlier — use the package, global
  (use-package editorconfig
    :demand t
    :config
    (editorconfig-mode 1)))

;;; ─── Presentation mode ───────────────────────────────────────────────────────

(defvar emacs-ide-presentation-mode--active nil
  "Non-nil when presentation mode is active.")

(defun emacs-ide-presentation-mode ()
  (interactive)
  (if emacs-ide-presentation-mode--active
      (progn (set-face-attribute 'default nil :height 110)
             (setq emacs-ide-presentation-mode--active nil)
             (message "Presentation mode OFF"))
    (set-face-attribute 'default nil :height 200)
    (setq emacs-ide-presentation-mode--active t)
    (message "Presentation mode ON")))

(defun emacs-ide-set-transparency (alpha-bg alpha-fg)
  (interactive "nBackground (0-100): \nnForeground (0-100): ")
  (set-frame-parameter nil 'alpha-background alpha-bg)
  (set-frame-parameter nil 'alpha alpha-fg))

(provide 'ui-core)
;;; ui-core.el ends here
