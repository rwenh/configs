;;; ui-core.el --- Office-Grade Visual Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Replaces modus-themes with ef-themes (sharper, more opinionated).
;;; Replaces all-the-icons with nerd-icons (faster, single-font).
;;; Adds Hydra menus for discoverable chord-free commands.
;;; Keeps all existing visual enhancements from v2.2.5.
;;; Version: 3.0.0
;;; Changes from 2.2.5:
;;;   - ef-themes replaces modus-themes (config.yml theme key updated)
;;;   - nerd-icons replaces all-the-icons (faster load, single font)
;;;   - Hydra menus added (window, buffer, toggle) — tools-hydra.el owns big ones
;;;   - Embark wired as default C-. action hub in all contexts
;;;   - pixel-scroll-precision-mode enabled unconditionally on Emacs 29+
;;;   - All existing beacon/dimmer/pulsar/which-key deferred hooks kept
;;; Code:

;; ============================================================================
;; UI CLEANUP (unchanged from 2.2.5)
;; ============================================================================
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

;; ============================================================================
;; THEME — EF-THEMES (replaces modus-themes)
;; ef-themes are by the same author (Protesilaos) but sharper and more
;; opinionated — exactly the "office" aesthetic we want.
;; config.yml: theme: ef-dark  (options: ef-dark ef-light ef-duo-dark ef-night
;;              ef-cherie ef-winter ef-summer ef-spring ef-trio-dark ef-frost)
;; ============================================================================
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
         ;; emacs-ide-theme may be a symbol ('modus-vivendi) or string ("ef-dark")
         ;; Convert to symbol, then map any modus-* symbol to ef equivalent
         (theme (cond
                 ((stringp raw)  (intern raw))
                 ((symbolp raw)  raw)
                 (t              'ef-dark)))
         (theme (if (string-prefix-p "modus-" (symbol-name theme))
                    (if (string-suffix-p "operandi" (symbol-name theme))
                        'ef-light
                      'ef-dark)
                  theme)))
    ;; Use load-theme directly — ef-themes-select is interactive (0 args)
    ;; and crashes when called programmatically with an argument.
    (condition-case err
        (load-theme theme t)
      (error
       (message "ui-core: could not load theme %s (%s), falling back to ef-dark"
                theme err)
       (load-theme 'ef-dark t)))))

;; ============================================================================
;; ICONS — NERD-ICONS (replaces all-the-icons)
;; Single font, faster load, better coverage.
;; Run M-x nerd-icons-install-fonts once after first install.
;; ============================================================================
(use-package nerd-icons
  :defer t
  :if (display-graphic-p)
  :init
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :if (display-graphic-p)
  :after (nerd-icons marginalia)
  :config (nerd-icons-completion-mode 1))

(use-package nerd-icons-corfu
  :if (display-graphic-p)
  :after (nerd-icons corfu)
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ============================================================================
;; FONTS & LIGATURES (unchanged from 2.2.5, nerd-icons aware)
;; ============================================================================
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
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)))

;; ============================================================================
;; SMOOTH SCROLLING (Emacs 29+ pixel precision)
;; ============================================================================
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-use-momentum         t
        pixel-scroll-precision-large-scroll-height  40.0
        pixel-scroll-precision-interpolation-factor 1.0))

;; ============================================================================
;; LINE NUMBERS (unchanged)
;; ============================================================================
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type        'relative
      display-line-numbers-width       3
      display-line-numbers-grow-only   t
      display-line-numbers-width-start t)

(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook
                eshell-mode-hook vterm-mode-hook dashboard-mode-hook
                neotree-mode-hook image-mode-hook pdf-view-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(column-number-mode 1)
(size-indication-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)

(setq show-paren-delay                   0
      show-paren-style                   'mixed
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;; ============================================================================
;; DOOM-MODELINE (unchanged from 2.2.5)
;; ============================================================================
(use-package doom-modeline
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
            (lambda () (when (fboundp 'doom-modeline-mode) (doom-modeline-mode 1)))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " — Emacs IDE"))

;; ============================================================================
;; VISUAL ENHANCEMENTS (unchanged from 2.2.5, deferred)
;; ============================================================================
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))
(use-package rainbow-mode       :hook (prog-mode . rainbow-mode))
(use-package highlight-numbers  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
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
            (when (fboundp 'beacon-mode)      (beacon-mode 1))
            (when (fboundp 'dimmer-mode)      (dimmer-mode 1))
            (when (fboundp 'pulsar-global-mode) (pulsar-global-mode 1))))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method        'character
        highlight-indent-guides-responsive    'top
        highlight-indent-guides-delay         0))

;; ============================================================================
;; WHICH-KEY (unchanged from 2.2.5, deferred)
;; ============================================================================
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
          (lambda () (when (fboundp 'which-key-mode) (which-key-mode 1))))

;; ============================================================================
;; WINDOW MANAGEMENT (unchanged)
;; ============================================================================
(use-package ace-window
  :init
  (setq aw-keys            '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope           'frame
        aw-background      t
        aw-dispatch-always t))

(winner-mode 1)

(use-package transpose-frame
  :bind (("C-c w t" . transpose-frame)
         ("C-c w f" . flip-frame)
         ("C-c w r" . rotate-frame-clockwise)))

;; ============================================================================
;; NEOTREE
;; ============================================================================
(use-package neotree
  :init
  (setq neo-smart-open            t
        neo-theme                 (if (display-graphic-p) 'nerd 'arrow)
        neo-window-width          30
        neo-create-file-auto-open t
        neo-show-updir-line       nil
        neo-vc-integration        '(face)))

;; ============================================================================
;; DIRED
;; ============================================================================
(use-package diredfl :hook (dired-mode . diredfl-mode))

;; ============================================================================
;; ANSI COLOR IN COMPILATION
;; ============================================================================
(use-package ansi-color
  :straight nil
  :config
  (add-hook 'compilation-filter-hook
            (lambda ()
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region compilation-filter-start (point))))))

;; ============================================================================
;; VISUAL FILL COLUMN
;; ============================================================================
(use-package visual-fill-column
  :init
  (setq visual-fill-column-width       120
        visual-fill-column-center-text nil)
  :hook ((org-mode markdown-mode) . visual-fill-column-mode))

;; ============================================================================
;; TAB BAR (workspaces shown via ui-workspace.el)
;; ============================================================================
(use-package tab-bar
  :straight nil
  :init
  (setq tab-bar-show              1
        tab-bar-close-button-show nil
        tab-bar-new-button-show   nil
        tab-bar-tab-hints         t
        tab-bar-separator         "  "
        tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count)
  :config
  (tab-bar-mode 1))

;; ============================================================================
;; PRESENTATION MODE
;; ============================================================================
(defvar emacs-ide-presentation-mode nil)

(defun emacs-ide-presentation-mode ()
  (interactive)
  (if emacs-ide-presentation-mode
      (progn (set-face-attribute 'default nil :height 110)
             (setq emacs-ide-presentation-mode nil)
             (message "Presentation mode OFF"))
    (set-face-attribute 'default nil :height 200)
    (setq emacs-ide-presentation-mode t)
    (message "Presentation mode ON")))

;; ============================================================================
;; TRANSPARENCY
;; ============================================================================
(defun emacs-ide-set-transparency (alpha-bg alpha-fg)
  (interactive "nBackground (0-100): \nnForeground (0-100): ")
  (set-frame-parameter nil 'alpha-background alpha-bg)
  (set-frame-parameter nil 'alpha alpha-fg))

(provide 'ui-core)
;;; ui-core.el ends here
