;;; ui-core.el --- Professional Visual Configuration (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; Elite UI/UX with modern design and maximum efficiency
;;; Code:

;; ============================================================================
;; UI CLEANUP
;; ============================================================================
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

(setq ring-bell-function 'ignore
      visible-bell nil
      use-dialog-box nil
      use-file-dialog nil
      echo-keystrokes 0.01
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t)

;; ============================================================================
;; THEME - MODUS THEMES (DEFERRED)
;; ============================================================================
(use-package modus-themes
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-custom-auto-reload t
        modus-themes-disable-other-themes t
        modus-themes-prompts '(bold intense)
        modus-themes-completions '((matches . (extrabold))
                                  (selection . (semibold accented))
                                  (popup . (accented intense)))
        modus-themes-org-blocks 'gray-background
        modus-themes-headings '((1 . (rainbow overline background 1.5))
                               (2 . (rainbow background 1.3))
                               (3 . (rainbow bold 1.2))
                               (t . (semilight 1.1)))
        modus-themes-scale-headings t
        modus-themes-syntax '(alt-syntax green-strings yellow-comments)
        modus-themes-hl-line '(intense)
        modus-themes-paren-match '(bold intense)
        modus-themes-region '(bg-only no-extend))
  :config
  (let ((theme (or (bound-and-true-p emacs-ide-theme) 'modus-vivendi)))
    (load-theme theme t)))

;; ============================================================================
;; DASHBOARD (DEFERRED)
;; ============================================================================
(use-package dashboard
  :init
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 15)
                         (projects . 10)
                         (bookmarks . 10)
                         (agenda . 5))
        dashboard-set-navigator t
        dashboard-set-init-info t
        dashboard-banner-logo-title "EMACS IDE - Professional Development Environment"
        dashboard-footer-messages '("Ready to code")
        dashboard-footer-icon (when (fboundp 'all-the-icons-octicon)
                               (all-the-icons-octicon "dashboard"
                                                      :height 1.1
                                                      :v-adjust -0.05
                                                      :face 'font-lock-keyword-face)))
  :config
  (when (fboundp 'dashboard-setup-startup-hook)
    (dashboard-setup-startup-hook))
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))

;; ============================================================================
;; ICONS (DEFERRED)
;; ============================================================================
(use-package all-the-icons
  :if (display-graphic-p)
  :config)

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; ============================================================================
;; FONTS - LIGATURES ENABLED (DEFERRED)
;; ============================================================================
(when (display-graphic-p)
  (setq frame-resize-pixelwise t
        window-resize-pixelwise t)
  
  (defun emacs-ide-set-font (font-name size &optional weight)
    "Set FONT-NAME at SIZE with optional WEIGHT safely."
    (when (member font-name (font-family-list))
      (set-face-attribute 'default nil
                          :font font-name
                          :height (* size 10)
                          :weight (or weight 'normal))
      t))
  
  ;; Use config values if available
  (let ((font (or (bound-and-true-p emacs-ide-font) "JetBrains Mono"))
        (size (or (bound-and-true-p emacs-ide-font-size) 11)))
    (or (emacs-ide-set-font font size 'medium)
        (emacs-ide-set-font "Fira Code" size)
        (emacs-ide-set-font "Cascadia Code" size)
        (emacs-ide-set-font "Iosevka" size)
        (emacs-ide-set-font "Source Code Pro" size)))
  
  ;; Variable pitch font
  (condition-case nil
      (or (set-face-attribute 'variable-pitch nil :font "Inter-11")
          (set-face-attribute 'variable-pitch nil :font "Cantarell-11")
          (set-face-attribute 'variable-pitch nil :font "DejaVu Sans-11"))
    (error nil))
  
  ;; Ligature support (macOS)
  (when (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))
  
  ;; Emoji support
  (when (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)))

;; ============================================================================
;; WAYLAND SMOOTH SCROLLING
;; ============================================================================
(when (and (bound-and-true-p emacs-ide-wayland-p)
           (fboundp 'pixel-scroll-precision-mode))
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-use-momentum t
        pixel-scroll-precision-large-scroll-height 40.0
        pixel-scroll-precision-interpolation-factor 1.0))

;; ============================================================================
;; LINE NUMBERS - OPTIMIZED
;; ============================================================================
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative
      display-line-numbers-width 3
      display-line-numbers-grow-only t
      display-line-numbers-width-start t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                dashboard-mode-hook
                neotree-mode-hook
                image-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(column-number-mode 1)
(size-indication-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)

(setq show-paren-delay 0
      show-paren-style 'mixed
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;; ============================================================================
;; MODE-LINE - DOOM-MODELINE (DEFERRED)
;; ============================================================================
(use-package doom-modeline
  :init
  (setq doom-modeline-height 30
        doom-modeline-bar-width 4
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count t
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-checker-simple-format t
        doom-modeline-number-limit 99
        doom-modeline-vcs-max-length 20
        doom-modeline-workspace-name t
        doom-modeline-persp-name t
        doom-modeline-lsp t
        doom-modeline-github t
        doom-modeline-github-interval (* 30 60)
        doom-modeline-modal-icon t
        doom-modeline-mu4e t
        doom-modeline-gnus t
        doom-modeline-irc t
        doom-modeline-env-version t)
  :config
  (doom-modeline-mode 1))

;; Set frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
               (when (buffer-modified-p) " •")
               " - Emacs IDE"
               (when (bound-and-true-p emacs-ide-wayland-p) " [Wayland]")))

;; ============================================================================
;; VISUAL ENHANCEMENTS (DEFERRED)
;; ============================================================================
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :init
  (setq hl-todo-keyword-faces
        '(("TODO" . "#ff6c6b")
          ("FIXME" . "#ff6c6b")
          ("HACK" . "#c678dd")
          ("NOTE" . "#98be65")
          ("DEPRECATED" . "#ECBE7B")
          ("XXX" . "#ff6c6b")
          ("BUG" . "#ff6c6b")
          ("OPTIMIZE" . "#51afef")
          ("PERF" . "#51afef")
          ("REVIEW" . "#c678dd"))))

(use-package beacon
  :init
  (setq beacon-blink-when-focused t
        beacon-blink-when-window-scrolls t
        beacon-blink-when-point-moves-vertically 10
        beacon-size 40)
  :config
  (beacon-mode 1))

(use-package dimmer
  :init
  (setq dimmer-fraction 0.4
        dimmer-adjustment-mode :foreground
        dimmer-use-colorspace :rgb
        dimmer-watch-frame-focus-events nil)
  :config
  (dimmer-mode 1))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0
        highlight-indent-guides-auto-enabled t
        highlight-indent-guides-auto-character-face-perc 20
        highlight-indent-guides-auto-top-character-face-perc 40))

(use-package pulsar
  :init
  (setq pulsar-pulse t
        pulsar-delay 0.055
        pulsar-iterations 10
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow)
  :config
  (pulsar-global-mode 1))

;; ============================================================================
;; WHICH-KEY (DEFERRED)
;; ============================================================================
(use-package which-key
  :init
  (setq which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0.05
        which-key-separator " → "
        which-key-prefix-prefix "+"
        which-key-sort-order 'which-key-key-order-alpha
        which-key-max-display-columns 4
        which-key-min-display-lines 6
        which-key-add-column-padding 1
        which-key-max-description-length 32
        which-key-allow-imprecise-window-fit nil
        which-key-show-prefix 'echo)
  :config
  (which-key-mode 1))

;; ============================================================================
;; WINDOW MANAGEMENT (DEFERRED)
;; ============================================================================
(use-package ace-window
  :bind ("M-o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t
        aw-dispatch-always t
        aw-minibuffer-flag t
        aw-ignore-current nil))

(winner-mode 1)

(use-package transpose-frame
  :bind (("C-c w t" . transpose-frame)
         ("C-c w f" . flip-frame)
         ("C-c w r" . rotate-frame-clockwise)))

;; ============================================================================
;; NEOTREE (DEFERRED)
;; ============================================================================
(use-package neotree
  :bind (("<f8>" . neotree-toggle))
  :init
  (setq neo-smart-open t
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-window-width 30
        neo-create-file-auto-open t
        neo-auto-indent-point t
        neo-modern-sidebar t
        neo-show-updir-line nil
        neo-vc-integration '(face)))

;; ============================================================================
;; DIRED ENHANCEMENT (DEFERRED)
;; ============================================================================
(use-package dired
  :straight nil
  :init
  (setq dired-listing-switches "-alGh --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-kill-when-opening-new-dired-buffer t
        dired-auto-revert-buffer t
        dired-hide-details-hide-symlink-targets nil)
  :bind (:map dired-mode-map
              ("h" . dired-up-directory)
              ("l" . dired-find-alternate-file)
              ("C-c C-p" . wdired-change-to-wdired-mode)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; ============================================================================
;; ANSI COLOR (DEFERRED)
;; ============================================================================
(use-package ansi-color
  :straight nil
  :config
  (defun emacs-ide-colorize-compilation-buffer ()
    "Colorize compilation output."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'emacs-ide-colorize-compilation-buffer))

;; ============================================================================
;; TRANSPARENCY
;; ============================================================================
(defun emacs-ide-set-transparency (alpha-background alpha-foreground)
  "Set frame transparency."
  (interactive "nBackground (0-100): \nForeground (0-100): ")
  (set-frame-parameter nil 'alpha-background alpha-background)
  (set-frame-parameter nil 'alpha alpha-foreground))

;; ============================================================================
;; VISUAL FILL COLUMN (DEFERRED)
;; ============================================================================
(use-package visual-fill-column
  :init
  (setq visual-fill-column-width 120
        visual-fill-column-center-text nil
        visual-fill-column-enable-sensible-window-split t)
  :hook ((org-mode markdown-mode) . visual-fill-column-mode))

;; ============================================================================
;; TAB BAR MODE
;; ============================================================================
(use-package tab-bar
  :straight nil
  :init
  (setq tab-bar-show 1
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-hints t
        tab-bar-separator " "
        tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))
  :config
  (tab-bar-mode 1))

;; ============================================================================
;; PRESENTATION MODE
;; ============================================================================
(defvar emacs-ide-presentation-mode nil)

(defun emacs-ide-presentation-mode ()
  "Toggle presentation mode."
  (interactive)
  (if emacs-ide-presentation-mode
      (progn
        (set-face-attribute 'default nil :height 110)
        (setq emacs-ide-presentation-mode nil)
        (message "Presentation mode OFF"))
    (set-face-attribute 'default nil :height 200)
    (setq emacs-ide-presentation-mode t)
    (message "Presentation mode ON")))

(global-set-key (kbd "C-c P") 'emacs-ide-presentation-mode)

(provide 'ui-core)
;;; ui-core.el ends here
