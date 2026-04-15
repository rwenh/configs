;;; ui-core.el --- Premium Visual Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Opinionated, polished visual layer with depth, motion cues, and craft.
;;; Version: 3.1.0
;;; Code:

(when (fboundp 'menu-bar-mode)     (menu-bar-mode   -1))
(when (fboundp 'tool-bar-mode)     (tool-bar-mode   -1))
(when (fboundp 'scroll-bar-mode)   (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode)      (tooltip-mode    -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode 1))  ;; Subtle cursor pulse

(setq ring-bell-function              'ignore
      visible-bell                    nil
      use-dialog-box                  nil
      use-file-dialog                 nil
      echo-keystrokes                 0.01
      inhibit-splash-screen           t
      inhibit-startup-echo-area-message t
      ;; Smoother scrolling
      scroll-step                     1
      scroll-conservatively           101
      scroll-margin                   4
      scroll-preserve-screen-position t
      ;; Better mouse scrolling
      mouse-wheel-scroll-amount       '(2 ((shift) . 5))
      mouse-wheel-progressive-speed   nil
      mouse-wheel-follow-mouse        t
      ;; Fringe prettiness
      indicate-empty-lines            t
      indicate-buffer-boundaries      'left)

;;; ─── Themes ────────────────────────────────────────────────────────────────

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
         (theme (cond ((stringp raw) (intern raw))
                      ((symbolp raw) raw)
                      (t             'ef-dark)))
         ;; Migrate modus- users gracefully
         (theme (if (string-prefix-p "modus-" (symbol-name theme))
                    (if (string-suffix-p "operandi" (symbol-name theme))
                        'ef-light 'ef-dark)
                  theme)))
    (condition-case err
        (load-theme theme t)
      (error
       (message "ui-core: theme %s failed (%s), using ef-dark" theme err)
       (load-theme 'ef-dark t)))))

;;; ─── Icons ──────────────────────────────────────────────────────────────────

(use-package nerd-icons
  :demand t
  :init
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"
        nerd-icons-scale-factor 1.1)
  :config
  (when (display-graphic-p)
    (unless (member "Symbols Nerd Font Mono" (font-family-list))
      (message "⚠  nerd-icons: run M-x nerd-icons-install-fonts once"))))

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

;;; ─── Font & Typography ──────────────────────────────────────────────────────

(when (display-graphic-p)
  (setq frame-resize-pixelwise  t
        window-resize-pixelwise t)

  (defun emacs-ide-set-font (font-name size &optional weight)
    "Set default font to FONT-NAME at SIZE (pt) with optional WEIGHT."
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

  ;; Variable-pitch for UI text / org / prose
  (condition-case nil
      (or (set-face-attribute 'variable-pitch nil :font "Inter"    :height 115)
          (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 115))
    (error nil))

  ;; Ligature support
  (use-package ligature
    :defer t
    :config
    (ligature-set-ligatures
     'prog-mode
     '("->" "<-" "=>" "<=" ">=" "!=" "==" "//=" "/*" "*/"
       "..." "::" "&&" "||" "++" "--" "<<" ">>" "<>" "|>"
       "<|" "##" "###" "####" ";;" ":=" "!!" "??" "<->"
       "<=>" "==>" ">=>" "-<" ">-" "<~" "~>" "-|" "|-")))
  (add-hook 'after-init-hook (lambda () (global-ligature-mode t)))

  ;; Emoji / Unicode fallback
  (when (fboundp 'set-fontset-font)
    (dolist (font '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji"))
      (when (member font (font-family-list))
        (set-fontset-font t 'unicode font nil 'prepend)
        (cl-return)))))

;;; ─── Pixel-smooth scrolling ─────────────────────────────────────────────────

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-use-momentum         t
        pixel-scroll-precision-large-scroll-height  40.0
        pixel-scroll-precision-interpolation-factor 1.0))

;;; ─── Line numbers & column ──────────────────────────────────────────────────

(let ((line-nums-on (if (fboundp 'emacs-ide-config-get)
                        (emacs-ide-config-get 'features 'line-numbers t)
                      t))
      (relative-on (if (fboundp 'emacs-ide-config-get)
                       (emacs-ide-config-get 'features 'relative-line-numbers nil)
                     nil)))
  (when line-nums-on
    (global-display-line-numbers-mode 1)
    (setq display-line-numbers-type        (if relative-on 'relative t)
          display-line-numbers-width       4
          display-line-numbers-grow-only   t
          display-line-numbers-width-start t
          display-line-numbers-widen       t)))

;; Disable line numbers in non-code buffers
(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook
                eshell-mode-hook vterm-mode-hook dashboard-mode-hook
                neotree-mode-hook image-mode-hook pdf-view-mode-hook
                treemacs-mode-hook messages-buffer-mode-hook
                help-mode-hook special-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(column-number-mode 1)
(size-indication-mode 1)

;;; ─── Parentheses & delimiters ───────────────────────────────────────────────

(show-paren-mode 1)
(setq show-paren-delay                   0
      show-paren-style                   'mixed
      show-paren-context-when-offscreen  'overlay
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;; ─── Cursor ─────────────────────────────────────────────────────────────────

;; Bar cursor in insert / box in normal; refined blink timing
(setq-default cursor-type 'bar)
(setq blink-cursor-interval  0.6
      blink-cursor-blinks     0)    ;; blink forever

;;; ─── Highlight current line ─────────────────────────────────────────────────

(global-hl-line-mode 1)
;; Disable hl-line in terminals and special modes
(dolist (hook '(vterm-mode-hook term-mode-hook eshell-mode-hook comint-mode-hook))
  (add-hook hook (lambda () (setq-local global-hl-line-mode nil)
                   (hl-line-mode -1))))

;;; ─── Modeline: doom-modeline ────────────────────────────────────────────────

(use-package doom-modeline
  :defer t
  :init
  (setq doom-modeline-height                  32
        doom-modeline-bar-width               5
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
        doom-modeline-vcs-max-length          24
        doom-modeline-workspace-name          t
        doom-modeline-lsp                     t
        doom-modeline-env-version             t
        doom-modeline-time                    t
        doom-modeline-time-icon               t
        doom-modeline-persp-name              t
        doom-modeline-persp-icon              t)
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (let ((modeline (if (fboundp 'emacs-ide-config-get)
                                  (emacs-ide-config-get 'features 'modeline 'doom-modeline)
                                'doom-modeline)))
                (when (eq modeline 'doom-modeline)
                  (when (fboundp 'doom-modeline-mode)
                    (doom-modeline-mode 1)))))))

;;; ─── Frame title ────────────────────────────────────────────────────────────

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat (abbreviate-file-name (buffer-file-name))
                           (if (buffer-modified-p) " ●" ""))
                 "%b"))
        " — Emacs IDE"))

;;; ─── Syntax + code decorations ──────────────────────────────────────────────

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :hook ((prog-mode css-mode html-mode web-mode) . rainbow-mode))

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
          ("PERF"       . "#51afef") ("REVIEW"     . "#c678dd")
          ("WARN"       . "#ff9800") ("SAFETY"     . "#ff5722")
          ("IMPORTANT"  . "#e91e63") ("NB"         . "#00bcd4"))))

;;; ─── Motion & attention ─────────────────────────────────────────────────────

(use-package beacon
  :defer t
  :init
  (setq beacon-blink-when-focused      t
        beacon-blink-when-point-moves  t
        beacon-size                    40
        beacon-blink-duration          0.3
        beacon-blink-delay             0.1
        beacon-color                   "#51afef"))

(use-package dimmer
  :defer t
  :init
  (setq dimmer-fraction          0.35
        dimmer-adjustment-mode   :foreground
        dimmer-use-colorspace    :rgb))

(use-package pulsar
  :defer t
  :init
  (setq pulsar-pulse          t
        pulsar-delay          0.055
        pulsar-iterations     10
        pulsar-face           'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow))

(add-hook 'after-init-hook
          (lambda ()
            (let ((cfg (lambda (key) (if (fboundp 'emacs-ide-config-get)
                                        (emacs-ide-config-get 'features key t)
                                      t))))
              (when (and (funcall cfg 'beacon) (fboundp 'beacon-mode))
                (beacon-mode 1))
              (when (and (funcall cfg 'dimmer) (fboundp 'dimmer-mode))
                (dimmer-mode 1))
              (when (and (funcall cfg 'pulsar) (fboundp 'pulsar-global-mode))
                (pulsar-global-mode 1)))))

;;; ─── Indent guides ──────────────────────────────────────────────────────────

(use-package highlight-indent-guides
  :defer t
  :if (or (not (fboundp 'emacs-ide-config-get))
          (emacs-ide-config-get 'features 'highlight-indent-guides t))
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method        'character
        highlight-indent-guides-character      ?│
        highlight-indent-guides-responsive    'top
        highlight-indent-guides-delay         0
        highlight-indent-guides-auto-enabled  t))

;;; ─── Which-key ──────────────────────────────────────────────────────────────

(use-package which-key
  :defer t
  :init
  (setq which-key-idle-delay            0.3
        which-key-idle-secondary-delay  0.05
        which-key-separator             "  "
        which-key-prefix-prefix         "+"
        which-key-sort-order            'which-key-key-order-alpha
        which-key-max-display-columns   5
        which-key-min-display-lines     6
        which-key-max-description-length 36
        which-key-add-column-padding    2
        which-key-show-docstrings       nil
        which-key-unicode-correction    3))

(add-hook 'after-init-hook
          (lambda ()
            (when (and (fboundp 'which-key-mode)
                       (or (not (fboundp 'emacs-ide-config-get))
                           (emacs-ide-config-get 'features 'which-key t)))
              (which-key-mode 1))))

;;; ─── Window management ──────────────────────────────────────────────────────

(use-package ace-window
  :defer t
  :init
  (setq aw-keys            '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope           'frame
        aw-background      t
        aw-dispatch-always t
        aw-minibuffer-flag t))

(winner-mode 1)

(use-package transpose-frame
  :defer t
  :bind (("C-c w t" . transpose-frame)
         ("C-c w f" . flip-frame)
         ("C-c w r" . rotate-frame-clockwise)))

;;; ─── Neotree ────────────────────────────────────────────────────────────────

(use-package neotree
  :defer t
  :commands (neotree-toggle neotree)
  :init
  (setq neo-smart-open            t
        neo-theme                 (if (display-graphic-p) 'nerd 'arrow)
        neo-window-width          32
        neo-create-file-auto-open t
        neo-show-updir-line       nil
        neo-vc-integration        '(face)
        neo-show-hidden-files     nil))

;;; ─── Dired prettiness ───────────────────────────────────────────────────────

(use-package diredfl
  :defer t
  :hook (dired-mode . diredfl-mode))

;;; ─── Prose / writing ────────────────────────────────────────────────────────

(use-package visual-fill-column
  :defer t
  :init
  (setq visual-fill-column-width       110
        visual-fill-column-center-text t)
  :hook ((org-mode markdown-mode) . visual-fill-column-mode))

;;; ─── Tab bar ────────────────────────────────────────────────────────────────

(use-package tab-bar
  :straight nil
  :defer t
  :init
  (setq tab-bar-show              1
        tab-bar-close-button-show nil
        tab-bar-new-button-show   nil
        tab-bar-tab-hints         t
        tab-bar-separator         "  "
        tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count
        tab-bar-auto-width        nil)
  :config
  (tab-bar-mode 1))

;;; ─── Presentation mode ──────────────────────────────────────────────────────

(defvar emacs-ide-presentation-mode--active nil)
(defvar emacs-ide-presentation-mode--saved-height nil)

(defun emacs-ide-presentation-mode ()
  "Toggle high-visibility presentation mode."
  (interactive)
  (if emacs-ide-presentation-mode--active
      (progn
        (when emacs-ide-presentation-mode--saved-height
          (set-face-attribute 'default nil
                              :height emacs-ide-presentation-mode--saved-height))
        (setq emacs-ide-presentation-mode--active nil
              emacs-ide-presentation-mode--saved-height nil)
        (when (fboundp 'doom-modeline-mode) (doom-modeline-mode 1))
        (message "Presentation mode OFF"))
    (setq emacs-ide-presentation-mode--saved-height
          (face-attribute 'default :height))
    (set-face-attribute 'default nil :height 200)
    (setq emacs-ide-presentation-mode--active t)
    (when (fboundp 'doom-modeline-mode) (doom-modeline-mode -1))
    (message "Presentation mode ON — F12 theme toggle still works")))

;;; ─── Transparency ───────────────────────────────────────────────────────────

(defun emacs-ide-set-transparency (alpha-bg alpha-fg)
  "Set frame transparency: ALPHA-BG (0–100) for bg, ALPHA-FG for fg."
  (interactive "nBackground opacity (0-100): \nnForeground opacity (0-100): ")
  (set-frame-parameter nil 'alpha-background alpha-bg)
  (set-frame-parameter nil 'alpha            (cons alpha-fg alpha-fg)))

;;; ─── Context-menu (Emacs 28+) ───────────────────────────────────────────────

(when (fboundp 'context-menu-mode)
  (context-menu-mode 1))

;;; ─── Misc visual polish ─────────────────────────────────────────────────────

;; Stretch cursor to match character width (tabs)
(setq x-stretch-cursor t)

;; Underline active region
(setq-default transient-mark-mode t)

;; Don't compact font caches during GC
(setq inhibit-compacting-font-caches t)

;; Resize windows proportionally
(setq window-combination-resize t)

;; Better *Messages* buffer
(setq message-log-max 10000)

;; Show recursion depth in minibuffer
(minibuffer-depth-indicate-mode 1)

(provide 'ui-core)
;;; ui-core.el ends here
