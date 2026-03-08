;;; ui-core.el --- Professional Visual Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Elite UI/UX with modern design and maximum efficiency.
;;; Version: 2.2.4
;;; Fixes:
;;;   - 2.2.4: Removed (horizontal-scroll-bar-mode -1) entirely. The function
;;;     does not exist in Emacs 29; the fboundp guard prevented a crash but
;;;     left dead unreachable code. horizontal scroll bars are already
;;;     suppressed in early-init.el via default-frame-alist.
;;;   - 2.2.3: doom-modeline now activated via after-init-hook only (not
;;;     direct call). ui-modeline.el's duplicate after-init-hook suppressed.
;;;   - 2.2.3: ace-window :bind removed — M-o is set in keybindings.el only.
;;;   - 2.2.2: Global C-c P and F12 bindings removed (keybindings.el owns them).
;;;   - Dashboard setup removed (owned by ui-dashboard.el).
;;;   - emacs-ide-colorize-compilation-buffer is canonical here only.
;;; Code:

;; ============================================================================
;; UI CLEANUP
;; ============================================================================
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; NOTE: horizontal-scroll-bar-mode does not exist in Emacs 29.
;;       Horizontal scroll bars are already suppressed via default-frame-alist
;;       in early-init.el. No call needed here.
(when (fboundp 'tooltip-mode)    (tooltip-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

(setq ring-bell-function              'ignore
      visible-bell                    nil
      use-dialog-box                  nil
      use-file-dialog                 nil
      echo-keystrokes                 0.01
      inhibit-splash-screen           t
      inhibit-startup-echo-area-message t)

;; ============================================================================
;; THEME — MODUS
;; ============================================================================
(use-package modus-themes
  ;; FIX: Missing :demand t caused this block to be deferred (global
  ;; use-package-always-defer t set in init.el). With no :hook/:bind/:commands
  ;; to trigger loading, :config — which contains the load-theme call — never
  ;; ran, so no theme loaded and Emacs started with bare default faces.
  :demand t
  :init
  ;; Variables that remain valid in modus-themes v4 (ships with Emacs 29):
  (setq modus-themes-bold-constructs      t
        modus-themes-italic-constructs    t
        modus-themes-mixed-fonts          t
        modus-themes-variable-pitch-ui    t
        modus-themes-custom-auto-reload   t
        modus-themes-disable-other-themes t
        ;; Headings format is unchanged in v4:
        modus-themes-headings
        '((1 . (rainbow overline background 1.5))
          (2 . (rainbow background 1.3))
          (3 . (rainbow bold 1.2))
          (t . (semilight 1.1)))
        ;; modus-themes-completions: v4 dropped 'accented/'semibold — use supported props
        modus-themes-completions
        '((matches   . (underline))
          (selection . (bold))
          (popup     . (italic))))

  ;; FIX: The following variables were REMOVED in modus-themes v4 and produce
  ;; void-variable warnings on Emacs 29 (they are silently ignored at best):
  ;;   modus-themes-syntax, modus-themes-scale-headings, modus-themes-hl-line,
  ;;   modus-themes-paren-match, modus-themes-region, modus-themes-prompts,
  ;;   modus-themes-org-blocks.
  ;; Replaced with palette overrides — the v4 idiomatic customisation mechanism.
  (setq modus-vivendi-palette-overrides
        '(;; Syntax: green strings, yellow comments (replaces modus-themes-syntax)
          (string          green-warmer)
          (comment         yellow-faint)
          ;; Highlighted line: distinct bg (replaces modus-themes-hl-line)
          (bg-hl-line      bg-blue-nuanced)
          ;; Paren match: intense (replaces modus-themes-paren-match)
          (bg-paren-match  bg-magenta-intense)
          ;; Region: soft bg (replaces modus-themes-region)
          (bg-region       bg-lavender)
          ;; Prompts: warm color (replaces modus-themes-prompts)
          (prompt          magenta-warmer)
          ;; Org src blocks: gray bg (replaces modus-themes-org-blocks)
          (bg-org-block    bg-dim))
        modus-operandi-palette-overrides
        '((string          green-cooler)
          (comment         yellow-faint)
          (bg-hl-line      bg-blue-nuanced)
          (bg-paren-match  bg-magenta-intense)
          (bg-region       bg-lavender)
          (prompt          magenta-warmer)
          (bg-org-block    bg-dim)))
  :config
  (let ((theme (or (bound-and-true-p emacs-ide-theme) 'modus-vivendi)))
    (load-theme theme t)))

;; ============================================================================
;; ICONS
;; ============================================================================
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; ============================================================================
;; FONTS & LIGATURES
;; ============================================================================
(when (display-graphic-p)
  (setq frame-resize-pixelwise  t
        window-resize-pixelwise t)

  (defun emacs-ide-set-font (font-name size &optional weight)
    "Set FONT-NAME at SIZE with optional WEIGHT safely."
    (when (member font-name (font-family-list))
      (set-face-attribute 'default nil
                          :font   font-name
                          :height (* size 10)
                          :weight (or weight 'normal))
      t))

  (let ((font (or (bound-and-true-p emacs-ide-font) "JetBrains Mono"))
        (size (or (bound-and-true-p emacs-ide-font-size) 11)))
    (or (emacs-ide-set-font font size 'medium)
        (emacs-ide-set-font "Fira Code"       size)
        (emacs-ide-set-font "Cascadia Code"   size)
        (emacs-ide-set-font "Iosevka"         size)
        (emacs-ide-set-font "Source Code Pro" size)))

  (condition-case nil
      (or (set-face-attribute 'variable-pitch nil :font "Inter-11")
          (set-face-attribute 'variable-pitch nil :font "Cantarell-11")
          (set-face-attribute 'variable-pitch nil :font "DejaVu Sans-11"))
    (error nil))

  ;; Ligatures (Linux/Windows)
  (use-package ligature
    :config
    (ligature-set-ligatures
     'prog-mode
     '("->" "<-" "=>" "<=" ">=" "!=" "==" "//" "/*" "*/" "..."
       "::" "&&" "||" "++" "--" "<<" ">>" "<>" "|>" "<|"
       "##" "###" "####" ";;"))
    (global-ligature-mode t))

  ;; macOS native ligatures
  (when (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

  ;; Emoji
  (when (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)))

;; ============================================================================
;; WAYLAND SMOOTH SCROLLING
;; ============================================================================
(when (and (bound-and-true-p emacs-ide-wayland-p)
           (fboundp 'pixel-scroll-precision-mode))
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-use-momentum         t
        pixel-scroll-precision-large-scroll-height  40.0
        pixel-scroll-precision-interpolation-factor 1.0))

;; ============================================================================
;; LINE NUMBERS
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
;; DOOM-MODELINE
;; FIX 2.2.3: Activates via after-init-hook only — not direct call.
;;   ui-modeline.el's fallback block is guarded by (unless (featurep 'doom-modeline))
;;   and handles the case where doom-modeline is absent entirely. The two files
;;   do not overlap: ui-core.el owns doom-modeline; ui-modeline.el owns fallbacks.
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
  (add-hook 'after-init-hook #'doom-modeline-mode))

;; Frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " - Emacs IDE"
        (:eval (when (bound-and-true-p emacs-ide-wayland-p) " [Wayland]"))))

;; ============================================================================
;; VISUAL ENHANCEMENTS
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
        '(("TODO"       . "#ff6c6b")
          ("FIXME"      . "#ff6c6b")
          ("HACK"       . "#c678dd")
          ("NOTE"       . "#98be65")
          ("DEPRECATED" . "#ECBE7B")
          ("XXX"        . "#ff6c6b")
          ("BUG"        . "#ff6c6b")
          ("OPTIMIZE"   . "#51afef")
          ("PERF"       . "#51afef")
          ("REVIEW"     . "#c678dd"))))

(use-package beacon
  :init
  (setq beacon-blink-when-focused              t
        beacon-blink-when-window-scrolls       t
        beacon-blink-when-point-moves-vertically 10
        beacon-size                            40)
  :config
  (beacon-mode 1))

(use-package dimmer
  :init
  (setq dimmer-fraction              0.4
        dimmer-adjustment-mode       :foreground
        dimmer-use-colorspace        :rgb
        dimmer-watch-frame-focus-events nil)
  :config
  (dimmer-mode 1))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method                        'character
        highlight-indent-guides-responsive                    'top
        highlight-indent-guides-delay                         0
        highlight-indent-guides-auto-enabled                  t
        highlight-indent-guides-auto-character-face-perc      20
        highlight-indent-guides-auto-top-character-face-perc  40))

(use-package pulsar
  :init
  (setq pulsar-pulse      t
        pulsar-delay      0.055
        pulsar-iterations 10
        pulsar-face       'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow)
  :config
  (pulsar-global-mode 1))

;; ============================================================================
;; WHICH-KEY
;; ============================================================================
(use-package which-key
  :init
  (setq which-key-idle-delay           0.3
        which-key-idle-secondary-delay 0.05
        which-key-separator            " → "
        which-key-prefix-prefix        "+"
        which-key-sort-order           'which-key-key-order-alpha
        which-key-max-display-columns  4
        which-key-min-display-lines    6
        which-key-max-description-length 32
        which-key-show-prefix          'echo)
  :config
  (which-key-mode 1))

;; ============================================================================
;; WINDOW MANAGEMENT
;; NOTE: M-o binding for ace-window lives in keybindings.el — not here.
;;       See keybindings.el commentary on binding ownership.
;; ============================================================================
(use-package ace-window
  :init
  (setq aw-keys            '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope           'frame
        aw-background      t
        aw-dispatch-always t
        aw-minibuffer-flag t))

(winner-mode 1)

(use-package transpose-frame
  :bind (("C-c w t" . transpose-frame)
         ("C-c w f" . flip-frame)
         ("C-c w r" . rotate-frame-clockwise)))

;; ============================================================================
;; NEOTREE
;; ============================================================================
(use-package neotree
  ;; FIX: C-<f8> removed from neotree :bind.
  ;; debug-core.el v2.2.4 assigns C-<f8> to dap-breakpoint-condition as part of
  ;; its F8-family breakpoint layout (F8=toggle, C-F8=condition, S-F8=log,
  ;; C-S-F8=delete-all). Both neotree and debug-core.el claimed C-<f8>; since
  ;; keybindings.el (loaded last) re-assigned it to neotree-toggle, dap-
  ;; breakpoint-condition was silently left unbound on every startup.
  ;; Neotree is now bound to C-c n in keybindings.el — unambiguous, no F-key conflict.
  :init
  (setq neo-smart-open            t
        neo-theme                 (if (display-graphic-p) 'icons 'arrow)
        neo-window-width          30
        neo-create-file-auto-open t
        neo-auto-indent-point     t
        neo-modern-sidebar        t
        neo-show-updir-line       nil
        neo-vc-integration        '(face)))

;; ============================================================================
;; DIRED
;; ============================================================================
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; ============================================================================
;; COMPILATION COLORIZATION
;; Canonical definition — tools-terminal.el must NOT redefine this function.
;; ============================================================================
(use-package ansi-color
  :straight nil
  :config
  (defun emacs-ide-colorize-compilation-buffer ()
    "Apply ANSI color codes in compilation output."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook #'emacs-ide-colorize-compilation-buffer))

;; ============================================================================
;; VISUAL FILL COLUMN
;; ============================================================================
(use-package visual-fill-column
  :init
  (setq visual-fill-column-width                       120
        visual-fill-column-center-text                 nil
        visual-fill-column-enable-sensible-window-split t)
  :hook ((org-mode markdown-mode) . visual-fill-column-mode))

;; ============================================================================
;; TAB BAR
;; ============================================================================
(use-package tab-bar
  :straight nil
  :init
  (setq tab-bar-show              1
        tab-bar-close-button-show nil
        tab-bar-new-button-show   nil
        tab-bar-tab-hints         t
        tab-bar-separator         " "
        tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count
        tab-bar-format
        '(tab-bar-format-tabs tab-bar-separator
          tab-bar-format-align-right tab-bar-format-global))
  :config
  (tab-bar-mode 1))

;; ============================================================================
;; TRANSPARENCY
;; ============================================================================
(defun emacs-ide-set-transparency (alpha-bg alpha-fg)
  "Set frame transparency: ALPHA-BG and ALPHA-FG (0-100)."
  (interactive "nBackground (0-100): \nForeground (0-100): ")
  (set-frame-parameter nil 'alpha-background alpha-bg)
  (set-frame-parameter nil 'alpha alpha-fg))

;; ============================================================================
;; PRESENTATION MODE
;; NOTE: C-c P and F12 are bound in keybindings.el. Do not add global-set-key here.
;; ============================================================================
(defvar emacs-ide-presentation-mode nil
  "Whether presentation mode is active.")

(defun emacs-ide-presentation-mode ()
  "Toggle presentation mode (enlarged font)."
  (interactive)
  (if emacs-ide-presentation-mode
      (progn
        (set-face-attribute 'default nil :height 110)
        (setq emacs-ide-presentation-mode nil)
        (message "Presentation mode OFF"))
    (set-face-attribute 'default nil :height 200)
    (setq emacs-ide-presentation-mode t)
    (message "Presentation mode ON")))

(provide 'ui-core)
;;; ui-core.el ends here
