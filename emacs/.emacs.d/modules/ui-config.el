;;; ui-config.el --- Enhanced Visual and UI Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Theme, mode-line, fonts, visual settings, and modern UI enhancements
;;; Save as: ~/.emacs.d/modules/ui-config.el
;;;
;;; Code:

;; ============================================================================
;; BASIC UI CLEANUP
;; ============================================================================
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

;; Disable bell
(setq ring-bell-function 'ignore
      visible-bell nil)

;; No dialog boxes
(setq use-dialog-box nil
      use-file-dialog nil)

;; Fast echo keystrokes
(setq echo-keystrokes 0.02)

;; ============================================================================
;; THEME DETECTION AND LOADING
;; ============================================================================
(defun emacs-ide-detect-system-theme ()
  "Detect theme preference from Sway config or environment."
  (let ((sway-config (expand-file-name "~/.config/sway/config")))
    (cond
     ((and (file-symlink-p sway-config)
           (string-match "latte" (file-symlink-p sway-config)))
      'light)
     ((string= (getenv "EMACS_THEME") "light")
      'light)
     ((getenv "WAYLAND_DISPLAY")
      'dark)
     (t 'dark))))

;; ============================================================================
;; MODUS THEMES - ENHANCED
;; ============================================================================
(use-package modus-themes
  :ensure t
  :demand t
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-mixed-fonts t
        modus-themes-subtle-line-numbers t
        modus-themes-success-deuteranopia t
        modus-themes-fringes 'subtle
        modus-themes-org-blocks 'gray-background
        modus-themes-headings '((1 . (rainbow overline background 1.4))
                                (2 . (rainbow background 1.3))
                                (3 . (rainbow bold 1.2))
                                (t . (semilight 1.1)))
        modus-themes-scale-headings t)
  
  (let ((theme-variant (emacs-ide-detect-system-theme)))
    (if (eq theme-variant 'light)
        (load-theme 'modus-operandi t)
      (load-theme 'modus-vivendi t)))
  
  (message "✓ Theme loaded: %s"
           (if (eq (emacs-ide-detect-system-theme) 'light)
               "Modus Operandi (Light)"
             "Modus Vivendi (Dark)")))

;; Theme toggling
(defun emacs-ide-toggle-theme ()
  "Toggle between light and dark theme."
  (interactive)
  (if (custom-theme-enabled-p 'modus-vivendi)
      (progn
        (disable-theme 'modus-vivendi)
        (load-theme 'modus-operandi t)
        (message "Switched to Modus Operandi (Light)"))
    (disable-theme 'modus-operandi)
    (load-theme 'modus-vivendi t)
    (message "Switched to Modus Vivendi (Dark)")))

(global-set-key (kbd "<f12>") 'emacs-ide-toggle-theme)

;; ============================================================================
;; DASHBOARD (Startup Screen) - NEW
;; ============================================================================
(use-package dashboard
  :ensure t
  :demand t
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((recents  . 10)
                     (projects . 5)
                     (bookmarks . 5)))
  (dashboard-set-navigator t)
  (dashboard-set-init-info t)
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; ============================================================================
;; FONTS - ENHANCED
;; ============================================================================
(when (display-graphic-p)
  (setq frame-resize-pixelwise t
        window-resize-pixelwise t)
  
  (defun emacs-ide-set-font (font-name size)
    "Set font FONT-NAME at SIZE if available."
    (when (member font-name (font-family-list))
      (set-face-attribute 'default nil
                          :font font-name
                          :height (* size 10)
                          :weight 'normal)))
  
  ;; Monospace fonts (try in order)
  (cond
   ((member "JetBrains Mono" (font-family-list))
    (emacs-ide-set-font "JetBrains Mono" 11))
   ((member "Fira Code" (font-family-list))
    (emacs-ide-set-font "Fira Code" 11))
   ((member "Cascadia Code" (font-family-list))
    (emacs-ide-set-font "Cascadia Code" 11))
   ((member "Source Code Pro" (font-family-list))
    (emacs-ide-set-font "Source Code Pro" 11))
   (t
    (set-face-attribute 'default nil :height 110)))
  
  ;; Variable pitch font
  (cond
   ((member "Inter" (font-family-list))
    (set-face-attribute 'variable-pitch nil :font "Inter-11"))
   ((member "Cantarell" (font-family-list))
    (set-face-attribute 'variable-pitch nil :font "Cantarell-11"))
   ((member "DejaVu Sans" (font-family-list))
    (set-face-attribute 'variable-pitch nil :font "DejaVu Sans-11")))
  
  ;; Wayland smooth scrolling
  (when (and (getenv "WAYLAND_DISPLAY")
             (fboundp 'pixel-scroll-precision-mode))
    (pixel-scroll-precision-mode 1)))

;; ============================================================================
;; LINE NUMBERS & VISUAL ELEMENTS - ENHANCED
;; ============================================================================
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type t
      display-line-numbers-width 3
      display-line-numbers-grow-only t)

;; Disable line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(column-number-mode 1)
(size-indication-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)

(setq show-paren-delay 0
      show-paren-style 'mixed
      show-paren-when-point-inside-paren t)

;; ============================================================================
;; MODE-LINE - ENHANCED
;; ============================================================================
(setq-default mode-line-format
              '("%e" 
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;; ============================================================================
;; BREADCRUMB (Show current function/class)
;; ============================================================================
(use-package breadcrumb
  :ensure t
  :demand t
  :config
  (breadcrumb-mode 1))

;; ============================================================================
;; FRAME TITLE
;; ============================================================================
(setq frame-title-format
      '(:eval (concat "Emacs IDE: "
                      (if (buffer-file-name)
                          (abbreviate-file-name (buffer-file-name))
                        (buffer-name))
                      (when (buffer-modified-p) " •")
                      (when emacs-ide-wayland-p " [Wayland]"))))

;; ============================================================================
;; VISUAL ENHANCEMENTS - EXPANDED
;; ============================================================================
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO" . "#ff6c6b")
     ("FIXME" . "#ff6c6b")
     ("HACK" . "#c678dd")
     ("NOTE" . "#98be65")
     ("DEPRECATED" . "#ECBE7B")
     ("XXX" . "#ff6c6b")
     ("BUG" . "#ff6c6b")
     ("OPTIMIZE" . "#51afef"))))

;; Beacon - highlight cursor on scroll
(use-package beacon
  :ensure t
  :demand t
  :custom
  (beacon-blink-when-focused t)
  (beacon-blink-when-window-scrolls t)
  :config
  (beacon-mode 1))

;; Dimmer - dim inactive windows
(use-package dimmer
  :ensure t
  :demand t
  :custom
  (dimmer-fraction 0.3)
  :config
  (dimmer-mode 1))

;; Highlight indent guides
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0))

;; ============================================================================
;; WHICH-KEY (Show available keybindings) - ENHANCED
;; ============================================================================
(use-package which-key
  :ensure t
  :demand t
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.05)
  (which-key-separator " → ")
  (which-key-prefix-prefix "+")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-max-display-columns 3)
  (which-key-add-column-padding 1)
  :config
  (which-key-mode 1))

;; ============================================================================
;; WINDOW MANAGEMENT - ENHANCED
;; ============================================================================
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  (aw-background t)
  (aw-dispatch-always t))

;; Winner mode
(winner-mode 1)

;; ============================================================================
;; NEOTREE (File Tree Sidebar) - NEW
;; ============================================================================
(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle)
         ("C-c n" . neotree-projectile-action))
  :custom
  (neo-smart-open t)
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config
  (defun neotree-projectile-action ()
    "Open NeoTree using the project root, or pwd if not in a project."
    (interactive)
    (let ((project-root (projectile-project-root)))
      (neotree-toggle)
      (if project-root
          (neotree-dir project-root)
        (neotree-dir default-directory)))))

;; ============================================================================
;; DIRED ENHANCEMENT
;; ============================================================================
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  :bind (:map dired-mode-map
              ("h" . dired-up-directory)
              ("l" . dired-find-alternate-file)))

;; All the icons for dired
(use-package all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; ============================================================================
;; ANSI COLOR IN COMPILATION BUFFERS
;; ============================================================================
(use-package ansi-color
  :ensure nil
  :config
  (defun emacs-ide-colorize-compilation-buffer ()
    "Colorize compilation buffer output."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  
  (add-hook 'compilation-filter-hook 'emacs-ide-colorize-compilation-buffer))

;; ============================================================================
;; DIFF MODE COLORS
;; ============================================================================
(use-package diff-mode
  :ensure nil
  :config
  (set-face-attribute 'diff-added nil :foreground "#98be65" :background nil)
  (set-face-attribute 'diff-removed nil :foreground "#ff6c6b" :background nil)
  (set-face-attribute 'diff-changed nil :foreground "#ECBE7B" :background nil))

;; ============================================================================
;; TRANSPARENCY (Optional)
;; ============================================================================
(defun emacs-ide-set-transparency (value)
  "Set frame transparency to VALUE (0-100)."
  (interactive "nTransparency (0-100): ")
  (set-frame-parameter (selected-frame) 'alpha value))

;; ============================================================================
;; VISUAL FILL COLUMN (Center text)
;; ============================================================================
(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode))

;; ============================================================================
;; MINIMAP (Code overview sidebar) - Optional
;; ============================================================================
(use-package minimap
  :ensure t
  :commands (minimap-mode)
  :custom
  (minimap-window-location 'right)
  (minimap-width-fraction 0.1))

;; ============================================================================
;; PRESENTATION MODE
;; ============================================================================
(defvar emacs-ide-presentation-mode nil
  "Non-nil if in presentation mode.")

(defun emacs-ide-presentation-mode ()
  "Toggle presentation mode with larger fonts."
  (interactive)
  (if emacs-ide-presentation-mode
      (progn
        (set-face-attribute 'default nil :height 110)
        (setq emacs-ide-presentation-mode nil)
        (message "Presentation mode OFF"))
    (set-face-attribute 'default nil :height 180)
    (setq emacs-ide-presentation-mode t)
    (message "Presentation mode ON")))

(provide 'ui-config)
;;; ui-config.el ends here