;;; completion-config.el --- Enhanced Completion Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; IDO, Company, Yasnippet, and advanced completion framework
;;; Save as: ~/.emacs.d/modules/completion-config.el
;;;
;;; Code:

;; ============================================================================
;; IDO MODE (Classic Emacs Completion) - ENHANCED
;; ============================================================================
(ido-mode 1)
(ido-everywhere 1)

(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always
      ido-use-virtual-buffers t
      ido-max-prospects 10
      ido-auto-merge-work-directories-length -1
      ido-ignore-extensions t
      ido-decorations '("\n→ " "" "\n   " "\n   ..." "[" "]" 
                        " [No match]" " [Matched]" " [Not readable]" 
                        " [Too big]" " [Confirm]")
      ido-file-extensions-order '(".org" ".txt" ".py" ".c" ".cpp" ".rs" 
                                  ".el" ".java" ".go" ".js" ".ts"))

;; ============================================================================
;; IMENU (Code Navigation) - ENHANCED
;; ============================================================================
(setq imenu-auto-rescan t
      imenu-max-item-length 100
      imenu-use-popup-menu nil
      imenu-sort-function 'imenu--sort-by-name)

(global-set-key (kbd "M-i") 'imenu)

;; Enhanced imenu with consult (if available)
(use-package consult
  :ensure t
  :bind (("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s r" . consult-ripgrep)
         ("M-s g" . consult-grep)
         ("C-x b" . consult-buffer))
  :custom
  (consult-preview-key 'any))

;; ============================================================================
;; COMPANY MODE (Code Completion) - ENHANCED
;; ============================================================================
(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)
         (text-mode . company-mode)
         (org-mode . company-mode))
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-selection)
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)
              ("C-h" . company-show-doc-buffer)
              ("C-w" . nil)
              ("RET" . nil))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-tooltip-limit 14)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-show-numbers t)
  (company-tooltip-flip-when-above t)
  (company-backends '((company-capf company-yasnippet company-files)
                      (company-dabbrev-code company-keywords)
                      company-dabbrev))
  :config
  ;; Enhanced company face
  (set-face-attribute 'company-tooltip nil :background "#2a2a2a")
  (set-face-attribute 'company-tooltip-selection nil :background "#4a4a4a")
  (set-face-attribute 'company-tooltip-common nil :foreground "#98be65")
  (set-face-attribute 'company-tooltip-annotation nil :foreground "#c678dd"))

;; Company quickhelp
(use-package company-quickhelp
  :ensure t
  :after company
  :custom
  (company-quickhelp-delay 0.3)
  (company-quickhelp-max-lines 10)
  :config
  (company-quickhelp-mode 1))

;; Company box (popup with icons)
(use-package company-box
  :ensure t
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-max-candidates 50)
  (company-box-icons-alist 'company-box-icons-all-the-icons))

;; ============================================================================
;; YASNIPPET (Code Snippets) - ENHANCED
;; ============================================================================
(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :bind (("C-c y e" . yas-expand)
         ("C-c y n" . yas-new-snippet)
         ("C-c y v" . yas-visit-snippet-file)
         ("C-c y i" . yas-insert-snippet))
  :custom
  (yas-verbosity 1)
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Auto-snippet for common patterns
(use-package auto-yasnippet
  :ensure t
  :bind (("C-c y w" . aya-create)
         ("C-c y y" . aya-expand)))

;; ============================================================================
;; HIPPIE EXPAND (Enhanced Completion) - ENHANCED
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

(global-set-key (kbd "M-/") 'hippie-expand)

;; ============================================================================
;; RECENTF (Recent Files) - ENHANCED
;; ============================================================================
(recentf-mode 1)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 30
      recentf-auto-cleanup 'never
      recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$" 
                        "\\.revive$" "/TAGS$" "^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'" "^/sudo:" "^/scp:"))

;; Save recentf list periodically
(run-at-time nil (* 5 60) 'recentf-save-list)

(defun emacs-ide-recentf-ido-find-file ()
  "Find recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-c r") 'emacs-ide-recentf-ido-find-file)

;; ============================================================================
;; SAVEHIST (Save Minibuffer History) - ENHANCED
;; ============================================================================
(savehist-mode 1)
(setq savehist-additional-variables '(search-ring regexp-search-ring 
                                      kill-ring compile-history)
      history-length 1000
      history-delete-duplicates t
      savehist-autosave-interval 60)

;; ============================================================================
;; SAVE PLACE (Remember Position in Files) - ENHANCED
;; ============================================================================
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil
      save-place-file (expand-file-name "places" user-emacs-directory))

;; ============================================================================
;; ABBREV MODE (Text Expansion) - ENHANCED
;; ============================================================================
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently
      abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

;; ============================================================================
;; BOOKMARKS - ENHANCED
;; ============================================================================
(setq bookmark-save-flag 1
      bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))

(global-set-key (kbd "C-c b s") 'bookmark-set)
(global-set-key (kbd "C-c b j") 'bookmark-jump)
(global-set-key (kbd "C-c b l") 'bookmark-bmenu-list)

;; ============================================================================
;; COMPLETION STYLES
;; ============================================================================
(setq completion-styles '(basic partial-completion emacs22 flex))
(setq completion-category-overrides '((file (styles basic partial-completion))))

;; ============================================================================
;; CORFU (Alternative to Company - Optional)
;; ============================================================================
;; Uncomment if you prefer Corfu over Company
;; (use-package corfu
;;   :ensure t
;;   :custom
;;   (corfu-auto t)
;;   (corfu-auto-delay 0.1)
;;   (corfu-auto-prefix 1)
;;   (corfu-cycle t)
;;   :init
;;   (global-corfu-mode))

;; ============================================================================
;; VERTICO (Alternative to IDO - Optional)
;; ============================================================================
;; Uncomment if you prefer Vertico over IDO
;; (use-package vertico
;;   :ensure t
;;   :init
;;   (vertico-mode)
;;   :custom
;;   (vertico-cycle t))

;; ============================================================================
;; ORDERLESS (Flexible completion style - Optional)
;; ============================================================================
;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion)))))

;; ============================================================================
;; MARGINALIA (Rich annotations in minibuffer)
;; ============================================================================
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy 
                           marginalia-annotators-light 
                           nil))
  :init
  (marginalia-mode))

;; ============================================================================
;; EMBARK (Contextual actions)
;; ============================================================================
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult))

;; ============================================================================
;; AUTO-COMPLETE PAIRS - ENHANCED
;; ============================================================================
(electric-pair-mode 1)
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\' . ?\')
        (?\{ . ?\})
        (?\[ . ?\])
        (?\( . ?\))))

;; ============================================================================
;; COMPLETION AT POINT
;; ============================================================================
(setq tab-always-indent 'complete
      completion-cycle-threshold 3
      completion-auto-help 'always)

;; ============================================================================
;; FILE NAME COMPLETION
;; ============================================================================
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; ============================================================================
;; CUSTOM COMPLETION FUNCTIONS
;; ============================================================================
(defun emacs-ide-company-complete-or-indent ()
  "Complete or indent depending on context."
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

(global-set-key (kbd "TAB") 'emacs-ide-company-complete-or-indent)

;; ============================================================================
;; COMPLETION STATISTICS
;; ============================================================================
(defun emacs-ide-show-completion-stats ()
  "Display completion usage statistics."
  (interactive)
  (message "Company completions: %d | Snippets: %d"
           (or (bound-and-true-p company-statistics--scores-table) 0)
           (length yas--tables)))

(provide 'completion-config)
;;; completion-config.el ends here