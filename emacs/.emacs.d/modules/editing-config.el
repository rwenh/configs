;;; editing-config.el --- Elite Editing Features -*- lexical-binding: t -*-
;;; Commentary:
;;; Professional text manipulation and ergonomic editing
;;; Code:

;; ============================================================================
;; BASIC EDITING MODES
;; ============================================================================
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(electric-pair-mode 1)

(setq-default auto-revert-avoid-polling t
              auto-revert-interval 3
              auto-revert-check-vc-info t
              auto-revert-verbose nil
              global-auto-revert-non-file-buffers t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ============================================================================
;; SMARTPARENS
;; ============================================================================
(use-package smartparens
  :hook ((prog-mode text-mode) . smartparens-mode)
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-M-t" . sp-transpose-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("M-<delete>" . sp-unwrap-sexp)
              ("M-<backspace>" . sp-backward-unwrap-sexp)
              ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
              ("C-M-<delete>" . sp-splice-sexp-killing-forward))
  :init
  (setq sp-show-pair-delay 0
        sp-show-pair-from-inside t
        sp-escape-quotes-after-insert nil
        sp-highlight-pair-overlay t
        sp-highlight-wrap-overlay t
        sp-max-pair-length 4
        sp-max-prefix-length 50
        sp-navigate-close-if-unbalanced t
        sp-message-width nil)
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  (sp-local-pair 'markdown-mode "```" "```"))

;; ============================================================================
;; UNDO-TREE
;; ============================================================================
(use-package undo-tree
  :demand t
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree-hist/" user-emacs-directory)))
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-enable-undo-in-region t
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000)
  :config
  (global-undo-tree-mode 1)
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize)
         ("C-c u" . undo-tree-visualize)))

;; ============================================================================
;; MULTIPLE CURSORS
;; ============================================================================
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c m l" . mc/edit-lines)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this)
         ("C-c m r" . mc/mark-all-in-region)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m b" . mc/edit-beginnings-of-lines))
  :init
  (setq mc/always-run-for-all t
        mc/insert-numbers-default 1))

;; ============================================================================
;; EXPAND-REGION
;; ============================================================================
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)
         ("C-c =" . er/expand-region)
         ("C-c -" . er/contract-region)))

;; ============================================================================
;; AVY - JUMP NAVIGATION
;; ============================================================================
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-c j c" . avy-goto-char)
         ("C-c j l" . avy-goto-line)
         ("C-c j w" . avy-goto-word-1)
         ("C-c j j" . avy-goto-char-timer))
  :init
  (setq avy-background t
        avy-style 'at-full
        avy-timeout-seconds 0.3
        avy-all-windows t
        avy-case-fold-search t
        avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p)))

;; ============================================================================
;; MOVE-TEXT
;; ============================================================================
(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)
         ("M-p" . move-text-up)
         ("M-n" . move-text-down)))

;; ============================================================================
;; CUSTOM EDITING FUNCTIONS
;; ============================================================================
(defun emacs-ide-duplicate-line ()
  "Duplicate current line."
  (interactive)
  (save-excursion
    (let ((line (thing-at-point 'line t)))
      (end-of-line)
      (newline)
      (insert line)))
  (forward-line 1))

(defun emacs-ide-duplicate-region ()
  "Duplicate region."
  (interactive)
  (when (region-active-p)
    (let ((text (buffer-substring (region-beginning) (region-end))))
      (goto-char (region-end))
      (insert text))))

(defun emacs-ide-smart-beginning-of-line ()
  "Move to first non-whitespace or beginning."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun emacs-ide-kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun emacs-ide-kill-other-buffers ()
  "Kill all buffers except current."
  (interactive)
  (let ((count 0))
    (dolist (buffer (delq (current-buffer) (buffer-list)))
      (unless (or (string-prefix-p " " (buffer-name buffer))
                  (string-prefix-p "*" (buffer-name buffer)))
        (kill-buffer buffer)
        (setq count (1+ count))))
    (message "Killed %d buffer(s)" count)))

(defun emacs-ide-comment-or-uncomment ()
  "Comment or uncomment intelligently."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun emacs-ide-join-lines ()
  "Join current line with next."
  (interactive)
  (delete-indentation 1))

(defun emacs-ide-split-line ()
  "Split line at point."
  (interactive)
  (newline-and-indent))

(defun emacs-ide-insert-current-date ()
  "Insert current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun emacs-ide-insert-current-time ()
  "Insert current time."
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun emacs-ide-insert-current-datetime ()
  "Insert current datetime."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun emacs-ide-reload-config ()
  "Reload configuration."
  (interactive)
  (load-file user-init-file)
  (message "Configuration reloaded!"))

(defun emacs-ide-create-non-existent-directory ()
  "Create parent directory if needed."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and parent-directory
               (not (file-exists-p parent-directory)))
      (if (y-or-n-p (format "Directory `%s' does not exist. Create? "
                            parent-directory))
          (make-directory parent-directory t)
        (error "Cannot save without parent directory")))))

(add-hook 'find-file-not-found-functions 'emacs-ide-create-non-existent-directory)

(defun emacs-ide-rename-current-file ()
  "Rename current file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer not visiting file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun emacs-ide-delete-current-file ()
  "Delete current file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer not visiting file!")
      (when (y-or-n-p (format "Delete %s? " filename))
        (delete-file filename)
        (kill-buffer (current-buffer))
        (message "Deleted: %s" filename)))))

(defun emacs-ide-copy-file-path ()
  "Copy file path."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied: %s" filepath))))

(defun emacs-ide-copy-file-name ()
  "Copy file name."
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied: %s" filename))))

(defun emacs-ide-split-horizontal-and-follow ()
  "Split horizontally and follow."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun emacs-ide-split-vertical-and-follow ()
  "Split vertically and follow."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun emacs-ide-indent-buffer ()
  "Indent entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (message "Buffer indented"))

(defun emacs-ide-cleanup-buffer ()
  "Cleanup buffer."
  (interactive)
  (save-excursion
    (emacs-ide-indent-buffer)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace))
  (message "Buffer cleaned"))

(defun emacs-ide-indent-region-or-buffer ()
  "Indent region or buffer."
  (interactive)
  (if (region-active-p)
      (indent-region (region-beginning) (region-end))
    (emacs-ide-indent-buffer)))

;; ============================================================================
;; CODE FOLDING
;; ============================================================================
(add-hook 'prog-mode-hook 'hs-minor-mode)

(defun emacs-ide-hs-display-code-line-counts (ov)
  "Display line counts in folding."
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'display
                 (format " ... (%d lines)"
                         (count-lines (overlay-start ov)
                                     (overlay-end ov))))))

(setq hs-set-up-overlay 'emacs-ide-hs-display-code-line-counts)

;; ============================================================================
;; TEXT TRANSFORMATION
;; ============================================================================
(defun emacs-ide-upcase-region-or-word ()
  "Upcase region or word."
  (interactive)
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word 1)))

(defun emacs-ide-downcase-region-or-word ()
  "Downcase region or word."
  (interactive)
  (if (region-active-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word 1)))

(defun emacs-ide-capitalize-region-or-word ()
  "Capitalize region or word."
  (interactive)
  (if (region-active-p)
      (capitalize-region (region-beginning) (region-end))
    (capitalize-word 1)))

;; ============================================================================
;; SEARCH & REPLACE
;; ============================================================================
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      isearch-allow-scroll t
      isearch-allow-motion t
      isearch-motion-changes-direction t
      isearch-repeat-on-direction-change t
      isearch-wrap-pause 'no)

;; ============================================================================
;; WHITESPACE
;; ============================================================================
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup)
  :init
  (setq whitespace-style '(face tabs empty trailing lines-tail)
        whitespace-line-column 120))

;; ============================================================================
;; AGGRESSIVE INDENT
;; ============================================================================
(use-package aggressive-indent
  :hook ((emacs-lisp-mode lisp-mode scheme-mode) . aggressive-indent-mode)
  :init
  (setq aggressive-indent-comments-too t))

;; ============================================================================
;; VISUAL REGEXP
;; ============================================================================
(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

;; ============================================================================
;; STRING-INFLECTION
;; ============================================================================
(use-package string-inflection
  :bind ("C-c C-u" . string-inflection-all-cycle))

(provide 'editing-config)
;;; editing-config.el ends here