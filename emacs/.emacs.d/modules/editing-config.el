;;; editing-config.el --- Enhanced Editing Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Custom functions, ergonomic editing, text manipulation, and advanced features
;;; Save as: ~/.emacs.d/modules/editing-config.el
;;;
;;; Code:

;; ============================================================================
;; BASIC EDITING MODES
;; ============================================================================
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(electric-pair-mode 1)

(setq-default auto-revert-avoid-polling t
              auto-revert-interval 5
              auto-revert-check-vc-info t
              auto-revert-verbose nil)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ============================================================================
;; SMARTPARENS (Better Bracket Handling) - ENHANCED
;; ============================================================================
(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (text-mode . smartparens-mode))
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-M-t" . sp-transpose-sexp)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp))
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-delay 0
        sp-show-pair-from-inside t
        sp-escape-quotes-after-insert nil
        sp-highlight-pair-overlay t
        sp-highlight-wrap-overlay t))

;; ============================================================================
;; UNDO-TREE (Visual Undo System) - ENHANCED
;; ============================================================================
(use-package undo-tree
  :ensure t
  :demand t
  :custom
  (undo-tree-auto-save-history nil)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-enable-undo-in-region t)
  :config
  (global-undo-tree-mode 1)
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize)
         ("C-c u" . undo-tree-visualize)))

(setq undo-limit 8000000
      undo-strong-limit 12000000
      undo-outer-limit 120000000)

;; ============================================================================
;; MULTIPLE CURSORS - ENHANCED
;; ============================================================================
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-c m l" . mc/edit-lines)
         ("C-c m a" . mc/mark-all-like-this)
         ("C-c m r" . mc/mark-all-in-region))
  :custom
  (mc/always-run-for-all t))

;; ============================================================================
;; EXPAND-REGION (Smart Selection) - ENHANCED
;; ============================================================================
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)
         ("C-c =" . er/expand-region)
         ("C-c -" . er/contract-region)))

;; ============================================================================
;; AVY (Jump to Visible Text) - ENHANCED
;; ============================================================================
(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-c j j" . avy-goto-char)
         ("C-c j l" . avy-goto-line)
         ("C-c j w" . avy-goto-word-1))
  :custom
  (avy-background t)
  (avy-style 'at-full))

;; ============================================================================
;; CUSTOM EDITING FUNCTIONS - ENHANCED
;; ============================================================================

(defun emacs-ide-move-line-up ()
  "Move current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun emacs-ide-move-line-down ()
  "Move current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun emacs-ide-duplicate-line ()
  "Duplicate the current line below."
  (interactive)
  (save-excursion
    (let ((line (thing-at-point 'line t)))
      (end-of-line)
      (newline)
      (insert line)))
  (forward-line 1))

(defun emacs-ide-duplicate-region ()
  "Duplicate the current region."
  (interactive)
  (when (region-active-p)
    (let ((text (buffer-substring (region-beginning) (region-end))))
      (goto-char (region-end))
      (insert text))))

(defun emacs-ide-smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun emacs-ide-kill-current-buffer ()
  "Kill the current buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(defun emacs-ide-kill-other-buffers ()
  "Kill all buffers except the current one and special buffers."
  (interactive)
  (let ((count 0))
    (dolist (buffer (delq (current-buffer) (buffer-list)))
      (unless (or (string-prefix-p " " (buffer-name buffer))
                  (string-prefix-p "*" (buffer-name buffer)))
        (kill-buffer buffer)
        (setq count (1+ count))))
    (message "Killed %d buffer(s)" count)))

(defun emacs-ide-comment-or-uncomment ()
  "Comment or uncomment region or current line intelligently."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun emacs-ide-hungry-delete-forward ()
  "Delete all whitespace forward from point."
  (interactive)
  (while (looking-at "[ \t\n]")
    (delete-char 1)))

(defun emacs-ide-hungry-delete-backward ()
  "Delete all whitespace backward from point."
  (interactive)
  (while (looking-back "[ \t\n]" 1)
    (backward-delete-char 1)))

(defun emacs-ide-join-lines ()
  "Join current line with next line."
  (interactive)
  (delete-indentation 1))

(defun emacs-ide-split-line ()
  "Split line at point and indent."
  (interactive)
  (newline-and-indent))

;; ============================================================================
;; DATE/TIME INSERTION
;; ============================================================================

(defun emacs-ide-insert-current-date ()
  "Insert current date in ISO format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun emacs-ide-insert-current-time ()
  "Insert current time."
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun emacs-ide-insert-current-datetime ()
  "Insert current date and time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun emacs-ide-insert-timestamp ()
  "Insert Unix timestamp."
  (interactive)
  (insert (format-time-string "%s")))

;; ============================================================================
;; FILE OPERATIONS - ENHANCED
;; ============================================================================

(defun emacs-ide-reload-config ()
  "Reload the init file."
  (interactive)
  (load-file user-init-file)
  (message "Configuration reloaded!"))

(defun emacs-ide-show-startup-time ()
  "Display Emacs startup time."
  (interactive)
  (message "Emacs loaded in %.2fs with %d packages"
           (float-time after-init-time)
           (length package-activated-list)))

(defun emacs-ide-show-system-info ()
  "Display system and Emacs information."
  (interactive)
  (message "Emacs %s | %s | %d packages | %s"
           emacs-version
           system-type
           (length package-activated-list)
           (if emacs-ide-wayland-p "Wayland" "X11/TTY")))

(defun emacs-ide-create-non-existent-directory ()
  "Offer to create parent directories if they don't exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and parent-directory
               (not (file-exists-p parent-directory)))
      (if (y-or-n-p (format "Directory `%s' does not exist. Create it? "
                            parent-directory))
          (make-directory parent-directory t)
        (error "Cannot save file without parent directory")))))

(add-hook 'find-file-not-found-functions 'emacs-ide-create-non-existent-directory)

(defun emacs-ide-rename-current-file ()
  "Rename the current file and buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun emacs-ide-delete-current-file ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (when (y-or-n-p (format "Really delete %s? " filename))
        (delete-file filename)
        (kill-buffer (current-buffer))
        (message "File deleted: %s" filename)))))

(defun emacs-ide-copy-file-path ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied: %s" filepath))))

(defun emacs-ide-copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied: %s" filename))))

;; ============================================================================
;; WINDOW SPLITTING WITH FOLLOW - ENHANCED
;; ============================================================================

(defun emacs-ide-split-horizontal-and-follow ()
  "Split window horizontally and move to the new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun emacs-ide-split-vertical-and-follow ()
  "Split window vertically and move to the new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun emacs-ide-rotate-windows ()
  "Rotate window layout."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                        (car next-win-edges))
                                    (<= (cadr this-win-edges)
                                        (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; ============================================================================
;; CODE FOLDING - ENHANCED
;; ============================================================================
(add-hook 'prog-mode-hook 'hs-minor-mode)

(defun emacs-ide-hs-display-code-line-counts (ov)
  "Display line counts in code folding overlay OV."
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'display
                 (format " ... (%d lines)"
                         (count-lines (overlay-start ov)
                                     (overlay-end ov))))))

(setq hs-set-up-overlay 'emacs-ide-hs-display-code-line-counts)

;; ============================================================================
;; INDENTATION HELPERS - ENHANCED
;; ============================================================================

(defun emacs-ide-indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (message "Buffer indented"))

(defun emacs-ide-cleanup-buffer ()
  "Cleanup buffer: indent, untabify, delete trailing whitespace."
  (interactive)
  (save-excursion
    (emacs-ide-indent-buffer)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace))
  (message "Buffer cleaned up"))

(defun emacs-ide-indent-region-or-buffer ()
  "Indent region if active, otherwise indent entire buffer."
  (interactive)
  (if (region-active-p)
      (indent-region (region-beginning) (region-end))
    (emacs-ide-indent-buffer)))

;; ============================================================================
;; SEARCH AND REPLACE - ENHANCED
;; ============================================================================

;; Use regex search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

;; Enhance isearch
(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      isearch-allow-scroll t)

(defun emacs-ide-search-project ()
  "Search in project using projectile."
  (interactive)
  (if (projectile-project-p)
      (projectile-ripgrep)
    (call-interactively 'grep)))

;; ============================================================================
;; TEXT TRANSFORMATION
;; ============================================================================

(defun emacs-ide-upcase-region-or-word ()
  "Upcase region if active, otherwise upcase word."
  (interactive)
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word 1)))

(defun emacs-ide-downcase-region-or-word ()
  "Downcase region if active, otherwise downcase word."
  (interactive)
  (if (region-active-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word 1)))

(defun emacs-ide-capitalize-region-or-word ()
  "Capitalize region if active, otherwise capitalize word."
  (interactive)
  (if (region-active-p)
      (capitalize-region (region-beginning) (region-end))
    (capitalize-word 1)))

;; ============================================================================
;; SORTING
;; ============================================================================

(defun emacs-ide-sort-lines-region ()
  "Sort lines in region."
  (interactive)
  (when (region-active-p)
    (sort-lines nil (region-beginning) (region-end))))

(defun emacs-ide-reverse-region ()
  "Reverse lines in region."
  (interactive)
  (when (region-active-p)
    (reverse-region (region-beginning) (region-end))))

;; ============================================================================
;; MACRO HELPERS
;; ============================================================================

(defun emacs-ide-toggle-record-macro ()
  "Toggle keyboard macro recording."
  (interactive)
  (if defining-kbd-macro
      (kmacro-end-macro nil)
    (kmacro-start-macro nil)))

(provide 'editing-config)
;;; editing-config.el ends here