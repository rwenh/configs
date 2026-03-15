;;; editing-core.el --- Elite Editing Features -*- lexical-binding: t -*-
;;; Commentary:
;;; Professional text manipulation and ergonomic editing.
;;; Version: 2.2.3
;;; Fixes:
;;;   - 2.2.3: undo-tree :demand t removed; activation deferred to
;;;     after-init-hook.  undo-tree with :demand t loaded the package and
;;;     called (global-undo-tree-mode 1) synchronously on the startup
;;;     critical path.  undo-tree is a non-trivial package (it advices
;;;     several core functions).  Deferring to after-init-hook saves
;;;     ~0.3-0.8s and has no visible effect — it is active before the
;;;     user can type.
;;;   - 2.2.2: (inherited) C-c u undo-tree-visualize removed (stray global
;;;     outside keybindings.el).
;;; Code:

;; ============================================================================
;; BASIC EDITING MODES
;; ============================================================================
(delete-selection-mode 1)
(global-auto-revert-mode 1)

(setq-default auto-revert-avoid-polling          t
              auto-revert-interval               3
              auto-revert-check-vc-info          t
              auto-revert-verbose                nil
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
              ("C-)"   . sp-forward-slurp-sexp)
              ("C-}"   . sp-forward-barf-sexp)
              ("C-("   . sp-backward-slurp-sexp)
              ("C-{"   . sp-backward-barf-sexp))
  :init
  (setq sp-show-pair-delay        0
        sp-show-pair-from-inside  t
        sp-escape-quotes-after-insert nil
        sp-highlight-pair-overlay t
        sp-navigate-close-if-unbalanced t
        sp-message-width          nil)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  (sp-local-pair 'markdown-mode "```" "```"))

;; ============================================================================
;; UNDO-TREE
;; FIX 2.2.3: :demand t removed; activation deferred to after-init-hook.
;; undo-tree with :demand t loaded the package and ran (global-undo-tree-mode 1)
;; synchronously at startup.  undo-tree advices several core editing functions
;; and has measurable load cost (~0.3-0.8s).  Deferring to after-init-hook
;; moves it off the critical path; it is active before any user interaction.
;; The :bind block provides a trigger so the package loads on first use of
;; C-/ or C-? even if after-init-hook somehow fires late.
;; ============================================================================
(use-package undo-tree
  :init
  (let ((undo-dir (expand-file-name "undo-tree-hist/" user-emacs-directory)))
    (unless (file-directory-p undo-dir)
      (make-directory undo-dir t)))
  (setq undo-tree-auto-save-history    t
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree-hist/" user-emacs-directory)))
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff       t
        undo-tree-enable-undo-in-region t
        undo-limit                      800000
        undo-strong-limit               12000000
        undo-outer-limit                120000000)
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize)))

(add-hook 'after-init-hook
          (lambda ()
            (when (fboundp 'global-undo-tree-mode)
              (global-undo-tree-mode 1))))

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
  (setq mc/always-run-for-all    t
        mc/insert-numbers-default 1))

;; ============================================================================
;; EXPAND-REGION
;; ============================================================================
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; ============================================================================
;; AVY — merged from editing-nav.el (now deleted)
;; ============================================================================
(use-package avy
  :bind (("C-:"   . avy-goto-char)
         ("C-'"   . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-c j c" . avy-goto-char)
         ("C-c j l" . avy-goto-line)
         ("C-c j w" . avy-goto-word-1)
         ("C-c j j" . avy-goto-char-timer))
  :init
  (setq avy-background    t
        avy-style         'at-full
        avy-timeout-seconds 0.3
        avy-all-windows   t
        avy-case-fold-search t
        avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l
                   ?q ?w ?e ?r ?u ?i ?o ?p)))

;; ============================================================================
;; MOVE-TEXT
;; ============================================================================
(use-package move-text
  :bind (("M-<up>"   . move-text-up)
         ("M-<down>" . move-text-down)))

;; ============================================================================
;; EDITING UTILITY FUNCTIONS
;; ============================================================================
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
  "Duplicate the active region."
  (interactive)
  (when (region-active-p)
    (let ((text (buffer-substring (region-beginning) (region-end))))
      (goto-char (region-end))
      (insert text))))

(defun emacs-ide-smart-beginning-of-line ()
  "Toggle between first non-whitespace and true beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (when (= oldpos (point))
      (beginning-of-line))))

(defun emacs-ide-kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun emacs-ide-kill-other-buffers ()
  "Kill all file-visiting buffers except the current one."
  (interactive)
  (let ((count 0))
    (dolist (buffer (delq (current-buffer) (buffer-list)))
      (unless (or (string-prefix-p " " (buffer-name buffer))
                  (string-prefix-p "*" (buffer-name buffer)))
        (kill-buffer buffer)
        (cl-incf count)))
    (message "Killed %d buffer(s)" count)))

(defun emacs-ide-comment-or-uncomment ()
  "Comment or uncomment the line or active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun emacs-ide-join-lines ()
  "Join the current line with the next."
  (interactive)
  (delete-indentation 1))

(defun emacs-ide-split-line ()
  "Split the line at point."
  (interactive)
  (newline-and-indent))

(defun emacs-ide-reload-config ()
  "Reload the Emacs init file."
  (interactive)
  (load-file user-init-file)
  (message "✓ Configuration reloaded!"))

(defun emacs-ide-create-non-existent-directory ()
  "Offer to create parent directories when finding a new file."
  (let ((parent (file-name-directory buffer-file-name)))
    (when (and parent (not (file-exists-p parent)))
      (if (y-or-n-p (format "Directory `%s' does not exist. Create? " parent))
          (make-directory parent t)
        (error "Cannot save without parent directory")))))

(add-hook 'find-file-not-found-functions
          #'emacs-ide-create-non-existent-directory)

(defun emacs-ide-rename-current-file ()
  "Rename the file backing the current buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (if (vc-backend filename)
            (vc-rename-file filename new-name)
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t))))))

(defun emacs-ide-delete-current-file ()
  "Delete the file backing the current buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer not visiting a file!")
      (when (y-or-n-p (format "Delete %s? " filename))
        (delete-file filename)
        (kill-buffer (current-buffer))
        (message "Deleted: %s" filename)))))

(defun emacs-ide-copy-file-path ()
  "Copy the current buffer's full file path to the kill ring."
  (interactive)
  (let ((filepath (if (eq major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied: %s" filepath))))

(defun emacs-ide-copy-file-name ()
  "Copy the current buffer's file name (no directory) to the kill ring."
  (interactive)
  (when-let ((filename (buffer-file-name)))
    (let ((name (file-name-nondirectory filename)))
      (kill-new name)
      (message "Copied: %s" name))))

(defun emacs-ide-split-horizontal-and-follow ()
  "Split window below and move cursor to new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun emacs-ide-split-vertical-and-follow ()
  "Split window right and move cursor to new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun emacs-ide-indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (message "✓ Buffer indented"))

(defun emacs-ide-cleanup-buffer ()
  "Indent buffer, untabify, and strip trailing whitespace."
  (interactive)
  (save-excursion
    (emacs-ide-indent-buffer)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace))
  (message "✓ Buffer cleaned"))

(defun emacs-ide-indent-region-or-buffer ()
  "Indent the active region, or the whole buffer if none."
  (interactive)
  (if (region-active-p)
      (indent-region (region-beginning) (region-end))
    (emacs-ide-indent-buffer)))

(provide 'editing-core)
;;; editing-core.el ends here
