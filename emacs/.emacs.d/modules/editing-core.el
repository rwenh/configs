;;; editing-core.el --- Elite Editing Features -*- lexical-binding: t -*-
;;; Version: 3.1.0
;;; Code:

(delete-selection-mode  1)
(global-auto-revert-mode 1)

(setq-default
 auto-revert-avoid-polling          t
 auto-revert-interval               3
 auto-revert-check-vc-info          t
 auto-revert-verbose                nil
 global-auto-revert-non-file-buffers t)

;; Smart trailing whitespace: strip on save, but only in lines you touched
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Auto-insert final newline
(setq require-final-newline t)

;; Better sentence detection
(setq sentence-end-double-space nil)

;;; ─── Meow (optional modal editing) ─────────────────────────────────────────

(defun emacs-ide-meow-setup ()
  (when (fboundp 'meow-global-mode)
    (meow-global-mode 1)
    (setq meow-use-clipboard             t
          meow-expand-hint-remove-delay  1.5
          meow-select-on-change          t
          meow-cursor-type-normal        'box
          meow-cursor-type-insert        'bar)
    (meow-normal-define-key
     '("0" . meow-expand-0) '("1" . meow-expand-1) '("2" . meow-expand-2)
     '("3" . meow-expand-3) '("4" . meow-expand-4) '("5" . meow-expand-5)
     '("6" . meow-expand-6) '("7" . meow-expand-7) '("8" . meow-expand-8)
     '("9" . meow-expand-9)
     '("-" . negative-argument)    '(";" . meow-reverse)
     '("," . meow-inner-of-thing)  '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing) '("]" . meow-end-of-thing)
     '("a" . meow-append)   '("A" . meow-open-below)
     '("b" . meow-back-word) '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)   '("D" . meow-backward-delete)
     '("e" . meow-next-word) '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection) '("G" . meow-grab)
     '("h" . meow-left)    '("H" . meow-left-expand)
     '("i" . meow-insert)  '("I" . meow-open-above)
     '("j" . meow-next)    '("J" . meow-next-expand)
     '("k" . meow-prev)    '("K" . meow-prev-expand)
     '("l" . meow-right)   '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)   '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)    '("Q" . meow-goto-line)
     '("r" . meow-replace) '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)    '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word) '("W" . meow-mark-symbol)
     '("x" . meow-line)    '("X" . meow-goto-line)
     '("y" . meow-save)    '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore))))

(defvar emacs-ide-meow-enabled nil)

(defun emacs-ide-toggle-meow ()
  (interactive)
  (if emacs-ide-meow-enabled
      (progn
        (when (fboundp 'meow-global-mode) (meow-global-mode -1))
        (setq emacs-ide-meow-enabled nil)
        (message "Meow disabled — back to Emacs default"))
    (if (require 'meow nil 'noerror)
        (progn
          (emacs-ide-meow-setup)
          (setq emacs-ide-meow-enabled t)
          (message "Meow enabled — modal editing active"))
      (message "Meow not installed. Use M-x emacs-ide-install-meow first"))))

(defun emacs-ide-install-meow ()
  (interactive)
  (when (fboundp 'straight-use-package)
    (straight-use-package 'meow)
    (message "Meow installed — M-x emacs-ide-toggle-meow to activate")))

(with-eval-after-load 'emacs-ide-config
  (when (and (boundp 'emacs-ide-config-data)
             (let ((editing (cdr (assoc 'editing emacs-ide-config-data))))
               (and editing (cdr (assoc 'meow editing)))))
    (add-hook 'after-init-hook
              (lambda ()
                (when (require 'meow nil 'noerror)
                  (emacs-ide-meow-setup)
                  (setq emacs-ide-meow-enabled t))))))

;;; ─── Smartparens ────────────────────────────────────────────────────────────

(use-package smartparens
  :defer t
  :hook ((prog-mode text-mode) . smartparens-mode)
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)    ("C-M-b" . sp-backward-sexp)
              ("C-M-a" . sp-beginning-of-sexp) ("C-M-e" . sp-end-of-sexp)
              ("C-M-k" . sp-kill-sexp)       ("C-M-t" . sp-transpose-sexp)
              ("C-M-n" . sp-next-sexp)       ("C-M-p" . sp-previous-sexp)
              ("C-)"   . sp-forward-slurp-sexp)  ("C-}" . sp-forward-barf-sexp)
              ("C-("   . sp-backward-slurp-sexp) ("C-{" . sp-backward-barf-sexp)
              ("M-d"   . sp-kill-word)
              ("M-[" . sp-unwrap-sexp))
  :init
  (setq sp-show-pair-delay                0
        sp-show-pair-from-inside          t
        sp-escape-quotes-after-insert     nil
        sp-highlight-pair-overlay         t
        sp-navigate-close-if-unbalanced   t
        sp-message-width                  nil)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  (sp-local-pair 'markdown-mode "```" "```")
  (sp-local-pair 'org-mode "=" "=" :actions '(wrap))
  (sp-local-pair 'org-mode "~" "~" :actions '(wrap)))

;;; ─── Undo-tree ──────────────────────────────────────────────────────────────

(use-package undo-tree
  :defer t
  :hook ((prog-mode text-mode) . undo-tree-mode)
  :init
  (setq undo-tree-visualizer-diff       t
        undo-tree-visualizer-timestamps t
        undo-tree-auto-save-history     nil
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "var/undo-tree" user-emacs-directory))))
  :bind (("C-/"   . undo-tree-undo)
         ("C-?"   . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :config
  (when (fboundp 'global-undo-tree-mode)
    (global-undo-tree-mode 1)))

;;; ─── Multiple cursors ───────────────────────────────────────────────────────

(use-package multiple-cursors
  :defer t
  :bind (("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c m"       . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-c C-<"     . mc/mark-all-like-this-in-defun)
         ("M-<mouse-1>" . mc/add-cursor-on-click)))

;;; ─── Expand region ──────────────────────────────────────────────────────────

(use-package expand-region
  :defer t
  :bind (("C-="   . er/expand-region)
         ("C--"   . er/contract-region)))

;;; ─── Avy (jump) ─────────────────────────────────────────────────────────────

(use-package avy
  :defer t
  :bind (("C-:"     . avy-goto-char)
         ("C-'"     . avy-goto-char-2)
         ("M-g f"   . avy-goto-line)
         ("M-g w"   . avy-goto-word-1)
         ("C-c j c" . avy-goto-char)
         ("C-c j l" . avy-goto-line)
         ("C-c j w" . avy-goto-word-1)
         ("C-c j j" . avy-goto-char-timer))
  :init
  (setq avy-background      t
        avy-style           'at-full
        avy-timeout-seconds  0.3
        avy-all-windows      t
        avy-keys             '(?a ?s ?d ?f ?g ?h ?j ?k ?l
                               ?q ?w ?e ?r ?u ?i ?o ?p)))

;;; ─── Move text ──────────────────────────────────────────────────────────────

(use-package move-text
  :defer t
  :bind (("M-<up>"   . move-text-up)
         ("M-<down>" . move-text-down)))

;;; ─── Surround ───────────────────────────────────────────────────────────────

(use-package surround
  :defer t
  :bind-keymap ("M-'" . surround-keymap))

;;; ─── Olivetti (zen mode) ────────────────────────────────────────────────────

(use-package olivetti
  :defer t
  :init (setq olivetti-body-width 100)
  :commands olivetti-mode)

;;; ─── wgrep (editable grep) ──────────────────────────────────────────────────

(use-package wgrep
  :defer t
  :init (setq wgrep-auto-save-buffer t))

;;; ─── Aggressive indent (optional) ───────────────────────────────────────────

(use-package aggressive-indent
  :defer t
  :if (and (fboundp 'emacs-ide-config-get)
           (emacs-ide-config-get 'advanced 'experimental nil))
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;;; ─── Whitespace visualisation ───────────────────────────────────────────────

(use-package whitespace
  :straight nil
  :defer t
  :init
  (setq whitespace-style '(face tabs tab-mark trailing)
        whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t] [?\\ ?\t])
          (space-mark ?  [?·] [?.]))))

;;; ─── Helpful ────────────────────────────────────────────────────────────────

(use-package helpful
  :defer t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         ("C-h d" . helpful-at-point)))

;;; ─── Electric pairs tuned ───────────────────────────────────────────────────

(setq electric-pair-pairs '((?\" . ?\")
                             (?{ . ?})
                             (?\[ . ?\])
                             (?\( . ?\))
                             (?` . ?`)))
(electric-pair-mode 1)

(provide 'editing-core)
;;; editing-core.el ends here
