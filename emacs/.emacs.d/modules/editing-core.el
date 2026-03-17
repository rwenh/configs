;;; editing-core.el --- Elite Editing Features -*- lexical-binding: t -*-
;;; Commentary:
;;; v3.0.1: Fixes: removed duplicate electric-pair-mode (FIX-9) and duplicate
;;; dumb-jump block (FIX-10). All other features from 3.0.0 unchanged.
;;; Version: 3.0.1
;;; Code:

;; ============================================================================
;; BASIC EDITING MODES (unchanged)
;; FIX-9: electric-pair-mode removed here — init.el core-settings block
;;   already enables it. Calling it twice is harmless but redundant.
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
;; MEOW — OPTIONAL EMACS-NATIVE MODAL EDITING
;; NOT Vim. Meow is built around Emacs idioms:
;;   - NORMAL mode: single-char selection movements (w e b f t)
;;   - INSERT mode: full Emacs key bindings (C-x C-s etc unchanged)
;;   - No Evil, no Vim muscle memory required
;;   - C-c / C-x / C-h fully preserved in all modes
;; Enable: set editing.meow: true in config.yml
;; ============================================================================
(defun emacs-ide-meow-setup ()
  "Configure Meow with Emacs-native bindings. Called only if enabled."
  (when (fboundp 'meow-global-mode)
    (meow-global-mode 1)
    ;; Meow leader key — replaces the need for Vim leader
    (setq meow-use-clipboard t
          meow-expand-hint-remove-delay 1.5)
    ;; Normal mode movement (Emacs-style, not Vim-style)
    (meow-normal-define-key
     '("0" . meow-expand-0) '("1" . meow-expand-1) '("2" . meow-expand-2)
     '("3" . meow-expand-3) '("4" . meow-expand-4) '("5" . meow-expand-5)
     '("6" . meow-expand-6) '("7" . meow-expand-7) '("8" . meow-expand-8)
     '("9" . meow-expand-9)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
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

(defvar emacs-ide-meow-enabled nil
  "Whether Meow modal editing is active.")

(defun emacs-ide-toggle-meow ()
  "Toggle Meow modal editing on/off."
  (interactive)
  (if emacs-ide-meow-enabled
      (progn
        (when (fboundp 'meow-global-mode) (meow-global-mode -1))
        (setq emacs-ide-meow-enabled nil)
        (message "Meow disabled — back to standard Emacs editing"))
    (if (require 'meow nil 'noerror)
        (progn
          (emacs-ide-meow-setup)
          (setq emacs-ide-meow-enabled t)
          (message "Meow enabled — Emacs-native modal editing"))
      (message "Meow not installed. Add it via M-x emacs-ide-install-meow"))))

(defun emacs-ide-install-meow ()
  "Install Meow via straight.el."
  (interactive)
  (when (fboundp 'straight-use-package)
    (straight-use-package 'meow)
    (message "Meow installed. Run M-x emacs-ide-toggle-meow to enable.")))

;; Auto-enable Meow if config.yml has editing.meow: true
(with-eval-after-load 'emacs-ide-config
  (when (and (boundp 'emacs-ide-config-data)
             (let ((editing (cdr (assoc "editing" emacs-ide-config-data))))
               (and editing (cdr (assoc "meow" editing)))))
    (add-hook 'after-init-hook
              (lambda ()
                (when (require 'meow nil 'noerror)
                  (emacs-ide-meow-setup)
                  (setq emacs-ide-meow-enabled t))))))

;; Optional: load meow package (deferred, no cost if not enabled)
(use-package meow
  :if (or emacs-ide-meow-enabled
          (and (boundp 'emacs-ide-config-data)
               (let ((e (cdr (assoc "editing" emacs-ide-config-data))))
                 (and e (cdr (assoc "meow" e))))))
  :defer t)

;; ============================================================================
;; SMARTPARENS (unchanged from 2.2.3)
;; ============================================================================
(use-package smartparens
  :hook ((prog-mode text-mode) . smartparens-mode)
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)   ("C-M-b" . sp-backward-sexp)
              ("C-M-a" . sp-beginning-of-sexp) ("C-M-e" . sp-end-of-sexp)
              ("C-M-k" . sp-kill-sexp)      ("C-M-t" . sp-transpose-sexp)
              ("C-M-n" . sp-next-sexp)      ("C-M-p" . sp-previous-sexp)
              ("C-)"   . sp-forward-slurp-sexp)  ("C-}" . sp-forward-barf-sexp)
              ("C-("   . sp-backward-slurp-sexp) ("C-{" . sp-backward-barf-sexp))
  :init
  (setq sp-show-pair-delay             0
        sp-show-pair-from-inside       t
        sp-escape-quotes-after-insert  nil
        sp-highlight-pair-overlay      t
        sp-navigate-close-if-unbalanced t
        sp-message-width               nil)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  (sp-local-pair 'markdown-mode "```" "```"))

;; ============================================================================
;; UNDO-TREE (unchanged from 2.2.3, deferred)
;; ============================================================================
(use-package undo-tree
  :defer t
  :init
  (setq undo-tree-visualizer-diff       t
        undo-tree-visualizer-timestamps t
        undo-tree-auto-save-history     nil
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "var/undo-tree" user-emacs-directory))))
  :bind (("C-/"   . undo-tree-undo)
         ("C-?"   . undo-tree-redo)
         ("C-x u" . undo-tree-visualize)))

(add-hook 'after-init-hook
          (lambda () (when (fboundp 'global-undo-tree-mode) (global-undo-tree-mode 1))))

;; ============================================================================
;; MULTIPLE CURSORS (unchanged)
;; ============================================================================
(use-package multiple-cursors
  :bind (("C->"   . mc/mark-next-like-this)
         ("C-<"   . mc/mark-previous-like-this)
         ("C-c m" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

;; ============================================================================
;; EXPAND REGION (unchanged)
;; ============================================================================
(use-package expand-region
  :bind (("C-="   . er/expand-region)
         ("C--"   . er/contract-region)))

;; ============================================================================
;; AVY — CHAR-BASED NAVIGATION (unchanged)
;; ============================================================================
(use-package avy
  :bind (("C-:"   . avy-goto-char)
         ("C-'"   . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("C-c j c" . avy-goto-char)
         ("C-c j l" . avy-goto-line)
         ("C-c j w" . avy-goto-word-1)
         ("C-c j j" . avy-goto-char-timer))
  :init
  (setq avy-background t
        avy-style      'at-full
        avy-timeout-seconds 0.3))

;; ============================================================================
;; MOVE-TEXT (unchanged)
;; ============================================================================
(use-package move-text
  :bind (("M-<up>"   . move-text-up)
         ("M-<down>" . move-text-down)))

;; ============================================================================
;; HELPFUL (unchanged)
;; ============================================================================
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         ("C-h d" . helpful-at-point)))

;; ============================================================================
;; OLIVETTI — FOCUSED WRITING MODE (new in 3.0)
;; ============================================================================
(use-package olivetti
  :defer t
  :init (setq olivetti-body-width 100)
  :commands olivetti-mode)

;; ============================================================================
;; SURROUND (without Evil)
;; ============================================================================
(use-package surround
  :bind-keymap ("M-'" . surround-keymap))

;; ============================================================================
;; WGREP — EDITABLE GREP RESULTS (unchanged)
;; ============================================================================
(use-package wgrep
  :init (setq wgrep-auto-save-buffer t))

;; ============================================================================
;; DUMB-JUMP — definition jumping without LSP
;; FIX-10: Block removed. tools-lsp.el is the canonical owner of dumb-jump
;;   (it is an LSP fallback). Having it here AND in tools-lsp.el caused
;;   dumb-jump-xref-activate to be added to xref-backend-functions twice,
;;   running on every xref lookup twice. Configure dumb-jump in tools-lsp.el.
;; ============================================================================

(provide 'editing-core)
;;; editing-core.el ends here
