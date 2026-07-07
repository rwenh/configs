;;; editing-core.el --- Elite Editing Features -*- lexical-binding: t -*-
;;; Version: 3.3.1
;;;
;;; Code:

(require 'cl-lib)

;;;; ── Basic editing modes (AUTHORITATIVE — see #79) ───────────────────────────

(delete-selection-mode  1)
(global-auto-revert-mode 1)

(setq-default
 auto-revert-avoid-polling           t
 auto-revert-interval                3
 auto-revert-check-vc-info           t
 auto-revert-verbose                 nil
 global-auto-revert-non-file-buffers t)

;; Strip trailing whitespace before save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Always end files with a newline
(setq require-final-newline t)

;; Single space ends a sentence (affects fill, M-e, etc.)
(setq sentence-end-double-space nil)

;;;; ── Scroll quality (CALIBRATION) ───────────────────────────────────────────
;;

(setq auto-window-vscroll             nil
      scroll-preserve-screen-position t
      kill-do-not-save-duplicates     t
      scroll-conservatively           101
      scroll-margin                   5
      fast-but-imprecise-scrolling    t)

;;;; ── repeat-mode (CALIBRATION) ──────────────────────────────────────────────
;;
;; repeat-mode (Emacs 28+) lets you repeat certain key sequences by pressing
;; only the last key after the first invocation.  Examples:
;;   C-x [ [ [ [   → page backward 4 times
;;   C-x o o o     → cycle windows 3 times
;;   C-u C-u       → no change (universal-argument)
;; Press C-g, a movement key, or type anything to exit repeat mode.

(unless (version< emacs-version "28")
  (repeat-mode 1))

;;;; ── editorconfig (CALIBRATION) ─────────────────────────────────────────────
;;
;; When a project has a .editorconfig file, this mode reads it and applies:
;;   indent_style   → whether to use tabs or spaces
;;   indent_size    → how many spaces/columns per indent level
;;   end_of_line    → lf / crlf / cr
;;   charset        → utf-8 / latin1 etc.
;;   trim_trailing_whitespace
;;   insert_final_newline
;;
;; Emacs 30 ships editorconfig built-in.  For Emacs 29 and earlier we install
;; the package.

(if (version<= "30" emacs-version)
    ;; Built-in since Emacs 30 — just enable the mode
    (add-hook 'prog-mode-hook #'editorconfig-mode)
  ;; Emacs 29 and earlier — use the package
  (use-package editorconfig
    :demand t
    :config
    (editorconfig-mode 1)))

;;;; ── Meow (optional modal editing) ──────────────────────────────────────────

(defun emacs-ide-meow-setup ()
  "Configure Meow modal editing key bindings."
  (when (fboundp 'meow-global-mode)
    (meow-global-mode 1)
    (setq meow-use-clipboard            t
          meow-expand-hint-remove-delay 1.5
          meow-select-on-change         t
          meow-cursor-type-normal       'box
          meow-cursor-type-insert       'bar)
    (meow-normal-define-key
     '("0" . meow-expand-0) '("1" . meow-expand-1) '("2" . meow-expand-2)
     '("3" . meow-expand-3) '("4" . meow-expand-4) '("5" . meow-expand-5)
     '("6" . meow-expand-6) '("7" . meow-expand-7) '("8" . meow-expand-8)
     '("9" . meow-expand-9)
     '("-" . negative-argument)      '(";" . meow-reverse)
     '("," . meow-inner-of-thing)    '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing) '("]" . meow-end-of-thing)
     '("a" . meow-append)    '("A" . meow-open-below)
     '("b" . meow-back-word) '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)    '("D" . meow-backward-delete)
     '("e" . meow-next-word) '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection) '("G" . meow-grab)
     '("h" . meow-left)     '("H" . meow-left-expand)
     '("i" . meow-insert)   '("I" . meow-open-above)
     '("j" . meow-next)     '("J" . meow-next-expand)
     '("k" . meow-prev)     '("K" . meow-prev-expand)
     '("l" . meow-right)    '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)    '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)     '("Q" . meow-goto-line)
     '("r" . meow-replace)  '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word) '("W" . meow-mark-symbol)
     '("x" . meow-line)     '("X" . meow-goto-line)
     '("y" . meow-save)     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore))))

(defvar emacs-ide-meow-enabled nil
  "Non-nil when Meow modal editing is active.")

(defun emacs-ide-toggle-meow ()
  "Toggle Meow modal editing on/off."
  (interactive)
  (if emacs-ide-meow-enabled
      (progn
        (when (fboundp 'meow-global-mode) (meow-global-mode -1))
        (setq emacs-ide-meow-enabled nil)
        (message "Meow disabled — back to Emacs default keybindings"))
    (if (require 'meow nil 'noerror)
        (progn
          (emacs-ide-meow-setup)
          (setq emacs-ide-meow-enabled t)
          (message "Meow enabled — modal editing active"))
      (message "Meow not installed.  Use M-x emacs-ide-install-meow first"))))

(defun emacs-ide-install-meow ()
  "Install Meow via straight.el."
  (interactive)
  (if (fboundp 'straight-use-package)
      (progn
        (straight-use-package 'meow)
        (message "Meow installed — M-x emacs-ide-toggle-meow to activate"))
    (message "straight.el not available")))

(with-eval-after-load 'emacs-ide-config
  (when (and (boundp 'emacs-ide-config-data)
             (let ((editing (cdr (assoc 'editing emacs-ide-config-data))))
               (and editing (cdr (assoc 'meow editing)))))
    (add-hook 'after-init-hook
              (lambda ()
                (when (require 'meow nil 'noerror)
                  (emacs-ide-meow-setup)
                  (setq emacs-ide-meow-enabled t))))))

;;;; ── Smartparens ─────────────────────────────────────────────────────────────

(defun emacs-ide--disable-electric-pair-locally ()
  "Disable `electric-pair-mode' in the current buffer."
  (electric-pair-local-mode -1))

(use-package smartparens
  :defer t
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
              ("C-{"   . sp-backward-barf-sexp)
              ("M-d"   . sp-kill-word)
              ("M-["   . sp-unwrap-sexp))
  :init
  (setq sp-show-pair-delay              0
        sp-show-pair-from-inside        t
        sp-escape-quotes-after-insert   nil
        sp-highlight-pair-overlay       t
        sp-navigate-close-if-unbalanced t
        sp-message-width                nil)
  :config
  (require 'smartparens-config)
  (add-hook 'smartparens-mode-hook #'emacs-ide--disable-electric-pair-locally)
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  (sp-local-pair 'markdown-mode   "```" "```")
  (sp-local-pair 'org-mode        "=" "=" :actions '(wrap))
  (sp-local-pair 'org-mode        "~" "~" :actions '(wrap)))

;;;; ── ide-pair: wrap-on-select for Markdown / Org ────────────────────────────
;; With a region active, pressing * _ ` wraps it rather than replacing it.
;; Org = and ~ are already handled above via sp-local-pair :actions '(wrap).

(with-eval-after-load 'markdown-mode
  (when (require 'ide-pair
                 (expand-file-name "lib/ide-pair.el" user-emacs-directory) t)
    (ide-pair-setup-markdown-bindings)))

(with-eval-after-load 'org
  (when (require 'ide-pair
                 (expand-file-name "lib/ide-pair.el" user-emacs-directory) t)
    (ide-pair-setup-org-bindings)))

;;;; ── Undo-tree ───────────────────────────────────────────────────────────────

(use-package undo-tree
  :defer t
  :hook ((prog-mode text-mode) . undo-tree-mode)
  :init
  (setq undo-tree-visualizer-diff        t
        undo-tree-visualizer-timestamps  t
        undo-tree-auto-save-history      nil
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "var/undo-tree" user-emacs-directory))))
  :bind (("C-/"   . undo-tree-undo)
         ("C-?"   . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :config
  (when (fboundp 'global-undo-tree-mode)
    (global-undo-tree-mode 1)))

;;;; ── Multiple cursors ────────────────────────────────────────────────────────

(use-package multiple-cursors
  :defer t
  :bind (("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c m"       . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-c C-<"     . mc/mark-all-like-this-in-defun)
         ("M-<mouse-1>" . mc/add-cursor-on-click)))

;;;; ── Expand region ───────────────────────────────────────────────────────────

(use-package expand-region
  :defer t
  :bind (("C-="  . er/expand-region)
         ("C--"  . er/contract-region)))

;;;; ── Avy (character jump) ────────────────────────────────────────────────────

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

;;;; ── Move text ───────────────────────────────────────────────────────────────

(use-package move-text
  :defer t
  :bind (("M-<up>"   . move-text-up)
         ("M-<down>" . move-text-down)))

;;;; ── Surround ────────────────────────────────────────────────────────────────

(use-package surround
  :defer t
  :bind-keymap ("M-'" . surround-keymap))

;;;; ── Olivetti (zen / focus mode) ────────────────────────────────────────────

(use-package olivetti
  :defer t
  :init (setq olivetti-body-width 100)
  :commands olivetti-mode)

;;;; ── wgrep (editable grep results) ──────────────────────────────────────────

(use-package wgrep
  :defer t
  :init (setq wgrep-auto-save-buffer t))

;;;; ── Aggressive indent (experimental) ───────────────────────────────────────

(use-package aggressive-indent
  :defer t
  :if (and (fboundp 'emacs-ide-config-get)
           (emacs-ide-config-get 'advanced 'experimental nil))
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;;;; ── Whitespace visualisation ────────────────────────────────────────────────

(use-package whitespace
  :straight nil
  :defer t
  :init
  (setq whitespace-style '(face tabs tab-mark trailing)
        whitespace-display-mappings
        '((tab-mark   ?\t [?› ?\t] [?\\ ?\t])
          (space-mark ?\s [?·]     [?.]))))

;;;; ── Electric pairs (global fallback) ───────────────────────────────────────

(setq electric-pair-pairs '((?\" . ?\")
                             (?{  . ?})
                             (?\[ . ?\])
                             (?\( . ?\))
                             (?`  . ?`)))
(electric-pair-mode 1)

(provide 'editing-core)
;;; editing-core.el ends here
