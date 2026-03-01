;;; keybindings.el --- Vanilla-first IDE Keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Philosophy: Emacs defaults are good. We only bind what Emacs has no key for.
;;; No overrides of core defaults. No invented collisions to patch.
;;;
;;; What this file does:
;;;   1. Upgrades a small number of built-ins to smarter equivalents
;;;      (ibuffer, consult variants) — same keys, better commands.
;;;   2. Adds global keys for features that simply don't exist in vanilla Emacs
;;;      (avy is in editing-core.el, ace-window, magit, helpful, org globals).
;;;   3. Leaves everything else alone — C-x C-s saves, M-; comments,
;;;      C-h f describes, C-/ undoes, exactly as Emacs intended.
;;;
;;; What this file does NOT do:
;;;   - Override C-c as a prefix-command (breaks all major-mode C-c bindings)
;;;   - Re-bind things Emacs already has a key for just to have a "custom" one
;;;   - Duplicate bindings that individual modules already own via :bind
;;;
;;; Module binding ownership (do not duplicate here):
;;;   debug-core.el      → F5-F9, C-c d h, C-c d ?, C-c D s/r/q
;;;   editing-core.el    → C-/ C-? C-x u (undo-tree), C-> C-< (mc),
;;;                        C-= (expand-region), C-: C-' M-g f/w (avy),
;;;                        M-up/down (move-text), smartparens C-M-* map
;;;   tools-lsp.el       → C-c l * (lsp-mode-map), M-g j/b/O (dumb-jump),
;;;                        C-c ! * (flycheck-mode-map), helpful bindings
;;;   tools-project.el   → C-c p * (projectile-mode-map), C-c T (treemacs)
;;;   tools-terminal.el  → C-c t/T/M-t/e (vterm, multi-vterm, eshell)
;;;   tools-spelling.el  → C-c S * (flyspell-mode-map)
;;;   completion-core.el → C-. C-; (embark), M-/ (hippie-expand)
;;;   ui-core.el         → M-o (ace-window via use-package :bind)
;;;
;;; Version: 3.0.0
;;; Code:

;; ============================================================================
;; BUILT-IN UPGRADES
;; Same keys the user already knows — just better commands underneath.
;; ============================================================================

;; ibuffer is strictly better than the default buffer-list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; consult variants: same muscle memory, adds preview + fuzzy matching
(global-set-key (kbd "C-x b")   'consult-buffer)
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)
(global-set-key (kbd "C-x r b") 'consult-bookmark)
(global-set-key (kbd "M-y")     'consult-yank-pop)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)

;; M-g is Emacs's own "goto" prefix — consult fits here naturally
(global-set-key (kbd "M-g g")   'consult-goto-line)
(global-set-key (kbd "M-g M-g") 'consult-goto-line)
(global-set-key (kbd "M-g i")   'consult-imenu)
(global-set-key (kbd "M-g I")   'consult-imenu-multi)
(global-set-key (kbd "M-g o")   'consult-outline)
(global-set-key (kbd "M-g m")   'consult-mark)
(global-set-key (kbd "M-g k")   'consult-global-mark)

;; M-s is Emacs's own "search" prefix — consult fits here naturally
(global-set-key (kbd "M-s l")   'consult-line)
(global-set-key (kbd "M-s L")   'consult-line-multi)
(global-set-key (kbd "M-s r")   'consult-ripgrep)
(global-set-key (kbd "M-s g")   'consult-grep)
(global-set-key (kbd "M-s G")   'consult-git-grep)
(global-set-key (kbd "M-s f")   'consult-find)
(global-set-key (kbd "M-s k")   'consult-keep-lines)
(global-set-key (kbd "M-s u")   'consult-focus-lines)

;; ============================================================================
;; WINDOW MANAGEMENT
;; Vanilla Emacs has no fast multi-window jump. ace-window fills the gap.
;; M-o is unbound in vanilla (it used to be facemenu, removed in Emacs 29).
;; winner-mode C-c left/right are set automatically when winner-mode enables.
;; ============================================================================

(global-set-key (kbd "M-o") 'ace-window)

;; ============================================================================
;; VERSION CONTROL — MAGIT
;; C-x g is the universally accepted convention. C-x v is Emacs vc-mode's
;; prefix; we leave it alone and add magit alongside it.
;; ============================================================================

(global-set-key (kbd "C-x g")   'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-x v t") 'git-timemachine)  ; fits the C-x v vc prefix

;; ============================================================================
;; HELP — HELPFUL
;; Replaces C-h sub-keys with helpful equivalents — identical interface,
;; richer output. C-c C-d is new (vanilla has no at-point help key).
;; NOTE: tools-lsp.el also sets these inside its own use-package block.
;;       Defining them here ensures they work even when LSP is disabled.
;; ============================================================================

(global-set-key (kbd "C-h f")   'helpful-callable)
(global-set-key (kbd "C-h v")   'helpful-variable)
(global-set-key (kbd "C-h k")   'helpful-key)
(global-set-key (kbd "C-h F")   'helpful-function)
(global-set-key (kbd "C-h C")   'helpful-command)
(global-set-key (kbd "C-c C-d") 'helpful-at-point)

;; ============================================================================
;; ORG MODE
;; These three are the bindings the Org manual recommends as globals.
;; C-c l in LSP buffers is overridden locally by lsp-mode-map — that's fine,
;; org-store-link remains available everywhere else.
;; ============================================================================

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

;; ============================================================================
;; COMPILE
;; Vanilla Emacs has no default key for compile/recompile.
;; C-c C-c / C-c C-r are free at the global level (major modes use them
;; locally, which is fine — local bindings always win).
;; ============================================================================

(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-c C-r") 'recompile)

;; ============================================================================
;; UTILITY
;; ============================================================================

;; Discoverability
(global-set-key (kbd "C-c ?") 'which-key-show-top-level)
(global-set-key (kbd "C-c H") 'emacs-ide-show-keybindings-help)

;; Config management
(global-set-key (kbd "C-c R") 'emacs-ide-reload-config)
(global-set-key (kbd "C-c L") 'emacs-ide-lsp-status)

;; UI toggles (functions defined in ui-core.el)
(global-set-key (kbd "<f8>")  'neotree-toggle)
;; <f9> treemacs is bound in tools-project.el via :bind — no duplicate needed here
(global-set-key (kbd "<f12>") 'emacs-ide-toggle-theme)
(global-set-key (kbd "C-c P") 'emacs-ide-presentation-mode)

;; ESC as a quit key for GUI users (doesn't affect terminal ESC-as-meta)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ============================================================================
;; CHEAT SHEET
;; Only documents what differs from vanilla or is non-obvious.
;; Vanilla bindings (C-x C-s, C-x C-f, M-;, C-/, M-. etc.) are omitted —
;; they work as always and C-h k will explain any of them on demand.
;; ============================================================================

(defun emacs-ide-show-keybindings-help ()
  "Show non-obvious IDE bindings. Vanilla Emacs defaults are omitted."
  (interactive)
  (with-output-to-temp-buffer "*IDE Keybindings*"
    (princ
     "=== EMACS IDE — NON-OBVIOUS BINDINGS ===
Vanilla defaults work normally. Use C-h k to look up any key.

UPGRADED BUILT-INS (same key, smarter command):
  C-x C-b       ibuffer              (was list-buffers)
  C-x b         consult-buffer       (adds live preview)
  M-y           consult-yank-pop     (adds search through kill ring)
  M-g g         consult-goto-line    (adds preview)
  C-x C-r       consult-recent-file

SEARCH  (M-s — Emacs search prefix):
  M-s l         consult-line         search current buffer
  M-s L         consult-line-multi   search all buffers
  M-s r         consult-ripgrep      project-wide
  M-s g         consult-grep
  M-s G         consult-git-grep
  M-s f         consult-find

GOTO  (M-g — Emacs goto prefix):
  M-g i         consult-imenu        jump to symbol
  M-g I         consult-imenu-multi
  M-g o         consult-outline      jump to heading
  M-g j         dumb-jump-go         (tools-lsp.el)

NAVIGATION  (editing-core.el):
  C-:           avy-goto-char
  C-'           avy-goto-char-2
  M-g f         avy-goto-line
  M-g w         avy-goto-word-1
  C-c j c/l/w/j avy sub-commands

WINDOWS:
  M-o           ace-window           fast jump/swap
  C-c left      winner-undo          (winner-mode default)
  C-c right     winner-redo

VERSION CONTROL:
  C-x g         magit-status
  C-x M-g       magit-dispatch
  C-x v t       git-timemachine

HELP  (same C-h keys, better output via helpful):
  C-h f/v/k/F/C helpful-*
  C-c C-d       helpful-at-point     (no vanilla equivalent)

ORG  (Org manual recommendations):
  C-c a         org-agenda
  C-c c         org-capture
  C-c l         org-store-link (global); lsp prefix in LSP buffers

COMPILE  (no vanilla default):
  C-c C-c       compile
  C-c C-r       recompile

DEBUG  (debug-core.el — F-keys are unambiguous IDE territory):
  F5            dap-debug
  F6            dap-debug-restart
  F7            dap-step-in
  S-F7          dap-next (step over)
  M-F7          dap-step-out
  C-F7          dap-continue
  F9            dap-breakpoint-toggle
  C-F9          dap-breakpoint-condition
  S-F9          dap-breakpoint-log-message
  C-S-F9        dap-breakpoint-delete-all
  C-c d h       debug hydra  (keys inside: n s o c b B L D i u d l e U w R q)
  C-c d ?       debug help

LSP  (tools-lsp.el — active only in LSP buffers):
  C-c l r       lsp-rename
  C-c l f       lsp-format-buffer
  C-c l F       lsp-format-region
  C-c l a       lsp-execute-code-action
  C-c l R       lsp-find-references
  C-c l i       lsp-find-implementation
  C-c l t       lsp-find-type-definition
  C-c l d       lsp-describe-thing-at-point
  C-c l o       lsp-organize-imports
  C-c l u       lsp-ui-doc-toggle
  M-.           lsp-ui-peek-find-definitions  (overrides xref locally)
  M-?           lsp-ui-peek-find-references

PROJECT  (tools-project.el / projectile):
  C-c p f       projectile-find-file
  C-c p p       projectile-switch-project
  C-c p s r     projectile-ripgrep
  C-c p c       projectile-compile-project
  C-c p t       projectile-test-project
  C-c p r       projectile-run-project
  C-c p k       projectile-kill-buffers
  C-c p d       projectile-dired

EDITING  (editing-core.el):
  C->           mc/mark-next-like-this
  C-<           mc/mark-previous-like-this
  C-=           er/expand-region
  C--           er/contract-region
  M-<up/down>   move-text-up/down
  C-/           undo-tree-undo       (replaces vanilla undo)
  C-?           undo-tree-redo
  C-x u         undo-tree-visualize

COMPLETION  (completion-core.el):
  C-.           embark-act
  C-;           embark-dwim
  M-/           hippie-expand

SPELLING  (tools-spelling.el):
  C-c S s       ispell-word
  C-c S b       flyspell-buffer
  C-c S n       flyspell-goto-next-error
  C-c S t       toggle flyspell
  C-c S c       flyspell-correct-wrapper

TERMINAL  (tools-terminal.el):
  C-c t         vterm (opens in current dir)
  C-c T         vterm-other-window
  C-c M-t       multi-vterm

UTILITY:
  C-c ?         which-key-show-top-level
  C-c H         this help
  C-c R         reload config
  C-c L         LSP status
  C-c P         presentation mode toggle
  F8            neotree-toggle
  F9            treemacs (set by tools-project.el)
  F12           toggle theme

Press q to close.\n")))

(provide 'keybindings)
;;; keybindings.el ends here
