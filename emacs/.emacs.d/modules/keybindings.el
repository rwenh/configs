;;; keybindings.el --- Vanilla-first IDE Keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Philosophy: Emacs defaults are good. We only bind what Emacs has no key for.
;;; No overrides of core defaults. No invented collisions to patch.
;;;
;;; What this file does:
;;;   1. Upgrades a small number of built-ins to smarter equivalents
;;;      (ibuffer, consult variants) — same keys, better commands.
;;;   2. Adds global keys for features that simply don't exist in vanilla Emacs
;;;      (ace-window, magit, helpful, org globals).
;;;   3. Leaves everything else alone — C-x C-s saves, M-; comments,
;;;      C-h f describes, C-/ undoes, exactly as Emacs intended.
;;;
;;; What this file does NOT do:
;;;   - Override C-c as a prefix-command (breaks all major-mode C-c bindings)
;;;   - Re-bind things Emacs already has a key for just to have a "custom" one
;;;   - Bind commands that major modes also use locally (local bindings win anyway)
;;;
;;; ON DUPLICATE BINDINGS WITH MODULE :bind BLOCKS:
;;;   Several consult commands are also bound inside their use-package :bind
;;;   blocks in completion-core.el (C-x b, M-y, M-g g, M-s l/r, etc.).
;;;   These global-set-key calls here are intentional: they serve as the
;;;   canonical, always-active fallback that works even if completion-core.el
;;;   fails to load. Since both sets map to the same commands, the last writer
;;;   (this file, loaded last) wins — behaviour is identical either way.
;;;   This is NOT a conflict; it is belt-and-suspenders insurance.
;;;
;;; Module binding ownership (do not add conflicting global bindings):
;;;   debug-core.el      → F5-F9, C-c d h, C-c d ?, C-c D s/r/q
;;;   editing-core.el    → C-/ C-? C-x u (undo-tree), C-> C-< (mc),
;;;                        C-= (expand-region), C-: C-' M-g f/w (avy),
;;;                        M-up/down (move-text), smartparens C-M-* map
;;;   tools-lsp.el       → C-c l * (lsp-mode-map), M-g j/b/O (dumb-jump),
;;;                        C-c ! * (flycheck-mode-map)
;;;   tools-project.el   → C-c p * (projectile-mode-map), F9 (treemacs)
;;;   tools-terminal.el  → C-c t/T/M-t/e (vterm, multi-vterm, eshell)
;;;   tools-spelling.el  → C-c S * (flyspell-mode-map)
;;;   completion-core.el → C-. C-; (embark), M-/ (hippie-expand)
;;;                        Also binds consult commands via :bind — see note above
;;;   ui-core.el         → C-c w t/f/r (transpose-frame), C-c n (neotree)
;;;
;;; Version: 3.0.2
;;; Changes from 3.0.1:
;;;   - C-c C-c / C-c C-r compile bindings replaced with C-c B / C-c b.
;;;     C-c C-c is owned by virtually every major mode; the global binding
;;;     was dead in 90% of buffers and surprising when it fired elsewhere.
;;; Code:

;; ============================================================================
;; BUILT-IN UPGRADES
;; Same keys the user already knows — just better commands underneath.
;; ============================================================================

;; ibuffer is strictly better than the default buffer-list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; consult variants: same muscle memory, adds preview + fuzzy matching
;; Note: also bound via :bind in completion-core.el — intentional, see commentary.
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
;; M-o is unbound in vanilla Emacs 29 (facemenu was removed).
;; ace-window is configured in ui-core.el; bound globally here.
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
(global-set-key (kbd "C-x v t") 'git-timemachine) ; fits the C-x v vc prefix

;; ============================================================================
;; HELP — HELPFUL
;; Replaces C-h sub-keys with helpful equivalents — identical interface,
;; richer output. C-c C-d is new (vanilla has no at-point help key).
;; NOTE: tools-lsp.el also binds these inside its use-package block so they
;; work when LSP is active. Defining them here ensures they work regardless
;; of whether tools-lsp.el loaded successfully.
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
;; C-c C-c is reserved by virtually every major mode (python, org, cc, etc.).
;; Binding compile globally there makes it dead in 90% of buffers and fires
;; unexpectedly in the rest. Use C-c B (Build) / C-c b (reBuild) instead —
;; these are unoccupied at the global level.
;; ============================================================================

(global-set-key (kbd "C-c B") 'compile)
(global-set-key (kbd "C-c b") 'recompile)

;; ============================================================================
;; UTILITY
;; ============================================================================

;; Discoverability
(global-set-key (kbd "C-c ?") 'which-key-show-top-level)
(global-set-key (kbd "C-c H") 'emacs-ide-show-keybindings-help)

;; Config management
(global-set-key (kbd "C-c R") 'emacs-ide-reload-config)
(global-set-key (kbd "C-c L") 'emacs-ide-lsp-status)
;; C-c t is vterm (tools-terminal.el) — test sub-keys use C-c T prefix
(global-set-key (kbd "C-c C-t") 'emacs-ide-test-run)

;; UI toggles (functions defined in ui-core.el and ui-theme.el)
;; FIX: C-<f8> removed from neotree — it collided with dap-breakpoint-condition
;; (debug-core.el F8 family: F8=toggle, C-F8=condition, S-F8=log, C-S-F8=del-all).
;; keybindings.el was re-assigning C-<f8> to neotree-toggle last, silently
;; killing dap-breakpoint-condition on every startup.
;; Neotree moves to C-c n (unoccupied). C-<f8> is now left to debug-core.el.
(global-set-key (kbd "C-c n") 'neotree-toggle)
;; F9 treemacs is bound in tools-project.el via :bind — no duplicate needed here
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

COMPILE  (C-c C-c is major-mode territory — use these instead):
  C-c B         compile
  C-c b         recompile
  C-c C-t       emacs-ide-test-run (full suite)
  C-c x p/l/r/h test at point / last / report / hydra

DEBUG  (debug-core.el — F-keys are unambiguous IDE territory):
  F5            dap-debug
  F6            dap-debug-restart
  F7            dap-step-in
  S-F7          dap-next (step over)
  M-F7          dap-step-out
  C-F7          dap-continue
  F8            dap-breakpoint-toggle
  C-F8          dap-breakpoint-condition
  S-F8          dap-breakpoint-log-message
  C-S-F8        dap-breakpoint-delete-all
  C-c d h       debug hydra  (keys inside: n s o c b B L D u d l e U w R q)
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
  C-c p F       treemacs-find-file        (find current file in tree)
  C-c p W       treemacs-select-window    (focus treemacs window)
  C-c p r       projectile-run-project
  C-c p k       projectile-kill-buffers
  C-c p d       projectile-dired
  F9            treemacs (set by tools-project.el)

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
  C-c n         neotree-toggle
  F9            treemacs (set by tools-project.el)
  F12           toggle theme

Press q to close.\n")))

(provide 'keybindings)
;;; keybindings.el ends here
