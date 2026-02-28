;;; keybindings.el --- Professional Keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Authoritative keybinding file — all other modules defer here.
;;; Version: 2.2.1
;;; Fixes:
;;;   - C-c j collision: was bound to windmove-down AND used as prefix for
;;;     avy sub-keys (C-c j c/l/w/j). windmove-down on C-c j consumed the
;;;     prefix making avy sub-keys unreachable. Windmove moved to C-c w-h/j/k/l
;;;     (w prefix = window). Avy C-c j prefix retained.
;;;   - C-c b collision: was bound to consult-project-buffer AND used as prefix
;;;     for bookmark sub-keys (C-c b s/j/l). consult-project-buffer moved to
;;;     C-c B (capital). Bookmark C-c b prefix retained.
;;;   - M-g o collision: consult-outline AND dumb-jump-go-other-window both
;;;     used M-g o. dumb-jump-go-other-window moved to M-g O.
;;;   - C-c C-d / C-c D d: preserved as fixed in previous audit.
;;;   - Removed bindings for undefined functions emacs-ide-upcase/downcase/capitalize.
;;; Code:

;; ============================================================================
;; FILE OPERATIONS
;; ============================================================================
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k")   'emacs-ide-kill-current-buffer)
(global-set-key (kbd "C-c w")   'save-buffer)
(global-set-key (kbd "C-c q")   'kill-buffer-and-window)
(global-set-key (kbd "C-c K")   'emacs-ide-kill-other-buffers)
(global-set-key (kbd "C-c f")   'consult-recent-file)
(global-set-key (kbd "C-c y p") 'emacs-ide-copy-file-path)
(global-set-key (kbd "C-c y n") 'emacs-ide-copy-file-name)
(global-set-key (kbd "C-c C-r") 'emacs-ide-rename-current-file)
;; delete-file stays at C-c D d (capital D) — avoids collision with helpful-at-point
(global-set-key (kbd "C-c D d") 'emacs-ide-delete-current-file)

;; ============================================================================
;; BUFFER NAVIGATION
;; ============================================================================
(global-set-key (kbd "C-x b")   'consult-buffer)
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)
;; FIX: moved from C-c b (collided with bookmark prefix) → C-c B
(global-set-key (kbd "C-c B")   'consult-project-buffer)

;; ============================================================================
;; LINE MANIPULATION
;; ============================================================================
(global-set-key (kbd "M-<up>")   'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "C-c C-u")  'emacs-ide-duplicate-line)
(global-set-key (kbd "C-c M-u")  'emacs-ide-duplicate-region)
(global-set-key (kbd "C-a")      'emacs-ide-smart-beginning-of-line)
(global-set-key (kbd "M-j")      'emacs-ide-join-lines)
(global-set-key (kbd "C-o")      'emacs-ide-split-line)

;; ============================================================================
;; COMMENTING
;; ============================================================================
(global-set-key (kbd "M-;")   'emacs-ide-comment-or-uncomment)
(global-set-key (kbd "C-c ;") 'comment-dwim)

;; ============================================================================
;; WINDOW MANAGEMENT
;; ============================================================================
(global-set-key (kbd "C-x 2")      'emacs-ide-split-horizontal-and-follow)
(global-set-key (kbd "C-x 3")      'emacs-ide-split-vertical-and-follow)
(global-set-key (kbd "M-o")        'ace-window)
(global-set-key (kbd "C-c <left>")  'winner-undo)
(global-set-key (kbd "C-c <right>") 'winner-redo)
;; FIX: Windmove moved from C-c h/j/k/l to C-c w-h/j/k/l
;;      C-c j was colliding with avy's C-c j prefix (C-c j c/l/w/j)
(global-set-key (kbd "C-c w h") 'windmove-left)
(global-set-key (kbd "C-c w j") 'windmove-down)
(global-set-key (kbd "C-c w k") 'windmove-up)
(global-set-key (kbd "C-c w l") 'windmove-right)

;; ============================================================================
;; AVY NAVIGATION — C-c j prefix (now free from windmove collision)
;; ============================================================================
(global-set-key (kbd "C-:")      'avy-goto-char)
(global-set-key (kbd "C-'")      'avy-goto-char-2)
(global-set-key (kbd "M-g f")    'avy-goto-line)
(global-set-key (kbd "M-g w")    'avy-goto-word-1)
(global-set-key (kbd "C-c j c")  'avy-goto-char)
(global-set-key (kbd "C-c j l")  'avy-goto-line)
(global-set-key (kbd "C-c j w")  'avy-goto-word-1)
(global-set-key (kbd "C-c j j")  'avy-goto-char-timer)

;; ============================================================================
;; DEBUGGING — F5-F9 (set by dap-mode in debug-core.el)
;; ============================================================================
(global-set-key (kbd "S-<f5>") 'projectile-compile-project)
(global-set-key (kbd "S-<f6>") 'projectile-test-project)
(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "C-c c r") 'recompile)

;; ============================================================================
;; THEME — F12
;; ============================================================================
(global-set-key (kbd "<f12>") 'emacs-ide-toggle-theme)

;; ============================================================================
;; UTILITY
;; ============================================================================
(global-set-key (kbd "C-c R") 'emacs-ide-reload-config)
(global-set-key (kbd "C-c L") 'emacs-ide-lsp-status)
(global-set-key (kbd "C-c ?") 'which-key-show-top-level)
(global-set-key (kbd "C-c H") 'emacs-ide-show-keybindings-help)

;; ============================================================================
;; FORMATTING
;; ============================================================================
(global-set-key (kbd "C-c F")   'format-all-region-or-buffer)
(global-set-key (kbd "C-c C-f") 'emacs-ide-indent-buffer)
(global-set-key (kbd "C-c M-f") 'emacs-ide-cleanup-buffer)
(global-set-key (kbd "C-M-\\")  'emacs-ide-indent-region-or-buffer)

;; ============================================================================
;; CODE FOLDING
;; ============================================================================
(global-set-key (kbd "C-c @ h") 'hs-hide-block)
(global-set-key (kbd "C-c @ s") 'hs-show-block)
(global-set-key (kbd "C-c @ t") 'hs-toggle-hiding)
(global-set-key (kbd "C-c @ a") 'hs-hide-all)
(global-set-key (kbd "C-c @ A") 'hs-show-all)

;; ============================================================================
;; SEARCH & NAVIGATION
;; ============================================================================
(global-set-key (kbd "M-s l") 'consult-line)
(global-set-key (kbd "M-s L") 'consult-line-multi)
(global-set-key (kbd "M-s r") 'consult-ripgrep)
(global-set-key (kbd "M-s g") 'consult-grep)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-g I") 'consult-imenu-multi)
;; FIX: M-g o was colliding with dumb-jump-go-other-window (in tools-lsp.el)
;;      consult-outline stays on M-g o; dumb-jump moved to M-g O
(global-set-key (kbd "M-g o") 'consult-outline)

;; ============================================================================
;; LSP NAVIGATION
;; ============================================================================
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)
(global-set-key (kbd "M-?") 'xref-find-references)

;; ============================================================================
;; HELP
;; FIX: C-c C-d → helpful-at-point (more useful daily; delete-file → C-c D d)
;; ============================================================================
(global-set-key (kbd "C-h f")   'helpful-callable)
(global-set-key (kbd "C-h v")   'helpful-variable)
(global-set-key (kbd "C-h k")   'helpful-key)
(global-set-key (kbd "C-h F")   'helpful-function)
(global-set-key (kbd "C-h C")   'helpful-command)
(global-set-key (kbd "C-c C-d") 'helpful-at-point)

;; ============================================================================
;; PROJECT (Projectile)
;; ============================================================================
(global-set-key (kbd "C-c p f")   'projectile-find-file)
(global-set-key (kbd "C-c p p")   'projectile-switch-project)
(global-set-key (kbd "C-c p s r") 'projectile-ripgrep)
(global-set-key (kbd "C-c p s g") 'projectile-grep)
(global-set-key (kbd "C-c p b")   'projectile-switch-to-buffer)
(global-set-key (kbd "C-c p c")   'projectile-compile-project)
(global-set-key (kbd "C-c p t")   'projectile-test-project)
(global-set-key (kbd "C-c p r")   'projectile-run-project)
(global-set-key (kbd "C-c p k")   'projectile-kill-buffers)
(global-set-key (kbd "C-c p d")   'projectile-dired)
(global-set-key (kbd "C-c p e")   'projectile-recentf)
(global-set-key (kbd "C-c p I")   'emacs-ide-project-info)

;; ============================================================================
;; GIT (Magit)
;; ============================================================================
(global-set-key (kbd "C-x g")   'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-x v t") 'git-timemachine)

(define-prefix-command 'emacs-ide-git-map)
(global-set-key (kbd "C-c g") 'emacs-ide-git-map)
(define-key emacs-ide-git-map (kbd "s") 'magit-status)
(define-key emacs-ide-git-map (kbd "b") 'magit-blame)
(define-key emacs-ide-git-map (kbd "l") 'magit-log-buffer-file)
(define-key emacs-ide-git-map (kbd "c") 'magit-clone)
(define-key emacs-ide-git-map (kbd "i") 'magit-init)
(define-key emacs-ide-git-map (kbd "d") 'magit-dispatch)
(define-key emacs-ide-git-map (kbd "f") 'magit-file-dispatch)

;; ============================================================================
;; BOOKMARKS — C-c b prefix (now free from consult-project-buffer collision)
;; ============================================================================
(global-set-key (kbd "C-c b s") 'bookmark-set)
(global-set-key (kbd "C-c b j") 'bookmark-jump)
(global-set-key (kbd "C-c b l") 'bookmark-bmenu-list)

;; ============================================================================
;; TERMINAL
;; ============================================================================
(when (fboundp 'vterm)
  (global-set-key (kbd "C-c t") 'vterm))
(when (fboundp 'vterm-other-window)
  (global-set-key (kbd "C-c T") 'vterm-other-window))
(when (fboundp 'multi-vterm)
  (global-set-key (kbd "C-c M-t") 'multi-vterm))

;; ============================================================================
;; FILE TREE
;; ============================================================================
(when (fboundp 'neotree-toggle)
  (global-set-key (kbd "<f8>") 'neotree-toggle))

;; ============================================================================
;; UNDO/REDO
;; ============================================================================
(with-eval-after-load 'undo-tree
  (global-set-key (kbd "C-_") 'undo-tree-undo)
  (global-set-key (kbd "C-/") 'undo-tree-undo)
  (global-set-key (kbd "C-?") 'undo-tree-redo)
  (global-set-key (kbd "M-_") 'undo-tree-redo)
  (global-set-key (kbd "C-x u") 'undo-tree-visualize))

;; ============================================================================
;; COMPLETION
;; ============================================================================
(global-set-key (kbd "M-y")     'consult-yank-pop)
(global-set-key (kbd "C-x r b") 'consult-bookmark)

;; ============================================================================
;; ERGONOMIC
;; ============================================================================
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-SPC")   'set-mark-command)
(global-set-key (kbd "C-z")     'repeat)

;; ============================================================================
;; SPELLING
;; ============================================================================
(global-set-key (kbd "C-c S s") 'ispell-word)
(global-set-key (kbd "C-c S b") 'flyspell-buffer)
(global-set-key (kbd "C-c S n") 'flyspell-goto-next-error)
(global-set-key (kbd "C-c S t") 'emacs-ide-spell-toggle)

;; ============================================================================
;; ORG MODE
;; ============================================================================
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o t") 'org-todo)

;; ============================================================================
;; WAYLAND CLIPBOARD
;; ============================================================================
(when (and (getenv "WAYLAND_DISPLAY")
           (executable-find "wl-copy"))
  (defun emacs-ide-wayland-copy (start end)
    "Copy region to Wayland clipboard."
    (interactive "r")
    (let ((text (buffer-substring-no-properties start end)))
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max)
                             "wl-copy" nil nil nil)))
    (deactivate-mark)
    (message "✓ Copied to Wayland clipboard"))
  (global-set-key (kbd "C-c C-w") 'emacs-ide-wayland-copy))

(when (and (getenv "WAYLAND_DISPLAY")
           (executable-find "wl-paste"))
  (defun emacs-ide-wayland-paste ()
    "Paste from Wayland clipboard."
    (interactive)
    (let ((text (shell-command-to-string "wl-paste -n 2>/dev/null")))
      (unless (string= text "")
        (insert text))))
  (global-set-key (kbd "C-c C-y") 'emacs-ide-wayland-paste))

;; ============================================================================
;; KEYBINDINGS HELP
;; ============================================================================
(defun emacs-ide-show-keybindings-help ()
  "Display keybindings cheat sheet."
  (interactive)
  (with-output-to-temp-buffer "*Keybindings*"
    (princ "=== EMACS IDE KEYBINDINGS ===

FILE & BUFFER:
  C-c w         Save buffer
  C-c q         Kill buffer and window
  C-c K         Kill other buffers
  C-c f         Recent files (consult)
  C-x b         Switch buffer (consult)
  C-c B         Project buffers
  C-c y p       Copy file path
  C-c y n       Copy file name
  C-c C-r       Rename file
  C-c D d       Delete file

EDITING:
  M-<up/dn>     Move line up/down
  C-c C-u       Duplicate line
  C-a           Smart home (indent → BOL)
  M-;           Comment/uncomment
  C-=           Expand region
  C-< / C->     Multiple cursor prev/next

NAVIGATION (Avy):
  C-:           Jump to char
  C-'           Jump to 2-char
  M-g f         Jump to line
  M-g w         Jump to word
  C-c j c/l/w/j Avy sub-commands

SEARCH:
  M-s l         consult-line
  M-s r         consult-ripgrep
  M-g i         consult-imenu
  M-g o         consult-outline
  M-.           xref definition
  M-,           xref pop
  M-?           xref references

WINDOWS:
  M-o           ace-window
  C-x 2/3       Split + follow
  C-c w h/j/k/l Windmove (window prefix)
  C-c </>       Winner undo/redo

DEBUG:
  F5            Start debug (dap)
  F7/S-F7       Step in/over
  M-F7          Step out
  C-F7          Continue
  F9            Toggle breakpoint
  C-c d h       Debug hydra

PROJECT (C-c p):
  C-c p f       Find file
  C-c p p       Switch project
  C-c p s r     Ripgrep in project
  C-c p c       Compile
  C-c p t       Test

GIT (C-c g):
  C-x g         Magit status
  C-c g s       Status
  C-c g b       Blame
  C-c g l       Log file
  C-x v t       Time machine

LSP (C-c l):
  C-c l r       Rename symbol
  C-c l f       Format buffer
  C-c l a       Code action
  C-c l R       Find references

HELP:
  C-h f         helpful-callable
  C-h v         helpful-variable
  C-h k         helpful-key
  C-c C-d       helpful-at-point

BOOKMARKS (C-c b):
  C-c b s       Set bookmark
  C-c b j       Jump to bookmark
  C-c b l       List bookmarks

SPELLING:
  C-c S s       Spell word
  C-c S b       Spell buffer
  C-c S n       Next error
  C-c S t       Toggle flyspell

ORG:
  C-c o a       Agenda
  C-c o c       Capture
  C-c o l       Store link

UTILITY:
  C-c ?         which-key
  C-c H         This help
  C-c R         Reload config
  F12           Toggle theme
  F8            File tree (neotree)
  C-c t         vterm
  C-c P         Presentation mode

Press q to close.
")))

(provide 'keybindings)
;;; keybindings.el ends here
