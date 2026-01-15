;;; keybindings.el --- Professional Keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Ergonomic, efficient, professional keybindings
;;; Code:

;; ============================================================================
;; FILE OPERATIONS
;; ============================================================================
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'emacs-ide-kill-current-buffer)
(global-set-key (kbd "C-c w") 'save-buffer)
(global-set-key (kbd "C-c q") 'kill-buffer-and-window)
(global-set-key (kbd "C-c K") 'emacs-ide-kill-other-buffers)
(global-set-key (kbd "C-c f") 'consult-recent-file)
(global-set-key (kbd "C-c y p") 'emacs-ide-copy-file-path)
(global-set-key (kbd "C-c y n") 'emacs-ide-copy-file-name)
(global-set-key (kbd "C-c C-r") 'emacs-ide-rename-current-file)
(global-set-key (kbd "C-c C-d") 'emacs-ide-delete-current-file)

;; ============================================================================
;; BUFFER NAVIGATION
;; ============================================================================
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)
(global-set-key (kbd "C-c b") 'consult-project-buffer)

;; ============================================================================
;; LINE MANIPULATION
;; ============================================================================
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "C-c C-u") 'emacs-ide-duplicate-line)
(global-set-key (kbd "C-c M-u") 'emacs-ide-duplicate-region)
(global-set-key (kbd "C-a") 'emacs-ide-smart-beginning-of-line)
(global-set-key (kbd "M-j") 'emacs-ide-join-lines)
(global-set-key (kbd "C-o") 'emacs-ide-split-line)

;; ============================================================================
;; COMMENTING
;; ============================================================================
(global-set-key (kbd "M-;") 'emacs-ide-comment-or-uncomment)
(global-set-key (kbd "C-c ;") 'comment-dwim)

;; ============================================================================
;; WINDOW MANAGEMENT
;; ============================================================================
(global-set-key (kbd "C-x 2") 'emacs-ide-split-horizontal-and-follow)
(global-set-key (kbd "C-x 3") 'emacs-ide-split-vertical-and-follow)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-c <left>") 'winner-undo)
(global-set-key (kbd "C-c <right>") 'winner-redo)

;; Windmove (vim-style)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

;; ============================================================================
;; COMPILATION & BUILDING
;; ============================================================================
(global-set-key (kbd "<f5>") 'dap-debug)
(global-set-key (kbd "<f6>") 'dap-debug-restart)
(global-set-key (kbd "S-<f5>") 'projectile-compile-project)
(global-set-key (kbd "S-<f6>") 'projectile-test-project)
(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "C-c c r") 'recompile)

;; ============================================================================
;; DEBUGGING
;; ============================================================================
(global-set-key (kbd "<f7>") 'dap-step-in)
(global-set-key (kbd "S-<f7>") 'dap-next)
(global-set-key (kbd "M-<f7>") 'dap-step-out)
(global-set-key (kbd "C-<f7>") 'dap-continue)
(global-set-key (kbd "<f9>") 'dap-breakpoint-toggle)
(global-set-key (kbd "C-<f9>") 'dap-breakpoint-condition)
(global-set-key (kbd "C-S-<f9>") 'dap-breakpoint-delete-all)

;; ============================================================================
;; THEME
;; ============================================================================
(global-set-key (kbd "<f12>") 'emacs-ide-toggle-theme)

;; ============================================================================
;; UTILITY
;; ============================================================================
(global-set-key (kbd "C-c R") 'emacs-ide-reload-config)
(global-set-key (kbd "C-c L") 'emacs-ide-lsp-status)
(global-set-key (kbd "C-c ?") 'which-key-show-top-level)
(global-set-key (kbd "C-c H") 'emacs-ide-show-keybindings-help)
(global-set-key (kbd "C-c S") 'emacs-ide-show-startup-time)
(global-set-key (kbd "C-c I") 'emacs-ide-show-system-info)

;; ============================================================================
;; FORMATTING
;; ============================================================================
(global-set-key (kbd "C-c f") 'format-all-buffer)
(global-set-key (kbd "C-c F") 'format-all-region-or-buffer)
(global-set-key (kbd "C-c C-f") 'emacs-ide-indent-buffer)
(global-set-key (kbd "C-c M-f") 'emacs-ide-cleanup-buffer)
(global-set-key (kbd "C-M-\\") 'emacs-ide-indent-region-or-buffer)

;; ============================================================================
;; CODE FOLDING
;; ============================================================================
(global-set-key (kbd "C-c @ h") 'hs-hide-block)
(global-set-key (kbd "C-c @ s") 'hs-show-block)
(global-set-key (kbd "C-c @ t") 'hs-toggle-hiding)
(global-set-key (kbd "C-c @ a") 'hs-hide-all)
(global-set-key (kbd "C-c @ A") 'hs-show-all)

;; ============================================================================
;; TEXT TRANSFORMATION
;; ============================================================================
(global-set-key (kbd "M-u") 'emacs-ide-upcase-region-or-word)
(global-set-key (kbd "M-l") 'emacs-ide-downcase-region-or-word)
(global-set-key (kbd "M-c") 'emacs-ide-capitalize-region-or-word)

;; ============================================================================
;; SEARCH & NAVIGATION
;; ============================================================================
(global-set-key (kbd "M-s l") 'consult-line)
(global-set-key (kbd "M-s L") 'consult-line-multi)
(global-set-key (kbd "M-s r") 'consult-ripgrep)
(global-set-key (kbd "M-s g") 'consult-grep)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-g I") 'consult-imenu-multi)
(global-set-key (kbd "M-g o") 'consult-outline)

;; Jump navigation
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

;; ============================================================================
;; LSP
;; ============================================================================
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)
(global-set-key (kbd "M-?") 'xref-find-references)

;; ============================================================================
;; PROJECT (Projectile)
;; ============================================================================
(global-set-key (kbd "C-c p") projectile-command-map)

;; ============================================================================
;; GIT (Magit)
;; ============================================================================
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-blame)
(global-set-key (kbd "C-c g l") 'magit-log-buffer-file)
(global-set-key (kbd "C-x v t") 'git-timemachine)

;; ============================================================================
;; HELP
;; ============================================================================
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h F") 'helpful-function)
(global-set-key (kbd "C-h C") 'helpful-command)
(global-set-key (kbd "C-c C-d") 'helpful-at-point)

;; ============================================================================
;; BOOKMARKS
;; ============================================================================
(global-set-key (kbd "C-c b s") 'bookmark-set)
(global-set-key (kbd "C-c b j") 'bookmark-jump)
(global-set-key (kbd "C-c b l") 'bookmark-bmenu-list)

;; ============================================================================
;; TERMINAL
;; ============================================================================
(global-set-key (kbd "C-c t") 'vterm)
(global-set-key (kbd "C-c T") 'vterm-other-window)
(global-set-key (kbd "C-c M-t") 'multi-vterm)

;; ============================================================================
;; FILE TREE
;; ============================================================================
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "C-c n") 'neotree-projectile-action)

;; ============================================================================
;; COMPLETION FRAMEWORK
;; ============================================================================
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "C-x r b") 'consult-bookmark)

;; ============================================================================
;; ERGONOMIC IMPROVEMENTS
;; ============================================================================
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-z") 'repeat)

;; ============================================================================
;; WAYLAND-SPECIFIC
;; ============================================================================
(when (getenv "WAYLAND_DISPLAY")
  (when (executable-find "wl-copy")
    (defun emacs-ide-wayland-copy (start end)
      "Copy to Wayland clipboard."
      (interactive "r")
      (let ((text (buffer-substring-no-properties start end)))
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max) "wl-copy" nil nil nil)))
      (deactivate-mark)
      (message "Copied to Wayland clipboard"))
    (global-set-key (kbd "C-c C-w") 'emacs-ide-wayland-copy))
  
  (when (executable-find "wl-paste")
    (defun emacs-ide-wayland-paste ()
      "Paste from Wayland clipboard."
      (interactive)
      (let ((text (shell-command-to-string "wl-paste -n 2>/dev/null")))
        (unless (string= text "")
          (insert text))))
    (global-set-key (kbd "C-c C-y") 'emacs-ide-wayland-paste))
  
  (when (and (executable-find "grim") (executable-find "slurp"))
    (defun emacs-ide-sway-screenshot ()
      "Take Sway screenshot."
      (interactive)
      (let ((filename (format "~/Pictures/screenshot-%s.png"
                             (format-time-string "%Y%m%d-%H%M%S"))))
        (call-process-shell-command
         (format "grim -g \"$(slurp)\" %s" filename))
        (message "Screenshot: %s" filename)))
    (global-set-key (kbd "C-c s") 'emacs-ide-sway-screenshot)))

;; ============================================================================
;; KEYBINDINGS HELP
;; ============================================================================
(defun emacs-ide-show-keybindings-help ()
  "Display keybindings cheat sheet."
  (interactive)
  (with-output-to-temp-buffer "*Keybindings*"
    (princ "=== EMACS IDE PROFESSIONAL KEYBINDINGS ===

FILE & BUFFER:
  C-c w       Save buffer
  C-c q       Kill buffer and window
  C-c K       Kill other buffers
  C-c f       Recent files (consult)
  C-x b       Switch buffer (consult)
  C-c b       Project buffers
  C-c y p     Copy file path
  C-c y n     Copy file name

EDITING:
  M-<up/down> Move line
  C-c C-u     Duplicate line
  C-a         Smart home
  M-;         Comment/uncomment
  C-=         Expand region
  C-->        Contract region
  C->         Mark next (multiple cursors)
  C-<         Mark previous

NAVIGATION:
  C-:         Jump to char (avy)
  M-g f       Jump to line
  M-s l       Search line (consult)
  M-s r       Ripgrep
  M-g i       Imenu
  M-.         Go to definition
  M-,         Pop back

WINDOWS:
  C-x 2/3     Split + follow
  M-o         Ace window
  C-c h/j/k/l Windmove (vim-style)
  C-c </→     Winner undo/redo

DEBUGGING:
  F5          Start debug
  F6          Restart
  F7          Step in
  S-F7        Step over
  M-F7        Step out
  C-F7        Continue
  F9          Toggle breakpoint
  C-c d h     Debug hydra

PROJECT:
  C-c p f     Find file
  C-c p p     Switch project
  C-c p s r   Ripgrep
  C-c p c     Compile
  C-c p t     Test

GIT:
  C-x g       Magit status
  C-c g b     Blame
  C-c g l     Log
  C-x v t     Time machine

LSP:
  C-c l r     Rename
  C-c l f     Format
  C-c l a     Code actions
  C-c l g     Go to definition
  C-c l R     Find references

UTILITY:
  C-c ?       Which-key
  C-c H       This help
  C-c L       LSP status
  C-c R       Reload config
  F12         Toggle theme
  C-c t       Terminal (vterm)
  F8          File tree

Press q to close.
")))

(provide 'keybindings)
;;; keybindings.el ends here