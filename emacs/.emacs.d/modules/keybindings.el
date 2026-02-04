;;; keybindings.el --- Professional Keybindings (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; Ergonomic, efficient, professional keybindings - CENTRALIZED
;;; IMPORTANT: This is the authoritative keybinding file. All other modules should defer to this.
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
;; DEBUGGING - F5-F9 RESERVED FOR DEBUG
;; ============================================================================
;; Note: These are set by dap-mode in debug-core.el
;; <f5>   = dap-debug
;; <f6>   = dap-debug-restart
;; <f7>   = dap-step-in
;; S-<f7> = dap-next
;; M-<f7> = dap-step-out
;; C-<f7> = dap-continue
;; <f9>   = dap-breakpoint-toggle
;; C-<f9> = dap-breakpoint-condition
;; S-<f9> = dap-breakpoint-log-message
;; C-S-<f9> = dap-breakpoint-delete-all

(global-set-key (kbd "S-<f5>") 'projectile-compile-project)
(global-set-key (kbd "S-<f6>") 'projectile-test-project)

;; Compilation
(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "C-c c r") 'recompile)

;; ============================================================================
;; THEME - F12 RESERVED FOR THEME TOGGLE
;; ============================================================================
;; Note: Theme toggle is set by ui-core.el
;; <f12> = emacs-ide-toggle-theme

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

;; Jump navigation (Avy)
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
;; PROJECT (Projectile) - Prefix C-c p
;; ============================================================================
(global-set-key (kbd "C-c p f") 'projectile-find-file)
(global-set-key (kbd "C-c p p") 'projectile-switch-project)
(global-set-key (kbd "C-c p s r") 'projectile-ripgrep)
(global-set-key (kbd "C-c p s g") 'projectile-grep)
(global-set-key (kbd "C-c p b") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-c p c") 'projectile-compile-project)
(global-set-key (kbd "C-c p t") 'projectile-test-project)
(global-set-key (kbd "C-c p r") 'projectile-run-project)
(global-set-key (kbd "C-c p k") 'projectile-kill-buffers)
(global-set-key (kbd "C-c p d") 'projectile-dired)
(global-set-key (kbd "C-c p e") 'projectile-recentf)

;; ============================================================================
;; GIT (Magit) - Prefix C-x g or C-c g
;; ============================================================================
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

(define-prefix-command 'emacs-ide-git-map)
(global-set-key (kbd "C-c g") 'emacs-ide-git-map)
(define-key emacs-ide-git-map (kbd "s") 'magit-status)
(define-key emacs-ide-git-map (kbd "b") 'magit-blame)
(define-key emacs-ide-git-map (kbd "l") 'magit-log-buffer-file)
(define-key emacs-ide-git-map (kbd "c") 'magit-clone)
(define-key emacs-ide-git-map (kbd "i") 'magit-init)
(define-key emacs-ide-git-map (kbd "d") 'magit-dispatch)
(define-key emacs-ide-git-map (kbd "f") 'magit-file-dispatch)

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
(when (and (getenv "WAYLAND_DISPLAY")
           (executable-find "wl-copy"))
  (defun emacs-ide-wayland-copy (start end)
    "Copy to Wayland clipboard."
    (interactive "r")
    (let ((text (buffer-substring-no-properties start end)))
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "wl-copy" nil nil nil)))
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
    (princ "=== EMACS IDE PROFESSIONAL KEYBINDINGS ===

FILE & BUFFER:
  C-c w       Save buffer
  C-c q       Kill buffer and window
  C-c K       Kill other buffers
  C-c f       Recent files
  C-x b       Switch buffer
  C-c b       Project buffers
  C-c y p     Copy file path
  C-c y n     Copy file name

EDITING:
  M-<up/dn>   Move line
  C-c C-u     Duplicate line
  C-a         Smart home
  M-;         Comment/uncomment
  M-u/l/c     Up/downcase word or region
  C-=         Expand region
  C-</>       Mark previous/next

NAVIGATION:
  C-: or C-'  Jump to char (avy)
  M-g f       Jump to line
  M-s l       Search line
  M-s r       Ripgrep
  M-g i       Imenu
  M-.         Go to definition
  M-,         Pop back

WINDOWS:
  C-x 2/3     Split + follow
  M-o         Ace window (jump)
  C-c h/j/k/l Windmove (vim-style)
  C-c </>     Winner undo/redo

DEBUGGING:
  F5          Start debug
  F6          Restart
  F7/S-F7     Step in/over
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
  C-c g s     Git status
  C-c g b     Blame
  C-c g l     Log
  C-x v t     Time machine

LSP:
  M-.         Go to definition
  M-?         Find references
  C-h f       Helpful (docs)

UTILITY:
  C-c ?       Which-key
  C-c H       This help
  C-c R       Reload config
  F12         Toggle theme
  C-c t       Terminal (vterm)
  F8          File tree

Press q to close.
")))

(provide 'keybindings)
;;; keybindings.el ends here