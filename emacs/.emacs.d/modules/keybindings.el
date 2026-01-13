;;; keybindings.el --- Enhanced Global Keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Centralized keybinding configuration for ergonomic editing and IDE features
;;; Save as: ~/.emacs.d/modules/keybindings.el
;;;
;;; Code:

;; ============================================================================
;; FILE OPERATIONS
;; ============================================================================
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'emacs-ide-kill-current-buffer)
(global-set-key (kbd "C-c w") 'save-buffer)
(global-set-key (kbd "C-c q") 'kill-buffer-and-window)
(global-set-key (kbd "C-c K") 'emacs-ide-kill-other-buffers)
(global-set-key (kbd "C-c r") 'emacs-ide-recentf-ido-find-file)
(global-set-key (kbd "C-c y p") 'emacs-ide-copy-file-path)
(global-set-key (kbd "C-c y n") 'emacs-ide-copy-file-name)

;; File management
(global-set-key (kbd "C-c C-r") 'emacs-ide-rename-current-file)
(global-set-key (kbd "C-c C-d") 'emacs-ide-delete-current-file)

;; ============================================================================
;; LINE MANIPULATION
;; ============================================================================
(global-set-key (kbd "M-<up>") 'emacs-ide-move-line-up)
(global-set-key (kbd "M-<down>") 'emacs-ide-move-line-down)
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
;; WINDOW MANAGEMENT - ENHANCED
;; ============================================================================
;; Splitting with follow
(global-set-key (kbd "C-x 2") 'emacs-ide-split-horizontal-and-follow)
(global-set-key (kbd "C-x 3") 'emacs-ide-split-vertical-and-follow)
(global-set-key (kbd "C-x |") 'emacs-ide-rotate-windows)

;; Window navigation (vim-style with C-c prefix)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

;; Winner mode (undo/redo window changes)
(global-set-key (kbd "C-c <left>") 'winner-undo)
(global-set-key (kbd "C-c <right>") 'winner-redo)

;; Ace window
(global-set-key (kbd "M-o") 'ace-window)

;; ============================================================================
;; COMPILATION & BUILDING
;; ============================================================================
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'recompile)
(global-set-key (kbd "S-<f5>") 'projectile-compile-project)
(global-set-key (kbd "S-<f6>") 'projectile-test-project)

;; ============================================================================
;; DEBUGGING
;; ============================================================================
(global-set-key (kbd "<f7>") 'dap-step-in)
(global-set-key (kbd "S-<f7>") 'dap-next)
(global-set-key (kbd "M-<f7>") 'dap-step-out)
(global-set-key (kbd "C-<f7>") 'dap-continue)
(global-set-key (kbd "C-<f5>") 'dap-breakpoint-toggle)
(global-set-key (kbd "C-S-<f5>") 'dap-breakpoint-delete-all)
(global-set-key (kbd "C-c C-v d") 'dap-debug)
(global-set-key (kbd "C-c C-v l") 'dap-ui-locals)
(global-set-key (kbd "C-c C-v r") 'dap-ui-repl)

;; ============================================================================
;; THEME
;; ============================================================================
(global-set-key (kbd "<f12>") 'emacs-ide-toggle-theme)

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================
(global-set-key (kbd "C-c R") 'emacs-ide-reload-config)
(global-set-key (kbd "C-c L") 'emacs-ide-lsp-status)
(global-set-key (kbd "C-c ?") 'which-key-show-top-level)
(global-set-key (kbd "C-c H") 'emacs-ide-show-common-keybindings)
(global-set-key (kbd "C-c M-d") 'emacs-ide-insert-current-date)
(global-set-key (kbd "C-c M-t") 'emacs-ide-insert-current-time)
(global-set-key (kbd "C-c S") 'emacs-ide-show-startup-time)
(global-set-key (kbd "C-c I") 'emacs-ide-show-system-info)

;; ============================================================================
;; FORMATTING & INDENTATION
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
(global-set-key (kbd "C-c @ l") 'hs-hide-level)

;; ============================================================================
;; HUNGRY DELETE
;; ============================================================================
(global-set-key (kbd "M-DEL") 'emacs-ide-hungry-delete-backward)
(global-set-key (kbd "M-d") 'emacs-ide-hungry-delete-forward)

;; ============================================================================
;; TEXT TRANSFORMATION
;; ============================================================================
(global-set-key (kbd "M-u") 'emacs-ide-upcase-region-or-word)
(global-set-key (kbd "M-l") 'emacs-ide-downcase-region-or-word)
(global-set-key (kbd "M-c") 'emacs-ide-capitalize-region-or-word)
(global-set-key (kbd "C-c s s") 'emacs-ide-sort-lines-region)
(global-set-key (kbd "C-c s r") 'emacs-ide-reverse-region)

;; ============================================================================
;; PROJECT MANAGEMENT (Projectile)
;; ============================================================================
(global-set-key (kbd "C-c P") 'projectile-command-map)

;; ============================================================================
;; GIT (Magit)
;; ============================================================================
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-c C-g") 'magit-file-dispatch)
(global-set-key (kbd "C-c M-g") 'magit-dispatch)
(global-set-key (kbd "C-c C-b") 'magit-blame)
(global-set-key (kbd "C-c C-l") 'magit-log-buffer-file)
(global-set-key (kbd "C-x v t") 'git-timemachine)

;; ============================================================================
;; LSP COMMANDS
;; ============================================================================
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)
(global-set-key (kbd "M-?") 'xref-find-references)

;; ============================================================================
;; SEARCH & NAVIGATION - ENHANCED
;; ============================================================================
(global-set-key (kbd "M-s p") 'emacs-ide-search-project)
(global-set-key (kbd "M-s d") 'find-grep-dired)
(global-set-key (kbd "M-s f") 'find-name-dired)

;; Consult (if available)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-s l") 'consult-line)
(global-set-key (kbd "M-s r") 'consult-ripgrep)

;; ============================================================================
;; HELP SYSTEM - ENHANCED
;; ============================================================================
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-h k") 'helpful-key)

;; ============================================================================
;; BOOKMARKS
;; ============================================================================
(global-set-key (kbd "C-c b s") 'bookmark-set)
(global-set-key (kbd "C-c b j") 'bookmark-jump)
(global-set-key (kbd "C-c b l") 'bookmark-bmenu-list)

;; ============================================================================
;; REGISTERS
;; ============================================================================
(global-set-key (kbd "C-c x") (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "C-c m") (lambda () (interactive) (switch-to-buffer "*Messages*")))

;; ============================================================================
;; MACROS
;; ============================================================================
(global-set-key (kbd "C-c M-r") 'emacs-ide-toggle-record-macro)
(global-set-key (kbd "C-c M-p") 'kmacro-end-and-call-macro)
(global-set-key (kbd "C-c M-n") 'kmacro-name-last-macro)

;; ============================================================================
;; TERMINAL
;; ============================================================================
(global-set-key (kbd "C-c t") 'vterm)
(global-set-key (kbd "C-c C-t") 'vterm-other-window)
(global-set-key (kbd "C-c M-T") 'multi-term)

;; ============================================================================
;; FILE TREE
;; ============================================================================
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "C-c n") 'neotree-projectile-action)

;; ============================================================================
;; WAYLAND-SPECIFIC KEYBINDINGS
;; ============================================================================
(when (getenv "WAYLAND_DISPLAY")
  (when (executable-find "wl-copy")
    (defun emacs-ide-wayland-copy-region (start end)
      "Copy region to Wayland clipboard."
      (interactive "r")
      (let ((text (buffer-substring-no-properties start end)))
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max) "wl-copy" nil nil nil)))
      (deactivate-mark)
      (message "Copied to Wayland clipboard"))
    
    (global-set-key (kbd "C-c C-w") 'emacs-ide-wayland-copy-region))
  
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
      "Take screenshot of region in Sway."
      (interactive)
      (let ((filename (format "~/Pictures/screenshot-%s.png"
                             (format-time-string "%Y%m%d-%H%M%S"))))
        (call-process-shell-command
         (format "grim -g \"$(slurp)\" %s" filename))
        (message "Screenshot saved to %s" filename)))
    
    (global-set-key (kbd "C-c s") 'emacs-ide-sway-screenshot)))

;; ============================================================================
;; ERGONOMIC IMPROVEMENTS
;; ============================================================================
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-z") 'repeat)

;; ============================================================================
;; CUSTOM KEYBINDING HINTS - ENHANCED
;; ============================================================================
(defun emacs-ide-show-common-keybindings ()
  "Display a cheat sheet of common keybindings."
  (interactive)
  (with-output-to-temp-buffer "*Emacs IDE Keybindings*"
    (princ "=== EMACS IDE KEYBINDINGS ===

FILE OPERATIONS:
  C-c w       Save buffer
  C-c q       Kill buffer and window
  C-c K       Kill other buffers
  C-c r       Recent files
  C-c y p     Copy file path
  C-c y n     Copy file name
  C-c C-r     Rename current file
  C-c C-d     Delete current file

LINE EDITING:
  M-<up/down> Move line up/down
  C-c C-u     Duplicate line
  C-c M-u     Duplicate region
  C-a         Smart beginning of line
  M-;         Comment/uncomment
  M-j         Join lines

WINDOW MANAGEMENT:
  C-x 2       Split horizontal + follow
  C-x 3       Split vertical + follow
  C-x |       Rotate windows
  C-c h/j/k/l Window navigation (vim-style)
  C-c <left>  Undo window change
  M-o         Ace window (quick jump)
  <f8>        Toggle file tree (NeoTree)

CODE NAVIGATION:
  M-i         Jump to function (imenu)
  M-g i       Consult imenu
  C-:         Jump to character (avy)
  C-'         Jump to two characters (avy)
  M-g f       Jump to line (avy)
  M-.         Go to definition
  M-,         Pop back
  M-?         Find references

SELECTION:
  C-=         Expand region
  C--         Contract region
  C->         Multiple cursors: mark next
  C-<         Multiple cursors: mark previous
  C-c m l     Edit lines with cursors
  C-c m a     Mark all like this

COMPILATION & DEBUGGING:
  C-c C-c     Language-specific compile/run
  F5          Compile
  F6          Recompile
  S-F5        Project compile
  S-F6        Project test
  F7          Debug: step in
  S-F7        Debug: step over
  M-F7        Debug: step out
  C-F7        Debug: continue
  C-F5        Toggle breakpoint

LSP (C-c l prefix):
  C-c l g     Go to definition
  C-c l R     Find references
  C-c l r     Rename symbol
  C-c l f     Format buffer
  C-c l a     Code actions
  C-c l i     Find implementation
  C-c l t     Find type definition
  C-c l o     Organize imports

PROJECT (C-c p prefix):
  C-c p f     Find file in project
  C-c p p     Switch project
  C-c p s r   Ripgrep in project
  C-c p s g   Grep in project

GIT:
  C-x g       Magit status
  C-c C-g     Git file dispatch
  C-c C-b     Git blame
  C-c C-l     Git log (file)
  C-x v t     Git time machine

FORMATTING:
  C-c f       Format buffer (format-all)
  C-c F       Format region or buffer
  C-c C-f     Indent buffer
  C-c M-f     Cleanup buffer (indent+untabify)
  C-M-\\      Indent region or buffer

UTILITY:
  C-c ?       Show all keybindings (which-key)
  C-c H       Show this help
  C-c L       LSP status
  C-c I       System info
  C-c S       Startup time
  C-c R       Reload config
  F12         Toggle theme

TERMINAL & TOOLS:
  C-c t       VTerm
  C-c C-t     VTerm (other window)
  C-c M-T     Multi-term

CODE FOLDING:
  C-c @ t     Toggle fold
  C-c @ h     Hide block
  C-c @ s     Show block
  C-c @ a     Hide all
  C-c @ A     Show all

BOOKMARKS:
  C-c b s     Set bookmark
  C-c b j     Jump to bookmark
  C-c b l     List bookmarks

Press q to close this buffer.
")))

(provide 'keybindings)
;;; keybindings.el ends here