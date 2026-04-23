;;; keybindings.el --- Vanilla-first IDE Keybindings -*- lexical-binding: t -*-
;;; Version: 3.2.2 | FIX: Moved emacs-ide-show-keybindings-help defun to before
;;;           its global-set-key call so (provide 'keybindings) always executes
;;;           and (fboundp) returns t at spot-check time.
;;; Code:

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b")   'consult-buffer)
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)
(global-set-key (kbd "C-x r b") 'consult-bookmark)
(global-set-key (kbd "M-y")     'consult-yank-pop)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)

(global-set-key (kbd "M-g g")   'consult-goto-line)
(global-set-key (kbd "M-g M-g") 'consult-goto-line)
(global-set-key (kbd "M-g i")   'consult-imenu)
(global-set-key (kbd "M-g I")   'consult-imenu-multi)
(global-set-key (kbd "M-g o")   'consult-outline)
(global-set-key (kbd "M-g m")   'consult-mark)
(global-set-key (kbd "M-g k")   'consult-global-mark)

(global-set-key (kbd "M-s l")   'consult-line)
(global-set-key (kbd "M-s L")   'consult-line-multi)
(global-set-key (kbd "M-s r")   'consult-ripgrep)
(global-set-key (kbd "M-s g")   'consult-grep)
(global-set-key (kbd "M-s G")   'consult-git-grep)
(global-set-key (kbd "M-s f")   'consult-find)
(global-set-key (kbd "M-s k")   'consult-keep-lines)
(global-set-key (kbd "M-s u")   'consult-focus-lines)

(global-set-key (kbd "M-o") 'ace-window)

(global-set-key (kbd "C-x g")   'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-x v t") 'git-timemachine)

(global-set-key (kbd "C-h f")   'helpful-callable)
(global-set-key (kbd "C-h v")   'helpful-variable)
(global-set-key (kbd "C-h k")   'helpful-key)
(global-set-key (kbd "C-h F")   'helpful-function)
(global-set-key (kbd "C-h C")   'helpful-command)
(global-set-key (kbd "C-h d")   'helpful-at-point)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(global-set-key (kbd "C-c B") 'compile)
(global-set-key (kbd "C-c b") 'recompile)

(global-set-key (kbd "C-c h w") (lambda () (interactive) (if (fboundp 'hydra-window/body)  (hydra-window/body)  (message "hydra-window not loaded"))))
(global-set-key (kbd "C-c h b") (lambda () (interactive) (if (fboundp 'hydra-buffer/body)  (hydra-buffer/body)  (message "hydra-buffer not loaded"))))
(global-set-key (kbd "C-c h g") (lambda () (interactive) (if (fboundp 'hydra-git/body)     (hydra-git/body)     (message "hydra-git not loaded"))))
(global-set-key (kbd "C-c h l") (lambda () (interactive) (if (fboundp 'hydra-lsp/body)     (hydra-lsp/body)     (message "hydra-lsp not loaded"))))
(global-set-key (kbd "C-c h p") (lambda () (interactive) (if (fboundp 'hydra-project/body) (hydra-project/body) (message "hydra-project not loaded"))))
(global-set-key (kbd "C-c h t") (lambda () (interactive) (if (fboundp 'hydra-test/body)    (hydra-test/body)    (message "hydra-test not loaded"))))
(global-set-key (kbd "C-c h d") (lambda () (interactive) (if (fboundp 'hydra-debug/body)   (hydra-debug/body)   (message "hydra-debug not loaded"))))
(global-set-key (kbd "C-c h u") (lambda () (interactive) (if (fboundp 'hydra-toggle/body)  (hydra-toggle/body)  (message "hydra-toggle not loaded"))))
(global-set-key (kbd "C-c h r") (lambda () (interactive) (if (fboundp 'hydra-repl/body)    (hydra-repl/body)    (message "hydra-repl not loaded"))))
(global-set-key (kbd "C-c h s") (lambda () (interactive) (if (fboundp 'hydra-search/body)  (hydra-search/body)  (message "hydra-search not loaded"))))
(global-set-key (kbd "C-c h h") (lambda () (interactive)
                                   (message "Hydras: w)indow b)uffer g)it l)sp p)roject t)est d)ebug u)toggle r)epl s)earch")))

(global-set-key (kbd "C-c x r") #'emacs-ide-repl-launch)
(global-set-key (kbd "C-c x s") #'emacs-ide-repl-send-region)
(global-set-key (kbd "C-c x b") #'emacs-ide-repl-send-buffer)
(global-set-key (kbd "C-c x d") #'emacs-ide-repl-send-defun)
(global-set-key (kbd "C-c x l") #'emacs-ide-repl-send-line)
(global-set-key (kbd "C-c x t") #'emacs-ide-repl-toggle-window)
(global-set-key (kbd "C-c x R") #'emacs-ide-test-report)

(global-set-key (kbd "C-c X f") #'emacs-ide-test-run-file)
(global-set-key (kbd "C-c X p") #'emacs-ide-test-run-project)
(global-set-key (kbd "C-c X .") #'emacs-ide-test-run-at-point)
(global-set-key (kbd "C-c X w") #'emacs-ide-test-watch)
(global-set-key (kbd "C-c X s") #'emacs-ide-test-runner-status)
(global-set-key (kbd "C-c X l") #'emacs-ide-test-run-last)
(global-set-key (kbd "C-c C-t") #'emacs-ide-test-run)
(global-set-key (kbd "C-c C-T") #'emacs-ide-test-run-all)

;;; ─── Recovery (C-c r prefix) ─────────────────────────────────────────────────

(global-set-key (kbd "C-c r r")   #'emacs-ide-recovery-report)
(global-set-key (kbd "C-c r v")   #'emacs-ide-recovery-view-log)
(global-set-key (kbd "C-c r b")   #'emacs-ide-recovery-backup-config)
(global-set-key (kbd "C-c r d")   #'emacs-ide-recovery-disable-package)
(global-set-key (kbd "C-c r C-r") #'emacs-ide-recovery-reset-crash-count)

;;; ─── Terminal / eshell ───────────────────────────────────────────────────────
;; These are also set via use-package :bind in tools-terminal.el, but we set
;; them here too so they appear in the canonical keybinding registry and are
;; verified by spot-check regardless of package load order.

(global-set-key (kbd "C-c t")
  (lambda () (interactive)
    (if (fboundp 'emacs-ide-vterm-here)
        (emacs-ide-vterm-here)
      (message "vterm not loaded yet"))))

(global-set-key (kbd "C-c e")
  (lambda () (interactive)
    (if (fboundp 'emacs-ide-eshell-here)
        (emacs-ide-eshell-here)
      (eshell 'N))))

(global-set-key (kbd "C-c D d") #'emacs-ide-detect-show-status)

(global-set-key (kbd "C-c D s")
  (lambda () (interactive)
    (if (fboundp 'emacs-ide-profile-start)
        (emacs-ide-profile-start)
      (profiler-start 'cpu+mem))))
(global-set-key (kbd "C-c D r")
  (lambda () (interactive)
    (if (fboundp 'emacs-ide-profile-report)
        (emacs-ide-profile-report)
      (profiler-report))))
(global-set-key (kbd "C-c D q")
  (lambda () (interactive)
    (if (fboundp 'emacs-ide-profile-stop)
        (emacs-ide-profile-stop)
      (profiler-stop))))

(defun emacs-ide-show-keybindings-help ()
  (interactive)
  (with-output-to-temp-buffer "*IDE Keybindings*"
    (princ
     "=== EMACS IDE v3 — NON-OBVIOUS BINDINGS ===
Vanilla defaults work normally. C-h k looks up any key.

HYDRA MENUS  (C-c h prefix — chord-free discoverable commands):
  C-c h w   window hydra    split/resize/ace/winner
  C-c h b   buffer hydra    switch/kill/scratch
  C-c h g   git hydra       magit/diff/blame/stash
  C-c h l   lsp hydra       rename/actions/refs/format
  C-c h p   project hydra   find/search/compile
  C-c h t   test hydra      run/watch/report
  C-c h d   debug hydra     step/break/inspect
  C-c h u   toggle hydra    theme/line-nos/etc
  C-c h r   repl hydra      launch/send/toggle
  C-c h s   search hydra    ripgrep/occur/symbol

REPL  (C-c x prefix):
  C-c x r   launch / switch to REPL
  C-c x s   send region
  C-c x b   send buffer
  C-c x d   send defun at point
  C-c x t   toggle REPL window
  C-c x R   test report

TESTS  (C-c X prefix -- uppercase X):
  C-c X f   run file tests
  C-c X p   run project tests
  C-c X .   run test at point
  C-c X w   watch mode
  C-c X s   runner status
  C-c X l   repeat last test
  C-c C-t   smart test dispatch

REST  (C-c V prefix):
  C-c V s   REST scratch buffer
  C-c V i   insert request template

WORKSPACES  (C-c W prefix):
  C-c W s   switch workspace
  C-c W n   new workspace
  C-c W k   kill workspace
  M-1..9    switch by index

SEARCH  (M-s prefix):
  M-s l     consult-line
  M-s r     consult-ripgrep
  M-s G     consult-git-grep

UPGRADED BUILT-INS:
  C-x C-b   ibuffer
  C-x b     consult-buffer
  M-y       consult-yank-pop
  C-x g     magit-status
  C-h f/v/k helpful-*

COMPILE:
  C-c B     compile
  C-c b     recompile

MISC:
  C-c D d   project detect status
  C-c D s   profiler-start
  C-c L     LSP status
  F12       toggle theme
  C-c P     presentation mode

Press q to close.\n")))

(global-set-key (kbd "C-c ?") 'which-key-show-top-level)
(global-set-key (kbd "C-c H") 'emacs-ide-show-keybindings-help)
(global-set-key (kbd "C-c R") 'emacs-ide-reload-config)
(global-set-key (kbd "C-c L") 'emacs-ide-lsp-status)
(global-set-key (kbd "C-c n") 'neotree-toggle)
(global-set-key (kbd "<f12>") 'emacs-ide-toggle-theme)
(global-set-key (kbd "C-c P") 'emacs-ide-presentation-mode)

;;; ─── Completion / editing ────────────────────────────────────────────────────
;; M-/ was set in completion-core.el — moved here for single source of truth.
(global-set-key (kbd "M-/") 'hippie-expand)

;;; ─── Notes ───────────────────────────────────────────────────────────────────
;; C-c n / was set in tools-notes.el — moved here.
(global-set-key (kbd "C-c n /")
  (lambda () (interactive)
    (if (fboundp 'emacs-ide-notes-search)
        (emacs-ide-notes-search)
      (message "tools-notes not loaded yet"))))

(provide 'keybindings)
;;; keybindings.el ends here
