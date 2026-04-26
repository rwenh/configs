;;; keybindings.el --- Vanilla-first IDE Keybindings -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; DESIGN RULES (enforced here):
;;;   1.  keybindings.el loads last in the feature-module list.
;;;   2.  All global-set-key calls for IDE commands live here.  No feature
;;;       module should set global keys that are not also in this file.
;;;   3.  Direct symbol bindings go in the DIRECT section; they are verified
;;;       by emacs-ide-spot-check with strict eq.
;;;   4.  Lambda / closure bindings (deferred commands, guards) go in the
;;;       LAMBDA section; spot-check verifies only that they are non-nil.
;;;   5.  Prefix maps are created with define-prefix-command so that C-h
;;;       and which-key can enumerate their children.
;;;
;;; Code:

;;;; ── Upgraded built-ins ───────────────────────────────────────────────────────

(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x b")   #'consult-buffer)
(global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)
(global-set-key (kbd "C-x r b") #'consult-bookmark)
(global-set-key (kbd "M-y")     #'consult-yank-pop)
(global-set-key (kbd "C-x C-r") #'consult-recent-file)

;;;; ── Go-to (M-g) ─────────────────────────────────────────────────────────────

(global-set-key (kbd "M-g g")   #'consult-goto-line)
(global-set-key (kbd "M-g M-g") #'consult-goto-line)
(global-set-key (kbd "M-g i")   #'consult-imenu)
(global-set-key (kbd "M-g I")   #'consult-imenu-multi)
(global-set-key (kbd "M-g o")   #'consult-outline)
(global-set-key (kbd "M-g m")   #'consult-mark)
(global-set-key (kbd "M-g k")   #'consult-global-mark)

;;;; ── Search (M-s) ────────────────────────────────────────────────────────────

(global-set-key (kbd "M-s l")   #'consult-line)
(global-set-key (kbd "M-s L")   #'consult-line-multi)
(global-set-key (kbd "M-s r")   #'consult-ripgrep)
(global-set-key (kbd "M-s g")   #'consult-grep)
(global-set-key (kbd "M-s G")   #'consult-git-grep)
(global-set-key (kbd "M-s f")   #'consult-find)
(global-set-key (kbd "M-s k")   #'consult-keep-lines)
(global-set-key (kbd "M-s u")   #'consult-focus-lines)

;;;; ── Window / navigation ─────────────────────────────────────────────────────

(global-set-key (kbd "M-o") #'ace-window)

;;;; ── Git ─────────────────────────────────────────────────────────────────────

(global-set-key (kbd "C-x g")   #'magit-status)
(global-set-key (kbd "C-x M-g") #'magit-dispatch)
(global-set-key (kbd "C-x v t") #'git-timemachine)

;;;; ── Help (C-h) ──────────────────────────────────────────────────────────────

(global-set-key (kbd "C-h f")   #'helpful-callable)
(global-set-key (kbd "C-h v")   #'helpful-variable)
(global-set-key (kbd "C-h k")   #'helpful-key)
(global-set-key (kbd "C-h F")   #'helpful-function)
(global-set-key (kbd "C-h C")   #'helpful-command)
(global-set-key (kbd "C-h d")   #'helpful-at-point)

;;;; ── Org ─────────────────────────────────────────────────────────────────────

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)

;;;; ── Compile ─────────────────────────────────────────────────────────────────

(global-set-key (kbd "C-c B") #'compile)
(global-set-key (kbd "C-c b") #'recompile)

;;;; ── Hydra menus (C-c h) — lambda-bound ─────────────────────────────────────

(global-set-key (kbd "C-c h w")
  (lambda () (interactive)
    (if (fboundp 'hydra-window/body)  (hydra-window/body)
      (message "hydra-window not loaded"))))
(global-set-key (kbd "C-c h b")
  (lambda () (interactive)
    (if (fboundp 'hydra-buffer/body)  (hydra-buffer/body)
      (message "hydra-buffer not loaded"))))
(global-set-key (kbd "C-c h g")
  (lambda () (interactive)
    (if (fboundp 'hydra-git/body)     (hydra-git/body)
      (message "hydra-git not loaded"))))
(global-set-key (kbd "C-c h l")
  (lambda () (interactive)
    (if (fboundp 'hydra-lsp/body)     (hydra-lsp/body)
      (message "hydra-lsp not loaded"))))
(global-set-key (kbd "C-c h p")
  (lambda () (interactive)
    (if (fboundp 'hydra-project/body) (hydra-project/body)
      (message "hydra-project not loaded"))))
(global-set-key (kbd "C-c h t")
  (lambda () (interactive)
    (if (fboundp 'hydra-test/body)    (hydra-test/body)
      (message "hydra-test not loaded"))))
(global-set-key (kbd "C-c h d")
  (lambda () (interactive)
    (if (fboundp 'hydra-debug/body)   (hydra-debug/body)
      (message "hydra-debug not loaded"))))
(global-set-key (kbd "C-c h u")
  (lambda () (interactive)
    (if (fboundp 'hydra-toggle/body)  (hydra-toggle/body)
      (message "hydra-toggle not loaded"))))
(global-set-key (kbd "C-c h r")
  (lambda () (interactive)
    (if (fboundp 'hydra-repl/body)    (hydra-repl/body)
      (message "hydra-repl not loaded"))))
(global-set-key (kbd "C-c h s")
  (lambda () (interactive)
    (if (fboundp 'hydra-search/body)  (hydra-search/body)
      (message "hydra-search not loaded"))))
(global-set-key (kbd "C-c h h")
  (lambda () (interactive)
    (message
     "Hydras: w)indow b)uffer g)it l)sp p)roject t)est d)ebug u)toggle r)epl s)earch")))

;;;; ── REPL (C-c x) — direct ───────────────────────────────────────────────────

(global-set-key (kbd "C-c x r") #'emacs-ide-repl-launch)
(global-set-key (kbd "C-c x s") #'emacs-ide-repl-send-region)
(global-set-key (kbd "C-c x b") #'emacs-ide-repl-send-buffer)
(global-set-key (kbd "C-c x d") #'emacs-ide-repl-send-defun)
(global-set-key (kbd "C-c x l") #'emacs-ide-repl-send-line)
(global-set-key (kbd "C-c x t") #'emacs-ide-repl-toggle-window)
(global-set-key (kbd "C-c x R") #'emacs-ide-test-report)

;;;; ── Tests (C-c X uppercase) — direct ───────────────────────────────────────

(global-set-key (kbd "C-c X f") #'emacs-ide-test-run-file)
(global-set-key (kbd "C-c X p") #'emacs-ide-test-run-project)
(global-set-key (kbd "C-c X .") #'emacs-ide-test-run-at-point)
(global-set-key (kbd "C-c X w") #'emacs-ide-test-watch)
(global-set-key (kbd "C-c X s") #'emacs-ide-test-runner-status)
(global-set-key (kbd "C-c X l") #'emacs-ide-test-run-last)
(global-set-key (kbd "C-c C-t") #'emacs-ide-test-run)
(global-set-key (kbd "C-c C-T") #'emacs-ide-test-run-all)

;;;; ── Recovery (C-c r) — direct ──────────────────────────────────────────────

(global-set-key (kbd "C-c r r")   #'emacs-ide-recovery-report)
(global-set-key (kbd "C-c r v")   #'emacs-ide-recovery-view-log)
(global-set-key (kbd "C-c r b")   #'emacs-ide-recovery-backup-config)
(global-set-key (kbd "C-c r d")   #'emacs-ide-recovery-disable-package)
(global-set-key (kbd "C-c r C-r") #'emacs-ide-recovery-reset-crash-count)

;;;; ── REST client (C-c V) — direct ───────────────────────────────────────────

(global-set-key (kbd "C-c V s") #'emacs-ide-rest-scratch)
(global-set-key (kbd "C-c V i") #'emacs-ide-rest-insert-request)

;;;; ── Terminal / eshell — lambda-bound ───────────────────────────────────────

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

;;;; ── Notes (C-c n) prefix map — FIXED #2 ────────────────────────────────────

(define-prefix-command 'emacs-ide-notes-map)
(global-set-key (kbd "C-c n") 'emacs-ide-notes-map)

;; C-c n n — neotree (direct symbol; spot-check verifies with eq)
(global-set-key (kbd "C-c n n") #'neotree-toggle)

;; C-c n / — full-text search across notes directory (lambda-bound)
(global-set-key (kbd "C-c n /")
  (lambda () (interactive)
    (if (fboundp 'emacs-ide-notes-search)
        (emacs-ide-notes-search)
      (message "tools-notes not loaded yet — open any .org file first"))))

;;;; ── Project detect / profiler (C-c D) — lambda-bound ───────────────────────

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

;;;; ── IDE utility — direct ────────────────────────────────────────────────────

(global-set-key (kbd "C-c ?") #'which-key-show-top-level)
(global-set-key (kbd "C-c H") #'emacs-ide-show-keybindings-help)
(global-set-key (kbd "C-c R") #'emacs-ide-reload-config)
(global-set-key (kbd "C-c L") #'emacs-ide-lsp-status)
(global-set-key (kbd "<f12>") #'emacs-ide-toggle-theme)
(global-set-key (kbd "C-c P") #'emacs-ide-presentation-mode)

;;;; ── Completion / editing ────────────────────────────────────────────────────

(global-set-key (kbd "M-/") #'hippie-expand)

;;;; ── Keybindings help ────────────────────────────────────────────────────────

(defun emacs-ide-show-keybindings-help ()
  "Display a buffer listing all non-obvious IDE keybindings."
  (interactive)
  (with-output-to-temp-buffer "*IDE Keybindings*"
    (princ
     "=== EMACS IDE v3.3 — NON-OBVIOUS BINDINGS ===
Vanilla defaults work normally.  C-h k looks up any key.

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
  C-c h h   list all hydra entry points

REPL  (C-c x — lowercase x):
  C-c x r   launch / switch to REPL for current language
  C-c x s   send region to REPL
  C-c x b   send buffer to REPL
  C-c x d   send defun at point to REPL
  C-c x l   send current line and advance
  C-c x t   toggle REPL side window
  C-c x R   show test report

TESTS  (C-c X — uppercase X):
  C-c X f   run file tests
  C-c X p   run project tests
  C-c X .   run test at point
  C-c X w   watch mode
  C-c X s   show runner registry
  C-c X l   repeat last test run
  C-c C-t   smart dispatch (file → project → auto-detect)
  C-c C-T   force full project suite

NOTES  (C-c n prefix):
  C-c n n   neotree file browser
  C-c n /   full-text search across notes directory
  C-c n f   org-roam find node
  C-c n i   org-roam insert link
  C-c n b   org-roam buffer toggle
  C-c n d   goto today's daily note
  C-c n g   open org-roam graph

REST  (C-c V prefix):
  C-c V s   open REST scratch buffer
  C-c V i   insert request template (in org-mode)

RECOVERY  (C-c r prefix):
  C-c r r   recovery report
  C-c r v   view recovery log
  C-c r b   backup config.yml
  C-c r d   disable a problematic package
  C-c r C-r reset crash counter

WORKSPACES  (C-c W prefix):
  C-c W s   switch workspace
  C-c W n   new workspace
  C-c W k   kill workspace
  C-c W r   rename workspace
  M-1..9    switch by index

SEARCH  (M-s prefix):
  M-s l     consult-line
  M-s L     consult-line (multi-buffer)
  M-s r     consult-ripgrep
  M-s G     consult-git-grep

GO-TO  (M-g prefix):
  M-g g     consult-goto-line
  M-g i     consult-imenu
  M-g o     consult-outline
  M-g s     consult-lsp-symbols
  M-g e     consult-lsp-diagnostics

UPGRADED BUILT-INS:
  C-x C-b   ibuffer
  C-x b     consult-buffer
  M-y       consult-yank-pop
  C-x g     magit-status
  C-h f/v/k helpful-*
  M-/       hippie-expand

TERMINAL:
  C-c t     vterm in current directory
  C-c e     eshell in current directory

PROFILER  (C-c D prefix):
  C-c D d   project detect status
  C-c D s   start profiler
  C-c D r   stop + show report
  C-c D q   stop profiler

MISC:
  C-c L     LSP status (current buffer)
  F12       toggle dark/light ef-theme
  C-c P     presentation mode (large font)
  C-c R     reload config.yml
  C-c H     this help buffer
  C-c ?     which-key top-level

Press q to close.\n")))

(provide 'keybindings)
;;; keybindings.el ends here
