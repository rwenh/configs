;;; keybindings.el --- Vanilla-first IDE Keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Philosophy: Emacs defaults are good. We only bind what Emacs has no key for.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.3 (audit):
;;;   - FIX-VERSION: Header bumped from 3.0.3 to 3.0.4.
;;;   - FIX-COMMENT: Line comment "unchanged from 3.0.4" corrected — it was a
;;;     stale copy-paste from another module; this file IS keybindings.el.
;;;   - FIX-MISSING-XL: C-c X l → emacs-ide-test-run-last was referenced in
;;;     the cheat sheet and tools-test.el but had no global-set-key here.
;;;     Added to match tools-test.el binding.
;;;   - FIX-MISSING-XR: C-c x R → emacs-ide-test-report was referenced in
;;;     the cheat sheet and tools-test.el but had no global-set-key here.
;;;     Added to match tools-test.el binding.
;;;   - FIX-ESCAPE: (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;;     overrides Emacs's own ESC handling globally — breaks minibuffer
;;;     single-key cancel, isearch-abort, and recursive-edit exit. Removed.
;;;     keyboard-escape-quit is already reachable as ESC ESC ESC (built-in).
;;;   - FIX-CHEATSHEET: Cheat sheet MISC section corrected:
;;;     "Ds=profiler" → "C-c D s=profiler" for clarity.
;;;     C-c x R and C-c X l entries added to match actual bindings.
;;; Fixes vs 3.0.2 (retained):
;;;   - FIX-1: C-c R = emacs-ide-reload-config (defalias in init.el).
;;;   - FIX-2: C-c x r = emacs-ide-repl-launch.
;;;   - FIX-2/3: REST prefix at C-c V (tools-rest.el), not C-c R.
;;; Code:

;; ============================================================================
;; BUILT-IN UPGRADES
;; ============================================================================
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b")   'consult-buffer)
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)
(global-set-key (kbd "C-x r b") 'consult-bookmark)
(global-set-key (kbd "M-y")     'consult-yank-pop)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)

;; M-g goto prefix
(global-set-key (kbd "M-g g")   'consult-goto-line)
(global-set-key (kbd "M-g M-g") 'consult-goto-line)
(global-set-key (kbd "M-g i")   'consult-imenu)
(global-set-key (kbd "M-g I")   'consult-imenu-multi)
(global-set-key (kbd "M-g o")   'consult-outline)
(global-set-key (kbd "M-g m")   'consult-mark)
(global-set-key (kbd "M-g k")   'consult-global-mark)

;; M-s search prefix
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
;; ============================================================================
(global-set-key (kbd "M-o") 'ace-window)

;; ============================================================================
;; VERSION CONTROL
;; ============================================================================
(global-set-key (kbd "C-x g")   'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-x v t") 'git-timemachine)

;; ============================================================================
;; HELP — HELPFUL
;; ============================================================================
(global-set-key (kbd "C-h f")   'helpful-callable)
(global-set-key (kbd "C-h v")   'helpful-variable)
(global-set-key (kbd "C-h k")   'helpful-key)
(global-set-key (kbd "C-h F")   'helpful-function)
(global-set-key (kbd "C-h C")   'helpful-command)
(global-set-key (kbd "C-h d")   'helpful-at-point)

;; ============================================================================
;; ORG MODE
;; ============================================================================
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

;; ============================================================================
;; COMPILE
;; ============================================================================
(global-set-key (kbd "C-c B") 'compile)
(global-set-key (kbd "C-c b") 'recompile)

;; ============================================================================
;; HYDRA MENUS — C-c h prefix (bodies defined in tools-hydra.el)
;; These are forward declarations — tools-hydra.el sets the actual functions.
;; Listed here for documentation and as fallback no-ops until hydra loads.
;; ============================================================================
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

;; ============================================================================
;; REPL DISPATCH — C-c x prefix (tools-repl.el)
;; ============================================================================
(global-set-key (kbd "C-c x r") #'emacs-ide-repl-launch)
(global-set-key (kbd "C-c x s") #'emacs-ide-repl-send-region)
(global-set-key (kbd "C-c x b") #'emacs-ide-repl-send-buffer)
(global-set-key (kbd "C-c x d") #'emacs-ide-repl-send-defun)
(global-set-key (kbd "C-c x t") #'emacs-ide-repl-toggle-window)
;; FIX-MISSING-XR: test report — was in cheat sheet and tools-test.el but
;; missing from global-set-key here. Added for consistency.
(global-set-key (kbd "C-c x R") #'emacs-ide-test-report)

;; ============================================================================
;; TEST DISPATCH — C-c X prefix (tools-test-runner-registry.el)
;; NOTE: C-c t = vterm, C-c T = vterm-other-window (tools-terminal.el).
;;       C-c X is the test prefix (unoccupied).
;;       C-c x r = emacs-ide-repl-launch (tools-repl.el) — do NOT override.
;;       C-c x R = emacs-ide-test-report  (tools-test.el)
;;       C-c X l = emacs-ide-test-run-last (tools-test.el)
;; ============================================================================
(global-set-key (kbd "C-c X f") #'emacs-ide-test-run-file)
(global-set-key (kbd "C-c X p") #'emacs-ide-test-run-project)
(global-set-key (kbd "C-c X .") #'emacs-ide-test-run-at-point)
(global-set-key (kbd "C-c X w") #'emacs-ide-test-watch)
(global-set-key (kbd "C-c X s") #'emacs-ide-test-runner-status)
;; FIX-MISSING-XL: repeat last test — was in cheat sheet and tools-test.el
;; but missing from global-set-key here. Added for consistency.
(global-set-key (kbd "C-c X l") #'emacs-ide-test-run-last)
;; Legacy C-c C-t kept for compatibility (tools-test.el smart dispatch)
(global-set-key (kbd "C-c C-t") #'emacs-ide-test-run)

;; ============================================================================
;; PROJECT DETECT STATUS
;; ============================================================================
;; FIX-CCC-D (retained): C-c D is a prefix (C-c D s/r/q=profiler, C-c D o=docker).
;; detect-show-status at C-c D d avoids command-vs-prefix conflict.
(global-set-key (kbd "C-c D d") #'emacs-ide-detect-show-status)

;; Profiler sub-keys — referenced in cheat sheet and spot-check.
;; FIX-PROFILER-KEYS: these were documented but never actually bound.
;; Bind to the wrapped profiler commands from emacs-ide-profiler.el
;; which add state tracking; fall back to raw profiler-* if not loaded.
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

;; ============================================================================
;; UTILITY
;; ============================================================================
(global-set-key (kbd "C-c ?") 'which-key-show-top-level)
(global-set-key (kbd "C-c H") 'emacs-ide-show-keybindings-help)
;; FIX-1 (retained): emacs-ide-reload-config is a defalias for
;; emacs-ide-config-reload defined in init.el.
(global-set-key (kbd "C-c R") 'emacs-ide-reload-config)
(global-set-key (kbd "C-c L") 'emacs-ide-lsp-status)
(global-set-key (kbd "C-c n") 'neotree-toggle)
(global-set-key (kbd "<f12>") 'emacs-ide-toggle-theme)
(global-set-key (kbd "C-c P") 'emacs-ide-presentation-mode)
;; FIX-ESCAPE: (global-set-key "<escape>" keyboard-escape-quit) removed.
;; Overriding ESC globally breaks minibuffer cancel, isearch-abort, and
;; recursive-edit exit. keyboard-escape-quit is already ESC ESC ESC built-in.

;; ============================================================================
;; CHEAT SHEET
;; ============================================================================
(defun emacs-ide-show-keybindings-help ()
  "Show non-obvious IDE bindings."
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
  C-c x R   test report  (tools-test.el)

TESTS  (C-c X prefix — uppercase X):
  C-c X f   run file tests
  C-c X p   run project tests
  C-c X .   run test at point
  C-c X w   watch mode
  C-c X s   runner status
  C-c X l   repeat last test
  C-c C-t   smart test dispatch (legacy)

REST  (C-c V prefix — tools-rest.el):
  C-c V s   REST scratch buffer
  C-c V i   insert request template

WORKSPACES  (C-c W prefix, ui-workspace.el):
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
  C-h d     helpful-at-point

COMPILE:
  C-c B     compile
  C-c b     recompile
  C-c C-t   emacs-ide-test-run (smart dispatch)

MISC:
  C-c D d   project detect status
  C-c D s   profiler-start  (C-c D prefix: C-c D r=report C-c D q=stop)
  C-c D o   docker
  C-c L     LSP status
  F12       toggle theme
  C-c P     presentation mode

Press q to close.\n")))

(provide 'keybindings)
;;; keybindings.el ends here
