;;; tools-hydra.el --- Hydra Menus for Discoverable Commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Chord-free, discoverable Hydra menus covering every major IDE workflow.
;;; This is the Emacs-ideology replacement for Vim leader-key bindings.
;;; Each hydra is invoked via a single memorable key and shows all options.
;;;
;;; Entry points (all via C-c h prefix):
;;;   C-c h w   window hydra     — split/resize/ace/winner
;;;   C-c h b   buffer hydra     — switch/kill/scratch/ibuffer
;;;   C-c h g   git hydra        — magit/diff/blame/stash
;;;   C-c h l   lsp hydra        — rename/actions/refs/format
;;;   C-c h p   project hydra    — find/search/compile/test
;;;   C-c h t   test hydra       — run/watch/report/at-point
;;;   C-c h d   debug hydra      — step/break/inspect/continue
;;;   C-c h u   toggle hydra     — theme/line-nos/indent/etc
;;;   C-c h r   repl hydra       — launch/send/toggle
;;;   C-c h s   search hydra     — ripgrep/occur/symbol
;;;
;;; Version: 1.0.0
;;; Code:

(use-package hydra :demand t)

(with-eval-after-load 'hydra

;; ============================================================================
;; WINDOW HYDRA — C-c h w
;; ============================================================================
(defhydra hydra-window (:hint nil :color pink)
  "
  window
  ──────────────────────────────────────────────
  split    _v_ vertical  _s_ horizontal  _z_ undo
  delete   _d_ this      _D_ others      _b_ balance
  resize   _←_ shrink    _→_ grow        _↑_ taller  _↓_ shorter
  move     _h_ left      _j_ down        _k_ up       _l_ right
  ace      _o_ switch    _x_ swap        _m_ maximize
  tabs     _n_ new tab   _N_ next tab    _P_ prev tab
  ──────────────────────────────────────────────
  _q_ quit
"
  ("v" split-window-right)
  ("s" split-window-below)
  ("z" winner-undo)
  ("d" delete-window)
  ("D" delete-other-windows)
  ("b" balance-windows)
  ("<left>"  shrink-window-horizontally)
  ("<right>" enlarge-window-horizontally)
  ("<up>"    enlarge-window)
  ("<down>"  shrink-window)
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("o" ace-window)
  ("x" ace-swap-window)
  ("m" delete-other-windows)
  ("n" tab-bar-new-tab)
  ("N" tab-bar-switch-to-next-tab)
  ("P" tab-bar-switch-to-prev-tab)
  ("q" nil :color blue))

;; ============================================================================
;; BUFFER HYDRA — C-c h b
;; ============================================================================
(defhydra hydra-buffer (:hint nil :color blue)
  "
  buffer
  ────────────────────────────────────
  _b_ switch      _B_ ibuffer     _r_ recent
  _k_ kill this   _K_ kill others _R_ revert
  _s_ scratch     _n_ new         _w_ save all
  _p_ prev        _f_ next        _i_ imenu
  ────────────────────────────────────
  _q_ quit
"
  ("b" consult-buffer)
  ("B" ibuffer)
  ("r" consult-recent-file)
  ("k" kill-this-buffer)
  ("K" (mapc #'kill-buffer
             (cl-remove-if (lambda (b) (eq b (current-buffer)))
                           (buffer-list))))
  ("R" revert-buffer)
  ("s" scratch-buffer)
  ("n" (switch-to-buffer (generate-new-buffer "*new*")))
  ("w" save-some-buffers)
  ("p" previous-buffer)
  ("f" next-buffer)
  ("i" consult-imenu)
  ("q" nil))

;; ============================================================================
;; GIT HYDRA — C-c h g
;; ============================================================================
(defhydra hydra-git (:hint nil :color blue)
  "
  git
  ────────────────────────────────────────────────
  magit    _s_ status    _l_ log       _d_ dispatch
  diff     _D_ diff      _b_ blame     _t_ timemachine
  hunk     _n_ next      _p_ prev      _r_ revert hunk
  stash    _z_ stash     _Z_ pop stash
  forge    _f_ forge     _P_ push      _F_ pull
  ────────────────────────────────────────────────
  _q_ quit
"
  ("s" magit-status)
  ("l" magit-log-current)
  ("d" magit-dispatch)
  ("D" magit-diff-working-tree)
  ("b" magit-blame-addition)
  ("t" git-timemachine)
  ("n" (when (fboundp 'diff-hl-next-hunk) (diff-hl-next-hunk)))
  ("p" (when (fboundp 'diff-hl-previous-hunk) (diff-hl-previous-hunk)))
  ("r" (when (fboundp 'diff-hl-revert-hunk) (diff-hl-revert-hunk)))
  ("z" magit-stash)
  ("Z" magit-stash-pop)
  ("f" (when (fboundp 'forge-dispatch) (forge-dispatch)))
  ("P" magit-push-current)
  ("F" magit-pull-from-upstream)
  ("q" nil))

;; ============================================================================
;; LSP HYDRA — C-c h l
;; ============================================================================
(defhydra hydra-lsp (:hint nil :color blue)
  "
  lsp
  ────────────────────────────────────────────────
  nav      _d_ definition  _r_ references  _i_ impl
  find     _t_ type def    _o_ outline     _s_ symbol
  edit     _R_ rename      _a_ action      _f_ format
  doc      _h_ hover       _u_ ui toggle   _k_ sig help
  misc     _e_ diagnostics _l_ lens        _w_ workspace
  ────────────────────────────────────────────────
  _q_ quit
"
  ("d" (when (fboundp 'lsp-find-definition) (lsp-find-definition)))
  ("r" (when (fboundp 'lsp-find-references) (lsp-find-references)))
  ("i" (when (fboundp 'lsp-find-implementation) (lsp-find-implementation)))
  ("t" (when (fboundp 'lsp-find-type-definition) (lsp-find-type-definition)))
  ("o" (when (fboundp 'lsp-ui-imenu) (lsp-ui-imenu)))
  ("s" consult-lsp-symbols)
  ("R" (when (fboundp 'lsp-rename) (call-interactively #'lsp-rename)))
  ("a" (when (fboundp 'lsp-execute-code-action) (lsp-execute-code-action)))
  ("f" (when (fboundp 'lsp-format-buffer) (lsp-format-buffer)))
  ("h" (when (fboundp 'lsp-describe-thing-at-point) (lsp-describe-thing-at-point)))
  ("u" (when (fboundp 'lsp-ui-doc-toggle) (lsp-ui-doc-toggle)))
  ("k" (when (fboundp 'lsp-signature-activate) (lsp-signature-activate)))
  ("e" (when (fboundp 'flycheck-list-errors) (flycheck-list-errors)))
  ("l" (when (fboundp 'lsp-lens-mode) (lsp-lens-mode 'toggle)))
  ("w" (when (fboundp 'lsp-describe-session) (lsp-describe-session)))
  ("q" nil))

;; ============================================================================
;; PROJECT HYDRA — C-c h p
;; ============================================================================
(defhydra hydra-project (:hint nil :color blue)
  "
  project
  ────────────────────────────────────────────────
  find     _f_ file        _d_ dir         _r_ recent
  search   _s_ ripgrep     _g_ grep        _o_ occur
  switch   _p_ project     _b_ buffer      _k_ kill bufs
  build    _c_ compile     _C_ recompile   _R_ run
  manage   _t_ treemacs    _n_ neotree     _i_ ibuffer
  ────────────────────────────────────────────────
  _q_ quit
"
  ("f" projectile-find-file)
  ("d" projectile-find-dir)
  ("r" consult-recent-file)
  ("s" consult-ripgrep)
  ("g" consult-grep)
  ("o" consult-line)
  ("p" projectile-switch-project)
  ("b" consult-project-buffer)
  ("k" projectile-kill-buffers)
  ("c" projectile-compile-project)
  ("C" recompile)
  ("R" projectile-run-project)
  ("t" treemacs)
  ("n" neotree-toggle)
  ("i" ibuffer)
  ("q" nil))

;; ============================================================================
;; TEST HYDRA — C-c h t
;; ============================================================================
(defhydra hydra-test (:hint nil :color blue)
  "
  test
  ────────────────────────────────────────
  _f_ file tests     _p_ project tests
  _._ test at point  _l_ last test
  _w_ watch mode     _r_ test report
  _s_ runner status
  ────────────────────────────────────────
  _q_ quit
"
  ("f" emacs-ide-test-run-file)
  ("p" emacs-ide-test-run-project)
  ("." emacs-ide-test-run-at-point)
  ("l" (when (fboundp 'emacs-ide-test-run-last) (emacs-ide-test-run-last)))
  ("w" emacs-ide-test-watch)
  ("r" (when (fboundp 'emacs-ide-test-report) (emacs-ide-test-report)))
  ("s" emacs-ide-test-runner-status)
  ("q" nil))

;; ============================================================================
;; DEBUG HYDRA — C-c h d
;; ============================================================================
(defhydra hydra-debug (:hint nil :color pink)
  "
  debug
  ────────────────────────────────────────────────────────
  control  _s_ step in    _n_ next       _o_ step out
           _c_ continue   _r_ restart    _q_ quit session
  break    _b_ toggle     _B_ condition  _L_ log msg
           _D_ del all
  inspect  _l_ locals     _e_ eval expr  _w_ watch
           _u_ up frame   _d_ down frame _R_ repl
  launch   _5_ debug      _6_ restart
  ────────────────────────────────────────────────────────
  _ESC_ close
"
  ("s" (when (fboundp 'dap-step-in) (dap-step-in)))
  ("n" (when (fboundp 'dap-next) (dap-next)))
  ("o" (when (fboundp 'dap-step-out) (dap-step-out)))
  ("c" (when (fboundp 'dap-continue) (dap-continue)))
  ("r" (when (fboundp 'dap-debug-restart) (dap-debug-restart)))
  ("q" (when (fboundp 'dap-disconnect) (dap-disconnect)) :color blue)
  ("b" (when (fboundp 'dap-breakpoint-toggle) (dap-breakpoint-toggle)) :color red)
  ("B" (when (fboundp 'dap-breakpoint-condition) (dap-breakpoint-condition)))
  ("L" (when (fboundp 'dap-breakpoint-log-message) (dap-breakpoint-log-message)))
  ("D" (when (fboundp 'dap-breakpoint-delete-all) (dap-breakpoint-delete-all)))
  ("l" (when (fboundp 'dap-ui-locals) (dap-ui-locals)))
  ("e" (when (fboundp 'dap-eval-thing-at-point) (dap-eval-thing-at-point)))
  ("w" (when (fboundp 'dap-ui-expressions) (dap-ui-expressions)))
  ("u" (when (fboundp 'dap-up-stack-frame) (dap-up-stack-frame)))
  ("d" (when (fboundp 'dap-down-stack-frame) (dap-down-stack-frame)))
  ("R" (when (fboundp 'dap-ui-repl) (dap-ui-repl)))
  ("5" (when (fboundp 'dap-debug) (call-interactively #'dap-debug)))
  ("6" (when (fboundp 'dap-debug-restart) (dap-debug-restart)))
  ("ESC" nil :color blue))

;; ============================================================================
;; TOGGLE HYDRA — C-c h u
;; ============================================================================
(defhydra hydra-toggle (:hint nil :color blue)
  "
  toggles
  ────────────────────────────────────────────────
  _t_ theme          _l_ line numbers    _r_ relative #s
  _i_ indent guides  _w_ whitespace      _W_ word wrap
  _c_ fill column    _f_ flycheck        _s_ flyspell
  _d_ dimmer         _b_ beacon          _p_ pulsar
  _T_ treemacs       _n_ neotree         _P_ presentation
  _z_ zen/olivetti   _v_ visual fill
  ────────────────────────────────────────────────
  _q_ quit
"
  ("t" emacs-ide-toggle-theme)
  ("l" display-line-numbers-mode)
  ("r" (setq display-line-numbers-type
             (if (eq display-line-numbers-type 'relative) t 'relative)))
  ("i" highlight-indent-guides-mode)
  ("w" whitespace-mode)
  ("W" word-wrap-whitespace-mode)
  ("c" display-fill-column-indicator-mode)
  ("f" (when (fboundp 'flycheck-mode) (flycheck-mode 'toggle)))
  ("s" (when (fboundp 'flyspell-mode) (flyspell-mode 'toggle)))
  ("d" (when (fboundp 'dimmer-mode) (call-interactively #'dimmer-mode)))
  ("b" (when (fboundp 'beacon-mode) (call-interactively #'beacon-mode)))
  ("p" (when (fboundp 'pulsar-global-mode) (call-interactively #'pulsar-global-mode)))
  ("T" treemacs)
  ("n" neotree-toggle)
  ("P" emacs-ide-presentation-mode)
  ("z" (when (fboundp 'olivetti-mode) (call-interactively #'olivetti-mode)))
  ("v" visual-fill-column-mode)
  ("q" nil))

;; ============================================================================
;; REPL HYDRA — C-c h r
;; ============================================================================
(defhydra hydra-repl (:hint nil :color blue)
  "
  repl
  ────────────────────────────────────────
  _r_ launch / switch   _t_ toggle window
  _s_ send region       _b_ send buffer
  _d_ send defun        _S_ status
  ────────────────────────────────────────
  _q_ quit
"
  ("r" emacs-ide-repl-launch)
  ("t" emacs-ide-repl-toggle-window)
  ("s" emacs-ide-repl-send-region)
  ("b" emacs-ide-repl-send-buffer)
  ("d" emacs-ide-repl-send-defun)
  ("S" emacs-ide-repl-status)
  ("q" nil))

;; ============================================================================
;; SEARCH HYDRA — C-c h s
;; ============================================================================
(defhydra hydra-search (:hint nil :color blue)
  "
  search
  ────────────────────────────────────────────────
  _l_ line          _L_ multi-buffer   _r_ ripgrep
  _g_ grep          _G_ git-grep       _f_ find file
  _i_ imenu         _I_ imenu multi    _o_ outline
  _s_ symbol        _m_ mark           _k_ global mark
  _K_ keep lines    _u_ focus lines
  ────────────────────────────────────────────────
  _q_ quit
"
  ("l" consult-line)
  ("L" consult-line-multi)
  ("r" consult-ripgrep)
  ("g" consult-grep)
  ("G" consult-git-grep)
  ("f" consult-find)
  ("i" consult-imenu)
  ("I" consult-imenu-multi)
  ("o" consult-outline)
  ("s" (when (fboundp 'consult-lsp-symbols) (consult-lsp-symbols)))
  ("m" consult-mark)
  ("k" consult-global-mark)
  ("K" consult-keep-lines)
  ("u" consult-focus-lines)
  ("q" nil))

) ;; end with-eval-after-load 'hydra

;; ============================================================================
;; GLOBAL HYDRA ENTRY KEYS  (C-c h prefix)
;; ============================================================================
(global-set-key (kbd "C-c h w") #'hydra-window/body)
(global-set-key (kbd "C-c h b") #'hydra-buffer/body)
(global-set-key (kbd "C-c h g") #'hydra-git/body)
(global-set-key (kbd "C-c h l") #'hydra-lsp/body)
(global-set-key (kbd "C-c h p") #'hydra-project/body)
(global-set-key (kbd "C-c h t") #'hydra-test/body)
(global-set-key (kbd "C-c h d") #'hydra-debug/body)
(global-set-key (kbd "C-c h u") #'hydra-toggle/body)
(global-set-key (kbd "C-c h r") #'hydra-repl/body)
(global-set-key (kbd "C-c h s") #'hydra-search/body)
;; Quick-access shortcuts
(global-set-key (kbd "C-c h h") (lambda () (interactive)
                                   (message "Hydras: w)indow b)uffer g)it l)sp p)roject t)est d)ebug u)toggle r)epl s)earch")))

(provide 'tools-hydra)
;;; tools-hydra.el ends here
