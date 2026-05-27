;;; emacs-ide-spot-check.el --- Spot-check commands and keybindings -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(defun emacs-ide-spot-check--command-ok-p (fn)
  "Return non-nil if FN is defined and callable."
  (or (fboundp fn)
      (condition-case nil
          (and (indirect-function fn) t)
        (error nil))))

(defun emacs-ide-spot-check--binding-ok-p (actual expected)
  "Return non-nil when ACTUAL key binding equals EXPECTED symbol exactly.
Uses strict symbol equality — no substring matching — so that
`emacs-ide-test-run-all' never falsely satisfies `emacs-ide-test-run'."
  (eq actual expected))

(defun emacs-ide-spot-check ()
  "Spot-check all IDE commands, keybindings, and module features."
  (interactive)
  (let ((cmd-failures  '())
        (key-failures  '())
        (feat-failures '()))

    (with-output-to-temp-buffer "*Emacs IDE Spot Check*"

      (princ "=== EMACS IDE SPOT CHECK ===\n")
      (princ (format "Emacs %s | %s\n\n"
                     emacs-version
                     (format-time-string "%Y-%m-%d %H:%M")))

      ;;; ── COMMANDS (M-x) ───────────────────────────────────────────────────
      (princ "COMMANDS (M-x):\n")
      (dolist (fn
               '(;; Core init helpers
                 emacs-ide-run-tests
                 emacs-ide-show-version
                 emacs-ide-startup-report
                 emacs-ide-reload
                 emacs-ide-purge-bytecode-cache
                 emacs-ide-update
                 emacs-ide-freeze-versions
                 ;; Config
                 emacs-ide-config-reload
                 emacs-ide-reload-config        ;; alias
                 emacs-ide-config-edit
                 emacs-ide-config-show
                 ;; Diagnostics
                 emacs-ide-diagnose
                 emacs-ide-diagnose-lsp
                 emacs-ide-diagnose-languages
                 ;; Health
                 emacs-ide-health-check-all
                 emacs-ide-health-status        ;; alias
                 emacs-ide-health-auto-fix
                 ;; LSP
                 emacs-ide-lsp-status
                 emacs-ide-lsp-check-servers
                 ;; Project detect
                 emacs-ide-detect-show-status
                 emacs-ide-detect-current-project
                 emacs-ide-detect-reset-cache
                 ;; REPL
                 emacs-ide-repl-launch
                 emacs-ide-repl-send-region
                 emacs-ide-repl-send-buffer
                 emacs-ide-repl-send-defun
                 emacs-ide-repl-send-line
                 emacs-ide-repl-toggle-window
                 emacs-ide-repl-status
                 ;; Tests
                 emacs-ide-test-run
                 emacs-ide-test-run-all
                 emacs-ide-test-run-file
                 emacs-ide-test-run-project
                 emacs-ide-test-run-at-point
                 emacs-ide-test-run-last
                 emacs-ide-test-report
                 emacs-ide-test-watch
                 emacs-ide-test-runner-status
                 ;; Theme
                 emacs-ide-toggle-theme
                 emacs-ide-select-theme
                 emacs-ide-theme-enable-auto
                 emacs-ide-theme-disable-auto
                 ;; UI
                 emacs-ide-presentation-mode
                 emacs-ide-show-keybindings-help
                 ;; Workspace
                 emacs-ide-workspace-status
                 emacs-ide-workspace-switch-by-index
                 ;; Git
                 emacs-ide-git-status
                 emacs-ide-git-stage-file
                 ;; REST
                 emacs-ide-rest-scratch
                 emacs-ide-rest-insert-request
                 ;; Recovery
                 emacs-ide-recovery-report
                 emacs-ide-recovery-backup-config
                 emacs-ide-recovery-restore-config
                 emacs-ide-recovery-view-log
                 emacs-ide-recovery-mode
                 emacs-ide-recovery-disable-package
                 emacs-ide-recovery-reset-crash-count
                 ;; Security
                 emacs-ide-security-check
                 emacs-ide-security-harden
                 ;; Profiler
                 emacs-ide-profile-startup
                 emacs-ide-profile-start
                 emacs-ide-profile-report
                 emacs-ide-profile-stop
                 emacs-ide-profile-reset
                 emacs-ide-early-init-report
                 ;; Package tracking
                 emacs-ide-package-report
                 emacs-ide-package-clear-times
                 ;; Telemetry
                 emacs-ide-telemetry-report
                 emacs-ide-telemetry-clear
                 emacs-ide-telemetry-enable
                 emacs-ide-telemetry-disable
                 ;; Formatters
                 emacs-ide-check-formatters
                 ;; Modal editing
                 emacs-ide-toggle-meow
                 emacs-ide-install-meow
                 ;; Spelling
                 emacs-ide-spell-toggle
                 emacs-ide-spell-buffer
                 ;; Terminal — real public functions, not just the bound lambdas
                 emacs-ide-vterm-here
                 emacs-ide-vterm-toggle
                 emacs-ide-vterm-project
                 emacs-ide-eshell-here
                 ;; Project helpers
                 emacs-ide-project-info
                 emacs-ide-project-compile
                 emacs-ide-project-run
                 emacs-ide-project-test
                 ;; Debug helpers
                 emacs-ide-debug-toggle-breakpoint
                 emacs-ide-debug-repl
                 ;; Hydra bodies (inside with-eval-after-load 'hydra)
                 hydra-window/body
                 hydra-buffer/body
                 hydra-git/body
                 hydra-lsp/body
                 hydra-project/body
                 hydra-test/body
                 hydra-debug/body
                 hydra-toggle/body
                 hydra-repl/body
                 hydra-search/body))
        (let ((ok (emacs-ide-spot-check--command-ok-p fn)))
          (unless ok (push fn cmd-failures))
          (princ (format "  %s %s\n" (if ok "✓" "✗") fn))))

      ;;; ── KEYBINDINGS — DIRECT (symbol equality) ───────────────────────────
      (princ "\nKEYBINDINGS (direct — verified by symbol eq):\n")
      (dolist (entry
               '(;; REPL (C-c x)
                 ("C-c x r"   emacs-ide-repl-launch)
                 ("C-c x s"   emacs-ide-repl-send-region)
                 ("C-c x b"   emacs-ide-repl-send-buffer)
                 ("C-c x d"   emacs-ide-repl-send-defun)
                 ("C-c x l"   emacs-ide-repl-send-line)
                 ("C-c x t"   emacs-ide-repl-toggle-window)
                 ("C-c x R"   emacs-ide-test-report)
                 ;; Tests (C-c X)
                 ("C-c X f"   emacs-ide-test-run-file)
                 ("C-c X p"   emacs-ide-test-run-project)
                 ("C-c X ."   emacs-ide-test-run-at-point)
                 ("C-c X w"   emacs-ide-test-watch)
                 ("C-c X s"   emacs-ide-test-runner-status)
                 ("C-c X l"   emacs-ide-test-run-last)
                 ("C-c C-t"   emacs-ide-test-run)
                 ("C-c C-T"   emacs-ide-test-run-all)
                 ;; REST (C-c V) — FIX #63: now canonical in keybindings.el
                 ("C-c V s"   emacs-ide-rest-scratch)
                 ("C-c V i"   emacs-ide-rest-insert-request)
                 ;; Recovery (C-c r)
                 ("C-c r r"   emacs-ide-recovery-report)
                 ("C-c r v"   emacs-ide-recovery-view-log)
                 ("C-c r b"   emacs-ide-recovery-backup-config)
                 ("C-c r d"   emacs-ide-recovery-disable-package)
                 ("C-c r C-r" emacs-ide-recovery-reset-crash-count)
                 ;; Notes (C-c n) — FIX #2: neotree moved to C-c n n
                 ("C-c n n"   neotree-toggle)
                 ;; Project detect / IDE utility
                 ("C-c D d"   emacs-ide-detect-show-status)
                 ;; Workspaces (set by ui-workspace.el :bind)
                 ("C-c W s"   persp-switch)
                 ("C-c W n"   persp-new)
                 ("C-c W k"   persp-kill)
                 ("C-c W r"   persp-rename)
                 ;; Git (set by keybindings.el and tools-git.el)
                 ("C-x g"     magit-status)
                 ("C-x M-g"   magit-dispatch)
                 ;; Compile
                 ("C-c B"     compile)
                 ("C-c b"     recompile)
                 ;; Org
                 ("C-c a"     org-agenda)
                 ("C-c c"     org-capture)
                 ("C-c l"     org-store-link)
                 ;; IDE misc
                 ("C-c R"     emacs-ide-reload-config)
                 ("C-c L"     emacs-ide-lsp-status)
                 ("C-c H"     emacs-ide-show-keybindings-help)
                 ("C-c P"     emacs-ide-presentation-mode)
                 ("C-c ?"     which-key-show-top-level)
                 ("<f12>"     emacs-ide-toggle-theme)))
        (let* ((key      (car entry))
               (expected (cadr entry))
               (actual   (with-temp-buffer (key-binding (kbd key))))
               (ok       (emacs-ide-spot-check--binding-ok-p actual expected)))
          (unless ok (push (list key expected actual) key-failures))
          (princ (format "  %s %-14s → %-44s%s\n"
                         (if ok "✓" "✗")
                         key
                         (cond
                          ((null actual)        "UNBOUND")
                          ((eq actual expected) (symbol-name actual))
                          ((symbolp actual)     (symbol-name actual))
                          (t                    "lambda/compiled"))
                         (if (and (not ok) actual)
                             (format " [expected %s]" expected)
                           "")))))

      ;;; ── KEYBINDINGS — LAMBDA-BOUND (non-nil check) ───────────────────────
      (princ "\nKEYBINDINGS (lambda-bound — verified as non-nil):\n")
      (dolist (entry
               '(;; Hydra menus (C-c h)
                 ("C-c h w" "hydra-window/body")
                 ("C-c h b" "hydra-buffer/body")
                 ("C-c h g" "hydra-git/body")
                 ("C-c h l" "hydra-lsp/body")
                 ("C-c h p" "hydra-project/body")
                 ("C-c h t" "hydra-test/body")
                 ("C-c h d" "hydra-debug/body")
                 ("C-c h u" "hydra-toggle/body")
                 ("C-c h r" "hydra-repl/body")
                 ("C-c h s" "hydra-search/body")
                 ;; Profiler (C-c D)
                 ("C-c D s" "emacs-ide-profile-start")
                 ("C-c D r" "emacs-ide-profile-report")
                 ("C-c D q" "emacs-ide-profile-stop")
                 ("C-c t"   "emacs-ide-vterm-here (lambda guard)")
                 ("C-c e"   "emacs-ide-eshell-here (lambda guard)")
                 ("C-c n /" "emacs-ide-notes-search (lambda guard)")))
        (let* ((key    (car entry))
               (label  (cadr entry))
               (actual (with-temp-buffer (key-binding (kbd key))))
               (ok     (not (null actual))))
          (unless ok (push (list key 'lambda-bound nil) key-failures))
          (princ (format "  %s %-14s → %-30s  %s\n"
                         (if ok "✓" "✗")
                         key
                         (if ok "lambda/compiled" "UNBOUND")
                         label))))

      ;;; ── MODULE FEATURES ──────────────────────────────────────────────────
      (princ "\nMODULE FEATURES PROVIDED:\n")
      (dolist (feat
               '(;; Core
                 emacs-ide-config
                 emacs-ide-health
                 emacs-ide-recovery
                 emacs-ide-package
                 emacs-ide-profiler
                 emacs-ide-security
                 emacs-ide-telemetry
                 emacs-ide-test
                 emacs-ide-spot-check
                 emacs-ide-diagnose
                 ;; UI
                 ui-core ui-theme ui-modeline
                 ui-dashboard ui-workspace
                 ;; Completion
                 completion-core completion-snippets
                 ;; Editing
                 editing-core
                 ;; Development core
                 core-dev
                 ;; Tools
                 tools-lsp
                 tools-project
                 tools-git
                 tools-terminal
                 tools-format
                 apheleia-langs-patch
                 tools-org
                 tools-spelling
                 tools-notes
                 tools-rest
                 tools-test-runner-registry
                 tools-test
                 debug-core
                 tools-repl
                 tools-project-detect
                 tools-hydra
                 keybindings))
        (let ((ok (featurep feat)))
          (unless ok (push feat feat-failures))
          (princ (format "  %s %s\n" (if ok "✓" "✗") feat))))

      ;;; ── SUMMARY ──────────────────────────────────────────────────────────
      (princ "\n=== SUMMARY ===\n")
      (let ((total (+ (length cmd-failures)
                      (length key-failures)
                      (length feat-failures))))
        (if (= total 0)
            (princ "✓ ALL CHECKS PASSED — everything is working correctly.\n")

          (princ (format "✗ %d issue%s found:\n"
                         total (if (= total 1) "" "s")))

          (when feat-failures
            (princ "\nFAILED MODULE FEATURES:\n")
            (dolist (f feat-failures)
              (princ (format "  ✗ %s\n" f)))
            (princ "  Action: Check *Messages* for load errors.\n")
            (when (memq 'emacs-ide-test feat-failures)
              (princ "  emacs-ide-test: confirm core/emacs-ide-test.el exists.\n")
              (princ "  Run: M-x emacs-ide-recovery-view-log\n")))

          (when cmd-failures
            (princ "\nMISSING COMMANDS:\n")
            (dolist (f cmd-failures)
              (princ (format "  ✗ %s\n" f)))
            (princ "  Action:\n")
            (princ "    If from init.el: confirm init.el is the patched version.\n")
            (princ "    If emacs-ide-run-tests: emacs-ide-test must be ✓ above.\n"))

          (when key-failures
            (princ "\nWRONG / MISSING KEYBINDINGS:\n")
            (dolist (entry key-failures)
              (princ (format "  ✗ %-14s  expected: %-30s  got: %s\n"
                             (nth 0 entry)
                             (nth 1 entry)
                             (or (nth 2 entry) "UNBOUND"))))
            (princ "  Action: C-h k <key> to inspect live binding.\n")
            (princ "  Common cause: module failed to load — check feat-failures above.\n"))))

      ;;; ── NEXT STEPS ───────────────────────────────────────────────────────
      (princ "\n--- NEXT STEPS ---\n")
      (princ "  M-x emacs-ide-run-tests          full ERT logic suite\n")
      (princ "  M-x emacs-ide-diagnose           per-module file+feature+cmd\n")
      (princ "  M-x emacs-ide-health-check-all   system tools + LSP + formatters\n")
      (princ "  M-x emacs-ide-security-check     TLS + GPG + auth-source\n")
      (princ "  M-x emacs-ide-startup-report     startup phase timings\n"))))


(when (boundp 'emacs-ide--spot-check-run)
  (fset 'emacs-ide--spot-check-run #'emacs-ide-spot-check))

(provide 'emacs-ide-spot-check)
;;; emacs-ide-spot-check.el ends here
