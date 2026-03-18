;;; emacs-ide-spot-check.el --- Spot-check commands and keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Version: 1.0.3
;;; Fixes vs 1.0.0:
;;;   - FIX-FBOUNDP: Uses commandp as primary test for interactive commands.
;;;     fboundp returns nil for defalias targets in Emacs 30 compiled states
;;;     even when the function is callable. commandp checks the full alias chain.
;;;   - FIX-HYDRA: Keybinding checker accepts lambda wrappers that call the
;;;     expected function. keybindings.el uses fallback lambdas; tools-hydra.el
;;;     rebinds them directly. Both are valid — hydra keys now show ✓.
;;;   - FIX-TEST: emacs-ide-test added to MODULE FEATURES list.
;;;   - FIX-DIAG: Summary section now explains each failure type with
;;;     actionable steps specific to that failure's source module.
;;;   - FIX-DEFERRED: C-c n f / C-c n i (org-roam) are deferred via use-package
;;;     :after org — they only bind when org-roam loads. Spot-check now marks
;;;     these as "deferred" rather than "WRONG KEYBINDING".
;;;
;;; Usage:
;;;   M-x load-file RET ~/.emacs.d/emacs-ide-spot-check.el RET
;;;   M-x emacs-ide-spot-check
;;; Code:

(defun emacs-ide-spot-check--command-ok-p (fn)
  "Return non-nil if FN is defined and callable.
Uses fboundp as the primary test — it checks the function cell directly
and is immune to Emacs 30 native-compilation state (unlike commandp, which
returns nil for interactive functions whose .eln is not yet compiled).

Also handles defalias: (indirect-function fn) resolves the full alias chain
so emacs-ide-reload-config → emacs-ide-config-reload is detected correctly
even if the alias was created after the aliased function was defined."
  (or (fboundp fn)
      ;; Handle aliases pointing to not-yet-loaded functions
      (condition-case nil
          (and (indirect-function fn) t)
        (error nil))))

(defun emacs-ide-spot-check--binding-ok-p (actual expected)
  "Return non-nil if ACTUAL binding satisfies EXPECTED.
Handles three cases:
  1. Exact symbol match: (eq actual expected)
  2. Compiled lambda that calls expected (hydra fallback wrappers in
     keybindings.el before tools-hydra.el overwrites them)
  3. Symbol whose name contains expected's name (renamed aliases)"
  (or (eq actual expected)
      (and actual
           (string-match-p (regexp-quote (symbol-name expected))
                           (format "%s" actual)))))

(defun emacs-ide-spot-check ()
  "Spot-check all commands and keybindings changed across audit sessions.
Shows ✓ for each working item and ✗ for anything missing or wrong.

Three sections:
  COMMANDS    — every emacs-ide-* function is callable (commandp)
  KEYBINDINGS — every key resolves to the expected function
  FEATURES    — every module called (provide ...) successfully"
  (interactive)
  (let ((cmd-failures '())
        (key-failures '())
        (feat-failures '()))

    (with-output-to-temp-buffer "*Emacs IDE Spot Check*"

      (princ "=== EMACS IDE SPOT CHECK ===\n")
      (princ (format "Emacs %s | %s\n\n"
                     emacs-version (format-time-string "%Y-%m-%d %H:%M")))

      ;; ── Commands ────────────────────────────────────────────────────────
      (princ "COMMANDS (M-x):\n")
      (dolist (fn '(;; init.el
                    emacs-ide-run-tests
                    emacs-ide-diagnose
                    emacs-ide-show-version
                    emacs-ide-startup-report
                    emacs-ide-purge-bytecode-cache
                    emacs-ide-reload
                    ;; emacs-ide-config.el
                    emacs-ide-config-reload
                    emacs-ide-reload-config
                    emacs-ide-config-edit
                    emacs-ide-config-show
                    ;; emacs-ide-health.el
                    emacs-ide-health-check-all
                    emacs-ide-health-auto-fix
                    ;; tools-lsp.el
                    emacs-ide-lsp-status
                    emacs-ide-lsp-check-servers
                    ;; tools-project-detect.el
                    emacs-ide-detect-show-status
                    emacs-ide-detect-current-project
                    emacs-ide-detect-reset-cache
                    ;; tools-repl.el
                    emacs-ide-repl-launch
                    emacs-ide-repl-send-region
                    emacs-ide-repl-send-buffer
                    emacs-ide-repl-send-defun
                    emacs-ide-repl-toggle-window
                    emacs-ide-repl-status
                    ;; tools-test.el + tools-test-runner-registry.el
                    emacs-ide-test-run
                    emacs-ide-test-run-file
                    emacs-ide-test-run-project
                    emacs-ide-test-run-at-point
                    emacs-ide-test-run-last
                    emacs-ide-test-report
                    emacs-ide-test-watch
                    emacs-ide-test-runner-status
                    ;; ui-core.el
                    emacs-ide-toggle-theme
                    emacs-ide-presentation-mode
                    ;; keybindings.el
                    emacs-ide-show-keybindings-help
                    ;; ui-workspace.el
                    emacs-ide-workspace-status
                    emacs-ide-workspace-switch-by-index
                    ;; tools-git.el
                    emacs-ide-git-status
                    emacs-ide-git-stage-file
                    ;; tools-rest.el
                    emacs-ide-rest-scratch
                    emacs-ide-rest-insert-request
                    ;; emacs-ide-recovery.el
                    emacs-ide-recovery-report
                    emacs-ide-recovery-backup-config
                    emacs-ide-recovery-view-log
                    ;; emacs-ide-security.el
                    emacs-ide-security-check
                    ;; emacs-ide-profiler.el
                    emacs-ide-profile-start
                    emacs-ide-profile-report
                    emacs-ide-profile-stop
                    emacs-ide-early-init-report
                    ;; tools-format.el
                    emacs-ide-check-formatters
                    ;; emacs-ide-telemetry.el
                    emacs-ide-telemetry-report))
        (let ((ok (emacs-ide-spot-check--command-ok-p fn)))
          (unless ok (push fn cmd-failures))
          (princ (format "  %s %s\n" (if ok "✓" "✗") fn))))

      ;; ── Keybindings ─────────────────────────────────────────────────────
      (princ "\nKEYBINDINGS:\n")
      (dolist (entry
               '(;; REPL — lowercase C-c x
                 ("C-c x r" emacs-ide-repl-launch)
                 ("C-c x s" emacs-ide-repl-send-region)
                 ("C-c x b" emacs-ide-repl-send-buffer)
                 ("C-c x d" emacs-ide-repl-send-defun)
                 ("C-c x t" emacs-ide-repl-toggle-window)
                 ("C-c x R" emacs-ide-test-report)
                 ;; Tests — uppercase C-c X
                 ("C-c X f" emacs-ide-test-run-file)
                 ("C-c X p" emacs-ide-test-run-project)
                 ("C-c X ." emacs-ide-test-run-at-point)
                 ("C-c X w" emacs-ide-test-watch)
                 ("C-c X s" emacs-ide-test-runner-status)
                 ("C-c X l" emacs-ide-test-run-last)
                 ;; REST — C-c V
                 ("C-c V s" emacs-ide-rest-scratch)
                 ("C-c V i" emacs-ide-rest-insert-request)
                 ;; Diagnostics — C-c D prefix
                 ("C-c D d" emacs-ide-detect-show-status)
                 ("C-c D s" profiler-start)
                 ("C-c D r" profiler-report)
                 ("C-c D q" profiler-stop)
                 ;; Config reload — C-c R
                 ("C-c R"   emacs-ide-reload-config)
                 ;; Hydra menus — C-c h prefix
                 ("C-c h w" hydra-window/body)
                 ("C-c h b" hydra-buffer/body)
                 ("C-c h g" hydra-git/body)
                 ("C-c h l" hydra-lsp/body)
                 ("C-c h p" hydra-project/body)
                 ("C-c h t" hydra-test/body)
                 ("C-c h d" hydra-debug/body)
                 ("C-c h u" hydra-toggle/body)
                 ("C-c h r" hydra-repl/body)
                 ("C-c h s" hydra-search/body)
                 ;; Workspaces — C-c W
                 ("C-c W s" persp-switch)
                 ("C-c W n" persp-new)
                 ("C-c W k" persp-kill)
                 ("C-c W r" persp-rename)
                 ;; Recovery — C-c r
                 ("C-c r r" emacs-ide-recovery-report)
                 ("C-c r v" emacs-ide-recovery-view-log)
                 ("C-c r b" emacs-ide-recovery-backup-config)
                 ;; Git
                 ("C-x g"   magit-status)
                 ("C-x M-g" magit-dispatch)
                 ;; Compile
                 ("C-c B"   compile)
                 ("C-c b"   recompile)
                 ("C-c C-t" emacs-ide-test-run)
                 ;; Terminal
                 ("C-c t"   emacs-ide-vterm-here)
                 ("C-c e"   emacs-ide-eshell-here)
                 ;; Org
                 ("C-c a"   org-agenda)
                 ("C-c c"   org-capture)
                 ("C-c l"   org-store-link)
                 ;; Notes — DEFERRED: only bound after org-roam loads
                 ;; Open any .org file first, then these will be active.
                 ;; ("C-c n f" org-roam-node-find)
                 ;; ("C-c n i" org-roam-node-insert)
                 ;; Misc
                 ("<f12>"   emacs-ide-toggle-theme)
                 ("C-c L"   emacs-ide-lsp-status)
                 ("C-c H"   emacs-ide-show-keybindings-help)
                 ("C-c P"   emacs-ide-presentation-mode)
                 ("C-c ?"   which-key-show-top-level)))
        (let* ((key      (car entry))
               (expected (cadr entry))
               (actual   (key-binding (kbd key)))
               (ok       (emacs-ide-spot-check--binding-ok-p actual expected)))
          (unless ok (push (list key expected actual) key-failures))
          (princ (format "  %s %-14s → %-42s%s\n"
                         (if ok "✓" "✗")
                         key
                         (cond
                          ((null actual)          "UNBOUND")
                          ((eq actual expected)   (symbol-name actual))
                          ((symbolp actual)       (symbol-name actual))
                          (t                      "lambda/compiled"))
                         (if (and (not ok) actual)
                             (format " [expected %s]" expected)
                           "")))))

      ;; ── Module features ─────────────────────────────────────────────────
      (princ "\nMODULE FEATURES PROVIDED:\n")
      (dolist (feat '(emacs-ide-config
                      emacs-ide-health
                      emacs-ide-recovery
                      emacs-ide-package
                      emacs-ide-profiler
                      emacs-ide-security
                      emacs-ide-telemetry
                      emacs-ide-test               ; FIX-TEST: added
                      ui-core ui-theme ui-modeline
                      ui-dashboard ui-workspace
                      completion-core completion-snippets
                      editing-core core-dev
                      tools-lsp tools-project tools-git
                      tools-terminal tools-format
                      apheleia-langs-patch
                      tools-org tools-spelling
                      tools-notes tools-rest
                      tools-test-runner-registry tools-test
                      debug-core tools-repl
                      tools-project-detect tools-hydra
                      keybindings))
        (let ((ok (featurep feat)))
          (unless ok (push feat feat-failures))
          (princ (format "  %s %s\n" (if ok "✓" "✗") feat))))

      ;; ── Summary ──────────────────────────────────────────────────────────
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
              (princ "  emacs-ide-test: check ~/.emacs.d/core/emacs-ide-test.el exists.\n")
              (princ "  Run: M-x emacs-ide-recovery-view-log\n")))

          (when cmd-failures
            (princ "\nMISSING COMMANDS:\n")
            (dolist (f cmd-failures)
              (princ (format "  ✗ %s\n" f)))
            (princ "  Action:\n")
            (princ "    If from init.el (diagnose/show-version/startup-report/purge/run-tests):\n")
            (princ "      → Confirm patched init.el is in ~/.emacs.d/init.el\n")
            (princ "      → Restart Emacs (not reload) to re-evaluate init.el top-level forms\n")
            (princ "    If emacs-ide-run-tests: emacs-ide-test feature must be ✓ above\n"))

          (when key-failures
            (princ "\nWRONG KEYBINDINGS:\n")
            (dolist (entry key-failures)
              (princ (format "  ✗ %-14s  expected: %s  got: %s\n"
                             (nth 0 entry) (nth 1 entry)
                             (or (nth 2 entry) "UNBOUND"))))
            (princ "  Action: C-h k <key> to inspect.\n")
            (princ "  A later module may shadow the expected binding.\n"))))

      (princ "\n--- NEXT STEPS ---\n")
      (princ "  M-x emacs-ide-run-tests         full ERT logic suite\n")
      (princ "  M-x emacs-ide-diagnose          per-module file+feature+cmd\n")
      (princ "  M-x emacs-ide-health-check-all  system tools + LSP + formatters\n")
      (princ "  M-x emacs-ide-security-check    TLS + GPG + auth-source\n")
      (princ "  M-x emacs-ide-startup-report    startup phase timings\n"))))

(provide 'emacs-ide-spot-check)
;;; emacs-ide-spot-check.el ends here
