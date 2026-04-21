;;; emacs-ide-spot-check.el --- Spot-check commands and keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Version: 3.2.1 | FIX: Added emacs-ide-diagnose-languages, emacs-ide-update,
;;;           emacs-ide-freeze-versions, emacs-ide-lsp-check-servers to command
;;;           list — all now defined in their respective modules.
;;; Code:

(defun emacs-ide-spot-check--command-ok-p (fn)
  "Return non-nil if FN is defined and callable."
  (or (fboundp fn)
      (condition-case nil
          (and (indirect-function fn) t)
        (error nil))))

(defun emacs-ide-spot-check--binding-ok-p (actual expected)
  "Return non-nil if ACTUAL binding satisfies EXPECTED."
  (or (eq actual expected)
      (and actual
           (string-match-p (regexp-quote (symbol-name expected))
                           (format "%s" actual)))))

(defun emacs-ide-spot-check ()
  "Spot-check all commands and keybindings."
  (interactive)
  (let ((cmd-failures '())
        (key-failures '())
        (feat-failures '()))

    (with-output-to-temp-buffer "*Emacs IDE Spot Check*"

      (princ "=== EMACS IDE SPOT CHECK ===\n")
      (princ (format "Emacs %s | %s\n\n"
                     emacs-version (format-time-string "%Y-%m-%d %H:%M")))

      (princ "COMMANDS (M-x):\n")
      (dolist (fn '(emacs-ide-run-tests
                    emacs-ide-diagnose
                    emacs-ide-diagnose-lsp
                    emacs-ide-diagnose-languages
                    emacs-ide-show-version
                    emacs-ide-startup-report
                    emacs-ide-purge-bytecode-cache
                    emacs-ide-reload
                    emacs-ide-config-reload
                    emacs-ide-reload-config
                    emacs-ide-config-edit
                    emacs-ide-config-show
                    emacs-ide-health-check-all
                    emacs-ide-health-auto-fix
                    emacs-ide-lsp-status
                    emacs-ide-lsp-check-servers
                    emacs-ide-detect-show-status
                    emacs-ide-detect-current-project
                    emacs-ide-detect-reset-cache
                    emacs-ide-repl-launch
                    emacs-ide-repl-send-region
                    emacs-ide-repl-send-buffer
                    emacs-ide-repl-send-defun
                    emacs-ide-repl-toggle-window
                    emacs-ide-repl-status
                    emacs-ide-test-run
                    emacs-ide-test-run-file
                    emacs-ide-test-run-project
                    emacs-ide-test-run-at-point
                    emacs-ide-test-run-last
                    emacs-ide-test-report
                    emacs-ide-test-watch
                    emacs-ide-test-runner-status
                    emacs-ide-toggle-theme
                    emacs-ide-presentation-mode
                    emacs-ide-show-keybindings-help
                    emacs-ide-workspace-status
                    emacs-ide-workspace-switch-by-index
                    emacs-ide-git-status
                    emacs-ide-git-stage-file
                    emacs-ide-rest-scratch
                    emacs-ide-rest-insert-request
                    emacs-ide-recovery-report
                    emacs-ide-recovery-backup-config
                    emacs-ide-recovery-view-log
                    emacs-ide-security-check
                    emacs-ide-security-harden
                    emacs-ide-profile-start
                    emacs-ide-profile-report
                    emacs-ide-profile-stop
                    emacs-ide-profile-reset
                    emacs-ide-early-init-report
                    emacs-ide-package-report
                    emacs-ide-package-clear-times
                    emacs-ide-recovery-mode
                    emacs-ide-recovery-disable-package
                    emacs-ide-recovery-reset-crash-count
                    emacs-ide-check-formatters
                    emacs-ide-telemetry-report
                    emacs-ide-telemetry-clear
                    emacs-ide-update
                    emacs-ide-freeze-versions))
        (let ((ok (emacs-ide-spot-check--command-ok-p fn)))
          (unless ok (push fn cmd-failures))
          (princ (format "  %s %s\n" (if ok "✓" "✗") fn))))

      (princ "\nKEYBINDINGS:\n")
      (dolist (entry
               '(("C-c x r" emacs-ide-repl-launch)
                 ("C-c x s" emacs-ide-repl-send-region)
                 ("C-c x b" emacs-ide-repl-send-buffer)
                 ("C-c x d" emacs-ide-repl-send-defun)
                 ("C-c x t" emacs-ide-repl-toggle-window)
                 ("C-c x R" emacs-ide-test-report)
                 ("C-c X f" emacs-ide-test-run-file)
                 ("C-c X p" emacs-ide-test-run-project)
                 ("C-c X ." emacs-ide-test-run-at-point)
                 ("C-c X w" emacs-ide-test-watch)
                 ("C-c X s" emacs-ide-test-runner-status)
                 ("C-c X l" emacs-ide-test-run-last)
                 ("C-c V s" emacs-ide-rest-scratch)
                 ("C-c V i" emacs-ide-rest-insert-request)
                 ("C-c D d" emacs-ide-detect-show-status)
                 ("C-c D s" emacs-ide-profile-start)
                 ("C-c D r" emacs-ide-profile-report)
                 ("C-c D q" emacs-ide-profile-stop)
                 ("C-c R"   emacs-ide-reload-config)
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
                 ("C-c W s" persp-switch)
                 ("C-c W n" persp-new)
                 ("C-c W k" persp-kill)
                 ("C-c W r" persp-rename)
                 ("C-c r r"   emacs-ide-recovery-report)
                 ("C-c r v"   emacs-ide-recovery-view-log)
                 ("C-c r b"   emacs-ide-recovery-backup-config)
                 ("C-c r d"   emacs-ide-recovery-disable-package)
                 ("C-c r C-r" emacs-ide-recovery-reset-crash-count)
                 ("C-x g"   magit-status)
                 ("C-x M-g" magit-dispatch)
                 ("C-c B"   compile)
                 ("C-c b"   recompile)
                 ("C-c C-t" emacs-ide-test-run)
                 ("C-c t"   emacs-ide-vterm-here)
                 ("C-c e"   emacs-ide-eshell-here)
                 ("C-c a"   org-agenda)
                 ("C-c c"   org-capture)
                 ("C-c l"   org-store-link)
                 ("<f12>"   emacs-ide-toggle-theme)
                 ("C-c L"   emacs-ide-lsp-status)
                 ("C-c H"   emacs-ide-show-keybindings-help)
                 ("C-c P"   emacs-ide-presentation-mode)
                 ("C-c ?"   which-key-show-top-level)))
        (let* ((key      (car entry))
               (expected (cadr entry))
               (actual   (with-temp-buffer (key-binding (kbd key))))
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

      (princ "\nMODULE FEATURES PROVIDED:\n")
      (dolist (feat '(emacs-ide-config
                      emacs-ide-health
                      emacs-ide-recovery
                      emacs-ide-package
                      emacs-ide-profiler
                      emacs-ide-security
                      emacs-ide-telemetry
                      emacs-ide-test
                      emacs-ide-spot-check
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
            (princ "    If from init.el: Confirm patched init.el in ~/.emacs.d/init.el\n")
            (princ "    If emacs-ide-run-tests: emacs-ide-test feature must be ✓ above\n"))

          (when key-failures
            (princ "\nWRONG KEYBINDINGS:\n")
            (dolist (entry key-failures)
              (princ (format "  ✗ %-14s  expected: %s  got: %s\n"
                             (nth 0 entry) (nth 1 entry)
                             (or (nth 2 entry) "UNBOUND"))))
            (princ "  Action: C-h k <key> to inspect.\n"))))

      (princ "\n--- NEXT STEPS ---\n")
      (princ "  M-x emacs-ide-run-tests         full ERT logic suite\n")
      (princ "  M-x emacs-ide-diagnose          per-module file+feature+cmd\n")
      (princ "  M-x emacs-ide-health-check-all  system tools + LSP + formatters\n")
      (princ "  M-x emacs-ide-security-check    TLS + GPG + auth-source\n")
      (princ "  M-x emacs-ide-startup-report    startup phase timings\n"))))

(provide 'emacs-ide-spot-check)
;;; emacs-ide-spot-check.el ends here
