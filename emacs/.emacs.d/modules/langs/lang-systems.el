;;; lang-systems.el --- Systems Languages IDE layer (Zig / Nix / D / V) -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.1 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.1 to 3.0.4.
;;;   - FIX-D-REGISTER: "d" was not registered with emacs-ide-dev-register —
;;;     lang-enabled-p "d" always returned t regardless of config. Added.
;;;   - FIX-V-REGISTER: "v" was not registered — same class of bug. Added.
;;;   - FIX-V-IN-GUARD: v-mode was only loaded when "d" was enabled (wrong).
;;;     The outer when guard now includes "v" explicitly.
;;;   - FIX-DEFUN-IN-CONFIG-NIX: emacs-ide-nix-repl was defined inside
;;;     nix-mode :config — not visible to M-x until nix-mode loads.
;;;     Moved to top-level defun.
;;;   - FIX-REPL-ATTACH-NIX: emacs-ide-dev-attach-repl requires an explicit
;;;     key arg (core-dev.el FIX-DEFAULT-REPL-KEY). Added (kbd "C-c x r").
;;;   - FIX-REPL-REGISTER-NIX: Added explicit emacs-ide-repl-register for
;;;     nix-mode inside nix-mode :config for reliable C-c x r dispatch.
;;;   - FIX-ZIG-FORMAT-CONFLICT: zig-format-on-save t ran zig fmt on save
;;;     while apheleia-langs-patch.el also registered zigfmt for zig-mode —
;;;     double format on every save. zig-format-on-save now set to nil;
;;;     apheleia owns formatting via the patch.
;;;   - FIX-TEST-REGISTER-ZIG: Added emacs-ide-test-register-runner for
;;;     zig-mode using zig test / zig build test.
;;;   - FIX-ZIG-LAMBDA-BIND: Inline lambda in emacs-ide-dev-bind-compile
;;;     replaced with named defun emacs-ide-zig-build for M-x visibility.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-LSP-GUARD: (bound-and-true-p emacs-ide-lsp-enable) :if guards.
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "zig" :tier 3 :lsp-server "zls"
  :formatter "zig fmt" :test-cmd "zig test" :repl nil :modes '(zig-mode))
(emacs-ide-dev-register "nix" :tier 3 :lsp-server "nil"
  :formatter "nixpkgs-fmt" :test-cmd nil :repl "nix repl" :modes '(nix-mode))
;; FIX-D-REGISTER + FIX-V-REGISTER: were missing — lang-enabled-p always t
(emacs-ide-dev-register "d" :tier 3 :lsp-server nil
  :formatter nil :test-cmd "dub test" :repl nil :modes '(d-mode))
(emacs-ide-dev-register "v" :tier 3 :lsp-server nil
  :formatter "v fmt" :test-cmd "v test" :repl nil :modes '(v-mode))

;; FIX-V-IN-GUARD: "v" added to the outer when condition
(when (or (emacs-ide-dev-lang-enabled-p "zig")
          (emacs-ide-dev-lang-enabled-p "nix")
          (emacs-ide-dev-lang-enabled-p "d")
          (emacs-ide-dev-lang-enabled-p "v"))

;; ============================================================================
;; Zig
;; FIX-ZIG-LAMBDA-BIND: named defuns for M-x visibility.
;; ============================================================================
(defun emacs-ide-zig-build ()
  "Build the Zig project via zig build."
  (interactive)
  (if (executable-find "zig")
      (compile "zig build")
    (message "lang-systems: zig not found")))

(defun emacs-ide-zig-test-project ()
  "Run Zig project tests via zig build test."
  (interactive)
  (if (executable-find "zig")
      (compile "zig build test")
    (message "lang-systems: zig not found")))

(defun emacs-ide-zig-test-file ()
  "Run zig test on the current file."
  (interactive)
  (if (and (executable-find "zig") (buffer-file-name))
      (compile (format "zig test %s"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-systems: zig not found or no file")))

(use-package zig-mode
  :if (and (emacs-ide-dev-lang-enabled-p "zig") (executable-find "zig"))
  :defer t
  :mode "\\.zig\\'"
  :init
  ;; FIX-ZIG-FORMAT-CONFLICT: set nil — apheleia-langs-patch.el owns formatting
  (setq zig-format-on-save nil)
  :config
  (emacs-ide-dev-bind-compile zig-mode-map #'emacs-ide-zig-build)
  ;; FIX-TEST-REGISTER-ZIG: register with test runner registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'zig-mode
      :file-fn    #'emacs-ide-zig-test-file
      :project-fn #'emacs-ide-zig-test-project)))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "zig")
           (executable-find "zls"))
  :hook (zig-mode . lsp-deferred))

(with-eval-after-load 'dap-mode
  (when (emacs-ide-dev-lang-enabled-p "zig")
    (emacs-ide-dev-attach-dap "Zig :: LLDB" 'dap-lldb)))

(with-eval-after-load 'tools-test
  (when (and (fboundp 'emacs-ide-test-register-runner)
             (emacs-ide-dev-lang-enabled-p "zig"))
    (emacs-ide-test-register-runner 'zig-mode
      :file-fn    #'emacs-ide-zig-test-file
      :project-fn #'emacs-ide-zig-test-project)))

;; ============================================================================
;; Nix
;; FIX-DEFUN-IN-CONFIG-NIX: moved to top-level.
;; ============================================================================
(defun emacs-ide-nix-repl ()
  "Open a nix repl."
  (interactive)
  (if (executable-find "nix")
      (progn (require 'comint)
             (make-comint "nix-repl" "nix" nil "repl")
             (switch-to-buffer "*nix-repl*"))
    (message "lang-systems: nix not found")))

(use-package nix-mode
  :if (emacs-ide-dev-lang-enabled-p "nix")
  :defer t
  :mode "\\.nix\\'"
  :config
  ;; FIX-REPL-ATTACH-NIX: explicit key avoids C-c r recovery collision
  (emacs-ide-dev-attach-repl nix-mode-map #'emacs-ide-nix-repl
                              (kbd "C-c x r"))
  ;; FIX-REPL-REGISTER-NIX: explicit registration for reliable C-c x r dispatch
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'nix-mode
      :launch         #'emacs-ide-nix-repl
      :buffer-name    "*nix-repl*"
      :send-region-fn nil)))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "nix")
           (executable-find "nil"))
  :hook (nix-mode . lsp-deferred))

(with-eval-after-load 'apheleia
  (when (and (emacs-ide-dev-lang-enabled-p "nix")
             (executable-find "nixpkgs-fmt"))
    (emacs-ide-dev-attach-formatter 'nixpkgs-fmt 'nix-mode)))

;; ============================================================================
;; D
;; FIX-D-REGISTER: now has its own lang-enabled-p check via the register call.
;; ============================================================================
(use-package d-mode
  :if (and (emacs-ide-dev-lang-enabled-p "d") (executable-find "dmd"))
  :defer t
  :mode "\\.d\\'")

;; ============================================================================
;; V (Vlang)
;; FIX-V-REGISTER + FIX-V-IN-GUARD: now has its own guard.
;; ============================================================================
(use-package v-mode
  :if (and (emacs-ide-dev-lang-enabled-p "v") (executable-find "v"))
  :defer t
  :mode "\\.v\\'")

) ;; end (when any systems lang enabled)

(provide 'lang-systems)
;;; lang-systems.el ends here
