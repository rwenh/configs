;;; lang-web.el --- Web Languages (JavaScript/TypeScript) -*- lexical-binding: t -*-
;;; Commentary:
;;; Complete web development support: JavaScript, TypeScript, JSX, TSX.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (recalibration):
;;;   - FIX-REQUIRE: Was (require 'emacs-ide-dev); feature is provided as
;;;     'core-dev. Fixed to (require 'core-dev).
;;;   - FIX-API-REGISTER-LANG: emacs-ide-dev-register-lang does not exist in
;;;     core-dev.el. The correct function is emacs-ide-dev-register. Rewired
;;;     for both javascript and typescript.
;;;   - FIX-API-TEST-RUNNER: emacs-ide-dev-register-test-runner does not exist
;;;     in core-dev.el. Replaced with emacs-ide-test-register-runner via
;;;     with-eval-after-load 'tools-test-runner-registry.
;;;   - FIX-LSP-INIT-VARS: lsp-typescript-* variables moved from :init to
;;;     :config (retained from prior audit).
;;;   - FIX-JS2-REFACTOR-BIND: Removed erroneous :bind-keymap causing errors
;;;     on C-c m (retained from prior audit).
;;; Code:

(require 'core-dev)

;; ============================================================================
;; REGISTRATION
;; FIX-API-REGISTER-LANG: emacs-ide-dev-register is the correct function.
;; ============================================================================
(emacs-ide-dev-register "javascript"
  :tier 1
  :lsp-server "typescript-language-server"
  :formatter "prettier"
  :test-cmd "jest"
  :repl "node"
  :modes '(js-mode js2-mode js-ts-mode))

(emacs-ide-dev-register "typescript"
  :tier 1
  :lsp-server "typescript-language-server"
  :formatter "prettier"
  :test-cmd "jest"
  :repl "node"
  :modes '(typescript-mode typescript-ts-mode tsx-ts-mode))

(when (or (emacs-ide-dev-lang-enabled-p "javascript")
          (emacs-ide-dev-lang-enabled-p "typescript"))

;; ============================================================================
;; JavaScript / TypeScript Modes
;; ============================================================================
(use-package typescript-mode
  :mode (("\\.ts\\'"  . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :init
  (setq typescript-indent-level 2)
  :config
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'typescript-mode
      :launch         (lambda ()
                        (if (executable-find "node")
                            (progn (make-comint "node-repl" "node")
                                   (switch-to-buffer "*node-repl*"))
                          (message "lang-web: node not found on PATH")))
      :buffer-name    "*node-repl*"
      :send-region-fn nil)))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :init
  (setq js2-mode-show-strict-warnings nil
        js2-mode-show-parse-errors    nil
        js-indent-level               2)
  :config
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'js2-mode
      :launch         (lambda ()
                        (if (executable-find "node")
                            (progn (make-comint "node-repl" "node")
                                   (switch-to-buffer "*node-repl*"))
                          (message "lang-web: node not found on PATH")))
      :buffer-name    "*node-repl*"
      :send-region-fn nil)))

;; ============================================================================
;; LSP: TYPESCRIPT-LANGUAGE-SERVER
;; FIX-LSP-INIT-VARS: Moved from :init to :config.
;; ============================================================================
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (executable-find "typescript-language-server"))
  :hook ((js2-mode          . lsp-deferred)
         (js-mode           . lsp-deferred)
         (js-ts-mode        . lsp-deferred)
         (typescript-mode   . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode       . lsp-deferred))
  :config
  ;; FIX-LSP-INIT-VARS: set after lsp-typescript loads, not before
  (with-eval-after-load 'lsp-javascript
    (setq lsp-typescript-display-return-type-hints                               t
          lsp-typescript-display-parameter-type-hints                            t
          lsp-typescript-display-variable-type-hints                             t
          lsp-typescript-display-enum-member-value-hints                         t
          lsp-typescript-display-inline-variable-hints                           t
          lsp-typescript-display-inline-parameter-hints                          t
          lsp-typescript-display-inline-enum-member-value-hints                  t
          lsp-typescript-inlay-hints-include-inlay-variable-hints-when-parameter-type-hints-enabled t
          lsp-typescript-inlay-hints-include-inlay-parameter-hints-when-argument-matches-parameter-name t
          lsp-typescript-inlay-hints-include-inlay-function-parameter-type-hints t)))

;; ============================================================================
;; JS2-REFACTOR
;; FIX-JS2-REFACTOR-BIND: Removed :bind-keymap that caused C-c m errors.
;; ============================================================================
(use-package js2-refactor
  :after js2-mode
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

;; ============================================================================
;; PRETTIER — formatter
;; ============================================================================
(with-eval-after-load 'apheleia
  (when (executable-find "prettier")
    (dolist (mode '(js2-mode js-mode js-ts-mode
                    typescript-mode typescript-ts-mode tsx-ts-mode
                    web-mode css-mode css-ts-mode scss-mode))
      (setf (alist-get mode apheleia-mode-alist) 'prettier))))

;; ============================================================================
;; TEST RUNNER
;; FIX-API-TEST-RUNNER: emacs-ide-dev-register-test-runner does not exist.
;; Use emacs-ide-test-register-runner from tools-test-runner-registry.el.
;; ============================================================================
(defun emacs-ide-web-test-file ()
  "Run tests for the current JS/TS file."
  (interactive)
  (cond
   ((and (executable-find "jest") (buffer-file-name))
    (compile (format "jest %s" (shell-quote-argument (buffer-file-name)))))
   ((executable-find "vitest")
    (compile "vitest run"))
   ((executable-find "npm")
    (compile "npm test"))
   (t (message "lang-web: no test runner found (jest/vitest/npm)"))))

(defun emacs-ide-web-test-project ()
  "Run all JS/TS project tests."
  (interactive)
  (cond
   ((executable-find "jest")   (compile "jest"))
   ((executable-find "vitest") (compile "vitest run"))
   ((executable-find "npm")    (compile "npm test"))
   (t (message "lang-web: no test runner found"))))

(with-eval-after-load 'tools-test-runner-registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(js2-mode js-mode js-ts-mode
                    typescript-mode typescript-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-web-test-file
        :project-fn #'emacs-ide-web-test-project))))

;; ============================================================================
;; NODE DEBUG
;; ============================================================================
(with-eval-after-load 'dap-mode
  (when (fboundp 'dap-register-debug-template)
    (require 'dap-node nil t)
    (dap-register-debug-template "Node :: launch"
      (list :type    "node"
            :request "launch"
            :name    "Node file"
            :program (lambda () (buffer-file-name))
            :cwd     (lambda ()
                       (or (and (fboundp 'projectile-project-root)
                                (ignore-errors (projectile-project-root)))
                           default-directory))))))

) ;; end (when js or ts enabled)

(provide 'lang-web)
;;; lang-web.el ends here
