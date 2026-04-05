;;; lang-web.el --- Web IDE layer (JS/TS/HTML/CSS/Vue/JSX) -*- lexical-binding: t -*-
;;; Commentary: Follows canonical lang-python.el template.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.1 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.1 to 3.0.4.
;;;   - FIX-DEFUN-IN-CONFIG-JS: emacs-ide-js-run was defined inside js2-mode
;;;     :config — not visible to M-x until js2-mode loads. Moved to top-level.
;;;   - FIX-DEFUN-IN-CONFIG-TS: emacs-ide-ts-run was defined inside
;;;     typescript-mode :config — same issue. Moved to top-level.
;;;   - FIX-LSP-GUARD: lsp-mode use-package had no :if guard on
;;;     emacs-ide-lsp-enable — web-mode and css-mode hooks fired even when
;;;     LSP is disabled in config. Added (bound-and-true-p emacs-ide-lsp-enable).
;;;   - FIX-LSP-INLAY-VARS: lsp-javascript-* and lsp-typescript-* inlay hint
;;;     vars may not exist in all lsp-mode versions. Wrapped with (boundp).
;;;   - FIX-REPL-ATTACH-ORDER: emacs-ide-dev-attach-repl was called via
;;;     with-eval-after-load instead of inside :config. Moved, and explicit
;;;     key arg added to avoid C-c r recovery collision.
;;;   - FIX-REPL-REGISTER: Added explicit emacs-ide-repl-register for js2-mode
;;;     and typescript-mode inside their :config blocks.
;;;   - FIX-DAP-PLACEHOLDER: "${file}" and "${workspaceFolder}" are VS Code
;;;     variables not expanded by dap-mode. Replaced with lambdas.
;;;   - FIX-JSON-DUPLICATE: (use-package json-mode) here duplicated lang-prose.el
;;;     registration — double straight recipe entry. Removed; lang-prose.el owns
;;;     json-mode, or it loads from core Emacs json.el.
;;;   - FIX-TS-MODE-TEST: Test runners now registered for js-ts-mode and
;;;     typescript-ts-mode (Emacs 29+ treesitter modes) alongside classic modes.
;;;   - FIX-TYPESCRIPT-REGISTER: "typescript" was not registered with
;;;     emacs-ide-dev-register — lang-enabled-p "typescript" always returned t.
;;;     Added register call.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-LSP: js2-mode and typescript-mode hooks removed — tools-lsp.el owns.
;;; Code:

(require 'core-dev)

;; 1. Registration
(emacs-ide-dev-register "javascript"
  :tier 1 :lsp-server "typescript-language-server"
  :formatter "prettier" :test-cmd "npm test" :repl "node"
  :modes '(js-mode js-ts-mode js2-mode))

;; FIX-TYPESCRIPT-REGISTER: was missing — lang-enabled-p always returned t
(emacs-ide-dev-register "typescript"
  :tier 1 :lsp-server "typescript-language-server"
  :formatter "prettier" :test-cmd "npm test" :repl "node"
  :modes '(typescript-mode typescript-ts-mode tsx-ts-mode))

;; 2. Config guard — either js or ts enables this module
(when (or (emacs-ide-dev-lang-enabled-p "javascript")
          (emacs-ide-dev-lang-enabled-p "typescript"))

;; 3. Treesitter
(emacs-ide-dev-ensure-treesit 'javascript)
(emacs-ide-dev-ensure-treesit 'typescript)
(emacs-ide-dev-ensure-treesit 'tsx)
(emacs-ide-dev-ensure-treesit 'css)
(emacs-ide-dev-ensure-treesit 'html)

;; ============================================================================
;; COMPILE / RUN COMMANDS
;; FIX-DEFUN-IN-CONFIG-JS + FIX-DEFUN-IN-CONFIG-TS: top-level defuns.
;; ============================================================================
(defun emacs-ide-js-run ()
  "Run the current JavaScript file via node."
  (interactive)
  (if (executable-find "node")
      (compile (format "node %s" (shell-quote-argument (buffer-file-name))))
    (message "lang-web: node not found")))

(defun emacs-ide-ts-run ()
  "Run the current TypeScript file via ts-node."
  (interactive)
  (cond
   ((executable-find "ts-node")
    (compile (format "ts-node %s" (shell-quote-argument (buffer-file-name)))))
   ((executable-find "npx")
    (compile (format "npx ts-node %s" (shell-quote-argument (buffer-file-name)))))
   (t (message "lang-web: ts-node not found. npm i -g ts-node"))))

;; ============================================================================
;; 7. REPL — Node.js (defined early; referenced in :config blocks below)
;; ============================================================================
(defun emacs-ide-js-repl ()
  "Open a Node.js REPL."
  (interactive)
  (if (executable-find "node")
      (progn (require 'comint)
             (make-comint "node-repl" "node")
             (switch-to-buffer "*node-repl*"))
    (message "lang-web: node not found")))

;; ============================================================================
;; 4a. JavaScript
;; ============================================================================
(use-package js2-mode
  :defer t
  :mode (("\\.js\\'"  . js2-mode)
         ("\\.mjs\\'" . js2-mode)
         ("\\.cjs\\'" . js2-mode))
  :interpreter "node"
  :init
  (setq js2-basic-offset               2
        js2-mode-show-parse-errors     nil
        js2-strict-missing-semi-warning nil
        js-indent-level                2)
  :config
  (emacs-ide-dev-bind-compile js2-mode-map #'emacs-ide-js-run)
  ;; FIX-REPL-ATTACH-ORDER: moved here with explicit key
  (emacs-ide-dev-attach-repl js2-mode-map #'emacs-ide-js-repl
                              (kbd "C-c x r"))
  ;; FIX-REPL-REGISTER: explicit registration for reliable C-c x r dispatch
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'js2-mode
      :launch         #'emacs-ide-js-repl
      :buffer-name    "*node-repl*"
      :send-region-fn nil))
  ;; FIX-TS-MODE-TEST: register test runners here (also done below)
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(js2-mode js-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-web-test-file
        :project-fn #'emacs-ide-web-test-project))))

;; ============================================================================
;; 4b. TypeScript
;; ============================================================================
(use-package typescript-mode
  :defer t
  :mode (("\\.ts\\'"  . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :init
  (setq typescript-indent-level 2)
  :config
  (emacs-ide-dev-bind-compile typescript-mode-map #'emacs-ide-ts-run)
  ;; FIX-REPL-ATTACH-ORDER: moved here with explicit key
  (emacs-ide-dev-attach-repl typescript-mode-map #'emacs-ide-js-repl
                              (kbd "C-c x r"))
  ;; FIX-REPL-REGISTER: explicit registration
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'typescript-mode
      :launch         #'emacs-ide-js-repl
      :buffer-name    "*node-repl*"
      :send-region-fn nil))
  ;; FIX-TS-MODE-TEST: register test runners
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(typescript-mode typescript-ts-mode tsx-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-web-test-file
        :project-fn #'emacs-ide-web-test-project))))

;; ============================================================================
;; 4c. Web / HTML / Vue — .tsx excluded (typescript-mode owns it)
;; ============================================================================
(use-package web-mode
  :defer t
  :mode (("\\.html\\'"   . web-mode)
         ("\\.htm\\'"    . web-mode)
         ("\\.vue\\'"    . web-mode)
         ("\\.jsx\\'"    . web-mode)
         ("\\.svelte\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset            2
        web-mode-css-indent-offset               2
        web-mode-code-indent-offset              2
        web-mode-enable-auto-pairing             t
        web-mode-enable-css-colorization         t
        web-mode-enable-current-element-highlight t))

;; ============================================================================
;; 4d. CSS
;; ============================================================================
(use-package css-mode
  :straight nil
  :defer t
  :mode "\\.css\\'"
  :init (setq css-indent-offset 2))

;; Emmet for HTML/CSS expansion
(use-package emmet-mode
  :hook ((web-mode html-mode css-mode sgml-mode) . emmet-mode))

;; ============================================================================
;; 5. LSP — typescript-language-server
;; FIX-LSP-GUARD: :if guard added for emacs-ide-lsp-enable.
;; FIX-LSP-INLAY-VARS: lsp-*-inlay-hints-* vars guarded with boundp.
;; FIX-LSP (retained): js2-mode and typescript-mode hooks removed — tools-lsp owns.
;; ============================================================================
(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :hook ((web-mode css-mode) . lsp-deferred)
  :init
  (when (boundp 'lsp-javascript-display-inlay-hints)
    (setq lsp-javascript-display-inlay-hints t))
  (when (boundp 'lsp-typescript-display-inlay-hints)
    (setq lsp-typescript-display-inlay-hints t))
  (when (boundp 'lsp-typescript-inlay-hints-include-inlay-parameter-name-hints)
    (setq lsp-typescript-inlay-hints-include-inlay-parameter-name-hints "all"))
  (when (boundp 'lsp-typescript-inlay-hints-include-inlay-variable-type-hints)
    (setq lsp-typescript-inlay-hints-include-inlay-variable-type-hints t))
  (when (boundp 'lsp-typescript-inlay-hints-include-inlay-function-like-return-type-hints)
    (setq lsp-typescript-inlay-hints-include-inlay-function-like-return-type-hints t)))

;; ============================================================================
;; 6. Formatter — prettier for all web filetypes
;; ============================================================================
(with-eval-after-load 'apheleia
  (dolist (mode '(js2-mode js-ts-mode
                  typescript-mode typescript-ts-mode tsx-ts-mode
                  web-mode css-mode css-ts-mode))
    (emacs-ide-dev-attach-formatter 'prettier mode)))

;; ============================================================================
;; 8. Test runner — jest / vitest / npm test
;; FIX-TS-MODE-TEST: js-ts-mode and typescript-ts-mode now registered.
;; ============================================================================
(defun emacs-ide-web-test-file ()
  "Run tests for the current web file."
  (interactive)
  (cond
   ((executable-find "jest")
    (compile (format "jest %s" (shell-quote-argument (buffer-file-name)))))
   ((executable-find "vitest") (compile "vitest run"))
   ((executable-find "npm")    (compile "npm test"))
   (t (message "lang-web: no test runner found (jest/vitest/npm)"))))

(defun emacs-ide-web-test-project ()
  "Run all web project tests."
  (interactive)
  (cond
   ((executable-find "jest")   (compile "jest"))
   ((executable-find "vitest") (compile "vitest run"))
   ((executable-find "npm")    (compile "npm test"))
   (t (message "lang-web: no test runner found"))))

(with-eval-after-load 'tools-test
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(js2-mode js-ts-mode
                    typescript-mode typescript-ts-mode tsx-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-web-test-file
        :project-fn #'emacs-ide-web-test-project))))

;; ============================================================================
;; 9. Debugger — dap-node
;; FIX-DAP-PLACEHOLDER: "${file}" and "${workspaceFolder}" replaced with lambdas.
;; ============================================================================
(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "Node :: launch" 'dap-node)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template
     "Node :: launch file"
     (list :type    "node"
           :request "launch"
           :name    "Node.js"
           ;; FIX-DAP-PLACEHOLDER: lambdas read at launch time
           :program (lambda () (buffer-file-name))
           :cwd     (lambda ()
                      (or (and (fboundp 'projectile-project-root)
                               (ignore-errors (projectile-project-root)))
                          default-directory))))))

;; ============================================================================
;; 10. Project mgmt — npm-mode
;; ============================================================================
(use-package npm-mode
  :if (executable-find "npm")
  :hook ((js2-mode typescript-mode) . npm-mode))

;; ============================================================================
;; 11. Extras
;; ============================================================================
(use-package js2-refactor
  :after js2-mode
  :hook (js2-mode . js2-refactor-mode)
  :bind (:map js2-mode-map ("C-c m" . js2-refactor-mode-map)))

;; FIX-JSON-DUPLICATE: json-mode use-package removed — lang-prose.el is the
;; canonical owner of json-mode. Duplicate registration caused straight to
;; register the recipe twice. If prose is disabled, Emacs 29+ json.el handles
;; json-mode natively.

) ;; end web guard

(provide 'lang-web)
;;; lang-web.el ends here
