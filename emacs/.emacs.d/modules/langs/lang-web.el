;;; lang-web.el --- Web IDE layer (JS/TS/HTML/CSS/Vue/JSX) -*- lexical-binding: t -*-
;;; Commentary: Follows canonical lang-python.el template.
;;; Version: 1.0.0
;;; Code:

(require 'core-dev)

;; 1. Registration
(emacs-ide-dev-register "javascript"
  :tier 1 :lsp-server "typescript-language-server"
  :formatter "prettier" :test-cmd "npm test" :repl "node"
  :modes '(js-mode js-ts-mode typescript-mode typescript-ts-mode web-mode))

;; 2. Config guard — either js or ts enables this module
(when (or (emacs-ide-dev-lang-enabled-p "javascript")
          (emacs-ide-dev-lang-enabled-p "typescript"))

;; 3. Treesitter
(emacs-ide-dev-ensure-treesit 'javascript)
(emacs-ide-dev-ensure-treesit 'typescript)
(emacs-ide-dev-ensure-treesit 'tsx)
(emacs-ide-dev-ensure-treesit 'css)
(emacs-ide-dev-ensure-treesit 'html)

;; 4a. JavaScript
(use-package js2-mode
  :defer t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.mjs\\'" . js2-mode)
         ("\\.cjs\\'" . js2-mode))
  :interpreter "node"
  :init
  (setq js2-basic-offset              2
        js2-mode-show-parse-errors    nil
        js2-strict-missing-semi-warning nil
        js-indent-level               2)
  :config
  (defun emacs-ide-js-run ()
    (interactive)
    (if (executable-find "node")
        (compile (format "node %s" (shell-quote-argument (buffer-file-name))))
      (message "lang-web: node not found")))
  (emacs-ide-dev-bind-compile js2-mode-map #'emacs-ide-js-run))

;; 4b. TypeScript — .tsx belongs here (not web-mode, avoids auto-mode conflict)
(use-package typescript-mode
  :defer t
  :mode (("\\.ts\\'"  . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :init (setq typescript-indent-level 2)
  :config
  (defun emacs-ide-ts-run ()
    (interactive)
    (cond
     ((executable-find "ts-node")
      (compile (format "ts-node %s" (shell-quote-argument (buffer-file-name)))))
     ((executable-find "npx")
      (compile (format "npx ts-node %s" (shell-quote-argument (buffer-file-name)))))
     (t (message "lang-web: ts-node not found. npm i -g ts-node"))))
  (emacs-ide-dev-bind-compile typescript-mode-map #'emacs-ide-ts-run))

;; 4c. Web / HTML / Vue — .tsx excluded (typescript-mode owns it)
(use-package web-mode
  :defer t
  :mode (("\\.html\\'" . web-mode)
         ("\\.htm\\'"  . web-mode)
         ("\\.vue\\'"  . web-mode)
         ("\\.jsx\\'"  . web-mode)
         ("\\.svelte\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset           2
        web-mode-css-indent-offset              2
        web-mode-code-indent-offset             2
        web-mode-enable-auto-pairing            t
        web-mode-enable-css-colorization        t
        web-mode-enable-current-element-highlight t))

;; 4d. CSS
(use-package css-mode
  :straight nil
  :defer t
  :mode "\\.css\\'"
  :init (setq css-indent-offset 2))

;; Emmet for HTML/CSS expansion
(use-package emmet-mode
  :hook ((web-mode html-mode css-mode sgml-mode) . emmet-mode))

;; 5. LSP — typescript-language-server handles JS + TS + TSX
(use-package lsp-mode
  :hook ((js2-mode typescript-mode web-mode css-mode) . lsp-deferred)
  :init
  (setq lsp-javascript-display-inlay-hints         t
        lsp-typescript-display-inlay-hints          t
        lsp-typescript-inlay-hints-include-inlay-parameter-name-hints "all"
        lsp-typescript-inlay-hints-include-inlay-variable-type-hints  t
        lsp-typescript-inlay-hints-include-inlay-function-like-return-type-hints t))

;; 6. Formatter — prettier for all web filetypes
(with-eval-after-load 'apheleia
  (dolist (mode '(js2-mode typescript-mode web-mode css-mode))
    (emacs-ide-dev-attach-formatter 'prettier mode)))

;; 7. REPL — Node.js
(defun emacs-ide-js-repl ()
  (interactive)
  (if (executable-find "node")
      (progn (require 'comint)
             (make-comint "node-repl" "node")
             (switch-to-buffer "*node-repl*"))
    (message "lang-web: node not found")))
(with-eval-after-load 'js2-mode
  (emacs-ide-dev-attach-repl js2-mode-map #'emacs-ide-js-repl))
(with-eval-after-load 'typescript-mode
  (emacs-ide-dev-attach-repl typescript-mode-map #'emacs-ide-js-repl))

;; 8. Test runner — jest / vitest / npm test
(defun emacs-ide-web-test-file ()
  (interactive)
  (cond
   ((executable-find "jest")   (compile (format "jest %s" (shell-quote-argument (buffer-file-name)))))
   ((executable-find "vitest") (compile "vitest run"))
   ((executable-find "npm")    (compile "npm test"))
   (t (message "lang-web: no test runner found (jest/vitest/npm)"))))
(defun emacs-ide-web-test-project ()
  (interactive)
  (cond
   ((executable-find "jest")   (compile "jest"))
   ((executable-find "vitest") (compile "vitest run"))
   ((executable-find "npm")    (compile "npm test"))
   (t (message "lang-web: no test runner found"))))
(with-eval-after-load 'tools-test
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(js2-mode typescript-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-web-test-file
        :project-fn #'emacs-ide-web-test-project))))

;; 9. Debugger — dap-node
(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "Node :: launch" 'dap-node)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template
     "Node :: launch file"
     (list :type    "node"
           :request "launch"
           :name    "Node.js"
           :program "${file}"
           :cwd     "${workspaceFolder}"))))

;; 10. Project mgmt — npm / yarn / pnpm
(use-package npm-mode
  :if (executable-find "npm")
  :hook ((js2-mode typescript-mode) . npm-mode))

;; 11. Extras
(use-package js2-refactor
  :after js2-mode
  :hook (js2-mode . js2-refactor-mode)
  :bind (:map js2-mode-map ("C-c m" . js2-refactor-mode-map)))

(use-package json-mode
  :defer t
  :mode "\\.json\\'")

) ;; end web guard

(provide 'lang-web)
;;; lang-web.el ends here
