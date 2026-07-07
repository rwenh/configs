;;; lang-web.el --- JavaScript / TypeScript / HTML / CSS IDE layer -*- lexical-binding: t -*-
;;; Version: 3.4.0
;;;
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "javascript"
  :tier 1
  :lsp-server "typescript-language-server"
  :formatter  "prettier"
  :test-cmd   "jest"
  :repl       "node"
  :modes      '(js-mode js2-mode js-ts-mode))

(emacs-ide-dev-register "typescript"
  :tier 1
  :lsp-server "typescript-language-server"
  :formatter  "prettier"
  :test-cmd   "jest"
  :repl       "node"
  :modes      '(typescript-mode typescript-ts-mode tsx-ts-mode))

(emacs-ide-dev-register "html"
  :tier 1
  :lsp-server "vscode-html-language-server"
  :formatter  "prettier"
  :test-cmd   nil
  :repl       nil
  :modes      '(web-mode mhtml-mode html-mode))

(emacs-ide-dev-register "css"
  :tier 1
  :lsp-server "vscode-css-language-server"
  :formatter  "prettier"
  :test-cmd   nil
  :repl       nil
  :modes      '(css-mode css-ts-mode scss-mode less-css-mode))

(when (or (emacs-ide-dev-lang-enabled-p "javascript")
          (emacs-ide-dev-lang-enabled-p "typescript")
          (emacs-ide-dev-lang-enabled-p "html")
          (emacs-ide-dev-lang-enabled-p "css"))

;;;; ── Tree-sitter grammars ────────────────────────────────────────────────────

(emacs-ide-dev-ensure-treesit 'javascript)
(emacs-ide-dev-ensure-treesit 'typescript)
(emacs-ide-dev-ensure-treesit 'tsx)
(emacs-ide-dev-ensure-treesit 'css)

;;;; ── LSP (typescript-language-server) ───────────────────────────────────────

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (executable-find "typescript-language-server"))
  :hook ((js2-mode js-ts-mode
          typescript-mode typescript-ts-mode tsx-ts-mode)
         . lsp-deferred))

;;;; ── js2-mode ────────────────────────────────────────────────────────────────

(use-package js2-mode
  :if (emacs-ide-dev-lang-enabled-p "javascript")
  :mode "\\.jsx?\\'"
  :init
  (setq js-indent-level  2
        js2-basic-offset 2))

;;;; ── typescript-mode ─────────────────────────────────────────────────────────

(use-package typescript-mode
  :if (emacs-ide-dev-lang-enabled-p "typescript")
  :mode "\\.tsx?\\'"
  :init
  (setq typescript-indent-level 2))

;;;; ── HTML — web-mode (full HTML/Jinja/Vue/Svelte support) ──────────────────

(use-package web-mode
  :if (emacs-ide-dev-lang-enabled-p "html")
  :defer t
  :mode (("\\.html\\'"       . web-mode)
         ("\\.htm\\'"        . web-mode)
         ("\\.jinja\\'"      . web-mode)
         ("\\.jinja2\\'"     . web-mode)
         ("\\.j2\\'"         . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.handlebars\\'" . web-mode)
         ("\\.hbs\\'"        . web-mode)
         ("\\.vue\\'"        . web-mode)
         ("\\.svelte\\'"     . web-mode)
         ("\\.tsx\\'"        . web-mode)
         ("\\.jsx\\'"        . web-mode)
         ("\\.erb\\'"        . web-mode)
         ("\\.php\\'"        . web-mode))   ; PHP files also open in web-mode for template editing
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset    2
        web-mode-code-indent-offset   2
        web-mode-enable-auto-pairing  t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight  nil
        web-mode-engines-alist
        '(("django"    . "\\.html\\'")
          ("jinja2"    . "\\.jinja2?\\'")
          ("blade"     . "\\.blade\\.php\\'"))))

;; LSP for HTML (vscode-html-language-server from @vscode/html-languageservice)
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "html")
           (executable-find "vscode-html-language-server"))
  :hook ((web-mode  . lsp-deferred)
         (mhtml-mode . lsp-deferred)
         (html-mode  . lsp-deferred)))

;;;; ── CSS / SCSS / LESS ───────────────────────────────────────────────────────

;; css-mode is built-in — just configure it
(use-package css-mode
  :straight nil
  :if (emacs-ide-dev-lang-enabled-p "css")
  :defer t
  :mode (("\\.css\\'"  . css-mode)
         ("\\.scss\\'" . scss-mode)
         ("\\.sass\\'" . css-mode))
  :init
  (setq css-indent-offset 2))

(use-package scss-mode
  :if (emacs-ide-dev-lang-enabled-p "css")
  :defer t
  :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save nil))  ; apheleia handles formatting

(use-package less-css-mode
  :if (emacs-ide-dev-lang-enabled-p "css")
  :defer t
  :mode "\\.less\\'")

;; LSP for CSS/SCSS (vscode-css-language-server from vscode-langservers-extracted)
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "css")
           (executable-find "vscode-css-language-server"))
  :hook ((css-mode    . lsp-deferred)
         (css-ts-mode . lsp-deferred)
         (scss-mode   . lsp-deferred)))

;;;; ── REPL (Node) ─────────────────────────────────────────────────────────────

(defun emacs-ide-web-node-repl ()
  "Open a Node.js REPL."
  (interactive)
  (if (executable-find "node")
      (progn
        (require 'comint)
        (make-comint "node-repl" "node")
        (switch-to-buffer "*node-repl*"))
    (message "lang-web: node not found on PATH")))

(with-eval-after-load 'tools-repl
  (when (fboundp 'emacs-ide-repl-register)
    (dolist (mode '(js-mode js2-mode js-ts-mode
                    typescript-mode typescript-ts-mode tsx-ts-mode))
      (emacs-ide-repl-register mode
        :launch         #'emacs-ide-web-node-repl
        :buffer-name    "*node-repl*"
        :send-region-fn nil))))

;;;; ── Formatter ───────────────────────────────────────────────────────────────

(with-eval-after-load 'apheleia
  ;; JS / TS
  (dolist (m '(js-mode js2-mode js-ts-mode
               typescript-mode typescript-ts-mode tsx-ts-mode))
    (setf (alist-get m apheleia-mode-alist) 'prettier))
  ;; HTML
  (dolist (m '(web-mode mhtml-mode html-mode))
    (setf (alist-get m apheleia-mode-alist) 'prettier))
  ;; CSS / SCSS / LESS
  (dolist (m '(css-mode css-ts-mode scss-mode less-css-mode))
    (setf (alist-get m apheleia-mode-alist) 'prettier)))

;;;; ── Test runners ────────────────────────────────────────────────────────────

(defun emacs-ide-web-test-file ()
  "Run jest on the current file."
  (interactive)
  (if (and (executable-find "jest") (buffer-file-name))
      (compile (format "jest --testPathPattern %s"
                       (shell-quote-argument
                        (file-relative-name (buffer-file-name)))))
    (message "lang-web: jest not found or no file")))

(defun emacs-ide-web-test-project ()
  "Run the project test suite (jest → vitest → npm test)."
  (interactive)
  (cond
   ((executable-find "jest")   (compile "jest"))
   ((executable-find "vitest") (compile "vitest run"))
   ((executable-find "npm")    (compile "npm test"))
   (t (message "lang-web: no test runner found (jest/vitest/npm)"))))

;; FIX: was (with-eval-after-load 'tools-test ...) — wrong module.
(with-eval-after-load 'tools-test-runner-registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (m '(js-mode js2-mode js-ts-mode
                 typescript-mode typescript-ts-mode tsx-ts-mode))
      (emacs-ide-test-register-runner m
        :file-fn    #'emacs-ide-web-test-file
        :project-fn #'emacs-ide-web-test-project))))

) ;; end js/ts/html/css enabled

(provide 'lang-web)
;;; lang-web.el ends here
