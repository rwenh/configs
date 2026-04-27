;;; lang-web.el --- JavaScript / TypeScript IDE layer -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;;   Also: tsx-ts-mode added to LSP hook and test runner registration.
;;;         Node REPL registered with the REPL hub so C-c x r / s / b / d
;;;         work in JS buffers.
;;;         js-ts-mode file-fn added to test runner (was missing).
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

(when (or (emacs-ide-dev-lang-enabled-p "javascript")
          (emacs-ide-dev-lang-enabled-p "typescript"))

;;;; ── LSP (typescript-language-server) ───────────────────────────────────────

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (executable-find "typescript-language-server"))
  :hook ((js2-mode js-ts-mode
          typescript-mode typescript-ts-mode tsx-ts-mode)
         . lsp-deferred))

;;;; ── js2-mode ────────────────────────────────────────────────────────────────

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :init
  (setq js-indent-level 2
        js2-basic-offset 2))

;;;; ── typescript-mode ─────────────────────────────────────────────────────────

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :init
  (setq typescript-indent-level 2))

;;;; ── REPL (Node) ─────────────────────────────────────────────────────────────
;; Register with the REPL hub so C-c x r / s / b / d work uniformly.

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
  (dolist (m '(js-mode js2-mode js-ts-mode
               typescript-mode typescript-ts-mode tsx-ts-mode
               web-mode css-mode css-ts-mode scss-mode))
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

(with-eval-after-load 'tools-test
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (m '(js-mode js2-mode js-ts-mode
                 typescript-mode typescript-ts-mode tsx-ts-mode))
      (emacs-ide-test-register-runner m
        :file-fn    #'emacs-ide-web-test-file
        :project-fn #'emacs-ide-web-test-project))))

) ;; end js/ts enabled

(provide 'lang-web)
;;; lang-web.el ends here
