;;; lang-jvm.el --- JVM IDE layer (Java / Kotlin / Scala / Groovy) -*- lexical-binding: t -*-
;;; Version: 1.0.2
;;; Fixes vs 1.0.1:
;;;   - FIX-LSP-GUARD: kotlin-mode and scala-mode lsp-deferred hooks were
;;;     unguarded — fired even when emacs-ide-lsp-enable is nil.
;;;     Added (bound-and-true-p emacs-ide-lsp-enable) :hook guard to both.
;;; Fixes vs 1.0.0:
;;;   - FIX-LSP: Removed lsp-deferred :hook for java-mode from lsp-java.
;;;     tools-lsp.el already hooks java-mode to emacs-ide-lsp-deferred-optimized.
;;;     java-ts-mode hook kept.
;;; Code:
(require 'core-dev)
(emacs-ide-dev-register "java" :tier 2 :lsp-server "jdtls"
  :formatter "google-java-format" :test-cmd "mvn test" :repl nil :modes '(java-mode java-ts-mode))
(emacs-ide-dev-register "kotlin" :tier 2 :lsp-server "kotlin-language-server"
  :formatter "ktlint" :test-cmd "gradle test" :repl nil :modes '(kotlin-mode kotlin-ts-mode))
(when (or (emacs-ide-dev-lang-enabled-p "java")
          (emacs-ide-dev-lang-enabled-p "kotlin")
          (emacs-ide-dev-lang-enabled-p "scala"))
(emacs-ide-dev-ensure-treesit 'java)

;; ── Java ──────────────────────────────────────────────────────────────────
(use-package lsp-java
  :if (emacs-ide-dev-lang-enabled-p "java")
  :defer t
  ;; FIX-LSP: java-mode hook removed — tools-lsp.el owns it.
  :hook (java-ts-mode . lsp-deferred)
  :init
  (setq lsp-java-format-settings-url
        "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile "GoogleStyle"
        lsp-java-save-action-organize-imports   t
        lsp-java-completion-guess-method-arguments t
        lsp-java-import-gradle-enabled           t
        lsp-java-import-maven-enabled            t))

(with-eval-after-load 'apheleia
  (when (emacs-ide-dev-lang-enabled-p "java")
    (emacs-ide-dev-attach-formatter 'google-java-format 'java-mode)))

(defun emacs-ide-java-test ()
  (interactive)
  (cond ((file-exists-p "pom.xml")    (compile "mvn test -q"))
        ((file-exists-p "build.gradle") (compile "gradle test"))
        (t (message "lang-jvm: no Maven/Gradle project found"))))

(with-eval-after-load 'tools-test
  (when (and (fboundp 'emacs-ide-test-register-runner) (emacs-ide-dev-lang-enabled-p "java"))
    (emacs-ide-test-register-runner 'java-mode
      :project-fn #'emacs-ide-java-test :file-fn #'emacs-ide-java-test)))

(with-eval-after-load 'dap-mode
  (when (emacs-ide-dev-lang-enabled-p "java")
    (emacs-ide-dev-attach-dap "Java :: jdtls" 'dap-java)
    (when (fboundp 'dap-register-debug-template)
      (dap-register-debug-template "Java :: launch"
        (list :type "java" :request "launch" :name "Java main")))))

;; ── Kotlin ────────────────────────────────────────────────────────────────
(use-package kotlin-mode
  :if (emacs-ide-dev-lang-enabled-p "kotlin")
  :defer t :mode (("\\.kt\\'" . kotlin-mode) ("\\.kts\\'" . kotlin-mode))
  :hook (kotlin-mode . (lambda ()
          (when (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
            (lsp-deferred)))))

(with-eval-after-load 'apheleia
  (when (emacs-ide-dev-lang-enabled-p "kotlin")
    (emacs-ide-dev-attach-formatter 'ktlint 'kotlin-mode)))

;; ── Scala ─────────────────────────────────────────────────────────────────
(use-package scala-mode
  :if (emacs-ide-dev-lang-enabled-p "scala")
  :defer t :mode "\\.\\(scala\\|sbt\\|sc\\)\\'"
  :hook (scala-mode . (lambda ()
          (when (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
            (lsp-deferred)))))

(use-package lsp-metals
  :if (and (emacs-ide-dev-lang-enabled-p "scala")
           (executable-find "cs"))
  :after scala-mode)

;; ── Groovy ────────────────────────────────────────────────────────────────
(use-package groovy-mode
  :if (emacs-ide-dev-lang-enabled-p "java")
  :defer t :mode (("\\.groovy\\'" . groovy-mode) ("\\.gradle\\'" . groovy-mode)))

) (provide 'lang-jvm)
;;; lang-jvm.el ends here
