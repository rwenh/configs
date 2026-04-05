;;; lang-jvm.el --- JVM IDE layer (Java / Kotlin / Scala / Groovy) -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.2 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.2 to 3.0.4.
;;;   - FIX-LSP-JAVA-GUARD: lsp-java had no :if guard on emacs-ide-lsp-enable
;;;     — java-ts-mode lsp-deferred hook fired even when LSP disabled in config.
;;;     Added (bound-and-true-p emacs-ide-lsp-enable) :if guard.
;;;   - FIX-KOTLIN-LAMBDA-HOOK: kotlin-mode :hook used an inline lambda for
;;;     the LSP guard — lambdas in :hook are unreliable across reloads. Replaced
;;;     with a separate (use-package lsp-mode :if ... :hook ...) block.
;;;   - FIX-SCALA-LAMBDA-HOOK: Same class of bug in scala-mode :hook. Replaced
;;;     with a separate lsp-mode block with :if guard.
;;;   - FIX-SCALA-REGISTER: scala was not registered with emacs-ide-dev-register
;;;     so lang-enabled-p "scala" always returned t regardless of config.
;;;     Added register call.
;;;   - FIX-GROOVY-REGISTER: groovy was not registered with emacs-ide-dev-register
;;;     and was gated on java being enabled (wrong). Added register call and
;;;     its own lang-enabled-p guard.
;;;   - FIX-JAVA-TEST-ROOT: emacs-ide-java-test used bare (file-exists-p "pom.xml")
;;;     without project root — only worked when CWD was the project root.
;;;     Now uses expand-file-name with project root.
;;;   - FIX-TEST-REGISTER-KOTLIN: Added emacs-ide-test-register-runner for
;;;     kotlin-mode (gradle test).
;;;   - FIX-TEST-REGISTER-SCALA: Added emacs-ide-test-register-runner for
;;;     scala-mode (sbt test).
;;;   - FIX-SCALA-METALS-IF: lsp-metals :if gated on (executable-find "cs")
;;;     but Metals can be installed without coursier. Relaxed to check for
;;;     the metals binary directly, with coursier as a fallback check.
;;;   - FIX-LSP-JAVA-NETWORK: Added comment noting lsp-java-format-settings-url
;;;     fetches a remote resource at startup; requires network access.
;;; Fixes vs 1.0.1 (retained):
;;;   - FIX-LSP-GUARD: kotlin-mode and scala-mode lsp hooks now use :if blocks.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-LSP: java-mode hook removed — tools-lsp.el owns it.
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "java" :tier 2 :lsp-server "jdtls"
  :formatter "google-java-format" :test-cmd "mvn test" :repl nil
  :modes '(java-mode java-ts-mode))
(emacs-ide-dev-register "kotlin" :tier 2 :lsp-server "kotlin-language-server"
  :formatter "ktlint" :test-cmd "gradle test" :repl nil
  :modes '(kotlin-mode kotlin-ts-mode))
;; FIX-SCALA-REGISTER: was missing — lang-enabled-p always returned t
(emacs-ide-dev-register "scala" :tier 2 :lsp-server "metals"
  :formatter "scalafmt" :test-cmd "sbt test" :repl nil
  :modes '(scala-mode))
;; FIX-GROOVY-REGISTER: was missing and gated on java (wrong)
(emacs-ide-dev-register "groovy" :tier 2 :lsp-server nil
  :formatter nil :test-cmd "gradle test" :repl nil
  :modes '(groovy-mode))

(when (or (emacs-ide-dev-lang-enabled-p "java")
          (emacs-ide-dev-lang-enabled-p "kotlin")
          (emacs-ide-dev-lang-enabled-p "scala")
          (emacs-ide-dev-lang-enabled-p "groovy"))

(emacs-ide-dev-ensure-treesit 'java)

;; ============================================================================
;; Java
;; ============================================================================

;; FIX-JAVA-TEST-ROOT: uses project root for file-exists-p checks
(defun emacs-ide-java-test ()
  "Run Java project tests via Maven or Gradle."
  (interactive)
  (let ((root (or (and (fboundp 'projectile-project-root)
                       (ignore-errors (projectile-project-root)))
                  default-directory)))
    (cond
     ((file-exists-p (expand-file-name "pom.xml"      root)) (compile "mvn test -q"))
     ((file-exists-p (expand-file-name "build.gradle" root)) (compile "gradle test"))
     (t (message "lang-jvm: no Maven/Gradle project found")))))

;; FIX-LSP-JAVA-GUARD: :if guard added for emacs-ide-lsp-enable
(use-package lsp-java
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "java"))
  :defer t
  ;; FIX-LSP (retained): java-mode hook removed — tools-lsp.el owns it.
  :hook (java-ts-mode . lsp-deferred)
  :init
  ;; FIX-LSP-JAVA-NETWORK: lsp-java-format-settings-url fetches a remote
  ;; resource on startup. Requires network access. Remove or cache locally
  ;; if offline use is needed.
  (setq lsp-java-format-settings-url
        "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile          "GoogleStyle"
        lsp-java-save-action-organize-imports     t
        lsp-java-completion-guess-method-arguments t
        lsp-java-import-gradle-enabled            t
        lsp-java-import-maven-enabled             t))

(with-eval-after-load 'apheleia
  (when (emacs-ide-dev-lang-enabled-p "java")
    (emacs-ide-dev-attach-formatter 'google-java-format 'java-mode)))

(with-eval-after-load 'tools-test
  (when (and (fboundp 'emacs-ide-test-register-runner)
             (emacs-ide-dev-lang-enabled-p "java"))
    (emacs-ide-test-register-runner 'java-mode
      :project-fn #'emacs-ide-java-test
      :file-fn    #'emacs-ide-java-test)))

(with-eval-after-load 'dap-mode
  (when (emacs-ide-dev-lang-enabled-p "java")
    (emacs-ide-dev-attach-dap "Java :: jdtls" 'dap-java)
    (when (fboundp 'dap-register-debug-template)
      (dap-register-debug-template "Java :: launch"
        (list :type "java" :request "launch" :name "Java main")))))

;; ============================================================================
;; Kotlin
;; FIX-KOTLIN-LAMBDA-HOOK: inline lambda in :hook replaced with a separate
;; lsp-mode block that has a proper :if guard.
;; ============================================================================
(use-package kotlin-mode
  :if (emacs-ide-dev-lang-enabled-p "kotlin")
  :defer t
  :mode (("\\.kt\\'"  . kotlin-mode)
         ("\\.kts\\'" . kotlin-mode))
  :config
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'kotlin-mode
      :project-fn (lambda ()
                    (interactive)
                    (if (executable-find "gradle")
                        (compile "gradle test")
                      (message "lang-jvm: gradle not found"))))))

;; FIX-KOTLIN-LAMBDA-HOOK: separate block with :if guard
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "kotlin"))
  :hook (kotlin-mode . lsp-deferred))

(with-eval-after-load 'apheleia
  (when (emacs-ide-dev-lang-enabled-p "kotlin")
    (emacs-ide-dev-attach-formatter 'ktlint 'kotlin-mode)))

;; ============================================================================
;; Scala
;; FIX-SCALA-LAMBDA-HOOK: inline lambda in :hook replaced with a separate
;; lsp-mode block that has a proper :if guard.
;; ============================================================================
(use-package scala-mode
  :if (emacs-ide-dev-lang-enabled-p "scala")
  :defer t
  :mode "\\.\\(scala\\|sbt\\|sc\\)\\'"
  :config
  ;; FIX-TEST-REGISTER-SCALA: register test runner
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'scala-mode
      :project-fn (lambda ()
                    (interactive)
                    (if (executable-find "sbt")
                        (compile "sbt test")
                      (message "lang-jvm: sbt not found"))))))

;; FIX-SCALA-LAMBDA-HOOK: separate block with :if guard
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "scala"))
  :hook (scala-mode . lsp-deferred))

;; FIX-SCALA-METALS-IF: relax the coursier-only gate — check metals binary
;; directly or coursier as a fallback installer.
(use-package lsp-metals
  :if (and (emacs-ide-dev-lang-enabled-p "scala")
           (or (executable-find "metals")
               (executable-find "cs")))
  :after scala-mode)

;; ============================================================================
;; Groovy
;; FIX-GROOVY-REGISTER: now has its own lang-enabled-p check instead of
;; being gated on java being enabled.
;; ============================================================================
(use-package groovy-mode
  :if (emacs-ide-dev-lang-enabled-p "groovy")
  :defer t
  :mode (("\\.groovy\\'" . groovy-mode)
         ("\\.gradle\\'" . groovy-mode)))

) ;; end (when any jvm lang enabled)

(provide 'lang-jvm)
;;; lang-jvm.el ends here
