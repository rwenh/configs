;;; lang-jvm.el --- JVM IDE layer (Java / Kotlin / Scala / Groovy) -*- lexical-binding: t -*-
;;; Version: 3.0.4

(require 'core-dev)

(emacs-ide-dev-register "java" :tier 2 :lsp-server "jdtls"
  :formatter "google-java-format" :test-cmd "mvn test" :repl nil
  :modes '(java-mode java-ts-mode))
(emacs-ide-dev-register "kotlin" :tier 2 :lsp-server "kotlin-language-server"
  :formatter "ktlint" :test-cmd "gradle test" :repl nil
  :modes '(kotlin-mode kotlin-ts-mode))
(emacs-ide-dev-register "scala" :tier 2 :lsp-server "metals"
  :formatter "scalafmt" :test-cmd "sbt test" :repl nil
  :modes '(scala-mode))
(emacs-ide-dev-register "groovy" :tier 2 :lsp-server nil
  :formatter nil :test-cmd "gradle test" :repl nil
  :modes '(groovy-mode))

(when (or (emacs-ide-dev-lang-enabled-p "java")
          (emacs-ide-dev-lang-enabled-p "kotlin")
          (emacs-ide-dev-lang-enabled-p "scala")
          (emacs-ide-dev-lang-enabled-p "groovy"))

(emacs-ide-dev-ensure-treesit 'java)

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

(use-package lsp-java
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "java"))
  :defer t
  :hook (java-ts-mode . lsp-deferred)
  :init
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

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "kotlin"))
  :hook (kotlin-mode . lsp-deferred))

(with-eval-after-load 'apheleia
  (when (emacs-ide-dev-lang-enabled-p "kotlin")
    (emacs-ide-dev-attach-formatter 'ktlint 'kotlin-mode)))

(use-package scala-mode
  :if (emacs-ide-dev-lang-enabled-p "scala")
  :defer t
  :mode "\\.\\(scala\\|sbt\\|sc\\)\\'"
  :config
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'scala-mode
      :project-fn (lambda ()
                    (interactive)
                    (if (executable-find "sbt")
                        (compile "sbt test")
                      (message "lang-jvm: sbt not found"))))))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "scala"))
  :hook (scala-mode . lsp-deferred))

(use-package lsp-metals
  :if (and (emacs-ide-dev-lang-enabled-p "scala")
           (or (executable-find "metals")
               (executable-find "cs")))
  :after scala-mode)

(use-package groovy-mode
  :if (emacs-ide-dev-lang-enabled-p "groovy")
  :defer t
  :mode (("\\.groovy\\'" . groovy-mode)
         ("\\.gradle\\'" . groovy-mode)))

)

(provide 'lang-jvm)
;;; lang-jvm.el ends here
