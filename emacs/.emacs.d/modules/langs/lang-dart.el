;;; lang-dart.el --- Dart / Flutter IDE layer -*- lexical-binding: t -*-
;;; Version: 3.4.0
;;;
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "dart"
  :tier 3
  :lsp-server "dart"
  :formatter  "dart format"
  :test-cmd   "flutter test"
  :repl       nil
  :modes      '(dart-mode))

(unless (emacs-ide-dev-lang-enabled-p "dart")
  (provide 'lang-dart)
  (message "lang-dart: disabled in config.yml"))

(when (emacs-ide-dev-lang-enabled-p "dart")

;;;; ── Detect Flutter vs plain Dart ───────────────────────────────────────────

(defun emacs-ide-dart--flutter-p ()
  "Return non-nil if the current project is a Flutter project."
  (when-let ((root (or (and (fboundp 'projectile-project-root)
                            (ignore-errors (projectile-project-root)))
                       default-directory)))
    (let ((pubspec (expand-file-name "pubspec.yaml" root)))
      (when (file-exists-p pubspec)
        (with-temp-buffer
          (insert-file-contents pubspec)
          (re-search-forward "flutter:" nil t))))))

;;;; ── Run helpers ─────────────────────────────────────────────────────────────

(defun emacs-ide-dart-run ()
  "Run the current Dart file or Flutter app."
  (interactive)
  (cond
   ((emacs-ide-dart--flutter-p)
    (if (executable-find "flutter")
        (compile "flutter run")
      (message "lang-dart: flutter not found on PATH")))
   ((executable-find "dart")
    (compile (format "dart run %s"
                     (shell-quote-argument (buffer-file-name)))))
   (t (message "lang-dart: dart not found on PATH"))))

(defun emacs-ide-dart-build ()
  "Build the Flutter app or compile Dart."
  (interactive)
  (if (emacs-ide-dart--flutter-p)
      (if (executable-find "flutter")
          (compile "flutter build apk")
        (message "lang-dart: flutter not found on PATH"))
    (if (executable-find "dart")
        (compile (format "dart compile exe %s"
                         (shell-quote-argument (buffer-file-name))))
      (message "lang-dart: dart not found on PATH"))))

;;;; ── Test helpers ────────────────────────────────────────────────────────────

(defun emacs-ide-dart-test-project ()
  "Run all tests for the Dart/Flutter project."
  (interactive)
  (cond
   ((and (emacs-ide-dart--flutter-p) (executable-find "flutter"))
    (compile "flutter test"))
   ((executable-find "dart")
    (compile "dart test"))
   (t (message "lang-dart: dart/flutter not found on PATH"))))

(defun emacs-ide-dart-test-file ()
  "Run tests in the current Dart test file."
  (interactive)
  (cond
   ((and (emacs-ide-dart--flutter-p)
         (executable-find "flutter")
         (buffer-file-name))
    (compile (format "flutter test %s"
                     (shell-quote-argument (buffer-file-name)))))
   ((and (executable-find "dart") (buffer-file-name))
    (compile (format "dart test %s"
                     (shell-quote-argument (buffer-file-name)))))
   (t (message "lang-dart: dart/flutter not found or no file"))))

;;;; ── dart-mode ───────────────────────────────────────────────────────────────

(use-package dart-mode
  :if (executable-find "dart")
  :defer t
  :mode "\\.dart\\'"
  :init
  (setq dart-format-on-save nil)   ; apheleia handles formatting
  :config
  (emacs-ide-dev-bind-compile dart-mode-map #'emacs-ide-dart-run)
  (define-key dart-mode-map (kbd "C-c C-b") #'emacs-ide-dart-build)
  (define-key dart-mode-map (kbd "C-c C-f")
    (lambda () (interactive)
      (when (executable-find "flutter")
        (compile "flutter run --debug")))))

;;;; ── LSP (Dart Analysis Server) ─────────────────────────────────────────────

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (executable-find "dart"))
  :hook (dart-mode . lsp-deferred)
  :config
  ;; Dart SDK includes the analysis server — lsp-mode auto-detects it
  (when (boundp 'lsp-dart-sdk-dir)
    (let* ((dart-bin (executable-find "dart"))
           (dart-sdk (when dart-bin
                       (file-name-directory
                        (file-truename dart-bin)))))
      (when dart-sdk
        (setq lsp-dart-sdk-dir (file-name-directory (directory-file-name dart-sdk)))))))

;;;; ── flutter.el (optional — enhanced Flutter commands) ──────────────────────

(use-package flutter
  :if (executable-find "flutter")
  :after dart-mode
  :defer t
  :bind (:map dart-mode-map
              ("C-M-x" . flutter-run-or-hot-reload)))

;;;; ── pubspec (pub package management) ───────────────────────────────────────

(defun emacs-ide-dart-pub-get ()
  "Run dart pub get / flutter pub get."
  (interactive)
  (if (emacs-ide-dart--flutter-p)
      (compile "flutter pub get")
    (compile "dart pub get")))

(defun emacs-ide-dart-pub-upgrade ()
  "Run dart pub upgrade / flutter pub upgrade."
  (interactive)
  (if (emacs-ide-dart--flutter-p)
      (compile "flutter pub upgrade")
    (compile "dart pub upgrade")))

;;;; ── Formatter (dart format via apheleia) ───────────────────────────────────

(with-eval-after-load 'apheleia
  (when (executable-find "dart")
    (unless (assq 'dart-format apheleia-formatters)
      (push '(dart-format "dart" "format" "--output=show" "-")
            apheleia-formatters))
    (setf (alist-get 'dart-mode apheleia-mode-alist) 'dart-format)))

;;;; ── Test runner registration ────────────────────────────────────────────────

(with-eval-after-load 'tools-test-runner-registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'dart-mode
      :file-fn    #'emacs-ide-dart-test-file
      :project-fn #'emacs-ide-dart-test-project)))

) ;; end dart-enabled

(provide 'lang-dart)
;;; lang-dart.el ends here
