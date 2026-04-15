;;; tools-spelling.el --- Spell Checking -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Code:

(defun emacs-ide-spell--find-program ()
  (cond
   ((executable-find "aspell")   "aspell")
   ((executable-find "hunspell") "hunspell")
   ((executable-find "ispell")   "ispell")
   (t nil)))

(when-let ((prog (emacs-ide-spell--find-program)))
  (setq ispell-program-name prog
        ispell-extra-args
        (cond
         ((string-suffix-p "aspell" prog)
          '("--run-together" "--run-together-limit=5"))
         ((string-suffix-p "hunspell" prog)
          '("-d" "en_US"))
         (t '()))
        ispell-silently-savep t
        ispell-dictionary
        (cond
         ((string-suffix-p "hunspell" prog) "en_US")
         ((string-suffix-p "aspell"   prog) "en_US")
         (t "english"))))

(defun emacs-ide-spell--check-word-p ()
  (let ((sym (thing-at-point 'symbol)))
    (not (and sym
              (or (string-match-p "[_/]" sym)
                  (string-match-p "^https?://" sym)
                  (> (length sym) 30))))))

(use-package flyspell
  :straight nil
  :if (emacs-ide-spell--find-program)
  :hook ((org-mode        . flyspell-mode)
         (markdown-mode   . flyspell-mode)
         (text-mode       . flyspell-mode)
         (git-commit-mode . flyspell-mode)
         (prog-mode       . flyspell-prog-mode))
  :bind (("C-c S s" . ispell-word)
         ("C-c S b" . emacs-ide-spell-buffer)
         ("C-c S n" . flyspell-goto-next-error)
         ("C-c S t" . emacs-ide-spell-toggle))
  :init
  (setq flyspell-issue-message-flag  nil
        flyspell-issue-welcome-flag  nil
        flyspell-delay               1.0
        flyspell-use-meta-tab        nil)
  :config
  (setq flyspell-generic-check-word-p #'emacs-ide-spell--check-word-p))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c S c" . flyspell-correct-wrapper)))

(use-package flyspell-correct-avy-menu
  :after flyspell-correct)

(defun emacs-ide-spell-toggle ()
  (interactive)
  (if (bound-and-true-p flyspell-mode)
      (progn (flyspell-mode -1) (message "Spell check OFF"))
    (if (derived-mode-p 'prog-mode)
        (flyspell-prog-mode)
      (flyspell-mode 1))
    (message "Spell check ON")))

(defun emacs-ide-spell-buffer ()
  (interactive)
  (if (bound-and-true-p flyspell-mode)
      (flyspell-buffer)
    (flyspell-mode 1)
    (flyspell-buffer)))

(provide 'tools-spelling)
;;; tools-spelling.el ends here
