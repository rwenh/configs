;;; tools-spelling.el --- Spell Checking -*- lexical-binding: t -*-
;;; Commentary:
;;; NEW MODULE — flyspell for prose buffers and flyspell-prog-mode for
;;; strings/comments in code. Previously only git commit messages got spell
;;; checking (via git-commit.el hook). Keybindings live in keybindings.el.
;;; Add "tools-spelling" to emacs-ide-feature-modules in init.el.
;;; Code:

;; ============================================================================
;; ISPELL BACKEND
;; ============================================================================
(defun emacs-ide-spell--find-program ()
  "Return the best available spell-check program."
  (cond
   ((executable-find "aspell")  "aspell")
   ((executable-find "hunspell") "hunspell")
   ((executable-find "ispell")  "ispell")
   (t nil)))

(when-let ((prog (emacs-ide-spell--find-program)))
  (setq ispell-program-name prog
        ispell-extra-args
        (cond
         ((string-suffix-p "aspell" prog)
          '("--camel-case" "--run-together" "--run-together-limit=5"))
         ((string-suffix-p "hunspell" prog)
          '("-d" "en_US"))
         (t '()))
        ispell-silently-savep t
        ispell-dictionary "english"))

;; ============================================================================
;; FLYSPELL — prose mode (org, markdown, text, git commits)
;; ============================================================================
(use-package flyspell
  :straight nil
  :if (emacs-ide-spell--find-program)
  :hook ((org-mode      . flyspell-mode)
         (markdown-mode . flyspell-mode)
         (text-mode     . flyspell-mode)
         (git-commit-mode . flyspell-mode)
         ;; In code: only check strings and comments
         (prog-mode     . flyspell-prog-mode))
  :init
  (setq flyspell-issue-message-flag   nil
        flyspell-issue-welcome-flag   nil
        flyspell-delay                1.0
        flyspell-use-meta-tab         nil)  ; avoid conflict with completion
  :config
  ;; Don't spell-check symbols, URLs, or code identifiers
  (defun emacs-ide-spell-skip-p ()
    "Return t if point is in a URL, symbol, or import statement."
    (let ((sym (thing-at-point 'symbol)))
      (and sym (or (string-match-p "[/_-]" sym)    ; paths/identifiers
                   (string-match-p "^https?://" sym) ; URLs
                   (> (length sym) 30)))))           ; very long tokens
  (setq flyspell-generic-check-word-p #'emacs-ide-spell-skip-p))

;; ============================================================================
;; FLYSPELL-CORRECT — consult-based correction UI
;; ============================================================================
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-avy-menu
  :after flyspell-correct)

;; ============================================================================
;; TOGGLE HELPER
;; ============================================================================
(defun emacs-ide-spell-toggle ()
  "Toggle flyspell in the current buffer."
  (interactive)
  (if (bound-and-true-p flyspell-mode)
      (progn (flyspell-mode -1) (message "Spell check OFF"))
    (if (derived-mode-p 'prog-mode)
        (flyspell-prog-mode)
      (flyspell-mode 1))
    (message "Spell check ON")))

(defun emacs-ide-spell-buffer ()
  "Run flyspell over the entire buffer."
  (interactive)
  (if (bound-and-true-p flyspell-mode)
      (flyspell-buffer)
    (flyspell-mode 1)
    (flyspell-buffer)))

(provide 'tools-spelling)
;;; tools-spelling.el ends here
