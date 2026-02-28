;;; tools-spelling.el --- Spell Checking -*- lexical-binding: t -*-
;;; Commentary:
;;; Flyspell for prose buffers and flyspell-prog-mode for code.
;;; Version: 2.2.1
;;; Fixes:
;;;   - flyspell-generic-check-word-p predicate was INVERTED: the function
;;;     returned t for URLs/paths/long-tokens (things to SKIP) and nil for
;;;     normal words (things to CHECK). flyspell-generic-check-word-p must
;;;     return t to CHECK a word and nil to SKIP it — the opposite of what
;;;     was written. Fixed: function now returns nil (skip) for symbols/URLs,
;;;     t (check) otherwise, which is the correct semantics.
;;; Code:

;; ============================================================================
;; ISPELL BACKEND
;; ============================================================================
(defun emacs-ide-spell--find-program ()
  "Return the best available spell-check program."
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
  :hook ((org-mode        . flyspell-mode)
         (markdown-mode   . flyspell-mode)
         (text-mode       . flyspell-mode)
         (git-commit-mode . flyspell-mode)
         ;; In code: only check strings and comments
         (prog-mode       . flyspell-prog-mode))
  :init
  (setq flyspell-issue-message-flag  nil
        flyspell-issue-welcome-flag  nil
        flyspell-delay               1.0
        flyspell-use-meta-tab        nil)  ; avoid conflict with completion
  :config
  ;; FIX: predicate was inverted.
  ;; flyspell-generic-check-word-p must return:
  ;;   t   → spell-check this word
  ;;   nil → skip this word
  ;; The old code returned t for URLs/paths (meaning "check these") and
  ;; implicitly nil for normal words (meaning "skip normal words") — backwards.
  ;; Corrected: return nil to SKIP symbols/URLs/long tokens, t to CHECK the rest.
  (defun emacs-ide-spell--check-word-p ()
    "Return t if the word at point should be spell-checked, nil to skip.
Skips URLs, file-path-like symbols, and very long tokens."
    (let ((sym (thing-at-point 'symbol)))
      (not (and sym
                (or (string-match-p "[/_-]" sym)      ; paths / identifiers
                    (string-match-p "^https?://" sym)  ; URLs
                    (> (length sym) 30))))))            ; very long tokens
  (setq flyspell-generic-check-word-p #'emacs-ide-spell--check-word-p))

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
;; TOGGLE HELPERS
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
