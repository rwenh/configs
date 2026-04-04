;;; tools-spelling.el --- Spell Checking -*- lexical-binding: t -*-
;;; Commentary:
;;; Flyspell for prose buffers and flyspell-prog-mode for code.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 2.2.2 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.2 to 3.0.4.
;;;   - FIX-ISPELL-DICT: ispell-dictionary was set to "english" which is not
;;;     a valid dictionary name for all backends — aspell uses "en" or "en_US",
;;;     hunspell uses "en_US", and "english" causes "No matching entry" errors
;;;     on some systems. Now set per-backend to the correct dictionary name.
;;;   - FIX-ASPELL-ARGS: --camel-case is only available in aspell 0.60.8+;
;;;     older installations error on startup. Removed from the default args —
;;;     --run-together handles compound word splitting sufficiently.
;;;   - FIX-DEFUN-IN-CONFIG: emacs-ide-spell--check-word-p was defined inside
;;;     use-package :config, meaning it only existed after flyspell loaded.
;;;     Moved to a top-level defun so it is always available and the
;;;     flyspell-generic-check-word-p assignment in :config is never lost.
;;;   - FIX-CHECK-WORD-HYPHEN: The predicate skipped any symbol containing "-"
;;;     via [/_-], incorrectly skipping valid hyphenated prose words like
;;;     "well-known", "co-author", "self-contained". Changed the pattern to
;;;     only skip underscore/slash (code identifiers), not hyphens.
;;;   - FIX-FLYSPELL-CORRECT-WRAPPER: flyspell-correct-wrapper was bound in
;;;     the flyspell :bind block but is defined in flyspell-correct. If
;;;     flyspell-correct had not loaded yet the binding resolved to void-function.
;;;     Moved to the flyspell-correct :bind block where it belongs.
;;;   - FIX-PROG-MODE-TOGGLE: emacs-ide-spell-toggle ON branch called
;;;     (flyspell-prog-mode) with no way to turn it OFF via (flyspell-mode -1).
;;;     flyspell-prog-mode is a thin wrapper that calls flyspell-mode internally
;;;     — (flyspell-mode -1) correctly disables both. The toggle now consistently
;;;     uses (flyspell-mode -1) for OFF in all cases.
;;; Fixes vs 2.2.2 (retained):
;;;   - Predicate inversion fix: nil=skip, t=check.
;;;   - C-; collision fixed: flyspell-correct-wrapper at C-c S c.
;;;   - C-c S bindings self-contained in this module.
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
         ;; FIX-ASPELL-ARGS: --camel-case removed (requires aspell 0.60.8+,
         ;; errors on older systems). --run-together handles compound words.
         ((string-suffix-p "aspell" prog)
          '("--run-together" "--run-together-limit=5"))
         ((string-suffix-p "hunspell" prog)
          '("-d" "en_US"))
         (t '()))
        ispell-silently-savep t
        ;; FIX-ISPELL-DICT: set per-backend to the correct dictionary name.
        ;; "english" is not valid for aspell/hunspell; use "en_US" universally
        ;; which both backends accept (aspell also accepts "en").
        ispell-dictionary
        (cond
         ((string-suffix-p "hunspell" prog) "en_US")
         ((string-suffix-p "aspell"   prog) "en_US")
         (t "english"))))   ; ispell legacy name

;; ============================================================================
;; WORD CHECK PREDICATE
;; FIX-DEFUN-IN-CONFIG: defined at top level so it is always available,
;; not just after flyspell loads.
;; FIX-CHECK-WORD-HYPHEN: hyphen removed from the skip pattern — hyphenated
;; prose words (well-known, co-author) should be spell-checked. Only
;; underscore and slash (code path/identifier markers) are skipped.
;; ============================================================================
(defun emacs-ide-spell--check-word-p ()
  "Return t if the word at point should be spell-checked, nil to skip.
Skips URLs, file-path/identifier symbols (with _ or /), and very long tokens.
Does NOT skip hyphenated words — those are valid English prose."
  (let ((sym (thing-at-point 'symbol)))
    (not (and sym
              (or (string-match-p "[_/]" sym)           ; code identifiers/paths
                  (string-match-p "^https?://" sym)     ; URLs
                  (> (length sym) 30))))))               ; very long tokens

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
  :bind (;; Global C-c S prefix — self-contained, not dependent on keybindings.el
         ("C-c S s" . ispell-word)
         ("C-c S b" . emacs-ide-spell-buffer)
         ("C-c S n" . flyspell-goto-next-error)
         ("C-c S t" . emacs-ide-spell-toggle))
  ;; FIX-FLYSPELL-CORRECT-WRAPPER: C-c S c binding moved to flyspell-correct
  ;; :bind block below — flyspell-correct-wrapper is defined there, not here.
  :init
  (setq flyspell-issue-message-flag  nil
        flyspell-issue-welcome-flag  nil
        flyspell-delay               1.0
        flyspell-use-meta-tab        nil)  ; avoid conflict with completion
  :config
  ;; FIX-DEFUN-IN-CONFIG: predicate now defined at top level above.
  ;; Assignment here is still correct — flyspell must be loaded before
  ;; flyspell-generic-check-word-p can be set meaningfully.
  (setq flyspell-generic-check-word-p #'emacs-ide-spell--check-word-p))

;; ============================================================================
;; FLYSPELL-CORRECT — consult-based correction UI
;; FIX-FLYSPELL-CORRECT-WRAPPER: C-c S c binding moved here from flyspell
;; :bind. flyspell-correct-wrapper is defined in this package; binding it
;; in the flyspell block caused void-function when flyspell-correct hadn't
;; loaded yet.
;; ============================================================================
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c S c" . flyspell-correct-wrapper)))

(use-package flyspell-correct-avy-menu
  :after flyspell-correct)

;; ============================================================================
;; TOGGLE HELPERS
;; FIX-PROG-MODE-TOGGLE: OFF branch now consistently uses (flyspell-mode -1)
;; for both prose and prog modes — flyspell-prog-mode is a thin wrapper that
;; enables flyspell-mode internally, so (flyspell-mode -1) disables both.
;; ============================================================================
(defun emacs-ide-spell-toggle ()
  "Toggle flyspell in the current buffer."
  (interactive)
  (if (bound-and-true-p flyspell-mode)
      ;; FIX-PROG-MODE-TOGGLE: single consistent OFF path for all modes
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
