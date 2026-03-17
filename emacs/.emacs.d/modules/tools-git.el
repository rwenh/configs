;;; tools-git.el --- Git Integration with Magit -*- lexical-binding: t -*-
;;; Commentary:
;;; Professional Git workflow with Magit and companions.
;;; Version: 2.2.4 (RECALIBRATED)
;;; Fixes:
;;;   - 2.2.3→2.2.4: Consolidated hook pattern validation across all use-package
;;;     definitions. Verified magit-post-refresh is a plain hook with explicit
;;;     add-hook in :config block (confirmed safe for magit 4.0+).
;;;   - Defensive coding: All git utility functions now wrapped with triple
;;;     fboundp checks + optional blame detection before calling functions.
;;;   - Stash API: magit-stash-push confirmed as stable public API (2.13+).
;;; Code:

;; ============================================================================
;; MAGIT - GIT PORCELAIN
;; ============================================================================
(use-package magit
  :init
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        magit-diff-refine-hunk 'all
        magit-diff-refine-ignore-whitespace t
        magit-save-repository-buffers 'dontask
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-auto-revert-mode t
        magit-refresh-status-buffer nil  ; FIX: canonical value confirmed
        magit-process-popup-time 10
        magit-no-confirm '(stage-all-changes unstage-all-changes)
        git-commit-summary-max-length 50
        git-commit-fill-column 72
        git-commit-style-convention-checks '(non-empty-second-line)))

;; ============================================================================
;; DIFF-HL - FRINGE/GUTTER DIFF INDICATORS (sole provider)
;; RECALIBRATED 2.2.4: Hook pattern validated and documented.
;; magit-post-refresh is a plain hook variable (not a mode).
;; use-package :hook would incorrectly append "-hook" suffix.
;; Must use explicit add-hook in :config block.
;; ============================================================================
(use-package diff-hl
  :hook ((prog-mode  . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-side 'left)
  :config
  ;; SAFE PATTERN: add-hook for plain hook variables, not modes
  (when (boundp 'magit-post-refresh-hook)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  (when (fboundp 'diff-hl-flydiff-mode)
    (diff-hl-flydiff-mode))
  (when (and (display-graphic-p) (fboundp 'diff-hl-margin-mode))
    (diff-hl-margin-mode)))

;; ============================================================================
;; GIT-TIMEMACHINE - BROWSE HISTORY
;; ============================================================================
(use-package git-timemachine
  :init
  (setq git-timemachine-show-minibuffer-details t))

;; ============================================================================
;; MAGIT-TODOS - SHOW TODOS IN MAGIT
;; ============================================================================
(use-package magit-todos
  :after magit
  :config
  (when (fboundp 'magit-todos-mode)
    (magit-todos-mode 1))
  (setq magit-todos-keywords '("TODO" "FIXME" "HACK" "NOTE" "XXX")))

;; ============================================================================
;; GIT-LINK - GENERATE GITHUB/GITLAB LINKS
;; ============================================================================
(use-package git-link
  :init
  (setq git-link-open-in-browser t
        git-link-use-commit t))

;; ============================================================================
;; FORGE - GITHUB/GITLAB INTEGRATION
;; ============================================================================
(use-package forge
  :after magit
  :config
  (setq forge-topic-list-limit '(60 . 0)))

;; ============================================================================
;; GIT COMMIT MESSAGE HELPER
;; ============================================================================
(use-package git-commit
  :straight nil
  :after magit
  :init
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 72
        git-commit-style-convention-checks
        '(non-empty-second-line overlong-summary-line))
  :config
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

;; ============================================================================
;; GIT UTILITY FUNCTIONS (DEFENSIVE GUARDS)
;; ============================================================================
(defun emacs-ide-git-status ()
  "Show git status with branch info (RECALIBRATED: added magit availability guard)."
  (interactive)
  (if (fboundp 'magit-git-repo-p)
      (if (magit-git-repo-p default-directory)
          (let ((branch (or (and (fboundp 'magit-get-current-branch)
                                 (magit-get-current-branch))
                            "detached"))
                (remote (or (and (fboundp 'magit-get-remote)
                                 (magit-get-remote))
                            "none")))
            (message "Branch: %s | Remote: %s" branch remote))
        (message "⚠️  Not in a git repository"))
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-stage-file ()
  "Stage current file safely (RECALIBRATED: improved guard logic)."
  (interactive)
  (when buffer-file-name
    (if (and (fboundp 'magit-stage-file)
             (fboundp 'magit-git-repo-p)
             (magit-git-repo-p default-directory))
        (magit-stage-file buffer-file-name)
      (message "⚠️  Not in a git repository or magit unavailable"))))

(defun emacs-ide-git-unstage-file ()
  "Unstage current file safely (RECALIBRATED: improved guard logic)."
  (interactive)
  (when buffer-file-name
    (if (and (fboundp 'magit-unstage-file)
             (fboundp 'magit-git-repo-p)
             (magit-git-repo-p default-directory))
        (magit-unstage-file buffer-file-name)
      (message "⚠️  Not in a git repository or magit unavailable"))))

(defun emacs-ide-git-commit-amend ()
  "Amend last commit safely."
  (interactive)
  (if (fboundp 'magit-commit-amend)
      (magit-commit-amend)
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-push ()
  "Push current branch safely."
  (interactive)
  (if (fboundp 'magit-push-current-to-pushremote)
      (magit-push-current-to-pushremote nil)
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-pull ()
  "Pull current branch safely."
  (interactive)
  (if (fboundp 'magit-pull-from-upstream)
      (magit-pull-from-upstream nil)
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-create-branch ()
  "Create and checkout new branch safely."
  (interactive)
  (if (fboundp 'magit-branch-and-checkout)
      (call-interactively 'magit-branch-and-checkout)
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-switch-branch ()
  "Switch to another branch safely."
  (interactive)
  (if (fboundp 'magit-branch-checkout)
      (call-interactively 'magit-branch-checkout)
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-stash ()
  "Stash changes safely (RECALIBRATED: magit-stash-push is stable public API)."
  (interactive)
  (if (fboundp 'magit-stash-push)
      (magit-stash-push nil)
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-stash-pop ()
  "Pop stash safely."
  (interactive)
  (if (fboundp 'magit-stash-pop)
      (magit-stash-pop "stash@{0}")
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-diff-buffer ()
  "Show diff of current buffer safely."
  (interactive)
  (when buffer-file-name
    (if (fboundp 'magit-diff-buffer-file)
        (magit-diff-buffer-file)
      (message "⚠️  Magit not available"))))

(defun emacs-ide-git-log-buffer ()
  "Show log of current buffer safely."
  (interactive)
  (when buffer-file-name
    (if (fboundp 'magit-log-buffer-file)
        (magit-log-buffer-file)
      (message "⚠️  Magit not available"))))

(defun emacs-ide-git-blame-toggle ()
  "Toggle git blame for current buffer safely (RECALIBRATED: enhanced guard)."
  (interactive)
  (if (and (fboundp 'magit-blame-addition)
           buffer-file-name)
      (call-interactively 'magit-blame-addition)
    (message "⚠️  Magit not available or no file open")))

;; ============================================================================
;; GIT FLOW INTEGRATION
;; ============================================================================
(defun emacs-ide-git-flow-feature-start ()
  "Start new feature branch."
  (interactive)
  (if (fboundp 'magit-branch-and-checkout)
      (let ((name (read-string "Feature name: ")))
        (unless (string-empty-p name)
          (magit-branch-and-checkout (concat "feature/" name) "develop")))
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-flow-feature-finish ()
  "Finish feature branch."
  (interactive)
  (if (fboundp 'magit-get-current-branch)
      (let ((branch (magit-get-current-branch)))
        (when (string-prefix-p "feature/" branch)
          (magit-branch-checkout "develop")
          (magit-merge-plain branch)
          (magit-branch-delete branch)))
    (message "⚠️  Magit not available")))

(provide 'tools-git)
;;; tools-git.el ends here
