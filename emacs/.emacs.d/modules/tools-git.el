;;; tools-git.el --- Git Integration with Magit -*- lexical-binding: t -*-
;;; Commentary:
;;; Professional Git workflow with Magit and companions.
;;; Version: 2.2.1
;;; Fixes:
;;;   - magit-refresh-status-buffer: set to t in :init then immediately
;;;     overridden to nil in :config — dead code removed; nil is the intent
;;;   - emacs-ide-git-status: called non-existent magit-rev-diff-count;
;;;     replaced with magit-git-string for a safe ahead/behind display
;;;   - emacs-ide-git-stash: magit-stash-both signature mismatch; replaced
;;;     with magit-stash-push which has a stable public API
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
        ;; FIX: was set to t in :init then nil in :config; canonical value is nil
        magit-refresh-status-buffer nil
        magit-process-popup-time 10
        magit-no-confirm '(stage-all-changes unstage-all-changes)
        git-commit-summary-max-length 50
        git-commit-fill-column 72
        git-commit-style-convention-checks '(non-empty-second-line)))

;; ============================================================================
;; GIT-GUTTER - VISUAL DIFF IN FRINGE
;; ============================================================================
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :init
  (setq git-gutter:update-interval 0.3
        git-gutter:modified-sign "│"
        git-gutter:added-sign    "│"
        git-gutter:deleted-sign  "│"
        git-gutter:hide-gutter   t)
  :config
  (set-face-foreground 'git-gutter:modified "#f9e2af")
  (set-face-foreground 'git-gutter:added    "#a6e3a1")
  (set-face-foreground 'git-gutter:deleted  "#f38ba8"))

;; ============================================================================
;; DIFF-HL - ALTERNATIVE TO GIT-GUTTER
;; ============================================================================
(use-package diff-hl
  :hook ((prog-mode    . diff-hl-mode)
         (dired-mode   . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-side 'left)
  :config
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
;; GIT UTILITY FUNCTIONS
;; ============================================================================
(defun emacs-ide-git-status ()
  "Show git status with branch info.
FIX: magit-rev-diff-count does not exist; simplified to show branch/remote
     using only stable magit APIs."
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
  "Stage current file safely."
  (interactive)
  (when buffer-file-name
    (if (fboundp 'magit-stage-file)
        (magit-stage-file buffer-file-name)
      (message "⚠️  Magit not available"))))

(defun emacs-ide-git-unstage-file ()
  "Unstage current file safely."
  (interactive)
  (when buffer-file-name
    (if (fboundp 'magit-unstage-file)
        (magit-unstage-file buffer-file-name)
      (message "⚠️  Magit not available"))))

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
  "Stash changes safely.
FIX: magit-stash-both had wrong signature; replaced with magit-stash-push."
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
  "Toggle git blame for current buffer safely."
  (interactive)
  (if (fboundp 'magit-blame-addition)
      (call-interactively 'magit-blame-addition)
    (message "⚠️  Magit not available")))

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
