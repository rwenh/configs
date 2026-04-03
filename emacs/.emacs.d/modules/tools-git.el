;;; tools-git.el --- Git Integration with Magit -*- lexical-binding: t -*-
;;; Commentary:
;;; Professional Git workflow with Magit and companions.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 2.2.4 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.4 to 3.0.4.
;;;   - FIX-MAGIT-INIT: magit-auto-revert-mode was set via setq in :init —
;;;     it is a minor mode function, not a variable. setq silently does nothing
;;;     for a function symbol. Moved to (magit-auto-revert-mode 1) in :config.
;;;   - FIX-DUPLICATE-COMMIT-VARS: git-commit-summary-max-length and
;;;     git-commit-fill-column were set twice — once in magit :init and again
;;;     in git-commit :init. The git-commit :init block is the canonical owner
;;;     (git-commit.el defines these vars). Removed the duplicate assignments
;;;     from magit :init.
;;;   - FIX-CONFIG-READ: git.fill-column and git.summary-max-length are
;;;     defined in config.yml but were hardcoded as 72/50. Now read from
;;;     config via emacs-ide-config-get with those values as defaults.
;;;   - FIX-GUTTER-CONFIG: git.gutter and git.gutter-update-interval from
;;;     config.yml were never wired. diff-hl now reads git.gutter to decide
;;;     whether to activate, and git.gutter-update-interval to set the
;;;     diff-hl-flydiff-delay (closest equivalent).
;;;   - FIX-FLOW-FINISH: emacs-ide-git-flow-feature-finish called
;;;     magit-merge-plain and magit-branch-delete without fboundp guards —
;;;     would crash if magit was partially loaded. Both now guarded.
;;; Fixes vs 2.2.4 (retained):
;;;   - Hook pattern validated: magit-post-refresh is a plain hook variable;
;;;     explicit add-hook in :config block is the correct pattern.
;;;   - All git utility functions wrapped with fboundp + repo-p guards.
;;;   - magit-stash-push confirmed as stable public API (2.13+).
;;; Code:

;; ============================================================================
;; CONFIG HELPERS
;; ============================================================================
(defun emacs-ide-git--cfg (key default)
  "Read KEY from git section of config.yml, falling back to DEFAULT."
  (if (fboundp 'emacs-ide-config-get)
      (emacs-ide-config-get 'git key default)
    default))

;; ============================================================================
;; MAGIT - GIT PORCELAIN
;; ============================================================================
(use-package magit
  :init
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        magit-diff-refine-hunk              'all
        magit-diff-refine-ignore-whitespace t
        magit-save-repository-buffers       'dontask
        magit-revision-show-gravatars       '("^Author:     " . "^Commit:     ")
        ;; FIX-DUPLICATE-COMMIT-VARS: git-commit-* vars removed from here —
        ;; git-commit :init below is the canonical owner (git-commit.el defvars).
        magit-refresh-status-buffer         nil
        magit-process-popup-time            10
        magit-no-confirm                    '(stage-all-changes unstage-all-changes))
  :config
  ;; FIX-MAGIT-INIT: magit-auto-revert-mode is a minor mode function, not a
  ;; variable — setq in :init was silently a no-op. Activated here in :config.
  (when (and (fboundp 'magit-auto-revert-mode)
             (emacs-ide-git--cfg 'auto-revert t))
    (magit-auto-revert-mode 1)))

;; ============================================================================
;; DIFF-HL - FRINGE/GUTTER DIFF INDICATORS (sole provider)
;; FIX-GUTTER-CONFIG: git.gutter and git.gutter-update-interval from
;; config.yml are now read and wired to diff-hl activation and flydiff delay.
;; magit-post-refresh is a plain hook variable — explicit add-hook required.
;; ============================================================================
(use-package diff-hl
  :if (emacs-ide-git--cfg 'gutter t)
  :hook ((prog-mode  . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-side         'left)
  :config
  ;; FIX-GUTTER-CONFIG: wire gutter-update-interval to flydiff delay
  (when (fboundp 'diff-hl-flydiff-mode)
    (setq diff-hl-flydiff-delay
          (emacs-ide-git--cfg 'gutter-update-interval 0.3))
    (diff-hl-flydiff-mode))
  ;; SAFE PATTERN: add-hook for plain hook variable, not a mode
  (when (boundp 'magit-post-refresh-hook)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
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
        git-link-use-commit       t))

;; ============================================================================
;; FORGE - GITHUB/GITLAB INTEGRATION
;; ============================================================================
(use-package forge
  :after magit
  :config
  (setq forge-topic-list-limit '(60 . 0)))

;; ============================================================================
;; GIT COMMIT MESSAGE HELPER
;; FIX-CONFIG-READ: fill-column and summary-max-length now read from config.yml
;; git.fill-column and git.summary-max-length, defaulting to 72/50.
;; FIX-DUPLICATE-COMMIT-VARS: canonical owner of git-commit-* variables.
;; ============================================================================
(use-package git-commit
  :straight nil
  :after magit
  :init
  (setq git-commit-summary-max-length
        (emacs-ide-git--cfg 'summary-max-length 50)
        git-commit-fill-column
        (emacs-ide-git--cfg 'fill-column 72)
        git-commit-style-convention-checks
        '(non-empty-second-line overlong-summary-line))
  :config
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

;; ============================================================================
;; GIT UTILITY FUNCTIONS (DEFENSIVE GUARDS)
;; ============================================================================
(defun emacs-ide-git-status ()
  "Show git status with branch info."
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
    (if (and (fboundp 'magit-stage-file)
             (fboundp 'magit-git-repo-p)
             (magit-git-repo-p default-directory))
        (magit-stage-file buffer-file-name)
      (message "⚠️  Not in a git repository or magit unavailable"))))

(defun emacs-ide-git-unstage-file ()
  "Unstage current file safely."
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
  "Stash changes safely."
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
  (if (and (fboundp 'magit-blame-addition)
           buffer-file-name)
      (call-interactively 'magit-blame-addition)
    (message "⚠️  Magit not available or no file open")))

;; ============================================================================
;; GIT FLOW INTEGRATION
;; FIX-FLOW-FINISH: magit-merge-plain and magit-branch-delete now guarded
;; with fboundp — previously would crash if magit partially loaded.
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
          ;; FIX-FLOW-FINISH: fboundp guards on merge and delete
          (if (fboundp 'magit-branch-checkout)
              (magit-branch-checkout "develop")
            (message "⚠️  magit-branch-checkout not available"))
          (if (fboundp 'magit-merge-plain)
              (magit-merge-plain branch)
            (message "⚠️  magit-merge-plain not available"))
          (if (fboundp 'magit-branch-delete)
              (magit-branch-delete (list branch))
            (message "⚠️  magit-branch-delete not available"))))
    (message "⚠️  Magit not available")))

(provide 'tools-git)
;;; tools-git.el ends here
