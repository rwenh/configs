;;; tools-git.el --- Git Integration with Magit -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(defun emacs-ide-git--cfg (key default)
  "Read KEY from the git config section, returning DEFAULT if absent."
  (if (fboundp 'emacs-ide-config-get)
      (emacs-ide-config-get 'git key default)
    default))

;; emacs-ide-git-enable is declared and set by emacs-ide-config.el.
(when (bound-and-true-p emacs-ide-git-enable)

;;; ─── Magit ───────────────────────────────────────────────────────────────────

(use-package magit
  :init
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        magit-diff-refine-hunk               'all
        magit-diff-refine-ignore-whitespace   t
        magit-save-repository-buffers        'dontask
        magit-revision-show-gravatars        '("^Author:     " . "^Commit:     ")
        magit-refresh-status-buffer           nil
        magit-process-popup-time              10
        magit-no-confirm                     '(stage-all-changes unstage-all-changes)
        magit-log-section-commit-count        10
        magit-diff-expansion-threshold        2.0
        magit-section-initial-visibility-alist
        '((stashes . hide) (untracked . show) (staged . show) (unstaged . show))
        magit-git-executable                 (or (executable-find "git") "git")
        magit-process-connection-type         nil)
  :config
  (when (and (fboundp 'magit-auto-revert-mode)
             (emacs-ide-git--cfg 'auto-revert t))
    (magit-auto-revert-mode 1))
  )

;;; ─── diff-hl (gutter indicators) ────────────────────────────────────────────

(use-package diff-hl
  :if (emacs-ide-git--cfg 'gutter t)
  :hook ((prog-mode  . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (text-mode  . diff-hl-mode))
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-side         'left
        diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
  :config
  (when (fboundp 'diff-hl-flydiff-mode)
    (setq diff-hl-flydiff-delay
          (emacs-ide-git--cfg 'gutter-update-interval 0.3))
    (diff-hl-flydiff-mode))
  (when (boundp 'magit-post-refresh-hook)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  (unless (display-graphic-p)
    (when (fboundp 'diff-hl-margin-mode) (diff-hl-margin-mode 1))))

;;; ─── git-timemachine ─────────────────────────────────────────────────────────

(use-package git-timemachine
  :init (setq git-timemachine-show-minibuffer-details t)
  :bind ("C-x v t" . git-timemachine))

;;; ─── magit-todos ─────────────────────────────────────────────────────────────

(use-package magit-todos
  :after magit
  :config
  (when (fboundp 'magit-todos-mode) (magit-todos-mode 1))
  (setq magit-todos-keywords
        '("TODO" "FIXME" "HACK" "NOTE" "XXX" "REVIEW" "WARN" "DEPRECATED" "PERF")))

;;; ─── git-link ────────────────────────────────────────────────────────────────

(use-package git-link
  :init (setq git-link-open-in-browser t
              git-link-use-commit       t))

;;; ─── forge ───────────────────────────────────────────────────────────────────

(use-package forge
  :after magit
  :config (setq forge-topic-list-limit '(60 . 0)))

;;; ─── git-commit ──────────────────────────────────────────────────────────────

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
  (add-hook 'git-commit-mode-hook #'flyspell-mode))

;;; ─── Convenience commands ────────────────────────────────────────────────────

(defun emacs-ide-git-status ()
  "Show current branch and remote in the echo area."
  (interactive)
  (if (not (fboundp 'magit-git-repo-p))
      (message "Magit not available")
    (if (magit-git-repo-p default-directory)
        (let ((branch (or (and (fboundp 'magit-get-current-branch)
                               (magit-get-current-branch))
                          "detached"))
              (remote (or (and (fboundp 'magit-get-remote)
                               (magit-get-remote))
                          "none")))
          (message "Git: %s  remote: %s" branch remote))
      (message "Not in a git repository"))))

(defun emacs-ide-git-stage-file ()
  "Stage the current file in Magit."
  (interactive)
  (when buffer-file-name
    (if (and (fboundp 'magit-stage-file)
             (fboundp 'magit-git-repo-p)
             (magit-git-repo-p default-directory))
        (progn
          (magit-stage-file buffer-file-name)
          (message "Staged: %s" (file-name-nondirectory buffer-file-name)))
      (message "Not in a git repository or Magit unavailable"))))

(defun emacs-ide-git-unstage-file ()
  "Unstage the current file in Magit."
  (interactive)
  (when buffer-file-name
    (if (and (fboundp 'magit-unstage-file)
             (fboundp 'magit-git-repo-p)
             (magit-git-repo-p default-directory))
        (magit-unstage-file buffer-file-name)
      (message "Not in a git repository or Magit unavailable"))))

(defun emacs-ide-git-commit-amend ()
  "Amend the last commit via Magit."
  (interactive)
  (if (fboundp 'magit-commit-amend)
      (magit-commit-amend)
    (message "Magit not available")))

(defun emacs-ide-git-push ()
  "Push to the push remote via Magit."
  (interactive)
  (if (fboundp 'magit-push-current-to-pushremote)
      (magit-push-current-to-pushremote nil)
    (message "Magit not available")))

(defun emacs-ide-git-pull ()
  "Pull from upstream via Magit."
  (interactive)
  (if (fboundp 'magit-pull-from-upstream)
      (magit-pull-from-upstream nil)
    (message "Magit not available")))

(defun emacs-ide-git-create-branch ()
  "Create and checkout a new branch via Magit."
  (interactive)
  (if (fboundp 'magit-branch-and-checkout)
      (call-interactively 'magit-branch-and-checkout)
    (message "Magit not available")))

(defun emacs-ide-git-diff-buffer ()
  "Show diff for the current file via Magit."
  (interactive)
  (when buffer-file-name
    (if (fboundp 'magit-diff-buffer-file)
        (magit-diff-buffer-file)
      (message "Magit not available"))))

(defun emacs-ide-git-log-buffer ()
  "Show git log for the current file via Magit."
  (interactive)
  (when buffer-file-name
    (if (fboundp 'magit-log-buffer-file)
        (magit-log-buffer-file)
      (message "Magit not available"))))

(defun emacs-ide-git-blame-toggle ()
  "Toggle git blame for the current file via Magit."
  (interactive)
  (if (and (fboundp 'magit-blame-addition) buffer-file-name)
      (call-interactively 'magit-blame-addition)
    (message "Magit not available or no file")))

(defun emacs-ide-git-copy-link ()
  "Copy a GitHub/GitLab permalink for the current line."
  (interactive)
  (if (fboundp 'git-link)
      (let ((git-link-open-in-browser nil))
        (call-interactively #'git-link)
        (message "Git link copied: %s" (car kill-ring)))
    (message "git-link not loaded")))

(defun emacs-ide-git-stash ()
  "Stash current changes via Magit."
  (interactive)
  (if (fboundp 'magit-stash-push)
      (magit-stash-push nil)
    (message "Magit not available")))

(defun emacs-ide-git-stash-pop ()
  "Pop the most recent stash via Magit."
  (interactive)
  (if (fboundp 'magit-stash-pop)
      (magit-stash-pop "stash@{0}")
    (message "Magit not available")))

;;; ─── Git-flow helpers ────────────────────────────────────────────────────────

(defun emacs-ide-git-flow-feature-start ()
  "Start a new feature branch from develop."
  (interactive)
  (if (fboundp 'magit-branch-and-checkout)
      (let ((name (read-string "Feature name: ")))
        (unless (string-empty-p name)
          (magit-branch-and-checkout (concat "feature/" name) "develop")))
    (message "Magit not available")))

(defun emacs-ide-git-flow-feature-finish ()
  "Merge current feature branch into develop and delete it."
  (interactive)
  (if (fboundp 'magit-get-current-branch)
      (let ((branch (magit-get-current-branch)))
        (when (string-prefix-p "feature/" branch)
          (when (fboundp 'magit-branch-checkout)  (magit-branch-checkout "develop"))
          (when (fboundp 'magit-merge-plain)       (magit-merge-plain branch))
          (when (fboundp 'magit-branch-delete)     (magit-branch-delete (list branch)))))
    (message "Magit not available")))

) ;; end (when emacs-ide-git-enable)

(provide 'tools-git)
;;; tools-git.el ends here
