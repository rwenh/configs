;;; tools-git.el --- Git Integration with Magit -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Code:

(defun emacs-ide-git--cfg (key default)
  (if (fboundp 'emacs-ide-config-get)
      (emacs-ide-config-get 'git key default)
    default))

(use-package magit
  :init
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        magit-diff-refine-hunk              'all
        magit-diff-refine-ignore-whitespace t
        magit-save-repository-buffers       'dontask
        magit-revision-show-gravatars       '("^Author:     " . "^Commit:     ")
        magit-refresh-status-buffer         nil
        magit-process-popup-time            10
        magit-no-confirm                    '(stage-all-changes unstage-all-changes))
  :config
  (when (and (fboundp 'magit-auto-revert-mode)
             (emacs-ide-git--cfg 'auto-revert t))
    (magit-auto-revert-mode 1)))

(use-package diff-hl
  :if (emacs-ide-git--cfg 'gutter t)
  :hook ((prog-mode  . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-side         'left)
  :config
  (when (fboundp 'diff-hl-flydiff-mode)
    (setq diff-hl-flydiff-delay
          (emacs-ide-git--cfg 'gutter-update-interval 0.3))
    (diff-hl-flydiff-mode))
  (when (boundp 'magit-post-refresh-hook)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  (when (and (display-graphic-p) (fboundp 'diff-hl-margin-mode))
    (diff-hl-margin-mode)))

(use-package git-timemachine
  :init
  (setq git-timemachine-show-minibuffer-details t))

(use-package magit-todos
  :after magit
  :config
  (when (fboundp 'magit-todos-mode)
    (magit-todos-mode 1))
  (setq magit-todos-keywords '("TODO" "FIXME" "HACK" "NOTE" "XXX")))

(use-package git-link
  :init
  (setq git-link-open-in-browser t
        git-link-use-commit       t))

(use-package forge
  :after magit
  :config
  (setq forge-topic-list-limit '(60 . 0)))

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

(defun emacs-ide-git-status ()
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
  (interactive)
  (when buffer-file-name
    (if (and (fboundp 'magit-stage-file)
             (fboundp 'magit-git-repo-p)
             (magit-git-repo-p default-directory))
        (magit-stage-file buffer-file-name)
      (message "⚠️  Not in a git repository or magit unavailable"))))

(defun emacs-ide-git-unstage-file ()
  (interactive)
  (when buffer-file-name
    (if (and (fboundp 'magit-unstage-file)
             (fboundp 'magit-git-repo-p)
             (magit-git-repo-p default-directory))
        (magit-unstage-file buffer-file-name)
      (message "⚠️  Not in a git repository or magit unavailable"))))

(defun emacs-ide-git-commit-amend ()
  (interactive)
  (if (fboundp 'magit-commit-amend)
      (magit-commit-amend)
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-push ()
  (interactive)
  (if (fboundp 'magit-push-current-to-pushremote)
      (magit-push-current-to-pushremote nil)
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-pull ()
  (interactive)
  (if (fboundp 'magit-pull-from-upstream)
      (magit-pull-from-upstream nil)
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-create-branch ()
  (interactive)
  (if (fboundp 'magit-branch-and-checkout)
      (call-interactively 'magit-branch-and-checkout)
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-switch-branch ()
  (interactive)
  (if (fboundp 'magit-branch-checkout)
      (call-interactively 'magit-branch-checkout)
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-stash ()
  (interactive)
  (if (fboundp 'magit-stash-push)
      (magit-stash-push nil)
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-stash-pop ()
  (interactive)
  (if (fboundp 'magit-stash-pop)
      (magit-stash-pop "stash@{0}")
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-diff-buffer ()
  (interactive)
  (when buffer-file-name
    (if (fboundp 'magit-diff-buffer-file)
        (magit-diff-buffer-file)
      (message "⚠️  Magit not available"))))

(defun emacs-ide-git-log-buffer ()
  (interactive)
  (when buffer-file-name
    (if (fboundp 'magit-log-buffer-file)
        (magit-log-buffer-file)
      (message "⚠️  Magit not available"))))

(defun emacs-ide-git-blame-toggle ()
  (interactive)
  (if (and (fboundp 'magit-blame-addition)
           buffer-file-name)
      (call-interactively 'magit-blame-addition)
    (message "⚠️  Magit not available or no file open")))

(defun emacs-ide-git-flow-feature-start ()
  (interactive)
  (if (fboundp 'magit-branch-and-checkout)
      (let ((name (read-string "Feature name: ")))
        (unless (string-empty-p name)
          (magit-branch-and-checkout (concat "feature/" name) "develop")))
    (message "⚠️  Magit not available")))

(defun emacs-ide-git-flow-feature-finish ()
  (interactive)
  (if (fboundp 'magit-get-current-branch)
      (let ((branch (magit-get-current-branch)))
        (when (string-prefix-p "feature/" branch)
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
