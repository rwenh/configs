;;; tools-git.el --- Git Integration with Magit -*- lexical-binding: t -*-
;;; Commentary:
;;; Professional Git workflow with Magit and companions
;;; Code:

;; ============================================================================
;; MAGIT - GIT PORCELAIN
;; ============================================================================
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g g" . magit-status)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g f" . magit-log-head)
         ("C-c g c" . magit-clone)
         ("C-c g i" . magit-init)
         ("C-c g d" . magit-dispatch)
         ("C-c g F" . magit-file-dispatch))
  :init
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        magit-diff-refine-hunk 'all
        magit-save-repository-buffers 'dontask
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-auto-revert-mode t
        magit-refresh-status-buffer t
        magit-process-popup-time 10
        magit-no-confirm '(stage-all-changes unstage-all-changes)
        
        ;; Commit message settings
        git-commit-summary-max-length 50
        git-commit-fill-column 72
        git-commit-style-convention-checks '(non-empty-second-line))
  :config
  ;; Performance improvements
  (setq magit-refresh-status-buffer nil)
  
  ;; Show word-granularity differences
  (setq magit-diff-refine-ignore-whitespace t))

;; ============================================================================
;; GIT-GUTTER - VISUAL DIFF IN FRINGE
;; ============================================================================
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :init
  (setq git-gutter:update-interval 0.3
        git-gutter:modified-sign "│"
        git-gutter:added-sign "│"
        git-gutter:deleted-sign "│"
        git-gutter:hide-gutter t)
  :config
  ;; Update colors based on theme
  (set-face-foreground 'git-gutter:modified "#f9e2af")
  (set-face-foreground 'git-gutter:added "#a6e3a1")
  (set-face-foreground 'git-gutter:deleted "#f38ba8"))

;; ============================================================================
;; GIT-TIMEMACHINE - BROWSE HISTORY
;; ============================================================================
(use-package git-timemachine
  :bind (("C-x v t" . git-timemachine)
         ("C-c g t" . git-timemachine))
  :init
  (setq git-timemachine-show-minibuffer-details t))

;; ============================================================================
;; DIFF-HL - ALTERNATIVE TO GIT-GUTTER
;; ============================================================================
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-side 'left)
  :config
  (diff-hl-flydiff-mode)
  (when (display-graphic-p)
    (diff-hl-margin-mode)))

;; ============================================================================
;; MAGIT-TODOS - SHOW TODOS IN MAGIT
;; ============================================================================
(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1)
  (setq magit-todos-keywords '("TODO" "FIXME" "HACK" "NOTE" "XXX")))

;; ============================================================================
;; GIT-LINK - GENERATE GITHUB/GITLAB LINKS
;; ============================================================================
(use-package git-link
  :bind (("C-c g u" . git-link)
         ("C-c g U" . git-link-commit))
  :init
  (setq git-link-open-in-browser t
        git-link-use-commit t))

;; ============================================================================
;; FORGE - GITHUB/GITLAB INTEGRATION
;; ============================================================================
(use-package forge
  :after magit
  :config
  ;; Requires auth setup in ~/.authinfo.gpg
  (setq forge-topic-list-limit '(60 . 0)))

;; ============================================================================
;; GIT COMMIT MESSAGE HELPER
;; ============================================================================
(use-package git-commit
  :ensure nil
  :after magit
  :init
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 72
        git-commit-style-convention-checks
        '(non-empty-second-line
          overlong-summary-line))
  :config
  ;; Add spell checking to commit messages
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

;; ============================================================================
;; GIT UTILITY FUNCTIONS
;; ============================================================================
(defun emacs-ide-git-status ()
  "Show git status with enhanced info."
  (interactive)
  (if (magit-git-repo-p)
      (let ((branch (magit-get-current-branch))
            (remote (magit-get-remote))
            (ahead-behind (magit-rev-diff-count "HEAD" "@{u}")))
        (message "Branch: %s | Remote: %s | Ahead: %s Behind: %s"
                 (or branch "detached")
                 (or remote "none")
                 (car ahead-behind)
                 (cdr ahead-behind)))
    (message "Not in a git repository")))

(defun emacs-ide-git-commit-conventional ()
  "Insert conventional commit type."
  (interactive)
  (let ((type (completing-read "Commit type: "
                              '("feat" "fix" "docs" "style" "refactor"
                                "perf" "test" "build" "ci" "chore"))))
    (insert (format "%s: " type))))

(defun emacs-ide-git-stage-file ()
  "Stage current file."
  (interactive)
  (when buffer-file-name
    (magit-stage-file buffer-file-name)
    (message "Staged: %s" (file-name-nondirectory buffer-file-name))))

(defun emacs-ide-git-unstage-file ()
  "Unstage current file."
  (interactive)
  (when buffer-file-name
    (magit-unstage-file buffer-file-name)
    (message "Unstaged: %s" (file-name-nondirectory buffer-file-name))))

(defun emacs-ide-git-commit-amend ()
  "Amend last commit."
  (interactive)
  (magit-commit-amend))

(defun emacs-ide-git-push ()
  "Push current branch."
  (interactive)
  (magit-push-current-to-pushremote nil))

(defun emacs-ide-git-pull ()
  "Pull current branch."
  (interactive)
  (magit-pull-from-upstream nil))

(defun emacs-ide-git-create-branch ()
  "Create and checkout new branch."
  (interactive)
  (call-interactively 'magit-branch-and-checkout))

(defun emacs-ide-git-switch-branch ()
  "Switch to another branch."
  (interactive)
  (call-interactively 'magit-branch-checkout))

(defun emacs-ide-git-stash ()
  "Stash changes."
  (interactive)
  (magit-stash-both "WIP" nil))

(defun emacs-ide-git-stash-pop ()
  "Pop stash."
  (interactive)
  (magit-stash-pop "stash@{0}"))

(defun emacs-ide-git-diff-buffer ()
  "Show diff of current buffer."
  (interactive)
  (when buffer-file-name
    (magit-diff-buffer-file)))

(defun emacs-ide-git-log-buffer ()
  "Show log of current buffer."
  (interactive)
  (when buffer-file-name
    (magit-log-buffer-file)))

(defun emacs-ide-git-blame-toggle ()
  "Toggle git blame for current buffer."
  (interactive)
  (call-interactively 'magit-blame-addition))

(defun emacs-ide-git-show-commit-at-point ()
  "Show commit at point."
  (interactive)
  (when-let ((commit (magit-commit-at-point)))
    (magit-show-commit commit)))

;; ============================================================================
;; GIT WORKTREE HELPERS
;; ============================================================================
(defun emacs-ide-git-worktree-create ()
  "Create new worktree."
  (interactive)
  (call-interactively 'magit-worktree-checkout))

(defun emacs-ide-git-worktree-list ()
  "List worktrees."
  (interactive)
  (magit-list-worktrees))

;; ============================================================================
;; GIT FLOW INTEGRATION
;; ============================================================================
(defun emacs-ide-git-flow-feature-start ()
  "Start new feature branch."
  (interactive)
  (let ((name (read-string "Feature name: ")))
    (magit-branch-and-checkout (concat "feature/" name) "develop")))

(defun emacs-ide-git-flow-feature-finish ()
  "Finish feature branch."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (when (string-prefix-p "feature/" branch)
      (magit-branch-checkout "develop")
      (magit-merge-plain branch)
      (magit-branch-delete branch))))

;; ============================================================================
;; GIT PREFIX MAP
;; ============================================================================
(define-prefix-command 'emacs-ide-git-map)
(global-set-key (kbd "C-c g") 'emacs-ide-git-map)

(define-key emacs-ide-git-map (kbd "g") 'magit-status)
(define-key emacs-ide-git-map (kbd "s") 'emacs-ide-git-stage-file)
(define-key emacs-ide-git-map (kbd "u") 'emacs-ide-git-unstage-file)
(define-key emacs-ide-git-map (kbd "c") 'magit-commit)
(define-key emacs-ide-git-map (kbd "a") 'emacs-ide-git-commit-amend)
(define-key emacs-ide-git-map (kbd "p") 'emacs-ide-git-push)
(define-key emacs-ide-git-map (kbd "P") 'emacs-ide-git-pull)
(define-key emacs-ide-git-map (kbd "b") 'emacs-ide-git-switch-branch)
(define-key emacs-ide-git-map (kbd "B") 'emacs-ide-git-create-branch)
(define-key emacs-ide-git-map (kbd "d") 'emacs-ide-git-diff-buffer)
(define-key emacs-ide-git-map (kbd "l") 'emacs-ide-git-log-buffer)
(define-key emacs-ide-git-map (kbd "t") 'git-timemachine)
(define-key emacs-ide-git-map (kbd "z") 'emacs-ide-git-stash)
(define-key emacs-ide-git-map (kbd "Z") 'emacs-ide-git-stash-pop)
(define-key emacs-ide-git-map (kbd "?") 'emacs-ide-git-status)

;; ============================================================================
;; ENHANCED GIT STATUS IN MODELINE
;; ============================================================================
(defun emacs-ide-git-modeline-status ()
  "Get git status for modeline."
  (when (and buffer-file-name (magit-git-repo-p))
    (let* ((branch (magit-get-current-branch))
           (status (magit-file-status buffer-file-name)))
      (format " [%s%s]"
              (or branch "detached")
              (if status (format " %s" status) "")))))

(provide 'tools-git)
;;; tools-git.el ends here
