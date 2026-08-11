;;; tools-notes.el --- Linked Developer Notes with org-roam -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(require 'cl-lib)

;;;; ── Notes directory management ─────────────────────────────────────────────

(defvar emacs-ide-notes-directory
  (expand-file-name "~/notes")
  "Root directory for org-roam notes.
Updated by `emacs-ide-notes--update-directory' from config.yml.")

(defun emacs-ide-notes--resolve-directory ()
  "Return the notes directory from config.yml, or ~/notes as default."
  (expand-file-name
   (or (and (boundp 'emacs-ide-config-data)
            (let ((gen (cdr (assoc 'general emacs-ide-config-data))))
              (cdr (assoc 'notes-directory gen))))
       "~/notes")))

(defun emacs-ide-notes--update-directory ()
  "Re-read notes directory from config.yml and propagate to org-roam."
  (setq emacs-ide-notes-directory (emacs-ide-notes--resolve-directory))
  ;; Create the directory if it does not exist
  (unless (file-directory-p emacs-ide-notes-directory)
    (make-directory emacs-ide-notes-directory t))
  ;; Propagate to org-roam if it is already loaded
  (when (boundp 'org-roam-directory)
    (setq org-roam-directory emacs-ide-notes-directory)))

(with-eval-after-load 'emacs-ide-config
  (emacs-ide-notes--update-directory))

(add-hook 'emacs-ide-config-reload-hook #'emacs-ide-notes--update-directory)

;;;; ── org-roam ────────────────────────────────────────────────────────────────

(use-package org-roam
  :after org
  :defer t
  :commands (org-roam-node-find
             org-roam-node-insert
             org-roam-buffer-toggle
             org-roam-dailies-goto-today
             org-roam-dailies-goto-date
             org-roam-capture)
  :init
  (setq org-roam-directory            emacs-ide-notes-directory
        org-roam-db-location
        (expand-file-name "var/org-roam.db" user-emacs-directory)
        org-roam-completion-everywhere t
        org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:20}" 'face 'org-tag)))
  ;; These bindings register under C-c n f / i / b / d / D / g / p / c.
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n d" . org-roam-dailies-goto-today)
         ("C-c n D" . org-roam-dailies-goto-date)
         ("C-c n g" . emacs-ide-notes-graph)
         ("C-c n p" . emacs-ide-notes-capture-project)
         ("C-c n c" . org-roam-capture)
         :map org-mode-map
         ("C-M-i"   . completion-at-point))
  :config
  (run-with-idle-timer
   3 nil
   (lambda ()
     (when (and (fboundp 'org-roam-db-autosync-mode)
                ;; FIX #62: only start if the notes directory actually exists
                (let ((dir (or (bound-and-true-p org-roam-directory)
                               emacs-ide-notes-directory)))
                  (and (stringp dir)
                       (file-directory-p dir))))
       (org-roam-db-autosync-mode))))

  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+title: ${title}\n#+date: %U\n#+filetags:\n\n")
           :unnarrowed t)
          ("p" "project note" plain
           "* Overview\n%?\n\n* Resources\n\n* Decisions\n\n* TODO\n- [ ] "
           :target (file+head "projects/%<%Y%m%d>-${slug}.org"
                               "#+title: ${title}\n#+date: %U\n#+filetags: :project:\n\n")
           :unnarrowed t)
          ("b" "bug / issue" plain
           "* Description\n%?\n\n* Reproduction Steps\n1. \n\n* Root Cause\n\n* Fix\n"
           :target (file+head "bugs/%<%Y%m%d>-${slug}.org"
                               "#+title: Bug: ${title}\n#+date: %U\n#+filetags: :bug:\n\n")
           :unnarrowed t)
          ("r" "reference" plain
           "* Summary\n%?\n\n* Key Points\n- \n\n* Links\n- "
           :target (file+head "references/${slug}.org"
                               "#+title: ${title}\n#+date: %U\n#+filetags: :reference:\n\n")
           :unnarrowed t)
          ("l" "literature note" plain
           "* Key Ideas\n%?\n\n* Quotes\n\n* My Thoughts\n"
           :target (file+head "literature/%<%Y%m%d>-${slug}.org"
                               "#+title: ${title}\n#+date: %U\n#+filetags: :lit:\n\n")
           :unnarrowed t)))

  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %<%H:%M> %?"
           :target (file+head "%<%Y-%m-%d>.org"
                               "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n")))))

;;;; ── org-roam-ui ─────────────────────────────────────────────────────────────

(use-package org-roam-ui
  :after org-roam
  :defer t
  :commands (org-roam-ui-open org-roam-ui-mode)
  :init
  (setq org-roam-ui-sync-theme     t
        org-roam-ui-follow         t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start  nil))

;;;; ── consult-org-roam ────────────────────────────────────────────────────────

(use-package consult-org-roam
  :after org-roam
  :defer t
  :commands (consult-org-roam-search
             consult-org-roam-backlinks
             consult-org-roam-forward-links)
  :init
  (setq consult-org-roam-grep-func #'consult-ripgrep)
  :config
  (consult-org-roam-mode 1)
  :bind (("C-c n s" . consult-org-roam-search)
         ("C-c n B" . consult-org-roam-backlinks)
         ("C-c n F" . consult-org-roam-forward-links)))

;;;; ── Interactive commands ────────────────────────────────────────────────────

(defun emacs-ide-notes-capture-project ()
  "Find or create an org-roam note for the current Projectile project."
  (interactive)
  (let* ((root (or (and (fboundp 'projectile-project-root)
                        (ignore-errors (projectile-project-root)))
                   default-directory))
         (name (file-name-nondirectory (directory-file-name root))))
    (if (fboundp 'org-roam-node-find)
        (org-roam-node-find nil name)
      (message "⚠️  org-roam not available — open a .org file first"))))

(defun emacs-ide-notes-graph ()
  "Open the org-roam graph in a browser via org-roam-ui."
  (interactive)
  (if (fboundp 'org-roam-ui-open)
      (org-roam-ui-open)
    (message "⚠️  org-roam-ui not loaded — open a roam note first (C-c n f)")))

(defun emacs-ide-notes-search ()
  "Full-text search across the notes directory using ripgrep or grep."
  (interactive)
  (if (fboundp 'consult-ripgrep)
      (consult-ripgrep emacs-ide-notes-directory)
    (let ((query (read-string "Search notes: ")))
      (grep-find (format "grep -r %s %s"
                         (shell-quote-argument query)
                         (shell-quote-argument emacs-ide-notes-directory))))))


(provide 'tools-notes)
;;; tools-notes.el ends here
