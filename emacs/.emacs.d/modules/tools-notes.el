;;; tools-notes.el --- Linked Developer Notes with org-roam -*- lexical-binding: t -*-
;;; Commentary:
;;; org-roam fully deferred — loads only when C-c n f or a roam file is opened.
;;; Version: 3.0.4-patched
;;; Startup fix: org-roam-db-autosync-mode deferred; org-roam-ui never eager.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; DIRECTORY SETUP
;; ============================================================================
(defvar emacs-ide-notes-directory (expand-file-name "~/notes")
  "Root directory for org-roam notes.")

(defun emacs-ide-notes--resolve-directory ()
  "Return the notes directory from config.yml or the default ~/notes."
  (expand-file-name
   (or (and (boundp 'emacs-ide-config-data)
            (let ((gen (cdr (assoc 'general emacs-ide-config-data))))
              (cdr (assoc 'notes-directory gen))))
       "~/notes")))

(defun emacs-ide-notes--update-directory ()
  "Sync emacs-ide-notes-directory from config and ensure it exists."
  (setq emacs-ide-notes-directory (emacs-ide-notes--resolve-directory))
  (unless (file-directory-p emacs-ide-notes-directory)
    (make-directory emacs-ide-notes-directory t))
  (when (boundp 'org-roam-directory)
    (setq org-roam-directory emacs-ide-notes-directory)))

(with-eval-after-load 'emacs-ide-config
  (emacs-ide-notes--update-directory))

(add-hook 'emacs-ide-config-reload-hook #'emacs-ide-notes--update-directory)

;; ============================================================================
;; ORG-ROAM — fully deferred, no autosync at startup
;; Loads when any C-c n command is called or a .org file in the roam dir opens.
;; org-roam-db-autosync-mode is started lazily after org-roam loads.
;; ============================================================================
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
  ;; Set variables before org-roam loads so they are correct on first use
  (setq org-roam-directory         emacs-ide-notes-directory
        org-roam-db-location
        (expand-file-name "var/org-roam.db" user-emacs-directory)
        org-roam-completion-everywhere t
        org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:20}" 'face 'org-tag)))
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
  ;; Autosync started at idle to avoid blocking the first org file open
  (run-with-idle-timer 3 nil
                       (lambda ()
                         (when (fboundp 'org-roam-db-autosync-mode)
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

;; ============================================================================
;; ORG-ROAM-UI — deferred, loaded only on explicit command
;; ============================================================================
(use-package org-roam-ui
  :after org-roam          ; will not load until org-roam itself loads
  :defer t
  :commands (org-roam-ui-open org-roam-ui-mode)
  :init
  (setq org-roam-ui-sync-theme     t
        org-roam-ui-follow         t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start  nil))   ; never auto-open on startup

;; ============================================================================
;; CONSULT-ORG-ROAM — deferred
;; ============================================================================
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

;; ============================================================================
;; UTILITY COMMANDS
;; ============================================================================
(defun emacs-ide-notes-capture-project ()
  "Create or find a note for the current project."
  (interactive)
  (let* ((root (or (and (fboundp 'projectile-project-root)
                        (ignore-errors (projectile-project-root)))
                   default-directory))
         (name (file-name-nondirectory (directory-file-name root))))
    (if (fboundp 'org-roam-node-find)
        (org-roam-node-find nil name)
      (message "⚠️  org-roam not available"))))

(defun emacs-ide-notes-graph ()
  "Open the org-roam knowledge graph in the browser."
  (interactive)
  (if (fboundp 'org-roam-ui-open)
      (org-roam-ui-open)
    (message "⚠️  org-roam-ui not loaded — use C-c n g after opening a roam note")))

(defun emacs-ide-notes-search ()
  "Full-text search across all notes using ripgrep."
  (interactive)
  (if (fboundp 'consult-ripgrep)
      (consult-ripgrep emacs-ide-notes-directory)
    (let ((query (read-string "Search notes: ")))
      (grep-find (format "grep -r %s %s"
                         (shell-quote-argument query)
                         (shell-quote-argument emacs-ide-notes-directory))))))

(global-set-key (kbd "C-c n /") 'emacs-ide-notes-search)

(provide 'tools-notes)
;;; tools-notes.el ends here
