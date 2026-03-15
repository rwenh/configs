;;; tools-notes.el --- Linked Developer Notes with org-roam -*- lexical-binding: t -*-
;;; Commentary:
;;; Linked knowledge base using org-roam. This is where Emacs genuinely beats
;;; Eclipse — a Zettelkasten-style network of developer notes that links across
;;; projects, connects to your org-agenda, and is fully version-controllable.
;;;
;;; Quick start:
;;;   C-c n f   — find or create a note (fuzzy search by title)
;;;   C-c n i   — insert a link to another note at point
;;;   C-c n g   — show the knowledge graph (roam-ui browser)
;;;   C-c n d   — open daily note (today's dev log)
;;;   C-c n p   — capture a note about the current project
;;;   C-c n b   — show backlinks (what links to this note?)
;;;
;;; Notes live in ~/notes/ by default (override in config.yml:
;;;   general.notes-directory: ~/my/notes)
;;;
;;; Add "tools-notes" to emacs-ide-feature-modules in init.el (after tools-org).
;;; Version: 1.0.0
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; DIRECTORY SETUP
;; ============================================================================
(defvar emacs-ide-notes-directory
  (expand-file-name
   (or (and (boundp 'emacs-ide-config-data)
            (let ((gen (cdr (assoc 'general emacs-ide-config-data))))
              (cdr (assoc 'notes-directory gen))))
       "~/notes"))
  "Root directory for org-roam notes.")

(unless (file-directory-p emacs-ide-notes-directory)
  (make-directory emacs-ide-notes-directory t))

;; ============================================================================
;; ORG-ROAM — the linked notes backbone
;; ============================================================================
(use-package org-roam
  :after org
  :init
  (setq org-roam-directory         emacs-ide-notes-directory
        org-roam-db-location
        (expand-file-name "var/org-roam.db" user-emacs-directory)
        org-roam-completion-everywhere t
        org-roam-db-autosync-on-save  t
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
  (org-roam-db-autosync-mode)

  ;; Capture templates
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

  ;; Daily note template
  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %<%H:%M> %?"
           :target (file+head "%<%Y-%m-%d>.org"
                               "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n")))))

;; ============================================================================
;; ORG-ROAM-UI — visual knowledge graph in the browser
;; ============================================================================
(use-package org-roam-ui
  :after org-roam
  :commands (org-roam-ui-open org-roam-ui-mode)
  :init
  (setq org-roam-ui-sync-theme     t
        org-roam-ui-follow         t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start  nil))

;; ============================================================================
;; CONSULT-ORG-ROAM — better search via consult
;; ============================================================================
(use-package consult-org-roam
  :after (org-roam consult)
  :init
  (setq consult-org-roam-grep-func #'consult-ripgrep)
  :config
  (consult-org-roam-mode 1)
  :bind (("C-c n s" . consult-org-roam-search)
         ("C-c n B" . consult-org-roam-backlinks)
         ("C-c n F" . consult-org-roam-forward-links)))

;; ============================================================================
;; PROJECT NOTE CAPTURE
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

;; ============================================================================
;; KNOWLEDGE GRAPH
;; ============================================================================
(defun emacs-ide-notes-graph ()
  "Open the org-roam knowledge graph in the browser."
  (interactive)
  (if (fboundp 'org-roam-ui-open)
      (org-roam-ui-open)
    (message "⚠️  org-roam-ui not available — install it via M-x straight-use-package RET org-roam-ui")))

;; ============================================================================
;; QUICK SEARCH ACROSS ALL NOTES
;; ============================================================================
(defun emacs-ide-notes-search ()
  "Full-text search across all notes using ripgrep."
  (interactive)
  (if (fboundp 'consult-ripgrep)
      (consult-ripgrep emacs-ide-notes-directory)
    (grep-find (format "grep -r \"%s\" %s"
                       (read-string "Search notes: ")
                       (shell-quote-argument emacs-ide-notes-directory)))))

(global-set-key (kbd "C-c n /") 'emacs-ide-notes-search)

(provide 'tools-notes)
;;; tools-notes.el ends here
