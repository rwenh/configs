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
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.0 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.0 to 3.0.4.
;;;   - FIX-DEFVAR-RELOAD: emacs-ide-notes-directory was a top-level defvar
;;;     evaluated once at load time. On M-x emacs-ide-config-reload the defvar
;;;     is a no-op (already bound), so a changed general.notes-directory in
;;;     config.yml was never picked up. Replaced with a defvar default + a
;;;     with-eval-after-load update that re-reads config after it loads, and
;;;     a config-reload-hook that keeps the var in sync.
;;;   - FIX-MKDIR-TOPLEVEL: (make-directory) ran at load time before config
;;;     was fully applied — if emacs-ide-notes-directory still held the defvar
;;;     default the wrong directory could be created. Deferred to a helper
;;;     function called after config is confirmed loaded.
;;;   - FIX-AUTOSYNC-VAR: org-roam-db-autosync-on-save is not a real org-roam
;;;     variable — it was silently setting a non-existent var. The correct
;;;     mechanism is org-roam-db-autosync-mode (already called in :config).
;;;     Spurious setq removed.
;;;   - FIX-NOTES-DIR-SYNC: org-roam-directory was set in :init from the
;;;     defvar but never updated on config reload. A config-reload-hook now
;;;     keeps org-roam-directory in sync with emacs-ide-notes-directory.
;;;   - FIX-GREP-SHELL-INJECTION: emacs-ide-notes-search fallback interpolated
;;;     (read-string) directly into a shell format string without quoting —
;;;     shell injection risk. Wrapped with shell-quote-argument.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; DIRECTORY SETUP
;; FIX-DEFVAR-RELOAD: defvar sets the initial default only. The real value
;; is populated/updated by emacs-ide-notes--update-directory, called both
;; after emacs-ide-config loads and on every config reload.
;; ============================================================================
(defvar emacs-ide-notes-directory (expand-file-name "~/notes")
  "Root directory for org-roam notes.
Set from config.yml general.notes-directory after config loads.")

(defun emacs-ide-notes--resolve-directory ()
  "Return the notes directory from config.yml or the default ~/notes."
  (expand-file-name
   (or (and (boundp 'emacs-ide-config-data)
            (let ((gen (cdr (assoc 'general emacs-ide-config-data))))
              (cdr (assoc 'notes-directory gen))))
       "~/notes")))

(defun emacs-ide-notes--update-directory ()
  "Sync emacs-ide-notes-directory from config and ensure it exists.
FIX-MKDIR-TOPLEVEL: directory creation deferred here, after config is loaded,
so we always create the correct configured path and not the defvar default.
FIX-NOTES-DIR-SYNC: also updates org-roam-directory if org-roam is loaded."
  (setq emacs-ide-notes-directory (emacs-ide-notes--resolve-directory))
  (unless (file-directory-p emacs-ide-notes-directory)
    (make-directory emacs-ide-notes-directory t))
  ;; Keep org-roam-directory in sync if org-roam is already loaded
  (when (boundp 'org-roam-directory)
    (setq org-roam-directory emacs-ide-notes-directory)))

;; Update after config loads and on every subsequent reload
(with-eval-after-load 'emacs-ide-config
  (emacs-ide-notes--update-directory))

(add-hook 'emacs-ide-config-reload-hook #'emacs-ide-notes--update-directory)

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
        ;; FIX-AUTOSYNC-VAR: org-roam-db-autosync-on-save does not exist.
        ;; org-roam-db-autosync-mode (called in :config) is the correct mechanism.
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
    (message "⚠️  org-roam-ui not available — install via M-x straight-use-package RET org-roam-ui")))

;; ============================================================================
;; QUICK SEARCH ACROSS ALL NOTES
;; FIX-GREP-SHELL-INJECTION: read-string result now wrapped in
;; shell-quote-argument before interpolation into the shell command string.
;; ============================================================================
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
