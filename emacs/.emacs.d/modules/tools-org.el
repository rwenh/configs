;;; tools-org.el --- Org-Mode Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; NEW MODULE — proper org-mode setup wired to config.yml environment
;;; settings (org_directory, org_agenda_files from work/home environments).
;;; lang-core.el has minimal org setup; this provides the full office-grade
;;; experience: agenda, capture templates, babel, export, and appearance.
;;; Add "tools-org" to emacs-ide-feature-modules in init.el (after lang-core).
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; DERIVE ORG PATHS FROM CONFIG
;; ============================================================================
(defun emacs-ide-org--config-get (key)
  "Get KEY from current environment section of emacs-ide-config-data."
  (when (boundp 'emacs-ide-config-data)
    (let* ((env     (or (bound-and-true-p emacs-ide-config-environment) "default"))
           (env-sym (intern env))
           (envs    (cdr (assoc 'environments emacs-ide-config-data)))
           (env-cfg (cdr (assoc env-sym envs))))
      (cdr (assoc key env-cfg)))))

(defvar emacs-ide-org-directory
  (expand-file-name
   (or (emacs-ide-org--config-get 'org-directory)
       "~/org"))
  "Org directory from config.")

(defvar emacs-ide-org-agenda-files
  (let ((files (emacs-ide-org--config-get 'org-agenda-files)))
    (if (listp files)
        (mapcar #'expand-file-name files)
      (list (expand-file-name "~/org/todo.org"))))
  "Org agenda files from config.")

;; Ensure directory exists
(unless (file-directory-p emacs-ide-org-directory)
  (make-directory emacs-ide-org-directory t))

;; ============================================================================
;; ORG-MODE CORE
;; ============================================================================
(use-package org
  :straight nil
  :defer t
  :init
  (setq org-directory               emacs-ide-org-directory
        org-agenda-files            emacs-ide-org-agenda-files
        org-default-notes-file
        (expand-file-name "inbox.org" emacs-ide-org-directory)

        ;; Appearance
        org-startup-indented        t
        org-hide-emphasis-markers   t
        org-hide-leading-stars      t
        org-src-fontify-natively    t
        org-src-tab-acts-natively   t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-startup-folded          'content
        org-ellipsis                " ▾"

        ;; Behaviour
        org-return-follows-link     t
        org-log-done                'time
        org-log-into-drawer         t
        org-enforce-todo-dependencies t
        org-track-ordered-property-with-tag t

        ;; Todo keywords
        org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i!)" "WAITING(w@)" "|"
                    "DONE(d!)" "CANCELLED(c@)"))

        ;; Tags
        org-tag-alist
        '((:startgroup)
          ("@work"   . ?W)
          ("@home"   . ?H)
          (:endgroup)
          ("urgent"  . ?u)
          ("review"  . ?r)
          ("blocked" . ?b))

        ;; Agenda
        org-agenda-span              'week
        org-agenda-start-on-weekday  1
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done  t
        org-agenda-include-deadlines      t
        org-agenda-block-separator        ?─
        org-agenda-compact-blocks         t

        ;; Export
        org-export-with-toc         t
        org-export-with-author      t
        org-export-headline-levels  4

        ;; Babel languages — offline only, no network
        org-confirm-babel-evaluate  nil)

  :config
  ;; Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python     . t)
     (shell      . t)
     (js         . t)
     (C          . t)))

  ;; Capture templates
  (setq org-capture-templates
        `(("t" "Task" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  SCHEDULED: %T\n  %i\n  %a\n"
           :empty-lines 1)

          ("n" "Note" entry
           (file+headline org-default-notes-file "Notes")
           "* %? :note:\n  %T\n  %i\n"
           :empty-lines 1)

          ("m" "Meeting" entry
           (file+headline org-default-notes-file "Meetings")
           "* MEETING %? :meeting:\n  %T\n** Attendees\n  - \n** Notes\n  \n** Action Items\n  - [ ] \n"
           :empty-lines 1)

          ("b" "Bug / Issue" entry
           (file+headline org-default-notes-file "Bugs")
           "* TODO [#A] Bug: %?\n  DEADLINE: %T\n  %i\n  %a\n"
           :empty-lines 1)))

  ;; Pretty bullets
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :init
    (setq org-bullets-bullet-list '("◉" "○" "✸" "✿"))))

;; ============================================================================
;; ORG AGENDA CUSTOM VIEWS
;; ============================================================================
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("d" "Today's agenda"
           ((agenda "" ((org-agenda-span 1)
                        (org-agenda-day-face-function
                         (lambda (date) 'org-agenda-date-today))))
            (todo "IN-PROGRESS"
                  ((org-agenda-overriding-header "In Progress:")))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting:")))))

          ("w" "Work view"
           ((tags-todo "@work"
                        ((org-agenda-overriding-header "Work Tasks:")))
            (agenda "" ((org-agenda-span 3))))
           nil nil)

          ("A" "All unscheduled"
           ((todo "TODO"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'scheduled 'deadline))
                   (org-agenda-overriding-header "Unscheduled:"))))))))

;; ============================================================================
;; ORG EXPORT BACKENDS
;; ============================================================================
(with-eval-after-load 'ox
  (require 'ox-md   nil t)
  (require 'ox-html nil t))

;; ============================================================================
;; UTILITY COMMANDS
;; ============================================================================
(defun emacs-ide-org-open-inbox ()
  "Open the org inbox file."
  (interactive)
  (find-file (expand-file-name "inbox.org" emacs-ide-org-directory)))

(defun emacs-ide-org-open-directory ()
  "Open the org directory in dired."
  (interactive)
  (dired emacs-ide-org-directory))

(defun emacs-ide-org-agenda-today ()
  "Show today's agenda."
  (interactive)
  (org-agenda nil "d"))

(provide 'tools-org)
;;; tools-org.el ends here
