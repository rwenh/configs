;;; tools-org.el --- Org-Mode Configuration -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(require 'cl-lib)

;;;; ── Config accessors ────────────────────────────────────────────────────────

(defun emacs-ide-org--config-get (key)
  "Return KEY's value from the current environment's config section."
  (when (boundp 'emacs-ide-config-data)
    (let* ((env     (or (bound-and-true-p emacs-ide-config-environment)
                        "default"))
           (env-sym (intern env))
           (envs    (cdr (assoc 'environments emacs-ide-config-data)))
           (env-cfg (cdr (assoc env-sym envs))))
      (cdr (assoc key env-cfg)))))

;;;; ── Path management ─────────────────────────────────────────────────────────

(defvar emacs-ide-org-directory
  (expand-file-name "~/org")
  "Org directory.  Updated by `emacs-ide-org--update-paths' from config.yml.")

(defvar emacs-ide-org-agenda-files
  (list (expand-file-name "~/org/todo.org"))
  "Org agenda file list.  Updated by `emacs-ide-org--update-paths' from config.yml.")

(defun emacs-ide-org--update-paths ()
  "Read org directory and agenda files from config.yml and apply them.
Called after config loads and on every explicit config reload.
Also updates live org variables when org is already loaded."
  (setq emacs-ide-org-directory
        (expand-file-name
         (or (emacs-ide-org--config-get 'org-directory) "~/org")))
  (unless (file-directory-p emacs-ide-org-directory)
    (make-directory emacs-ide-org-directory t))
  (let ((raw-files (emacs-ide-org--config-get 'org-agenda-files)))
    (setq emacs-ide-org-agenda-files
          (if (and raw-files (listp raw-files))
              (mapcar #'expand-file-name raw-files)
            (list (expand-file-name "~/org/todo.org")))))
  ;; Apply to live org session if org is already loaded
  (when (featurep 'org)
    (setq org-directory          emacs-ide-org-directory
          org-agenda-files       emacs-ide-org-agenda-files
          org-default-notes-file
          (expand-file-name "inbox.org" emacs-ide-org-directory))))

(with-eval-after-load 'emacs-ide-config
  (emacs-ide-org--update-paths))

(add-hook 'emacs-ide-config-reload-hook #'emacs-ide-org--update-paths)

;;;; ── babel safety predicate ─────────────────────────────────────────────────

(defun emacs-ide-org--babel-confirm-p (_lang _body)
  "Return nil (auto-approve) for files in the home directory.
Return t (prompt for confirmation) for files outside the home directory.
LANG and BODY are the code block's language and content (ignored)."
  (let ((file (or (buffer-file-name) "")))
    ;; nil = auto-approve; t = require confirmation
    (not (string-prefix-p (expand-file-name "~/") file))))

;;;; ── use-package org ─────────────────────────────────────────────────────────

(use-package org
  :straight nil
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-directory               emacs-ide-org-directory
        org-default-notes-file
        (expand-file-name "inbox.org" emacs-ide-org-directory)
        org-startup-indented         t
        org-hide-emphasis-markers    t
        org-hide-leading-stars       t
        org-src-fontify-natively     t
        org-src-tab-acts-natively    t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-startup-folded           'content
        org-ellipsis                 " ▾"
        org-return-follows-link      t
        org-log-done                 'time
        org-log-into-drawer          t
        org-enforce-todo-dependencies t
        org-track-ordered-property-with-tag t
        org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i!)" "WAITING(w@)" "|"
                    "DONE(d!)" "CANCELLED(c@)"))
        org-tag-alist
        '((:startgroup)
          ("@work"   . ?W)
          ("@home"   . ?H)
          (:endgroup)
          ("urgent"  . ?u)
          ("review"  . ?r)
          ("blocked" . ?b))
        org-agenda-span               'week
        org-agenda-start-on-weekday   1
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done  t
        org-agenda-include-deadlines      t
        org-agenda-block-separator        ?─
        org-agenda-compact-blocks         t
        org-export-with-toc           t
        org-export-with-author        t
        org-export-headline-levels    4

        org-confirm-babel-evaluate #'emacs-ide-org--babel-confirm-p)

  :config
  (emacs-ide-org--update-paths)

  (run-with-idle-timer
   5 nil
   (lambda ()
     (when (featurep 'org)
       (org-babel-do-load-languages
        'org-babel-load-languages
        '((emacs-lisp . t)
          (python     . t)
          (shell      . t)
          (js         . t)
          (C          . t))))))

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
           :empty-lines 1))))

(use-package org-bullets
  :after org
  :defer t
  :hook (org-mode . org-bullets-mode)
  :init
  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿")))

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

(with-eval-after-load 'ox
  (require 'ox-md   nil t)
  (require 'ox-html nil t))

;;;; ── Convenience commands ────────────────────────────────────────────────────

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
