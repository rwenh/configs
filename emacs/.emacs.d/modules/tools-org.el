;;; tools-org.el --- Org-Mode Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Proper org-mode setup wired to config.yml environment settings
;;; (org-directory, org-agenda-files from work/home environments).
;;; Provides the full office-grade experience: agenda, capture templates,
;;; babel, export, and appearance.
;;; Add "tools-org" to emacs-ide-feature-modules in init.el (after core-dev).
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.0 (audit):
;;;   - FIX-VERSION: Version header added (file had none).
;;;   - FIX-DEFVAR-RELOAD: emacs-ide-org-directory and emacs-ide-org-agenda-files
;;;     were defvars evaluated once at top level. On M-x emacs-ide-config-reload
;;;     or environment switch (work↔home) they were never updated — defvar is
;;;     a no-op when the variable is already bound. Replaced with a helper
;;;     emacs-ide-org--update-paths that re-reads config and is called via
;;;     with-eval-after-load 'emacs-ide-config and emacs-ide-config-reload-hook.
;;;   - FIX-MKDIR-TOPLEVEL: (make-directory emacs-ide-org-directory) ran at
;;;     load time before config was confirmed loaded. Deferred into
;;;     emacs-ide-org--update-paths so the correct configured path is always
;;;     used when the directory is created.
;;;   - FIX-NESTED-USE-PACKAGE: (use-package org-bullets) was nested inside
;;;     the org :config block. Nested use-package calls are unsupported —
;;;     the inner call may run before straight has registered the package.
;;;     Moved to a top-level (use-package org-bullets :after org ...) block.
;;;   - FIX-COMMENT-LANGCORE: Header comment referenced lang-core.el which
;;;     was removed in v3.0.0 and replaced by modules/langs/. Updated.
;;;   - FIX-ORG-DIR-SYNC: org-directory, org-agenda-files, and
;;;     org-default-notes-file were set in :init from a defvar snapshot and
;;;     never updated on reload. emacs-ide-org--update-paths now also updates
;;;     these live org variables when org is already loaded.
;;;   - FIX-AGENDA-FILES-NIL: If emacs-ide-org--config-get returned nil
;;;     (e.g. "default" environment with no agenda files configured),
;;;     (listp nil) is t so mapcar ran over nil producing nil — the fallback
;;;     todo.org was silently dropped. Added explicit nil check before mapcar.
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

;; FIX-DEFVAR-RELOAD: defvars set initial defaults only.
;; emacs-ide-org--update-paths populates them correctly after config loads.
(defvar emacs-ide-org-directory (expand-file-name "~/org")
  "Org directory derived from config.yml environment settings.")

(defvar emacs-ide-org-agenda-files (list (expand-file-name "~/org/todo.org"))
  "Org agenda files derived from config.yml environment settings.")

(defun emacs-ide-org--update-paths ()
  "Re-read org paths from config and sync live org variables.
FIX-DEFVAR-RELOAD: called after config loads and on every reload so that
environment switches (work↔home) take effect without restarting Emacs.
FIX-MKDIR-TOPLEVEL: directory creation happens here, not at load time.
FIX-ORG-DIR-SYNC: updates live org-* variables when org is already loaded.
FIX-AGENDA-FILES-NIL: explicit nil check before mapcar."
  ;; Resolve directory
  (setq emacs-ide-org-directory
        (expand-file-name
         (or (emacs-ide-org--config-get 'org-directory) "~/org")))
  ;; FIX-MKDIR-TOPLEVEL: defer directory creation to here
  (unless (file-directory-p emacs-ide-org-directory)
    (make-directory emacs-ide-org-directory t))
  ;; FIX-AGENDA-FILES-NIL: (listp nil) is t — check for non-nil before mapcar
  (let ((raw-files (emacs-ide-org--config-get 'org-agenda-files)))
    (setq emacs-ide-org-agenda-files
          (if (and raw-files (listp raw-files))
              (mapcar #'expand-file-name raw-files)
            (list (expand-file-name "~/org/todo.org")))))
  ;; FIX-ORG-DIR-SYNC: update live org variables if org is already loaded
  (when (featurep 'org)
    (setq org-directory          emacs-ide-org-directory
          org-agenda-files       emacs-ide-org-agenda-files
          org-default-notes-file (expand-file-name "inbox.org" emacs-ide-org-directory))))

;; Update after config loads and on every subsequent reload
(with-eval-after-load 'emacs-ide-config
  (emacs-ide-org--update-paths))

(add-hook 'emacs-ide-config-reload-hook #'emacs-ide-org--update-paths)

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

        ;; Babel — disable eval confirmation (sources are local/trusted)
        org-confirm-babel-evaluate  nil)

  :config
  ;; Defer babel language loading — ob-python/shell/js/C add ~1.7s if eager
  (run-with-idle-timer
   3 nil
   (lambda ()
     (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (python     . t)
        (shell      . t)
        (js         . t)
        (C          . t)))))

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
           :empty-lines 1))))

;; ============================================================================
;; ORG-BULLETS — pretty heading bullets
;; FIX-NESTED-USE-PACKAGE: moved from inside org :config to a top-level block.
;; Nested use-package is unsupported — straight may not have registered the
;; package when the inner call runs inside :config.
;; ============================================================================
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :init
  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿")))

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
