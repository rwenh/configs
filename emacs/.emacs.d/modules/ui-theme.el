;;; ui-theme.el --- Premium Theme System -*- lexical-binding: t -*-
;;; Commentary:
;;; Smooth theme toggling, ambient palette hooks, and per-environment overrides.
;;; Version: 3.1.0
;;; Code:

(require 'cl-lib)

;;; ─── Theme registry ─────────────────────────────────────────────────────────

(defvar emacs-ide-theme--dark-themes
  '(ef-dark ef-night ef-duo-dark ef-trio-dark ef-cherie ef-winter
    ef-autumn ef-bio ef-elea-dark ef-cyprus ef-kassio ef-melissa-dark
    ef-maris-dark ef-rosa)
  "Known dark ef-themes variants.")

(defvar emacs-ide-theme--light-themes
  '(ef-light ef-day ef-duo-light ef-trio-light ef-spring ef-summer
    ef-frost ef-elea-light ef-melissa-light ef-maris-light ef-arbutus)
  "Known light ef-themes variants.")

;;; ─── After-load hooks ────────────────────────────────────────────────────────

(defvar emacs-ide-after-theme-hook nil
  "Hook run after any theme is loaded.  Useful for per-theme tweaks.")

(advice-add 'load-theme :after
            (lambda (&rest _) (run-hooks 'emacs-ide-after-theme-hook)))

;;; ─── Apply environment theme override ───────────────────────────────────────

(defun emacs-ide-theme--apply-env-override ()
  "Load the theme specified in config.yml for the current environment."
  (when (and (boundp 'emacs-ide-config-data) emacs-ide-config-data)
    (let* ((env  (or (bound-and-true-p emacs-ide-config-environment) "default"))
           (envs (cdr (assoc 'environments emacs-ide-config-data)))
           (ecfg (cdr (assoc (intern env) envs)))
           (raw  (and ecfg (cdr (assoc 'theme ecfg)))))
      (when raw
        (let ((theme (if (stringp raw) (intern raw) raw)))
          (unless (eq theme (car (custom-enabled-themes)))
            (condition-case nil (load-theme theme t)
              (error nil))))))))

(with-eval-after-load 'emacs-ide-config
  (emacs-ide-theme--apply-env-override))

(add-hook 'emacs-ide-config-reload-hook #'emacs-ide-theme--apply-env-override)

;;; ─── Toggle ──────────────────────────────────────────────────────────────────

(defun emacs-ide-toggle-theme ()
  "Toggle between the dark and light ef-theme configured in config.yml."
  (interactive)
  (if (fboundp 'ef-themes-toggle)
      (progn
        (ef-themes-toggle)
        (message "Theme: %s" (car (custom-enabled-themes))))
    (let* ((current      (car (custom-enabled-themes)))
           (load-fn      (if (fboundp 'ef-themes-load-theme)
                             #'ef-themes-load-theme
                           (lambda (theme) (load-theme theme t)))))
      (cond
       ((memq current emacs-ide-theme--dark-themes)
        (funcall load-fn 'ef-light)
        (message "Theme → ef-light"))
       ((memq current emacs-ide-theme--light-themes)
        (funcall load-fn 'ef-dark)
        (message "Theme → ef-dark"))
       (t
        (funcall load-fn 'ef-dark)
        (message "Theme → ef-dark"))))))

;;; ─── Select ──────────────────────────────────────────────────────────────────

(defun emacs-ide-select-theme ()
  "Interactively select any ef-theme."
  (interactive)
  (if (fboundp 'ef-themes-select)
      (call-interactively #'ef-themes-select)
    (let* ((all   (append emacs-ide-theme--dark-themes
                          emacs-ide-theme--light-themes))
           (names (mapcar #'symbol-name all))
           (choice (completing-read "Theme: " names nil t)))
      (when choice
        (load-theme (intern choice) t)
        (message "Theme → %s" choice)))))

;;; ─── Modus migration ─────────────────────────────────────────────────────────

(defun emacs-ide-theme--migrate-modus (theme)
  "Return ef equivalent for modus THEME, or THEME unchanged."
  (pcase theme
    ((pred (lambda (t) (string-prefix-p "modus-operandi" (symbol-name t)))) 'ef-light)
    ((pred (lambda (t) (string-prefix-p "modus-vivendi"  (symbol-name t)))) 'ef-dark)
    (_ theme)))

;;; ─── Convenience predicates ─────────────────────────────────────────────────

(defun emacs-ide-theme-dark-p ()
  "Return non-nil if the current theme is dark."
  (memq (car (custom-enabled-themes)) emacs-ide-theme--dark-themes))

(defun emacs-ide-theme-light-p ()
  "Return non-nil if the current theme is light."
  (memq (car (custom-enabled-themes)) emacs-ide-theme--light-themes))

;;; ─── Auto dark/light by time ─────────────────────────────────────────────────

(defvar emacs-ide-theme-auto-dark-hour  19
  "Hour (0–23) at which to switch to the dark theme automatically.")
(defvar emacs-ide-theme-auto-light-hour  7
  "Hour (0–23) at which to switch to the light theme automatically.")
(defvar emacs-ide-theme--auto-timer nil)

(defun emacs-ide-theme--auto-switch ()
  "Switch between dark/light based on time of day."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (cond
     ((and (>= hour emacs-ide-theme-auto-dark-hour)
           (emacs-ide-theme-light-p))
      (load-theme 'ef-dark t))
     ((and (>= hour emacs-ide-theme-auto-light-hour)
           (< hour emacs-ide-theme-auto-dark-hour)
           (emacs-ide-theme-dark-p))
      (load-theme 'ef-light t)))))

(defun emacs-ide-theme-enable-auto ()
  "Enable automatic day/night theme switching."
  (interactive)
  (when emacs-ide-theme--auto-timer
    (cancel-timer emacs-ide-theme--auto-timer))
  (setq emacs-ide-theme--auto-timer
        (run-with-timer 0 (* 30 60) #'emacs-ide-theme--auto-switch))
  (message "Auto theme switching enabled (checks every 30 min)"))

(defun emacs-ide-theme-disable-auto ()
  "Disable automatic day/night theme switching."
  (interactive)
  (when emacs-ide-theme--auto-timer
    (cancel-timer emacs-ide-theme--auto-timer)
    (setq emacs-ide-theme--auto-timer nil))
  (message "Auto theme switching disabled"))

(provide 'ui-theme)
;;; ui-theme.el ends here
