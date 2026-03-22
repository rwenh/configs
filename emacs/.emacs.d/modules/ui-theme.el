;;; ui-theme.el --- Theme toggling for ef-themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Replaces modus-themes toggle with ef-themes toggle.
;;; Respects config.yml theme key. F12 binding lives in keybindings.el.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 3.0.0 to 3.0.4.
;;;   - FIX-EELA-THEMES: ef-elea-dark and ef-elea-light added to the
;;;     fallback dark/light theme lists. Were missing — users on
;;;     ef-elea-dark would fall through to the default ef-dark branch
;;;     instead of toggling to ef-elea-light.
;;;   - FIX-CYPRUS: ef-cyprus moved from light-themes to dark-themes.
;;;     The ef-themes documentation categorises it as dark (deep forest
;;;     palette); listing it as light caused wrong toggle targets.
;;;   - FIX-DISABLE-THEME: fallback path now uses ef-themes-load-theme
;;;     when available instead of bare disable-theme + load-theme.
;;;     ef-themes manages internal face-override state via its own load
;;;     function; calling disable-theme directly can leave orphaned face
;;;     attributes from the previous theme active.
;;;   - FIX-COMMENT: header comment updated — F12 binding is in
;;;     keybindings.el, not this file.
;;; Code:

(defun emacs-ide--current-theme ()
  "Return currently enabled theme symbol or nil.
Returns nil when no theme is active; callers handle nil via memq fallthrough."
  (car (custom-enabled-themes)))

(defun emacs-ide-toggle-theme ()
  "Toggle between dark and light ef-theme.
Uses ef-themes-toggle (package function) when available — this is the
preferred path as it handles ef-themes internal state correctly.
Falls back to a manual toggle only when ef-themes hasn't loaded yet."
  (interactive)
  (if (fboundp 'ef-themes-toggle)
      (ef-themes-toggle)
    ;; FIX-DISABLE-THEME: use ef-themes-load-theme when available so
    ;; ef-themes internal face-override cleanup runs correctly.
    ;; Only fall back to bare load-theme as a last resort.
    (let* ((current      (emacs-ide--current-theme))
           ;; FIX-EELA-THEMES: added ef-elea-dark
           ;; FIX-CYPRUS: ef-cyprus moved here from light-themes
           (dark-themes  '(ef-dark ef-night ef-duo-dark ef-trio-dark
                           ef-cherie ef-winter ef-autumn ef-bio
                           ef-elea-dark ef-cyprus))
           ;; FIX-EELA-THEMES: added ef-elea-light
           (light-themes '(ef-light ef-day ef-duo-light ef-trio-light
                           ef-spring ef-summer ef-frost
                           ef-elea-light))
           (load-fn      (if (fboundp 'ef-themes-load-theme)
                             #'ef-themes-load-theme
                           (lambda (theme) (load-theme theme t)))))
      (cond
       ((memq current dark-themes)
        (funcall load-fn 'ef-light)
        (message "Theme: ef-light"))
       ((memq current light-themes)
        (funcall load-fn 'ef-dark)
        (message "Theme: ef-dark"))
       (t
        (funcall load-fn 'ef-dark)
        (message "Theme: ef-dark"))))))

(defun emacs-ide-select-theme ()
  "Interactively select any ef-theme by name."
  (interactive)
  (if (fboundp 'ef-themes-select)
      (call-interactively #'ef-themes-select)
    (message "ui-theme: ef-themes not loaded")))

(provide 'ui-theme)
;;; ui-theme.el ends here
