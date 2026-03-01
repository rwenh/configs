;;; editing-nav.el --- DELETED: content merged into editing-core.el -*- lexical-binding: t -*-
;;; Commentary:
;;; ACTION REQUIRED: Delete this file from your modules/ directory.
;;;
;;;   rm ~/.emacs.d/modules/editing-nav.el
;;;
;;; All navigation config (avy, keybindings) now lives in editing-core.el.
;;; Keeping this file risks a stale/conflicting avy config loading on top.
;;; This stub raises a hard error so the problem is visible immediately
;;; rather than silently producing wrong keybindings.
;;; Code:

(error "editing-nav.el must be deleted — run: rm %s"
       (or load-file-name buffer-file-name "~/.emacs.d/modules/editing-nav.el"))

(provide 'editing-nav)
;;; editing-nav.el ends here
