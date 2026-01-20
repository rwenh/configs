;;; lib-wayland.el --- Wayland utilities -*- lexical-binding: t -*-
(defun my/wayland-p ()
  "Check if running under Wayland."
  (getenv "WAYLAND_DISPLAY"))
(provide 'lib-wayland)
