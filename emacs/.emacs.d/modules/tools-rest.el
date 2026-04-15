;;; tools-rest.el --- HTTP REST Client -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Code:

(require 'cl-lib)

(use-package verb
  :after org
  :commands (verb-send-request-on-point
             verb-send-request-on-point-other-window
             verb-export-request-on-point-curl)
  :init
  (setq verb-auto-kill-response-buffers   t
        verb-json-use-mode                'js-mode
        verb-inhibit-cookies              nil
        verb-show-headers-buffer          'when-present
        verb-response-body-byte-limit     (* 1024 1024))
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :init
  (setq restclient-same-buffer-response       t
        restclient-response-size-threshold    (* 512 1024)))

(use-package restclient-jq
  :after restclient)

(defun emacs-ide-rest-scratch ()
  (interactive)
  (let ((buf (get-buffer-create "*REST Scratch*")))
    (with-current-buffer buf
      (when (zerop (buffer-size))
        (insert "# REST Scratch — C-c C-c to send, C-c C-n for next request\n\n")
        (insert "GET https://httpbin.org/get\n")
        (insert "Accept: application/json\n\n")
        (insert "###\n\n")
        (insert "# POST example:\n")
        (insert "# POST https://httpbin.org/post\n")
        (insert "# Content-Type: application/json\n")
        (insert "#\n")
        (insert "# {\"key\": \"value\"}\n"))
      (unless (derived-mode-p 'restclient-mode)
        (restclient-mode)))
    (pop-to-buffer buf)))

(defun emacs-ide-rest-insert-request ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command works in org-mode buffers only"))
  (let* ((method   (completing-read "Method: "
                                    '("GET" "POST" "PUT" "PATCH" "DELETE")
                                    nil t "GET"))
         (url      (read-string "URL: " "https://"))
         (parsed   (url-generic-parse-url url))
         (host     (and parsed (url-host parsed)))
         (label    (if (and host (not (string-empty-p host))) host url))
         (has-body (member method '("POST" "PUT" "PATCH"))))
    (insert (format "* %s %s :verb:\n" method label))
    (insert (format "%s %s\n" (downcase method) url))
    (insert "Accept: application/json\n")
    (when has-body
      (insert "Content-Type: application/json\n")
      (insert "\n{\"key\": \"value\"}\n"))
    (insert "\n")))

(define-prefix-command 'emacs-ide-rest-map)
(global-set-key (kbd "C-c V")   'emacs-ide-rest-map)
(global-set-key (kbd "C-c V s") #'emacs-ide-rest-scratch)
(global-set-key (kbd "C-c V i") #'emacs-ide-rest-insert-request)

(provide 'tools-rest)
;;; tools-rest.el ends here
