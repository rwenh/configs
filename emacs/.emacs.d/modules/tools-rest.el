;;; tools-rest.el --- HTTP REST Client -*- lexical-binding: t -*-
;;; Commentary:
;;; Inline HTTP client using verb.el — write requests in org buffers,
;;; execute them, inspect responses. Beats Eclipse's REST client plugins
;;; because requests live in version-controlled org files alongside your code.
;;;
;;; Usage:
;;;   1. Open any .org file (or create rest-requests.org in your project)
;;;   2. Add a verb heading:
;;;
;;;      * GET example                           :verb:
;;;      get https://httpbin.org/get
;;;      Accept: application/json
;;;
;;;   3. C-c C-r C-r to execute (or C-c C-r r to send + keep focus)
;;;   4. Response opens in a dedicated buffer with syntax highlighting
;;;
;;; Add "tools-rest" to emacs-ide-feature-modules in init.el.
;;; Version: 1.0.0
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; VERB — the core REST client
;; ============================================================================
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
        verb-response-body-byte-limit     (* 1024 1024))  ; 1MB display limit
  :config
  ;; Wire verb into org-mode's C-c C-r prefix
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))

;; ============================================================================
;; RESTCLIENT — alternative for plain .http files
;; ============================================================================
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :init
  (setq restclient-same-buffer-response t
        restclient-response-size-threshold (* 512 1024))
  :config
  (use-package restclient-jq
    :after restclient))

;; ============================================================================
;; HTTP SCRATCH BUFFER
;; ============================================================================
(defun emacs-ide-rest-scratch ()
  "Open an HTTP scratch buffer for quick REST requests."
  (interactive)
  (let ((buf (get-buffer-create "*REST Scratch*")))
    (with-current-buffer buf
      (when (zerop (buffer-size))
        (restclient-mode)
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

;; ============================================================================
;; ORG-MODE REQUEST TEMPLATE HELPER
;; ============================================================================
(defun emacs-ide-rest-insert-request ()
  "Insert a verb request template at point in an org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command works in org-mode buffers only"))
  (let* ((method (completing-read "Method: " '("GET" "POST" "PUT" "PATCH" "DELETE") nil t "GET"))
         (url    (read-string "URL: " "https://"))
         (has-body (member method '("POST" "PUT" "PATCH"))))
    (insert (format "* %s %s :verb:\n" method (url-host (url-generic-parse-url url))))
    (insert (format "%s %s\n" (downcase method) url))
    (insert "Accept: application/json\n")
    (when has-body
      (insert "Content-Type: application/json\n")
      (insert "\n{\"key\": \"value\"}\n"))
    (insert "\n")))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================
(global-set-key (kbd "C-c h r") 'emacs-ide-rest-scratch)
(global-set-key (kbd "C-c h i") 'emacs-ide-rest-insert-request)

(provide 'tools-rest)
;;; tools-rest.el ends here
