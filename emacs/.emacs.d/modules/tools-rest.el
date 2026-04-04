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
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.2 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.2 to 3.0.4.
;;;   - FIX-NESTED-USE-PACKAGE: (use-package restclient-jq) was nested inside
;;;     restclient :config. Nested use-package is unsupported — straight may
;;;     not have registered the inner package when :config runs. Moved to a
;;;     top-level (use-package restclient-jq :after restclient ...) block.
;;;   - FIX-VERB-ORG-NESTED: (with-eval-after-load 'org ...) was nested inside
;;;     verb :config. Since verb already has :after org, org is guaranteed
;;;     loaded by the time :config runs — the inner with-eval-after-load fires
;;;     immediately and is dead wrapper code. Replaced with a direct define-key.
;;;   - FIX-URL-HOST-EMPTY: (url-host (url-generic-parse-url url)) returns ""
;;;     when url is the default "https://" prompt value — produces a heading
;;;     "* GET  :verb:" with a blank host. Added a fallback to use the full
;;;     URL when the host is empty or nil.
;;;   - FIX-SCRATCH-MODE-ORDER: restclient-mode was activated inside the
;;;     (when (zerop (buffer-size))) block before inserting the template text,
;;;     triggering mode hooks prematurely. Moved mode activation to after
;;;     content insertion, with a single unified (unless derived-mode-p) guard.
;;;   - FIX-VERB-CCC-R: Added comment documenting that C-c C-r in org-mode
;;;     is used by verb, noting the potential conflict with org-reveal
;;;     (org's built-in C-c C-r). Users who need org-reveal should remap.
;;; Fixes vs 1.0.1 (retained):
;;;   - FIX-2: REST prefix at C-c V (was C-c R which collides with reload).
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
  ;; FIX-VERB-ORG-NESTED: org is guaranteed loaded (:after org) so no need
  ;; for with-eval-after-load here — direct define-key is correct.
  ;; FIX-VERB-CCC-R: C-c C-r is used here for verb in org-mode.
  ;; NOTE: org's built-in C-c C-r is org-reveal (unfold to show current
  ;; heading). If you use org-reveal regularly, remap verb to another key
  ;; by changing this binding before tools-rest.el loads.
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;; ============================================================================
;; RESTCLIENT — alternative for plain .http files
;; ============================================================================
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :init
  (setq restclient-same-buffer-response       t
        restclient-response-size-threshold    (* 512 1024)))

;; FIX-NESTED-USE-PACKAGE: moved from inside restclient :config to top level.
;; Nested use-package is unreliable — straight may not have registered
;; restclient-jq when the outer :config block runs.
(use-package restclient-jq
  :after restclient)

;; ============================================================================
;; HTTP SCRATCH BUFFER
;; FIX-SCRATCH-MODE-ORDER: restclient-mode now activated after content
;; insertion to avoid triggering mode hooks on an empty buffer mid-setup.
;; ============================================================================
(defun emacs-ide-rest-scratch ()
  "Open an HTTP scratch buffer for quick REST requests."
  (interactive)
  (let ((buf (get-buffer-create "*REST Scratch*")))
    (with-current-buffer buf
      (when (zerop (buffer-size))
        ;; Insert template content BEFORE activating the mode so that
        ;; mode hooks (syntax highlighting, etc.) see the complete buffer.
        (insert "# REST Scratch — C-c C-c to send, C-c C-n for next request\n\n")
        (insert "GET https://httpbin.org/get\n")
        (insert "Accept: application/json\n\n")
        (insert "###\n\n")
        (insert "# POST example:\n")
        (insert "# POST https://httpbin.org/post\n")
        (insert "# Content-Type: application/json\n")
        (insert "#\n")
        (insert "# {\"key\": \"value\"}\n"))
      ;; Activate mode if not already active (covers both new and existing buffers)
      (unless (derived-mode-p 'restclient-mode)
        (restclient-mode)))
    (pop-to-buffer buf)))

;; ============================================================================
;; ORG-MODE REQUEST TEMPLATE HELPER
;; FIX-URL-HOST-EMPTY: url-host returns "" for partial URLs like "https://".
;; Now falls back to the full URL string when the host is empty or nil.
;; ============================================================================
(defun emacs-ide-rest-insert-request ()
  "Insert a verb request template at point in an org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command works in org-mode buffers only"))
  (let* ((method   (completing-read "Method: "
                                    '("GET" "POST" "PUT" "PATCH" "DELETE")
                                    nil t "GET"))
         (url      (read-string "URL: " "https://"))
         ;; FIX-URL-HOST-EMPTY: use full URL as heading label when host
         ;; cannot be parsed (e.g. user entered the bare "https://" default)
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

;; ============================================================================
;; KEYBINDINGS
;; FIX-2 (retained): Prefix at C-c V — C-c R is owned by emacs-ide-reload-config.
;; ============================================================================
(define-prefix-command 'emacs-ide-rest-map)
(global-set-key (kbd "C-c V")   'emacs-ide-rest-map)
(global-set-key (kbd "C-c V s") #'emacs-ide-rest-scratch)
(global-set-key (kbd "C-c V i") #'emacs-ide-rest-insert-request)

(provide 'tools-rest)
;;; tools-rest.el ends here
