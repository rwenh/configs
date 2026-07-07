;;; tools-devops.el --- DevOps Integration -*- lexical-binding: t -*-
;;;
;;; Version: 3.5.0
;;; Code:

(require 'cl-lib)
(eval-and-compile
  (require 'ide-common
           (expand-file-name "lib/ide-common.el" user-emacs-directory) t))

;;;; ── Config helpers ──────────────────────────────────────────────────────────

(defun emacs-ide-devops--cfg (key &optional default)
  (if (fboundp 'emacs-ide-config-get)
      (emacs-ide-config-get 'devops key default)
    default))

(defun emacs-ide-devops--docker-enabled-p ()
  (emacs-ide-devops--cfg 'docker-enable t))

(defun emacs-ide-devops--k8s-enabled-p ()
  (emacs-ide-devops--cfg 'kubernetes-enable nil))

(defun emacs-ide-devops--terraform-enabled-p ()
  (emacs-ide-devops--cfg 'terraform-enable t))

;;;; ── Shell helpers ───────────────────────────────────────────────────────────

(defun emacs-ide-devops--run (cmd &optional buf-name)
  "Run CMD in a compilation buffer named BUF-NAME (default *DevOps*)."
  (let ((compilation-buffer-name-function
         (lambda (_) (or buf-name "*DevOps*"))))
    (compile cmd)))

(defun emacs-ide-devops--shell-lines (cmd)
  "Run CMD and return trimmed non-empty output lines, or nil."
  (with-temp-buffer
    (when (= 0 (call-process shell-file-name nil t nil shell-command-switch cmd))
      (cl-remove-if #'string-empty-p
                    (mapcar #'string-trim
                            (split-string (buffer-string) "\n"))))))

(defun emacs-ide-devops--choose-or-read (items prompt)
  "Completing-read from ITEMS, or read-string if ITEMS is nil."
  (if items
      (completing-read prompt items nil t)
    (read-string prompt)))

;;;; ── Docker ─────────────────────────────────────────────────────────────────

(defun emacs-ide-devops--docker ()
  "Return \"docker\" path or signal an error."
  (or (executable-find "docker")
      (user-error "tools-devops: docker not found on PATH")))

(defun emacs-ide-devops-docker-ps ()
  "List running Docker containers."
  (interactive)
  (unless (emacs-ide-devops--docker-enabled-p)
    (user-error "Docker disabled in config.yml (devops.docker-enable: false)"))
  (with-output-to-temp-buffer "*Docker PS*"
    (let ((lines (emacs-ide-devops--shell-lines
                   "docker ps --format 'table {{.ID}}\t{{.Names}}\t{{.Image}}\t{{.Status}}\t{{.Ports}}'")))
      (if lines
          (dolist (l lines) (princ l) (princ "\n"))
        (princ "(no running containers)\n")))))

(defun emacs-ide-devops-docker-logs (container)
  "Stream logs for CONTAINER."
  (interactive
   (list (emacs-ide-devops--choose-or-read
          (emacs-ide-devops--shell-lines
           "docker ps --format '{{.Names}}'")
          "Container: ")))
  (emacs-ide-devops--run (format "docker logs --tail=200 -f %s"
                                   (shell-quote-argument container))
                          (format "*docker-logs:%s*" container)))

(defun emacs-ide-devops-docker-exec (container cmd)
  "Run CMD inside CONTAINER."
  (interactive
   (let ((c (emacs-ide-devops--choose-or-read
              (emacs-ide-devops--shell-lines
               "docker ps --format '{{.Names}}'")
              "Container: ")))
     (list c (read-string (format "Command in %s: " c) "bash"))))
  (emacs-ide-devops--run (format "docker exec -it %s %s"
                                   (shell-quote-argument container) cmd)
                          (format "*docker-exec:%s*" container)))

(defun emacs-ide-devops-docker-build (&optional tag)
  "Build a Docker image from the project Dockerfile."
  (interactive)
  (let* ((root (or (and (fboundp 'projectile-project-root)
                        (ignore-errors (projectile-project-root)))
                   default-directory))
         (name (or (and (fboundp 'projectile-project-name)
                        (ignore-errors (projectile-project-name)))
                   (file-name-nondirectory (directory-file-name root))))
         (tag  (or tag (read-string "Image tag: " name)))
         (default-directory root))
    (emacs-ide-devops--run (format "docker build -t %s ."
                                    (shell-quote-argument tag))
                            "*docker-build*")))

(defun emacs-ide-devops-docker-run (image &optional cmd)
  "Run IMAGE as a new Docker container."
  (interactive
   (list (emacs-ide-devops--choose-or-read
          (emacs-ide-devops--shell-lines
           "docker images --format '{{.Repository}}:{{.Tag}}'")
          "Image: ")
         (read-string "Command (blank = default): ")))
  (let ((run-cmd (format "docker run --rm -it %s%s"
                          (shell-quote-argument image)
                          (if (string-blank-p cmd) "" (concat " " cmd)))))
    (emacs-ide-devops--run run-cmd "*docker-run*")))

(defun emacs-ide-devops-docker-stop (container)
  "Stop a running Docker container."
  (interactive
   (list (emacs-ide-devops--choose-or-read
          (emacs-ide-devops--shell-lines "docker ps --format '{{.Names}}'")
          "Stop container: ")))
  (shell-command (format "docker stop %s" (shell-quote-argument container)))
  (message "Stopped: %s" container))

(defun emacs-ide-devops-docker-pull (image)
  "Pull an image from a Docker registry."
  (interactive "sImage: ")
  (emacs-ide-devops--run (format "docker pull %s"
                                   (shell-quote-argument image)) "*docker-pull*"))

(defun emacs-ide-devops-docker-prune ()
  "Remove stopped containers, dangling images, and unused networks."
  (interactive)
  (when (yes-or-no-p "Prune stopped containers, dangling images, and unused networks? ")
    (emacs-ide-devops--run "docker system prune -f" "*docker-prune*")))

;;;; ── Compose ─────────────────────────────────────────────────────────────────

(defun emacs-ide-devops--compose-file ()
  "Find the nearest docker-compose file going up from `default-directory'."
  (let ((configured (emacs-ide-devops--cfg 'compose-file nil)))
    (if (and configured (file-exists-p configured))
        (expand-file-name configured)
      (let ((candidates '("docker-compose.yml" "docker-compose.yaml"
                           "compose.yml" "compose.yaml"))
            (dir default-directory)
            found)
        (while (and (not found) dir (not (string= dir "/")))
          (dolist (f candidates)
            (let ((path (expand-file-name f dir)))
              (when (file-exists-p path)
                (setq found path))))
          (unless found
            (setq dir (file-name-directory (directory-file-name dir)))))
        found))))

(defun emacs-ide-devops--compose-cli ()
  "Return \"docker compose\" or \"docker-compose\", whichever is available."
  (cond
   ((zerop (call-process "docker" nil nil nil "compose" "version"))
    "docker compose")
   ((executable-find "docker-compose")
    "docker-compose")
   (t (user-error "tools-devops: neither 'docker compose' nor 'docker-compose' found"))))

(defun emacs-ide-devops-compose-up (&optional detach)
  "Start Compose services.  With prefix argument, run in background."
  (interactive "P")
  (let* ((f   (emacs-ide-devops--compose-file))
         (cli (emacs-ide-devops--compose-cli))
         (cmd (format "%s%s up%s"
                       cli
                       (if f (format " -f %s" (shell-quote-argument f)) "")
                       (if detach " -d" ""))))
    (emacs-ide-devops--run cmd "*compose-up*")))

(defun emacs-ide-devops-compose-down (&optional volumes)
  "Stop and remove Compose services.  With prefix arg, also remove volumes."
  (interactive "P")
  (let* ((f   (emacs-ide-devops--compose-file))
         (cli (emacs-ide-devops--compose-cli))
         (cmd (format "%s%s down%s"
                       cli
                       (if f (format " -f %s" (shell-quote-argument f)) "")
                       (if volumes " -v" ""))))
    (emacs-ide-devops--run cmd "*compose-down*")))

(defun emacs-ide-devops-compose-logs (&optional service)
  "Show Compose logs, optionally filtered to SERVICE."
  (interactive)
  (let* ((f       (emacs-ide-devops--compose-file))
         (cli     (emacs-ide-devops--compose-cli))
         (service (read-string "Service (blank = all): "))
         (cmd     (format "%s%s logs --tail=200 -f%s"
                           cli
                           (if f (format " -f %s" (shell-quote-argument f)) "")
                           (if (string-blank-p service) ""
                             (concat " " (shell-quote-argument service))))))
    (emacs-ide-devops--run cmd "*compose-logs*")))

;;;; ── Kubernetes ──────────────────────────────────────────────────────────────

(defun emacs-ide-devops-k8s-overview ()
  "Show running pods, services, and current context."
  (interactive)
  (unless (emacs-ide-devops--k8s-enabled-p)
    (user-error "Kubernetes disabled (set devops.kubernetes-enable: true in config.yml)"))
  (unless (executable-find "kubectl")
    (user-error "tools-devops: kubectl not found on PATH"))
  (with-output-to-temp-buffer "*Kubernetes Overview*"
    (princ "=== CURRENT CONTEXT ===\n")
    (dolist (l (emacs-ide-devops--shell-lines "kubectl config current-context"))
      (princ l) (princ "\n"))
    (princ "\n=== PODS ===\n")
    (dolist (l (emacs-ide-devops--shell-lines "kubectl get pods -A"))
      (princ l) (princ "\n"))
    (princ "\n=== SERVICES ===\n")
    (dolist (l (emacs-ide-devops--shell-lines "kubectl get svc -A"))
      (princ l) (princ "\n"))))

;;;; ── Terraform ───────────────────────────────────────────────────────────────

(defun emacs-ide-devops--tf-root ()
  "Return the Terraform working directory."
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      default-directory))

(defun emacs-ide-devops-terraform-init ()
  "Run terraform init."
  (interactive)
  (unless (emacs-ide-devops--terraform-enabled-p)
    (user-error "Terraform disabled in config.yml"))
  (let ((default-directory (emacs-ide-devops--tf-root)))
    (emacs-ide-devops--run "terraform init" "*terraform-init*")))

(defun emacs-ide-devops-terraform-plan ()
  "Run terraform plan, saving the plan to .tfplan."
  (interactive)
  (unless (emacs-ide-devops--terraform-enabled-p)
    (user-error "Terraform disabled in config.yml"))
  (let ((default-directory (emacs-ide-devops--tf-root)))
    (emacs-ide-devops--run "terraform plan -out=.tfplan" "*terraform-plan*")))

(defun emacs-ide-devops-terraform-apply ()
  "Apply the saved .tfplan, or run terraform apply if no plan file exists."
  (interactive)
  (unless (emacs-ide-devops--terraform-enabled-p)
    (user-error "Terraform disabled in config.yml"))
  (let* ((root (emacs-ide-devops--tf-root))
         (plan (expand-file-name ".tfplan" root))
         (cmd  (if (file-exists-p plan)
                   "terraform apply .tfplan"
                 "terraform apply"))
         (default-directory root))
    (when (yes-or-no-p (format "Run: %s ? " cmd))
      (emacs-ide-devops--run cmd "*terraform-apply*"))))

;;;; ── Env file loader ─────────────────────────────────────────────────────────

(defvar emacs-ide-devops--env-snapshot nil
  "Saved `process-environment' before the last env load, for restoration.")

(defun emacs-ide-devops--parse-env-file (path)
  "Return alist of (KEY . VALUE) from a .env file at PATH.
Handles VAR=value, 'VAR=value', \"VAR=value\", comments (#...), and
blank lines.  Does NOT execute shell substitutions."
  (with-temp-buffer
    (insert-file-contents path)
    (let (result)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))
          (when (and (not (string-empty-p line))
                     (not (string-prefix-p "#" line))
                     (string-match "^\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.*\\)$" line))
            (let* ((key (match-string 1 line))
                   (raw (match-string 2 line))
                   ;; strip surrounding quotes if consistent
                   (val (if (and (> (length raw) 1)
                                 (or (and (string-prefix-p "'" raw)
                                          (string-suffix-p "'" raw))
                                     (and (string-prefix-p "\"" raw)
                                          (string-suffix-p "\"" raw))))
                            (substring raw 1 -1)
                          raw)))
              (push (cons key val) result))))
        (forward-line 1))
      (nreverse result))))

(defun emacs-ide-devops-env-load (&optional path)
  "Load a .env file into the Emacs process environment.
PATH defaults to the project root's .env file."
  (interactive (list (read-file-name ".env file: "
                                      (or (and (fboundp 'projectile-project-root)
                                               (ignore-errors (projectile-project-root)))
                                          default-directory)
                                      nil t ".env")))
  (let ((env-file (or path (expand-file-name ".env" default-directory))))
    (unless (file-exists-p env-file)
      (user-error "File not found: %s" env-file))
    ;; Save snapshot before first load in this session
    (unless emacs-ide-devops--env-snapshot
      (setq emacs-ide-devops--env-snapshot (copy-sequence process-environment)))
    (let ((pairs (emacs-ide-devops--parse-env-file env-file))
          (loaded 0))
      (dolist (entry pairs)
        (setenv (car entry) (cdr entry))
        (cl-incf loaded))
      (message "Loaded %d vars from %s" loaded env-file))))

(defun emacs-ide-devops-env-unload ()
  "Restore the process environment to the state before the last env-load."
  (interactive)
  (if (null emacs-ide-devops--env-snapshot)
      (message "tools-devops: no env snapshot — call emacs-ide-devops-env-load first")
    (setq process-environment emacs-ide-devops--env-snapshot
          emacs-ide-devops--env-snapshot nil)
    (message "Process environment restored")))

;;;; ── Status ──────────────────────────────────────────────────────────────────

(defun emacs-ide-devops-status ()
  "Display a DevOps environment summary."
  (interactive)
  (with-output-to-temp-buffer "*DevOps Status*"
    (princ "=== DEVOPS STATUS ===\n\n")
    (princ (format "Docker:    %s (%s)\n"
                   (if (emacs-ide-devops--docker-enabled-p) "enabled" "disabled")
                   (if (executable-find "docker") "found" "not found")))
    (when (emacs-ide-devops--docker-enabled-p)
      (let ((running (emacs-ide-devops--shell-lines "docker ps -q 2>/dev/null")))
        (princ (format "  running containers: %d\n" (length running)))))
    (let ((cf (emacs-ide-devops--compose-file)))
      (princ (format "Compose:   %s\n" (if cf cf "no file found"))))
    (princ (format "Kubernetes: %s (%s)\n"
                   (if (emacs-ide-devops--k8s-enabled-p) "enabled" "disabled")
                   (if (executable-find "kubectl") "found" "not found")))
    (princ (format "Terraform:  %s (%s)\n"
                   (if (emacs-ide-devops--terraform-enabled-p) "enabled" "disabled")
                   (if (executable-find "terraform") "found" "not found")))
    (princ (format "Env loaded: %s\n\n"
                   (if emacs-ide-devops--env-snapshot "yes (snapshot saved)" "no")))
    (princ "Commands:\n")
    (princ "  M-x emacs-ide-devops-docker-ps      docker container list\n")
    (princ "  M-x emacs-ide-devops-compose-up     start compose services\n")
    (princ "  M-x emacs-ide-devops-terraform-plan terraform plan\n")
    (princ "  M-x emacs-ide-devops-env-load       load .env file\n")))

;;;; ── Hydra ───────────────────────────────────────────────────────────────────

(with-eval-after-load 'hydra
  (defhydra hydra-devops (:hint nil :color blue)
    "
  devops
  ──────────────────────────────────────────────────────
  docker   _p_ ps       _l_ logs    _e_ exec    _b_ build
           _r_ run      _S_ stop    _P_ pull    _X_ prune
  compose  _U_ up       _D_ down    _L_ logs
  k8s      _k_ overview (requires devops.kubernetes-enable: true)
  tf       _i_ init     _n_ plan    _a_ apply
  env      _v_ load .env  _V_ unload
           _s_ status
  ──────────────────────────────────────────────────────
  _ESC_ quit
"
    ("p" emacs-ide-devops-docker-ps)
    ("l" (call-interactively #'emacs-ide-devops-docker-logs))
    ("e" (call-interactively #'emacs-ide-devops-docker-exec))
    ("b" emacs-ide-devops-docker-build)
    ("r" (call-interactively #'emacs-ide-devops-docker-run))
    ("S" (call-interactively #'emacs-ide-devops-docker-stop))
    ("P" (call-interactively #'emacs-ide-devops-docker-pull))
    ("X" emacs-ide-devops-docker-prune)
    ("U" emacs-ide-devops-compose-up)
    ("D" emacs-ide-devops-compose-down)
    ("L" emacs-ide-devops-compose-logs)
    ("k" emacs-ide-devops-k8s-overview)
    ("i" emacs-ide-devops-terraform-init)
    ("n" emacs-ide-devops-terraform-plan)
    ("a" emacs-ide-devops-terraform-apply)
    ("v" emacs-ide-devops-env-load)
    ("V" emacs-ide-devops-env-unload)
    ("s" emacs-ide-devops-status)
    ("ESC" nil)))

;;;; ── Keybindings ─────────────────────────────────────────────────────────────

(define-prefix-command 'emacs-ide-devops-map)
(global-set-key (kbd "C-c o") 'emacs-ide-devops-map)
(global-set-key (kbd "C-c o s") #'emacs-ide-devops-status)
(global-set-key (kbd "C-c h o")
  (lambda () (interactive)
    (if (fboundp 'hydra-devops/body)
        (hydra-devops/body)
      (message "hydra-devops not loaded"))))

(provide 'tools-devops)
;;; tools-devops.el ends here
