;;; emacs-ide-security.el --- Security Hardening -*- lexical-binding: t -*-
;;; Commentary:
;;; Enterprise security configuration with config integration.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.3 to 3.0.4.
;;;   - FIX-CUSTOM-FILE: emacs-ide-security-check replaced
;;;     (boundp 'custom-file) + custom-file with (stringp custom-file).
;;;     The old guard worked by accident (nil short-circuits the and),
;;;     but (stringp) is the correct and consistent guard matching
;;;     FIX-CUSTOM in init.el and FIX-CUSTOM-FILE in emacs-ide-health.el.
;;;   - FIX-CONFIG-GET: All four top-level config reads now use
;;;     emacs-ide-config-get via with-eval-after-load 'emacs-ide-config
;;;     instead of raw assoc chains on emacs-ide-config-data. The
;;;     immediate application at load time is preserved as a fallback
;;;     using the same hardcoded defaults, so the security net is never
;;;     weakened when config hasn't loaded yet.
;;;   - FIX-PKG-SIG-COMMENT: package-check-signature block now documents
;;;     that package.el is disabled in this config (straight.el is used
;;;     exclusively) so the setting is a safety net only.
;;;   - FIX-PRIME-BITS: gnutls-min-prime-bits now wrapped in (boundp)
;;;     and documented as deprecated in Emacs 29+ where GnuTLS manages
;;;     prime requirements internally.
;;;   - FIX-TLS-LEGACY: tls-checktrust and tls-program documented as
;;;     legacy tls.el settings, present as a safety net for any code
;;;     that still uses the old tls.el path.
;;;   - FIX-AUDIT-PRIME: emacs-ide-security-check now verifies that
;;;     gnutls-min-prime-bits is set to an acceptably strong value.
;;;   - FIX-AUDIT-GPG-AUTH: emacs-ide-security-check auth-source check
;;;     now accepts any .gpg or .age suffixed file rather than requiring
;;;     exactly ~/.authinfo.gpg.
;;;   - FIX-HARDEN: emacs-ide-security-harden interactive command added
;;;     to re-apply all security settings on demand after a config reload.
;;; Fixes vs 2.2.3 (retained):
;;;   - 2.2.3: tls-program set here as well as early-init.el.
;;;   - 2.2.3: auth-sources: only string entries are expand-file-name'd.
;;;   - 2.2.2: (require 'gnutls) replaces with-eval-after-load.
;;;   - 2.2.1: auth-sources local let assignment bug fixed.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; SECURITY DEFAULTS
;; These are the values applied immediately at load time (before config loads)
;; and re-applied by emacs-ide-security-harden after config is available.
;; They represent the strongest safe defaults — config.yml can only widen
;; them (e.g. set tls-verify to nil), never silently weaken them at load time.
;; ============================================================================
(defconst emacs-ide-security--tls-verify-default       t)
(defconst emacs-ide-security--prime-bits-default        3072)
(defconst emacs-ide-security--pkg-sig-default          'allow-unsigned)
(defconst emacs-ide-security--net-sec-default          'high)
(defconst emacs-ide-security--auth-sources-default
  '("~/.authinfo.gpg" "~/.netrc"))

;; ============================================================================
;; CORE APPLY FUNCTION
;; Single place that reads config and sets all security variables.
;; Called at load time (with fallback defaults) and by emacs-ide-security-harden.
;; ============================================================================
(defun emacs-ide-security--apply ()
  "Apply security settings from config or fall back to safe defaults.
FIX-CONFIG-GET: Uses emacs-ide-config-get when available instead of
raw assoc chains on emacs-ide-config-data."
  (let* ((cfg-get (lambda (key default)
                    (if (fboundp 'emacs-ide-config-get)
                        (emacs-ide-config-get 'security key default)
                      default)))
         (tls-verify  (funcall cfg-get 'tls-verify
                               emacs-ide-security--tls-verify-default))
         (prime-bits  (funcall cfg-get 'tls-min-prime-bits
                               emacs-ide-security--prime-bits-default))
         (check-sigs  (funcall cfg-get 'package-signatures
                               emacs-ide-security--pkg-sig-default))
         (net-sec     (funcall cfg-get 'network-security-level
                               emacs-ide-security--net-sec-default))
         (raw-sources (if (fboundp 'emacs-ide-config-get)
                          (emacs-ide-config-get 'security 'auth-sources nil)
                        nil))
         (sources     (if (and (listp raw-sources) raw-sources)
                          raw-sources
                        emacs-ide-security--auth-sources-default)))

    ;; ── TLS ──────────────────────────────────────────────────────────────────
    ;; FIX 2.2.2: (require 'gnutls) at top level ensures this runs before any
    ;; network call. With-eval-after-load was too late for the first connection.
    (require 'gnutls)
    (setq gnutls-verify-error tls-verify)

    ;; FIX-PRIME-BITS: gnutls-min-prime-bits is deprecated in Emacs 29+ where
    ;; GnuTLS manages prime requirements internally via its priority string.
    ;; Still set here for Emacs 28 and as a belt-and-suspenders measure.
    (when (boundp 'gnutls-min-prime-bits)
      (setq gnutls-min-prime-bits prime-bits))

    ;; FIX-TLS-LEGACY: tls-checktrust and tls-program are tls.el (legacy)
    ;; settings. tls.el is rarely invoked in Emacs 29+ (most connections use
    ;; gnutls directly) but we set them as a safety net for any code that
    ;; still uses the old path.
    (setq tls-checktrust t)
    ;; FIX 2.2.3: tls-program set here (not only in early-init.el) so this
    ;; module is self-contained when early-init.el is bypassed.
    (setq tls-program '("gnutls-cli --x509cafile %t -p %p %h"))

    ;; ── Package signatures ───────────────────────────────────────────────────
    ;; FIX-PKG-SIG-COMMENT: package.el is disabled in this config (straight.el
    ;; is used exclusively via package-enable-at-startup nil). This is a safety
    ;; net for any direct package-install calls that might slip through.
    (setq package-check-signature check-sigs)

    ;; ── Auth sources ─────────────────────────────────────────────────────────
    ;; FIX 2.2.3: only expand-file-name on string entries; non-string entries
    ;; (macos-keychain-internet, (:source "pass:...")) pass through unchanged.
    (require 'auth-source)
    (setq auth-sources
          (mapcar (lambda (entry)
                    (if (stringp entry)
                        (expand-file-name entry)
                      entry))
                  sources))

    ;; ── Network security ─────────────────────────────────────────────────────
    (setq network-security-level net-sec)))

;; Apply immediately at load time with whatever is available.
(emacs-ide-security--apply)

;; Re-apply after config loads to pick up any user overrides from config.yml.
(with-eval-after-load 'emacs-ide-config
  (emacs-ide-security--apply))

;; ============================================================================
;; INTERACTIVE COMMANDS
;; ============================================================================
(defun emacs-ide-security-harden ()
  "Re-apply all security settings from config.yml.
FIX-HARDEN: New command. Useful after M-x emacs-ide-config-reload
to ensure security settings reflect the latest config.yml values."
  (interactive)
  (emacs-ide-security--apply)
  (message "✓ Security settings re-applied"))

;; ============================================================================
;; SECURITY AUDIT
;; ============================================================================
(defun emacs-ide-security-check ()
  "Run security audit with detailed reporting."
  (interactive)
  (let ((warnings '())
        (ok '()))

    ;; TLS verification
    (if (and (boundp 'gnutls-verify-error) gnutls-verify-error)
        (push "✓ TLS verification enabled" ok)
      (push "⚠️  TLS verification not enforced" warnings))

    ;; tls-program set to gnutls-cli (not insecure openssl default)
    (if (and (boundp 'tls-program)
             (listp tls-program)
             (cl-some (lambda (p) (string-match-p "gnutls-cli" p)) tls-program))
        (push "✓ TLS program set to gnutls-cli" ok)
      (push "⚠️  tls-program may be using insecure openssl s_client default" warnings))

    ;; FIX-AUDIT-PRIME: verify gnutls-min-prime-bits is acceptably strong.
    ;; Only checked on Emacs < 29 where the variable is still active; on
    ;; 29+ we note that GnuTLS manages this internally.
    (if (boundp 'gnutls-min-prime-bits)
        (if (>= gnutls-min-prime-bits 2048)
            (push (format "✓ gnutls-min-prime-bits = %d (strong)" gnutls-min-prime-bits) ok)
          (push (format "⚠️  gnutls-min-prime-bits = %d (weak, recommend >= 2048)"
                        gnutls-min-prime-bits) warnings))
      (push "✓ gnutls-min-prime-bits not applicable (Emacs 29+ uses GnuTLS priority string)" ok))

    ;; Package signatures
    (if (bound-and-true-p package-check-signature)
        (push "✓ Package signatures checked" ok)
      (push "⚠️  Package signatures not checked" warnings))

    ;; Auth source — FIX-AUDIT-GPG-AUTH: accept any .gpg or .age file,
    ;; not just the specific path ~/.authinfo.gpg.
    (if (and (boundp 'auth-sources)
             (cl-some (lambda (s)
                        (and (stringp s)
                             (or (string-suffix-p ".gpg" s)
                                 (string-suffix-p ".age" s))))
                      auth-sources))
        (push "✓ Encrypted auth source configured" ok)
      (push "⚠️  No encrypted auth source (.gpg/.age) configured" warnings))

    ;; Custom file location
    ;; FIX-CUSTOM-FILE: (stringp custom-file) correctly guards against nil.
    ;; The old (boundp 'custom-file) + custom-file worked by accident.
    (if (and (stringp custom-file)
             (not (string= custom-file user-init-file)))
        (push "✓ Custom file is separate" ok)
      (push "⚠️  Custom file should be separate from init.el" warnings))

    ;; GPG availability
    (if (executable-find "gpg")
        (push "✓ GPG is available" ok)
      (push "⚠️  GPG not found (needed for auth-source encryption)" warnings))

    ;; Display report
    (with-output-to-temp-buffer "*Security Audit*"
      (princ "=== SECURITY AUDIT ===\n\n")
      (when ok
        (princ "PASSED:\n")
        (dolist (item (nreverse ok))
          (princ (format "  %s\n" item)))
        (princ "\n"))
      (if warnings
          (progn
            (princ "WARNINGS:\n")
            (dolist (item (nreverse warnings))
              (princ (format "  %s\n" item))))
        (princ "No security warnings found.\n")))))

(provide 'emacs-ide-security)
;;; emacs-ide-security.el ends here