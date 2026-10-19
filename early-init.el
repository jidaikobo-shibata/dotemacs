;;; early-init.el --- Compatibility before system package startup -*- lexical-binding: t; -*-

;;; Commentary:
;; Debian's Mozc snippet expects a dynamically bound `flavor'.  Some versions
;; of debian-startup bind their argument lexically instead.  Supply the dynamic
;; value only while those system package snippets are being loaded.

;;; Code:

(require 'cl-lib)

(defun my/debian-startup-with-flavor (original emacs-flavor)
  "Call ORIGINAL with EMACS-FLAVOR also available to package snippets."
  (cl-progv '(flavor) (list emacs-flavor)
    (funcall original emacs-flavor)))

(when (and (eq system-type 'gnu/linux)
           (file-readable-p "/etc/emacs/site-start.d/50emacs-mozc.el"))
  (with-eval-after-load "debian-startup"
    (advice-add 'debian-startup :around #'my/debian-startup-with-flavor)))

;;; early-init.el ends here
