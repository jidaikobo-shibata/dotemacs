;;; platform.init.el --- Platform settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Apply only settings supported by the current display.
;;; Code:
(require 'settings.init)
(require 'cl-lib)
(when my/use-mozc
  (when (file-directory-p "/usr/share/emacs/site-lisp/emacs-mozc")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc"))
  (unless (and (locate-library "mozc") (executable-find "mozc_emacs_helper"))
    (setq my/use-mozc nil)
    (display-warning 'my/settings
                     "Mozc or mozc_emacs_helper unavailable; Mozc integration disabled")))
(when (eq system-type 'darwin)
  (setq ns-command-modifier my/mac-command-modifier
        ns-option-modifier my/mac-option-modifier
        mac-command-modifier my/mac-command-modifier
        mac-option-modifier my/mac-option-modifier))
(defun my/apply-frame-fonts (&optional frame)
  "Apply available preferred fonts to graphical FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (let ((font (cl-find-if (lambda (name) (find-font (font-spec :family name)))
                              my/font-families))
            (japanese (cl-find-if (lambda (name) (find-font (font-spec :family name)))
                                  my/japanese-font-families)))
        (set-face-attribute 'default (selected-frame) :height my/font-height)
        (when font (set-face-attribute 'default (selected-frame) :family font))
        (when japanese
          (set-fontset-font t 'japanese-jisx0208 japanese (selected-frame)))))))
(provide 'platform.init)
;;; platform.init.el ends here
