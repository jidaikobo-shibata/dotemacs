;;; foreign-regexp-closures.el --- Captured callbacks for foreign-regexp -*- lexical-binding: t; -*-

;; Local cl-lib port, 2026-10-17.  This file is licensed under GPL-3.0-or-later,
;; like foreign-regexp.el.  See COPYING.

;;; Commentary:
;; Keep lexical capture limited to delayed callbacks.  The legacy implementation
;; remains dynamically bound; search-center's options and advice keep their scope.

;;; Code:
(require 'cl-lib)
(require 'isearch)

(declare-function foreign-regexp/.signal-option-changed "foreign-regexp")

(defun foreign-regexp/.make-option-hook (buffer hook toggle)
  "Return a self-removing option callback capturing BUFFER, HOOK and TOGGLE."
  (letrec ((callback
            (lambda ()
              (with-current-buffer buffer
                (cl-progv (list hook) (list (symbol-value hook))
                  ;; Suppress only this callback during the recursive toggle.
                  (remove-hook hook callback)
                  (funcall toggle t)))
              (foreign-regexp/.signal-option-changed))))
    callback))

(defun foreign-regexp/.make-isearch-setup-hook (pattern)
  "Return a one-shot isearch callback retaining PATTERN."
  (letrec ((callback
            (lambda ()
              (remove-hook 'isearch-mode-hook callback)
              (isearch-push-state)
              (setq isearch-string pattern
                    isearch-message pattern))))
    callback))

(defun foreign-regexp/.call-isearch-with-regexp (pattern command)
  "Enter isearch COMMAND with the previously captured PATTERN."
  (add-hook 'isearch-mode-hook (foreign-regexp/.make-isearch-setup-hook pattern))
  (let ((this-command command))
    (call-interactively command)))

(provide 'foreign-regexp-closures)
;;; foreign-regexp-closures.el ends here
