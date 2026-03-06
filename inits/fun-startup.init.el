;;; fun-startup.init.el --- Silly startup messages -*- lexical-binding: t; -*-

;;; Commentary:
;; Show a random fun quote when startup emitted "Package cl is deprecated".

;;; Code:

(defvar my/fun-startup-messages
  '("Yabba Dabba Doo!"
    "Hasta la vista, baby."
    "There is an element of fun in every job."
    "You will never age for me, nor fade, nor die."
    "Why so serious?"
    "Roads? Where we're going, we don't need roads."
    "The only way to do great work is to love what you do."
    "Fortune favors the bold.")
  "Fun messages shown when deprecated `cl' package warning appears at startup.")

(defun my/show-fun-message-if-cl-deprecated ()
  "Show a random fun message when deprecated `cl' warning is in *Messages*."
  (when (with-current-buffer (messages-buffer)
          (save-excursion
            (goto-char (point-max))
            (re-search-backward "Package cl is deprecated" nil t)))
    (message "%s"
             (nth (random (length my/fun-startup-messages))
                  my/fun-startup-messages))))

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Delay a little so this appears after startup warnings.
            (run-with-timer 0.1 nil #'my/show-fun-message-if-cl-deprecated)))

(provide 'fun-startup.init)

;;; fun-startup.init.el ends here
