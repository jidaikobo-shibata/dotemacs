;;; minibuffer-focus.init.el --- Cancel selected minibuffer prompts on focus out
;;; Commentary:
;; Provide opt-in helpers for minibuffer prompts that should be cancelled when
;; the user moves to another window.
;;; Code:

(defvar my/cancel-minibuffer-on-focus-out nil
  "Non-nil means cancel the active minibuffer after selecting another window.
Bind this dynamically around a minibuffer reader instead of enabling it for
all minibuffer sessions.")

(defun my/cancel-minibuffer-on-focus-out--maybe-abort (_frame)
  "Cancel an opted-in minibuffer after focus moves to another window."
  (let ((minibuffer-window (active-minibuffer-window)))
    (when (and my/cancel-minibuffer-on-focus-out
               minibuffer-window
               (not (eq (selected-window) minibuffer-window)))
      (abort-recursive-edit))))

(add-hook 'window-selection-change-functions
          #'my/cancel-minibuffer-on-focus-out--maybe-abort)

(defmacro my/with-minibuffer-cancel-on-focus-out (&rest body)
  "Run BODY and cancel its minibuffer if another window is selected."
  (declare (indent 0) (debug t))
  `(let ((my/cancel-minibuffer-on-focus-out t))
     ,@body))

(defun my/read-string-cancel-on-focus-out (&rest args)
  "Call `read-string' with ARGS and cancel it after selecting another window."
  (my/with-minibuffer-cancel-on-focus-out
    (apply #'read-string args)))

(defun my/read-number-cancel-on-focus-out (&rest args)
  "Call `read-number' with ARGS and cancel it after selecting another window."
  (my/with-minibuffer-cancel-on-focus-out
    (apply #'read-number args)))

(provide 'minibuffer-focus.init)

;;; minibuffer-focus.init.el ends here
