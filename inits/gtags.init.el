;;; gtags.init.el
;;; Commentary:
;; provide gtags.
;;; Code:

(require 'gtags)
(with-eval-after-load 'gtags
  (setq gtags-global-command "/home/jdkb/bin/global")
  (setq gtags-auto-update t)
  (define-key gtags-mode-map (kbd "M-.") 'gtags-find-tag)
  (define-key gtags-mode-map (kbd "M->") 'gtags-find-rtag) ; M-shift-.
  (define-key gtags-mode-map (kbd "M-s-.") 'gtags-parse-current-file) ; M-super-.
  (define-key gtags-mode-map (kbd "M-,") 'gtags-pop-stack))

;;; ------------------------------------------------------------
;;; gtags-parse-fileをカレントバッファに対して行う

(defun gtags-parse-current-file ()
  "Show the list of tags in the current buffer's file."
  (interactive)
  (if (buffer-file-name)
      (let ((current-file (buffer-file-name)))
        (if (file-regular-p current-file)
            (let ((tagname (expand-file-name current-file)))
              (gtags-push-context)
              (gtags-goto-tag tagname "f"))
          (message "Current file is not a regular file.")))
    (message "Current buffer is not visiting a file.")))

;;; ------------------------------------------------------------
;;; provides

(provide 'gtags.init)

;;; gtags.init.el ends here
