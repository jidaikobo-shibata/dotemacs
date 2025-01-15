;;; elisp.init.el --- init for elisp-mode
;;; Commentary:
;; provide elisp.
;;; Code:

;;; ------------------------------------------------------------
;;;; auto-async-byte-compile
;; 保存時にelcの生成がうまく行かない。
;; M-: (enable-auto-async-byte-compile-mode)
;; のあと保存するとうまく行く
;; thx http://www.emacswiki.org/emacs/auto-async-byte-compile.el
;; (require 'auto-async-byte-compile)
;; (setq auto-async-byte-compile-exclude-files-regexp "jidaikobo.init.el\\|storage.el")
;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;; ------------------------------------------------------------
;;; s+RETでeval-bufferかeval-region
(global-set-key (kbd "<s-return>")
                (lambda () (interactive)
                  (if (region-active-p)
                      (eval-region (region-beginning) (region-end))
                    (eval-buffer))
                  (message "eval done.")))

;;; ------------------------------------------------------------
;;; *Messages*バッファを開く

(defun display-messages-buffer ()
  "Split the window and display the *Messages* buffer."
  (interactive)
  (let ((messages-window (split-window-below)))
    (set-window-buffer messages-window "*Messages*")))
(global-set-key (kbd "M-s-m") 'display-messages-buffer)

;;; ------------------------------------------------------------
;;; *Messages*バッファを自動スクロール
;; thx http://stackoverflow.com/questions/4682033/in-emacs-can-i-set-up-the-messages-buffer-so-that-it-tails
(defun modi/messages-auto-tail (&rest _)
  "Make *Messages* buffer auto-scroll to the end after each message."
  (let* ((buf-name "*Messages*")
         (buf (get-buffer-create buf-name)))
    (when (not (string= buf-name (buffer-name)))
      (dolist (win (get-buffer-window-list buf-name nil :all-frames))
        (with-selected-window win
          (goto-char (point-max))))
      (with-current-buffer buf
        (goto-char (point-max))))))
(advice-add 'message :after #'modi/messages-auto-tail)

;;; ------------------------------------------------------------
;;; provides

(provide 'elisp.init)

;;; elisp.init.el ends here
