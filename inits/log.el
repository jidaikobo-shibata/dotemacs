;;; log.el
;;; Commentary:
;; provide my-log.
;;; Code:

(defun my-log (text)
  "ログメッセージをファイルに追記する"
  (let ((logfile (expand-file-name "~/Desktop/emacs-log.txt")))
    (with-temp-buffer
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] ")) ; タイムスタンプ
      (insert text)
      (insert "\n")
      (write-region (point-min) (point-max) logfile 'append 'silent))))

;;=====================
;; isearchモード中に打鍵したキーを詳細にログに記録する
(defun my-log-isearch-keys ()
  "isearchモード中に打鍵したキーを詳細にログに記録する"
  (when (and (bound-and-true-p isearch-mode)
             (this-command-keys-vector))
    (my-log
     (format "キー入力: %s | コマンド: %s | イベント型: %s"
             (key-description (this-command-keys-vector))
             this-command
             (event-basic-type last-command-event)))))

; フックをかける
(add-hook 'post-command-hook #'my-log-isearch-keys)
; フックをはずす
;; (remove-hook 'post-command-hook #'my-log-isearch-keys)

;;; log.el ends here
