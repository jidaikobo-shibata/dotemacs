;;; tramp.init.el --- init for tramp
;;; Commentary:
;; provide tramp.init.
;;; Code:

(require 'ange-ftp)
(require 'tramp)

;; TRAMPではバックアップとロックファイルをリモート側に作らない
(defun my/backup-enable-for-local-file-p (file)
  "Return non-nil when FILE is local and eligible for backup."
  (and (not (file-remote-p file))
       (normal-backup-enable-predicate file)))

(setq backup-enable-predicate #'my/backup-enable-for-local-file-p)
(setq remote-file-name-inhibit-locks t)

;; TRAMPの自動保存ファイルはローカルの専用ディレクトリに置く
(setq tramp-auto-save-directory my/auto-save-directory)

;; TRAMP ファイルでは trash を使わない
(add-hook 'dired-mode-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local delete-by-moving-to-trash nil))))

;; パッシブモードで接続しようとするとエラーになるようなのでnil
(setq-default ange-ftp-try-passive-mode nil)

;; 接続方法
;; (setq tramp-default-method "scp")
;; (setq tramp-methods (assq-delete-all "scp" tramp-methods))
(setq tramp-default-method "ssh")

;;; ------------------------------------------------------------
;;; provides

(provide 'tramp.init)

;;; tramp.init.el ends here
