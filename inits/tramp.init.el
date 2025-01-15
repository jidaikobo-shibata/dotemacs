;;; tramp.init.el --- init for tramp
;;; Commentary:
;; provide tramp.init.
;;; Code:

(require 'ange-ftp)
(require 'tramp)

;; TRAMPでは自動バックアップしない
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; パッシブモードで接続しようとするとエラーになるようなのでnil
(setq-default ange-ftp-try-passive-mode nil)

;; scpで接続
(setq tramp-default-method "scp")

;;; ------------------------------------------------------------
;;; provides

(provide 'tramp.init)

;;; tramp.init.el ends here
