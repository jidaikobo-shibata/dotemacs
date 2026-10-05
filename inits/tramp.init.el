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
(setq tramp-default-method "scp")

;; ヘテムルのSSH環境でunameがない場合、LinuxとしてOS確認を補完する。
;; TRAMP 2.7.3の確認コマンドに限定し、他のコマンドのエラーはそのまま扱う。
(defvar my/tramp-ssh-config-file (expand-file-name "~/.ssh/config")
  "SSH configuration used to resolve aliases for the Heteml fallback.")

(defun my/tramp-heteml-host-p (host)
  "Return non-nil when HOST or its SSH HostName belongs to Heteml."
  (let ((case-fold-search t)
        (default-directory temporary-file-directory))
    (or (string-match-p "\\.heteml\\.net\\.?\\'" host)
        (and (file-readable-p my/tramp-ssh-config-file)
             (executable-find "ssh")
             (with-temp-buffer
               ;; -Gは接続せず、IncludeやHostワイルドカードも解釈する。
               (and (eq 0 (call-process "ssh" nil t nil "-G" "-F"
                                        my/tramp-ssh-config-file "--" host))
                    (progn
                      (goto-char (point-min))
                      (re-search-forward
                       "^hostname [^ \n]+\\.heteml\\.net\\.?$" nil t))))))))

(defun my/tramp-uname-fallback-args (args)
  "Adjust TRAMP command ARGS for restricted Heteml SSH hosts."
  (let ((vec (car args))
        (command (cadr args)))
    (if (and (member (tramp-file-name-method vec) '("ssh" "scp"))
             (equal command "echo \\\"`uname -sr`\\\"")
             (my/tramp-heteml-host-p (tramp-file-name-host vec)))
        (cons vec
              (cons (concat "echo \\\"`if command -v uname >/dev/null 2>&1; "
                            "then uname -sr; "
                            "else printf 'Linux (uname unavailable)'; fi`\\\"")
                    (cddr args)))
      args)))

(with-eval-after-load 'tramp-sh
  (advice-add 'tramp-send-command-and-read :filter-args
              #'my/tramp-uname-fallback-args))

;;; ------------------------------------------------------------
;;; provides

(provide 'tramp.init)

;;; tramp.init.el ends here
