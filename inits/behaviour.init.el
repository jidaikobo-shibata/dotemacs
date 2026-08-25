;;; behaviour.init.el --- init for behaviour-mode
;;; Commentary:
;; provide behaviour.
;;; Code:

;; yes/noをy/nへ
(fset 'yes-or-no-p 'y-or-n-p)

;; y/n確認中はミニバッファではなく単一キー入力で待つ
(setq y-or-n-p-use-read-key t)

;; 起動画面を抑止
(setq inhibit-startup-message t)

;; スクラッチメッセージを抑止
(setq initial-scratch-message nil)

;; 警告音とフラッシュを無効
(setq ring-bell-function 'ignore)

;; Emacs終了時に確認をする
(setq confirm-kill-emacs 'y-or-n-p)

;; バックアップ・自動保存を ~/.emacs.d/.tmp/ 配下へ集約
(defvar my/tmp-directory
  (expand-file-name ".tmp/" user-emacs-directory))
(defvar my/backup-directory
  (expand-file-name "backups/" my/tmp-directory))
(defvar my/auto-save-directory
  (expand-file-name "auto-save/" my/tmp-directory))
(defvar my/auto-save-list-directory
  (expand-file-name "auto-save-list/" my/tmp-directory))
(defvar my/junk-directory
  (expand-file-name "junk/" my/tmp-directory))
(defvar my/tmp-retention-days 30
  "Number of days to retain files in managed temporary directories.")
(defvar my/tmp-managed-directories
  (list my/backup-directory
        my/auto-save-directory
        my/auto-save-list-directory
        my/junk-directory)
  "Temporary directories managed by `my/tmp-garbage-collect'.")
(defvar my/tmp-garbage-collection-timer nil
  "Idle timer used to clean managed temporary directories.")

(defun my/ensure-private-directory (directory)
  "Create DIRECTORY when necessary and restrict it to the current user."
  (unless (file-directory-p directory)
    (make-directory directory t))
  (set-file-modes directory #o700))

(my/ensure-private-directory my/tmp-directory)
(dolist (directory my/tmp-managed-directories)
  (my/ensure-private-directory directory))

(defun my/tmp-garbage-collect ()
  "Delete old regular files from managed temporary directories.
Files older than `my/tmp-retention-days' are deleted.  Symbolic links,
subdirectories, and files outside `my/tmp-managed-directories' are ignored."
  (interactive)
  (let ((cutoff (time-subtract
                 (current-time)
                 (days-to-time my/tmp-retention-days)))
        (deleted 0))
    (dolist (directory my/tmp-managed-directories)
      (when (file-directory-p directory)
        (dolist (file (directory-files
                       directory t directory-files-no-dot-files-regexp t))
          (when (and (not (file-symlink-p file))
                     (file-regular-p file)
                     (time-less-p
                      (file-attribute-modification-time
                       (file-attributes file 'string))
                      cutoff))
            (condition-case err
                (progn
                  (delete-file file)
                  (setq deleted (1+ deleted)))
              (file-error
               (message "Cannot delete temporary file %s: %s"
                        file (error-message-string err))))))))
    (when (or (called-interactively-p 'interactive) (> deleted 0))
      (message "Deleted %d old temporary file(s)" deleted))
    deleted))

(when (timerp my/tmp-garbage-collection-timer)
  (cancel-timer my/tmp-garbage-collection-timer))
(setq my/tmp-garbage-collection-timer
      (run-with-idle-timer 10 nil #'my/tmp-garbage-collect))

(setq make-backup-files t)
(setq backup-directory-alist `(("." . ,my/backup-directory)))
(setq auto-save-list-file-prefix
      (expand-file-name ".saves-" my/auto-save-list-directory))
(setq auto-save-file-name-transforms
      `((".*" ,my/auto-save-directory t)))
(setq create-lockfiles t)
(setq auto-save-default t)
(setq delete-auto-save-files t)

;; rootパスワードを ~/.authinfo に保存しない
(setq auth-source-save-behavior nil)

;; ミニバッファ履歴を保存
(savehist-mode 1)

;; キーストロークのミニバッファへの表示を早く
(setq echo-keystrokes 0.1)

;; ミニバッファのプロンプトにカーソルが入らないように
;; reference | http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt
                  face minibuffer-prompt))

;; Helpバッファは、ウィンドウを分割せず、常に選択する
(setq help-window-select t)
(add-to-list 'same-window-buffer-names "*Help*")

;; grepバッファは、ウィンドウを分割しない
(add-to-list 'same-window-buffer-names "*grep*")

;; ほとんどの場合、window分割はしない
(add-to-list 'same-window-regexps "^[a-zA-Z0-9_ -]+")

;; 複数フレームを開かないようにする
(setq-default ns-pop-up-frames nil)

;; 機能の有効化
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'delete-region 'disabled nil)

;;; ------------------------------------------------------------
;;; provides

(provide 'behaviour.init)

;;; behaviour.init.el ends here
