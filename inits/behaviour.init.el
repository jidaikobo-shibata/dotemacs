;;; behaviour.init.el --- init for behaviour-mode
;;; Commentary:
;; provide behaviour.
;;; Code:

;; yes/noをy/nへ
(fset 'yes-or-no-p 'y-or-n-p)

;; 起動画面を抑止
(setq inhibit-startup-message t)

;; スクラッチメッセージを抑止
(setq initial-scratch-message nil)

;; 警告音とフラッシュを無効
(setq ring-bell-function 'ignore)

;; Emacs終了時に確認をする
(setq confirm-kill-emacs 'y-or-n-p)

;; バックアップ・自動保存を無効
(setq make-backup-files nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq delete-auto-save-files t)

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
