;;; anything.init.el --- init for anything-mode
;;; Commentary:
;; provide anything.init.
;;; Code:

(require 'anything)
(require 'anything-config)
(require 'anything-migemo)
(setq anything-use-migemo t)

;; key binds
(define-key anything-map [escape] 'anything-keyboard-quit)
(define-key anything-map (kbd "C-;") 'anything-keyboard-quit)
(define-key anything-map (kbd "<tab>") 'anything-select-action)

;; Anythingのプロンプトがある状態でほかのウィンドウに移動したらanything-keyboard-quitする

(defvar my-last-selected-buffer nil
  "The buffer that was selected before the current window change.")

(defun my-window-selection-change (frame)
  "Handle anything-keyboard-quit when switching from minibuffer."
  (let ((current-buffer (buffer-name (window-buffer (selected-window)))))
    (when (and my-last-selected-buffer
               (stringp (buffer-name my-last-selected-buffer))
               (string= (string-trim (buffer-name my-last-selected-buffer)) "*Minibuf-1*")
               (anything-window))
      (setq my-last-selected-buffer nil)
      (when (fboundp 'anything-keyboard-quit)
        (anything-keyboard-quit)))
    (setq my-last-selected-buffer (or (window-buffer (selected-window)) nil))))

(add-hook 'window-selection-change-functions #'my-window-selection-change)

;; Anythingでファイルを開く方法をFind file as rootにしたときにsudoで開くように
(setq anything-su-or-sudo "sudo")

;; Anythingのショートカットをoff
(setq anything-enable-shortcuts nil)

;; Anythingの画面更新を早く
(setq anything-input-idle-delay 0.1)

;; Anythingの表示されていない候補を遅延対象にする
(setq anything-quick-update t)

;; 編集対象でないバッファを除外(必要な場合、switch-to-buffer `C-x b`)
(setq anything-c-boring-buffer-regexp
      (rx
       (or
        ;; start with space / asterisk / plus
        (group bos " ")
        (group bos "*")
        (group bos "+"))))

;; gtags-find-tag（M-.）すると、Anythingが開くように
;; gtags.init.elも参照

(require 'gtags)
(require 'anything-gtags)

;; isearchをanything-occurで上書き

(global-set-key (kbd "C-s") 'anything-occur)

;; ------------------------------------------------------------
;;; Anything - buffers

;; 自分好みのバッファ選択。anythingの情報源にも - focus-on-editable-buffers
(add-to-list 'load-path "~/.emacs.d/elisp/focus-on-editable-buffers")

(setq-default foeb/is-use-advice-delete-window t)
(setq-default foeb/is-use-anything-execute-persistent-action t)
(setq-default foeb/non-ignore-buffers
              (rx
               (or
                "*scratch*" "*grep*" "*eww*")))
(require 'focus-on-editable-buffers)
(require 'anything-focus-on-editable-buffers)

(define-key anything-map (kbd "C-d") 'foeb/anything-execute-persistent-kill)
(define-key anything-map (kbd "s-w") 'foeb/anything-execute-persistent-kill)
(define-key anything-map (kbd "M-s-<left>") 'anything-previous-line)
(define-key anything-map (kbd "M-s-<right>") 'anything-next-line)

;;; ------------------------------------------------------------
;; alist-anything-for-files
(defvar alist-anything-for-files '())

;;; ------------------------------------------------------------
;;; ~/.ssh/configを情報源として、tramp接続
;; thx rubikitch

(defvar anything-c-source-my-hosts
  '((name . "SSH hosts")
    (candidates . anything-c-source-my-hosts-candidates)
    (type . file)
    (action . find-file)))

(defun anything-c-source-my-hosts-candidates ()
  "Tramp candidates."
  (let ((source (split-string
                 (with-temp-buffer
;                   (insert-file-contents "/Users/shiba/.ssh/config")
                   (insert-file-contents "~/.ssh/config")
                   (buffer-string))
                 "\n"))
        (hosts (list)))
    (dolist (host source)
      (when (string-match "[H\\|h]ost +\\(.+?\\)$" host)
        (setq host (string-trim (substring host (match-beginning 1) (match-end 2))))
        (unless (string= host "*")
          (add-to-list
           'hosts
           (concat "/" tramp-default-method ":" host ":") t))))
    hosts))

;;; ------------------------------------------------------------
;;; ~/.ftp/configを情報源として、ftp接続

(defvar anything-c-source-my-ftp-hosts
  '((name . "FTP hosts")
    (candidates . anything-c-source-my-ftp-hosts-candidates)
    (type . file)
    (action . (("FTP" . anything-tramp-ftp-open)))))

(defun anything-c-source-my-ftp-hosts-candidates ()
  "Tramp candidates."
  (let (alias
        path
        password
        (source (split-string
                 (with-temp-buffer
                   (insert-file-contents "~/.ftp/config")
                   (buffer-string))
                 "Alias"))
        (hosts (list)))
    (dolist (lines source)
      (dolist (line (split-string lines "\n"))
        (cond ((string-match "[P\\|p]ath +\\(.+?\\)$" line)
               (setq path (string-trim (substring line (match-beginning 1) (match-end 2)))))
              ((string-match "[P\\|p]assword +\\(.+?\\)$" line)
               (setq password (string-trim (substring line (match-beginning 1) (match-end 2)))))
              ((not (string= "" line))
               (setq alias (string-trim (string-trim line))))))
      (when path (add-to-list 'hosts (concat alias "  " path "  " password))))
      hosts))

(defun anything-tramp-ftp-open (str)
  "Tramp FTP open.  STR is path and password."
  (let* ((strs (split-string str "  "))
         (path (car (cdr strs)))
         (password (car (reverse strs))))
    (kill-new password)
    (find-file path)))

;;; ------------------------------------------------------------
;;; diredを情報源

(defvar anything-c-source-my-dired-buffer
  '((name . "Dired Buffers")
    (candidates . (lambda ()
                    (mapcar (lambda (buf)
                              (with-current-buffer buf
                                (cons (or (expand-file-name default-directory) (buffer-name buf)) buf)))
                            (cl-remove-if-not
                             (lambda (buf)
                               (with-current-buffer buf
                                 (eq major-mode 'dired-mode)))
                             (buffer-list)))))
    (action . (("Switch to Dired Buffer" . switch-to-buffer)))))
;;; ------------------------------------------------------------
;;; my-anything-for-files

(add-to-list 'alist-anything-for-files 'foeb/anything-c-source-buffers t)
(add-to-list 'alist-anything-for-files 'anything-c-source-my-dired-buffer t)
(add-to-list 'alist-anything-for-files 'anything-c-source-bookmarks t)
(add-to-list 'alist-anything-for-files 'anything-c-source-my-hosts t)
(add-to-list 'alist-anything-for-files 'anything-c-source-my-ftp-hosts t)
(add-to-list 'alist-anything-for-files 'anything-c-source-recentf t)
;; (add-to-list 'alist-anything-for-files 'anything-c-source-files-in-current-dir t)
;; (add-to-list 'alist-anything-for-files 'anything-c-source-my-file-of-working-dir t)
;; (add-to-list 'alist-anything-for-files 'anything-c-source-buffers-list t)
;; (add-to-list 'alist-anything-for-files 'anything-c-source-emacs-functions-with-abbrevs t)
;; (add-to-list 'alist-anything-for-files 'anything-c-source-emacs-commands t)
;; (add-to-list 'alist-anything-for-files 'anything-c-source-emacs-variables t)
;; (add-to-list 'alist-anything-for-files 'anything-c-source-imenu t)

(defun my-anything-for-files ()
  "Anything command for files and commands."
  (interactive)
  (anything-other-buffer
   alist-anything-for-files
   "*my-anything-for-files*"))
(global-set-key (kbd "C-;") 'my-anything-for-files)
(global-set-key (kbd "M-s-<left>") 'my-anything-for-files)
(global-set-key (kbd "M-s-<right>") 'my-anything-for-files)
;; (global-set-key (kbd "M-s-<left>") 'foeb/anything-for-buffers)
;; (global-set-key (kbd "M-s-<right>") 'foeb/anything-for-buffers)

;;; ------------------------------------------------------------
;;; Anything - Encode and Line folding

(defvar anything-c-source-coding-system
  '((name . "Encode and Line Folding")
    (candidates . (lambda ()
                    '("set UTF-8"
                      "set EUC-JP"
                      "set Shift-JIS"
                      "set ISO-2022-JP"
                      "set LF"
                      "set CR"
                      "set CR+LF")))
    (action ("default" . anything-coding-system))))

(defun anything-coding-system (act)
  "Change Encode and Lin folding.  ACT is what to do."
  (message act)
  (cond ((string= act "set UTF-8")       (set-buffer-file-coding-system 'utf-8))
        ((string= act "set EUC-JP")      (set-buffer-file-coding-system 'euc-jp))
        ((string= act "set Shift-JIS")   (set-buffer-file-coding-system 'shift_jis))
        ((string= act "set ISO-2022-JP") (set-buffer-file-coding-system 'iso-2022-jp))
        ((string= act "set LF")          (set-buffer-file-coding-system 'unix))
        ((string= act "set CR")          (set-buffer-file-coding-system 'mac))
        ((string= act "set CR+LF")       (set-buffer-file-coding-system 'dos)))
  (save-buffer)
  (revert-buffer))

(defun my-anything-for-coding-system ()
  "Anything command for program."
  (interactive)
  (anything-other-buffer
   '(anything-c-source-coding-system)
   "*my-anything-c-source-coding-system*"))
(global-set-key (kbd "C-^") 'my-anything-for-coding-system)

;;; ------------------------------------------------------------
;;; provides

(provide 'anything.init)

;;; anything.init.el ends here
