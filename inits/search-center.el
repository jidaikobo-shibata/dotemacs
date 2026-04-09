;;; search-center.el --- search center. -*- lexical-binding: t; -*-
;; Copyright (C) 2016 by jidaikobo-shibata
;; Author: jidaikobo-shibata
;; URL: https://github.com/jidaikobo-shibata/dotemacs

;;; Commentary:
;; minor mode for search and replace.
;; 検索置換のためのマイナーモード。

;;; Todo:
;; 補助的依存状態のforeign-regexpとsmartrepがなくても動くようにする
;; http://qiita.com/mkit0031/items/85d66d08cd51c1c9e8a2 を参考に*grep*バッファの名称を変更したほうが便利か？ 現在は*grep*バッファを消すようにしている。
;; ;;; .emacs.elの末尾に追加
;; ;; grep実施時に複数のバッファで開けるよう設定
;; (defadvice grep (after my-grep activate)
;;   (let ((grep-buffer (get-buffer "*grep*")))
;;     (set-buffer grep-buffer)
;;     (rename-buffer (concat "*grep*" "<\"" (ad-get-arg 0) "\">" ) t)))

;; マルチファイル検索置換
;; M-x find-dired
;; Run find in directory: /path/to/dir/
;; Run find (with args): -name "*php"
;; スペースキーでファイルを選択する or M-t で全選択
;; Qで検索文字列と置換文字列を入力
;; yで置換Yで問答無用で全置換
;; C-x s
;; ! で全保存

;;; Usage:
;; | s-f | show search/replace windows        | 検索置換窓の表示
;; | s-F | toggle regexp or not               | 正規表現モードトグル
;; | s-e | set region to search               | 検索文字にセット
;; | s-E | set region to replace              | 置換文字にセット
;; | s-g | search next                        | 次を検索
;; | s-G | Search previous                    | 前を検索
;; | s-l | replace and search next            | 置換して次を検索
;; | s-r | replace current region             | 選択範囲を置換
;; | s-R | replace all                        | バッファ内全てを置換
;; | s-M | interactive rgrep by search string | 対話的にrgrep
;; | s-h | select target window               | 対象ウィンドウを選択
;;
;; sc/is-use-super nil
;; sc/is-use-default-key-binds t
;; sc/is-sync-isearch t

;;; Code:

;;; ------------------------------------------------------------
;;; defgroup

(defgroup sc/search-center nil
  "Provides search center."
  :group 'Convenience)

(defcustom sc/is-use-super nil
  "*Mac-like behavior."
  :group 'sc/search-center
  :type 'boolean)

(defcustom sc/is-use-default-key-binds t
  "*Emacs behavior."
  :group 'sc/search-center
  :type 'boolean)

(defcustom sc/is-sync-isearch t
  "*Sync with isearch."
  :group 'sc/search-center
  :type 'boolean)

(defcustom sc/is-use-mozc-search-bridge t
  "*Use `C-s <henkan>' to hand off Japanese search text to search-center."
  :group 'sc/search-center
  :type 'boolean)

(defcustom sc/history-max 40
  "*Maximum number of search/replace history entries to keep."
  :group 'sc/search-center
  :type 'integer)

;;; ------------------------------------------------------------
;;; defvar

(defvar sc/target-window (selected-window))
(defvar sc/target-buffer (current-buffer))
(defvar sc/search-str-buffer "*search string*")
(defvar sc/replace-str-buffer "*replace string*")
(defvar sc/previous-searched-str)
(defvar sc/previous-replaced-str)
(defvar sc/previous-searced-direction nil)
(defvar sc/previous-direction)
(defvar sc/modeline-saved nil)
(defvar sc/modeline-background)
(defvar sc/modeline-foreground)
(defvar sc/re-modeline-background "orange")
(defvar sc/re-modeline-foreground "black")
(defvar sc/is-foreign-regexp nil)
(defvar sc/is-renaming-rgrep-buffer nil)
(defvar sc/is-use-timer-for-modeline t)
(defvar sc/is-use-timer-for-buffers t)
(defvar sc/split-direction "horizontal")
(defvar search-center-mode-map (make-keymap))
(defvar search-center-re-mode-map (make-keymap))
(defvar sc/mozc-search-prompt "Mozc search: ")
(defvar sc/mozc-search-history nil)
(defvar sc/mozc-search--resume-isearch nil)
(defvar sc/mozc-search--resume-string nil)
(defvar sc/mozc-search--resume-forward t)
(defvar sc/mozc-search--action nil)
(defvar sc/history nil)
(defvar sc/history-index nil)

;;; ------------------------------------------------------------
;;; dependencies

(require 'subr-x)
(require 'cl-lib)

;; Emacs 26でforeign-regexpがエラーを出すので抑止
;; (defvaralias 'lazy-highlight-face 'isearch-lazy-highlight)

;; foreign-regexp
(when (require 'foreign-regexp nil t)
  (eval-after-load "foreign-regexp"
    (progn
      (custom-set-variables
       '(foreign-regexp/regexp-type 'perl)
       '(reb-re-syntax 'foreign-regexp))
      (setq sc/is-foreign-regexp t)))
  (declare-function foreign-regexp/search/forward "foreign-regexp")
  (declare-function foreign-regexp/search/backward "foreign-regexp")
  (declare-function foreign-regexp/replace/perform-replace "foreign-regexp"))

;;; ------------------------------------------------------------
;;; minor-modes

;; minor-mode for sc/
;; 検索置換用のマイナーモード
(define-minor-mode search-center-mode
  "Provide search/replace environment."
  :init-value
  nil
  :lighter
  " SC"
  :keymap
  search-center-mode-map)

;; minor-mode for regular expression
;; 正規表現のマイナーモード
(define-minor-mode search-center-re-mode
  "Provide regular expression search/replace environment."
  :init-value
  nil
  :lighter
  " re-SC"
  :keymap
  search-center-re-mode-map
  :after-hook
  (sc/toggle-mode-line))

;; Toggle Mode line
;; モードラインのトグル
(defun sc/toggle-mode-line (&rest _args)
  "Sync the mode line color with `search-center-re-mode'."
  (interactive)
  (let* ((event-obj (car _args))
         (target-window
          (cond
           ((window-live-p event-obj) event-obj)
           ((framep event-obj) (frame-selected-window event-obj))
           (t (selected-window))))
         (target-buffer (and (window-live-p target-window)
                             (window-buffer target-window)))
         (is-re (and (buffer-live-p target-buffer)
                     (buffer-local-value 'search-center-re-mode target-buffer))))
    ;; keep current color of mode line at the first time
    ;; 最初に現在のモードラインを配色を保存しておく
    (unless sc/modeline-saved
      (setq sc/modeline-background (face-attribute 'mode-line :background)
            sc/modeline-foreground (face-attribute 'mode-line :foreground)
            sc/modeline-saved t))
    (cond
     ;; 正規表現モード
     (is-re
      (set-face-attribute 'mode-line nil
                          :foreground sc/re-modeline-foreground
                          :background sc/re-modeline-background))
     ;; 標準モード
     (t
      (set-face-attribute 'mode-line nil
                          :foreground sc/modeline-foreground
                          :background sc/modeline-background))))
  (force-mode-line-update))

(defun sc/reset-mode-line-cache (&rest _args)
  "Forget cached normal mode-line colors and resync after theme changes."
  (setq sc/modeline-saved nil)
  (sc/toggle-mode-line))

;; event-driven mode line sync
;; モードラインの配色はイベント発火時だけ同期する
(when sc/is-use-timer-for-modeline
  (add-hook 'search-center-re-mode-hook #'sc/toggle-mode-line)
  (when (boundp 'window-selection-change-functions)
    (add-hook 'window-selection-change-functions #'sc/toggle-mode-line))
  (when (boundp 'window-buffer-change-functions)
    (add-hook 'window-buffer-change-functions #'sc/toggle-mode-line))
  (advice-add 'load-theme :after #'sc/reset-mode-line-cache)
  (advice-add 'enable-theme :after #'sc/reset-mode-line-cache)
  (advice-add 'disable-theme :after #'sc/reset-mode-line-cache))

;; event-driven auto close for search/replace windows
;; 検索置換窓は選択が外れたときだけ閉じる
(defun sc/close-string-windows ()
  "Close visible search-center helper windows."
  (let ((search-window (get-buffer-window sc/search-str-buffer))
        (replace-window (get-buffer-window sc/replace-str-buffer)))
    (when (window-live-p search-window)
      (delete-window search-window))
    (when (window-live-p replace-window)
      (delete-window replace-window))))

(defun sc/maybe-close-string-windows (&rest _args)
  "Close helper windows when focus is no longer on them."
  (let ((selected (selected-window))
        (search-window (get-buffer-window sc/search-str-buffer))
        (replace-window (get-buffer-window sc/replace-str-buffer)))
    (when (and (or (window-live-p search-window)
                   (window-live-p replace-window))
               (not (eq selected search-window))
               (not (eq selected replace-window)))
      (sc/close-string-windows))))

(when sc/is-use-timer-for-buffers
  (when (boundp 'window-selection-change-functions)
    (add-hook 'window-selection-change-functions #'sc/maybe-close-string-windows))
  (when (boundp 'window-buffer-change-functions)
    (add-hook 'window-buffer-change-functions #'sc/maybe-close-string-windows)))

;;; ------------------------------------------------------------
;;; hook

;; compatible with isearch command
;; i-search、i-search-backwardコマンドとの同期
;; (when sc/is-sync-isearch
;;   (add-hook 'isearch-update-post-hook 'sc/isearch-update-string)
;;   (defun sc/isearch-update-string ()
;;     (when (and (not (or isearch-regexp isearch-regexp-function))
;;                (eq this-command 'isearch-printing-char))
;;       (unless (get-buffer sc/search-str-buffer) (get-buffer-create sc/search-str-buffer))
;;       (sc/keep-target-buffer)
;;       ;; set strings
;;       ;; 文字列をセット
;;       (with-current-buffer sc/search-str-buffer
;;         (delete-region (point-min) (point-max))
;;         (insert isearch-string))
;;       ;; keep strings
;;       ;; 次回用に文字列を保存
;;       (setq sc/previous-searched-str isearch-string))))

(when sc/is-sync-isearch
  (add-hook 'isearch-update-post-hook 'sc/isearch-update-string)
  (defun sc/isearch-update-string ()
    (when (not (or isearch-regexp isearch-regexp-function))
  (unless (get-buffer sc/search-str-buffer)
    (get-buffer-create sc/search-str-buffer))
  (sc/keep-target-buffer)
  (with-current-buffer sc/search-str-buffer
    (delete-region (point-min) (point-max))
    (insert isearch-string))
  (setq sc/previous-searched-str isearch-string))))

(defun sc/set-search-string (strings)
  "Store STRINGS into the search-center search buffer and history."
  (unless (get-buffer sc/search-str-buffer)
    (get-buffer-create sc/search-str-buffer))
  (with-current-buffer sc/search-str-buffer
    (delete-region (point-min) (point-max))
    (insert strings))
  (setq sc/previous-searched-str strings))

(defun sc/set-replace-string (strings)
  "Store STRINGS into the search-center replace buffer and history."
  (unless (get-buffer sc/replace-str-buffer)
    (get-buffer-create sc/replace-str-buffer))
  (with-current-buffer sc/replace-str-buffer
    (delete-region (point-min) (point-max))
    (insert (or strings "")))
  (setq sc/previous-replaced-str strings))

(defun sc/set-regexp-mode (enabled)
  "Apply search-center regexp mode ENABLED to live related buffers."
  (let ((state (if enabled 1 -1)))
    (dolist (buffer (list (and (buffer-live-p sc/target-buffer) sc/target-buffer)
                          (get-buffer sc/search-str-buffer)
                          (get-buffer sc/replace-str-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (search-center-re-mode state))))))

(defun sc/make-history-entry (search-str replace-str regexp)
  "Create a history entry from SEARCH-STR, REPLACE-STR and REGEXP."
  (list :search search-str
        :replace replace-str
        :regexp regexp))

(defun sc/history-entry-equal-p (left right)
  "Return non-nil when history entries LEFT and RIGHT are equivalent."
  (equal left right))

(defun sc/push-history-entry (search-str replace-str regexp)
  "Push SEARCH-STR, REPLACE-STR and REGEXP into search-center history."
  (let ((entry (sc/make-history-entry search-str replace-str regexp)))
    (unless (sc/history-entry-equal-p entry (car sc/history))
      (push entry sc/history)
      (when (> (length sc/history) sc/history-max)
        (setcdr (nthcdr (1- sc/history-max) sc/history) nil)))
    (setq sc/history-index 0)))

(defun sc/apply-history-entry (entry)
  "Apply search-center history ENTRY to the helper buffers."
  (let ((search-str (plist-get entry :search))
        (replace-str (plist-get entry :replace))
        (regexp (plist-get entry :regexp)))
    (sc/set-search-string search-str)
    (sc/set-replace-string replace-str)
    (sc/set-regexp-mode regexp)
    (message "history: search=%s replace=%s%s"
             search-str
             (or replace-str "")
             (if regexp " [re]" ""))))

(defun sc/history-prev ()
  "Load the previous search-center history entry."
  (interactive)
  (unless sc/history
    (user-error "Error: history is empty"))
  (setq sc/history-index
        (min (1+ (or sc/history-index -1))
             (1- (length sc/history))))
  (sc/apply-history-entry (nth sc/history-index sc/history)))

(defun sc/history-next ()
  "Load the next (newer) search-center history entry."
  (interactive)
  (unless sc/history
    (user-error "Error: history is empty"))
  (setq sc/history-index (max 0 (1- (or sc/history-index 0))))
  (sc/apply-history-entry (nth sc/history-index sc/history)))

(defun sc/prepare-search-from-string (strings)
  "Prepare search-center state from STRINGS without moving point."
  (sc/set-search-string strings)
  (setq sc/previous-searced-direction nil)
  (when (window-live-p sc/target-window)
    (select-window sc/target-window))
  (when (buffer-live-p sc/target-buffer)
    (set-buffer sc/target-buffer))
  (deactivate-mark)
  (message "search ready: %s" strings))

(defun sc/mozc-search-minibuffer-setup ()
  "Prepare the temporary minibuffer for Mozc search text."
  (local-set-key (kbd "<muhenkan>") #'sc/mozc-search-cancel-to-isearch)
  (local-set-key (kbd "s-g") #'sc/mozc-search-finish-next)
  (local-set-key (kbd "s-G") #'sc/mozc-search-finish-prev)
  (local-set-key (kbd "<henkan>")
                 (lambda ()
                   (interactive)
                   (sc/mozc-search-enable-input-method)))
  (sc/mozc-search-enable-input-method))

(defun sc/mozc-search-enable-input-method ()
  "Enable Mozc reliably in the temporary search minibuffer."
  (when default-input-method
    (activate-input-method default-input-method)
    ;; `activate-input-method' alone sometimes leaves Mozc half-enabled.
    (when (and (equal current-input-method default-input-method)
               (null input-method-function))
      (setq current-input-method nil
            current-input-method-title nil
            describe-current-input-method-function nil)
      (activate-input-method default-input-method))
    (when (and (equal current-input-method default-input-method)
               (boundp 'mozc-mode)
               (not mozc-mode))
      (ignore-errors (mozc-mode 1)))))

(defun sc/mozc-search-cancel-to-isearch ()
  "Abort Japanese search input and resume the previous isearch."
  (interactive)
  (setq sc/mozc-search--resume-isearch t)
  (abort-recursive-edit))

(defun sc/mozc-search-finish-next ()
  "Finish Mozc search input and search next with search-center."
  (interactive)
  (setq sc/mozc-search--action 'next)
  (exit-minibuffer))

(defun sc/mozc-search-finish-prev ()
  "Finish Mozc search input and search previous with search-center."
  (interactive)
  (setq sc/mozc-search--action 'prev)
  (exit-minibuffer))

(defun sc/mozc-search-read-string ()
  "Read Japanese search text through the minibuffer."
  (setq sc/mozc-search--resume-isearch nil)
  (setq sc/mozc-search--action nil)
  (let ((enable-recursive-minibuffers t))
    (condition-case err
        (let ((ret
               (minibuffer-with-setup-hook #'sc/mozc-search-minibuffer-setup
                 (read-from-minibuffer sc/mozc-search-prompt
                                       nil
                                       nil
                                       nil
                                       'sc/mozc-search-history))))
          ret)
      (quit
       nil)
      (error
       (message "Mozc search canceled: %s" (error-message-string err))
       nil))))

(defun sc/resume-isearch ()
  "Resume a plain isearch after leaving the Mozc search bridge."
  (when sc/mozc-search--resume-isearch
    (setq sc/mozc-search--resume-isearch nil)
    (isearch-mode sc/mozc-search--resume-forward nil nil nil)
    (when (and (stringp sc/mozc-search--resume-string)
               (> (length sc/mozc-search--resume-string) 0))
      (isearch-yank-string sc/mozc-search--resume-string))))

(defun sc/bridge-isearch-to-mozc-search ()
  "Abort isearch, read Japanese text, then hand off to search-center."
  (interactive)
  (unless isearch-mode
    (user-error "This command is only available during isearch"))
  (setq sc/mozc-search--resume-string isearch-string
        sc/mozc-search--resume-forward isearch-forward)
  (sc/keep-target-buffer)
  (isearch-done)
  (isearch-clean-overlays)
  (setq unread-command-events nil
        overriding-terminal-local-map nil)
  (let ((last-command-event nil)
        (last-input-event nil)
        (strings (sc/mozc-search-read-string)))
    (if sc/mozc-search--resume-isearch
        (sc/resume-isearch)
      (when (and (stringp strings) (> (length strings) 0))
        (sc/prepare-search-from-string strings)
        (pcase sc/mozc-search--action
          ('next (sc/alias-search-next))
          ('prev (sc/alias-search-prev)))))))

;;; ------------------------------------------------------------
;;; function alias for key-binds

(defun sc/alias-search-next ()
  "Alias."
  (interactive)
  (if (eq search-center-re-mode nil)
      (sc/search-replace "next")
    (sc/search-replace "re-next")))

(defun sc/alias-search-prev ()
  "Alias."
  (interactive)
  (if (eq search-center-re-mode nil)
      (sc/search-replace "prev")
    (sc/search-replace "re-prev")))

(defun sc/alias-replace-next ()
  "Alias."
  (interactive)
  (if (eq search-center-re-mode nil)
      (sc/search-replace "rep-next")
    (sc/search-replace "re-rep-next")))

(defun sc/alias-replace-here ()
  "Alias."
  (interactive)
  (if (eq search-center-re-mode nil)
      (sc/search-replace "rep-here")
    (sc/search-replace "re-rep-here")))

(defun sc/alias-replace-region ()
  "Alias."
  (interactive)
  (if (eq search-center-re-mode nil)
      (sc/replace-all "")
    (sc/replace-all "re")))

;;; ------------------------------------------------------------
;;; key-binds - s-key Mac like behavior

(when sc/is-use-super
  (global-set-key (kbd "s-f") 'sc/show-windows)
  (global-set-key (kbd "s-F") 'sc/toggle-search-mode)
  (global-set-key (kbd "s-e") (lambda () (interactive) (sc/set-strings "search")))
  (global-set-key (kbd "s-E") (lambda () (interactive) (sc/set-strings "replace")))
  (global-set-key (kbd "C-s-e") 'sc/set-strings-from-kill-ring)
  (global-set-key (kbd "s-g") 'sc/alias-search-next)
  (global-set-key (kbd "s-G") 'sc/alias-search-prev)
  (global-set-key (kbd "s-l") 'sc/alias-replace-next)
  (global-set-key (kbd "s-r") 'sc/alias-replace-here)
  (global-set-key (kbd "s-R") 'sc/alias-replace-region)
  (global-set-key (kbd "s-p") 'sc/history-prev)
  (global-set-key (kbd "s-P") 'sc/history-next)
  (global-set-key (kbd "s-M") 'sc/grep) ;; s-m is used by Mac OS X
  (global-set-key (kbd "s-h") 'sc/select-target-window)
  (global-set-key (kbd "C-g") 'sc/quit-str-windows))

(with-eval-after-load 'isearch
  (when sc/is-use-mozc-search-bridge
    (setq isearch-use-input-method nil)
    (define-key isearch-mode-map (kbd "<henkan>")
      #'sc/bridge-isearch-to-mozc-search)))

;;; ------------------------------------------------------------
;;; Quit

(defun sc/quit-str-windows ()
  "Quit string windows.  Delete string windows via event hooks."
  (interactive)
  (if (or (equal (selected-window) (get-buffer-window sc/search-str-buffer))
          (equal (selected-window) (get-buffer-window sc/replace-str-buffer)))
      (sc/select-target-window)
    (keyboard-quit)))

;;; ------------------------------------------------------------
;;; keep target-buffer

(defun sc/keep-target-buffer ()
  "Keep target buffer."
  (unless (or (equal (selected-window) (get-buffer-window sc/search-str-buffer))
              (equal (selected-window) (get-buffer-window sc/replace-str-buffer)))
    (setq sc/target-window (selected-window)
          sc/target-buffer (current-buffer))))

(defun sc/string-buffer-p (buffer)
  "Return non-nil when BUFFER is a search-center helper buffer."
  (and (buffer-live-p buffer)
       (member (buffer-name buffer)
               (list sc/search-str-buffer sc/replace-str-buffer))))

(defun sc/find-target-window ()
  "Return a live non-helper window to use as the search target."
  (catch 'target-window
    (dolist (window (window-list nil 'no-minibuf))
      (unless (sc/string-buffer-p (window-buffer window))
        (throw 'target-window window)))
    nil))

(defun sc/get-target-buffer ()
  "Return the current target buffer, falling back to another live buffer."
  (cond
   ((and (buffer-live-p sc/target-buffer)
         (not (sc/string-buffer-p sc/target-buffer)))
    sc/target-buffer)
   ((when-let ((target-window (sc/find-target-window)))
     (setq sc/target-window target-window
           sc/target-buffer (window-buffer target-window))))
   (t
    (user-error "Error: no target buffer"))))

(defun sc/select-target-window ()
  "Select the current target window, falling back to another live window."
  (interactive)
  (cond
   ((and (window-live-p sc/target-window)
         (not (sc/string-buffer-p (window-buffer sc/target-window))))
    (select-window sc/target-window))
   ((when-let ((target-window (sc/find-target-window)))
     (setq sc/target-window target-window
           sc/target-buffer (window-buffer target-window))
     (select-window target-window)))
   (t
    (user-error "Error: no target window"))))

;;; ------------------------------------------------------------
;;; toggle re/search mode

(defun sc/toggle-search-mode ()
  "Toggle search mode."
  (interactive)
  (let ((target-buffer nil))
    (unless (get-buffer sc/search-str-buffer) (get-buffer-create sc/search-str-buffer))
    (unless (get-buffer sc/replace-str-buffer) (get-buffer-create sc/replace-str-buffer))
    (sc/keep-target-buffer)
    (setq target-buffer (sc/get-target-buffer))
    (if (eq search-center-re-mode nil)
      ;; turn into regex
      ;; 正規表現モードをオン
        (progn
          (with-current-buffer target-buffer
            (search-center-re-mode t))
          (with-current-buffer sc/search-str-buffer
            (search-center-re-mode t))
          (with-current-buffer sc/replace-str-buffer
            (search-center-re-mode t))
          (message "%s" (concat "turned into RE with " (buffer-name target-buffer))))
      ;; turn off regex
      ;; 正規表現モードをオフ
      (with-current-buffer target-buffer
        (search-center-re-mode -1))
      (with-current-buffer sc/search-str-buffer
        (search-center-re-mode -1))
      (with-current-buffer sc/replace-str-buffer
        (search-center-re-mode -1))
      (message "%s" (concat "turned off RE with " (buffer-name target-buffer))))))

;;; ------------------------------------------------------------
;;; set clip board strings to search buffer
;; クリップボードから検索文字列をセット

(defun sc/set-strings-from-kill-ring ()
  "Set `kill-ring' strings to search buffer."
  (interactive)
  (let* ((target sc/search-str-buffer))

    (unless (get-buffer target) (get-buffer-create target))
    (sc/keep-target-buffer)

    ;; set strings
    ;; 文字列をセット
    (with-current-buffer target
      (delete-region (point-min) (point-max))
      (yank)

      ;; keep strings
      (setq sc/previous-searched-str (buffer-substring-no-properties (point-min) (point-max)))

      (message "%s" (concat "set search strings: " sc/previous-searched-str)))))

;;; ------------------------------------------------------------
;;; set strings to search/replace buffer
;; 検索・置換文字列をセット

(defun sc/set-strings (mode)
  "Set strings to search or replace buffer.  MODE [search|replace]."
  (interactive)
  (let* ((beg (if mark-active (region-beginning)))
         (end (if mark-active (region-end)))
         (strings (if mark-active
                      (buffer-substring-no-properties beg end)
                    (read-string (concat mode " word: ") "")))
         (target (if (string= mode "search")
                     sc/search-str-buffer
                   sc/replace-str-buffer)))

    (unless (get-buffer target) (get-buffer-create target))

    (sc/keep-target-buffer)

    ;; set strings
    ;; 文字列をセット
    (with-current-buffer target
      (delete-region (point-min) (point-max))
      (insert strings)
      (message "%s" (concat "set " mode " strings: " strings)))

    ;; keep strings
    ;; 次回用に文字列を保存
    (if (string= mode "search")
        (setq sc/previous-searched-str strings)
      (setq sc/previous-replaced-str strings))))

;;; ------------------------------------------------------------
;;; get strings from search/replace buffer
;; 検索・置換文字列を取得

(defun sc/get-strings (mode)
  "Get strings from search or replace buffer.  MODE [search|replace]."
  (interactive)
  (let* ((target (if (string= mode "search")
                     sc/search-str-buffer
                   sc/replace-str-buffer))
         ret)

    (unless (get-buffer target) (get-buffer-create target))

    ;; strings from buffer
    (with-current-buffer target
      (setq ret (buffer-substring-no-properties (point-min) (point-max))))

    ;; empty search string should be treated as unset
    ;; 検索文字列では空文字を未設定扱いにする
    (when (and (string= mode "search")
               (stringp ret)
               (string-empty-p ret))
      (setq ret nil))

    ;; ask global
    (if (and (not ret) (string= mode "search"))
        (setq ret (if (boundp 'sc/previous-searched-str) sc/previous-searched-str nil)))

    ;; no search string error
    (when (and (not ret) (string= mode "search"))
      (error "Error: search word is empty"))
    ret))

;;; ------------------------------------------------------------
;;; show search/replace windows
;; 検索・置換窓を表示

(defun sc/show-windows ()
  "Show search/replace window."
  (interactive)
  (let* ((is-search-window-exist (windowp (get-buffer-window sc/search-str-buffer)))
         (is-replace-window-exist (windowp (get-buffer-window sc/replace-str-buffer))))

    (sc/keep-target-buffer)

    ;; search/replace windows cannot exist alone
    ;; どちらかだけ開いていたら、もう片方を閉じる
    (when (and is-search-window-exist (not is-replace-window-exist))
      (delete-window (get-buffer-window sc/search-str-buffer)))
    (when (and is-replace-window-exist (not is-search-window-exist))
      (delete-window (get-buffer-window sc/replace-str-buffer)))

    ;; split direction
    ;; 上下分割か左右分割を選択できるようにする
    (unless (or is-search-window-exist is-replace-window-exist)
      (if (string= sc/split-direction "horizontal")
          (split-window-horizontally)
        (split-window-vertically)))

    ;; prepare search window
    ;; 検索窓を準備
    (unless is-search-window-exist
      (select-window (next-window))
      (switch-to-buffer sc/search-str-buffer)
      (set-window-dedicated-p (selected-window) t) ;専用のバッファにする
      (display-line-numbers-mode -1)
      (search-center-mode t))

    ;; prepare replace window
    ;; 置換窓を用意
    (unless is-replace-window-exist
      (if (string= sc/split-direction "vertical")
          (split-window-horizontally)
        (split-window-vertically))
      (select-window (next-window))
      (switch-to-buffer sc/replace-str-buffer)
      (set-window-dedicated-p (selected-window) t) ;専用のバッファにする
      (display-line-numbers-mode -1)
      (search-center-mode t))

    ;; suitable size
    ;; 上下分割ではあまりスペースを取らないように
    (unless is-search-window-exist
      (if (string= sc/split-direction "vertical")
          (shrink-window 12)
        ;; enlarge frame when there is not enough space
        ;; 左右分割で検索置換窓を出せないくらい狭い時には拡張
        (when (< (frame-width) 110)
          (set-frame-size (selected-frame) (+ (frame-width) 100) (frame-height)))))

    ;; move cursor to search window
    ;; キャレットを検索窓にセットして選択
    (select-window (get-buffer-window sc/search-str-buffer))

    ;; 全選択状態にする
    (goto-char (point-min))
    (set-mark (point)) ;; not into mark-ring
    (goto-char (point-max))))

;;; ------------------------------------------------------------
;;; common function
;; 共通関数

;; replace region
;; 選択範囲の置換用関数
(defun sc/replace-region (search-str replace-str is-re)
  "(string)SEARCH-STR, (string)REPLACE-STR, (bool)IS-RE."
  (when mark-active
    (let ((beg (region-beginning))
          (end (region-end))
          is-replaced)
      (progn
        ;; prepare replace string or replace by foreign-regexp
        (when is-re
          (if sc/is-foreign-regexp
              ;; perform replace by foreign-regexp
              (progn
                (foreign-regexp/replace/perform-replace
                 search-str replace-str nil nil nil nil nil beg end)
                (setq is-replaced t))
            ;; prepare replace string
            (setq replace-str (replace-regexp-in-string
                               search-str
                               replace-str
                               (buffer-substring-no-properties beg end)))))
        ;; perform replace (normal and regexp)
        (unless is-replaced
          (delete-region beg end)
          (insert replace-str))))))

;; generate region and control direction of search
;; 選択範囲の作成用関数
;; 検索方向に応じて、キャレットの位置を適切にする
(defun sc/generate-region (direction len-search-string)
  "(string)DIRECTION, (int)LEN-SEARCH-STRING."
  (let
      (beg
       end)
    (progn
      (cond
       ((string= direction "next")
        (setq beg (- (point) len-search-string))
        (goto-char beg)
        (setq end (+ (point) len-search-string))
        (set-mark (point)) ;; not into mark-ring
        (goto-char end))
       ((string= direction "prev")
        (setq end (+ (point) len-search-string))
        (goto-char end)
        (setq beg (- (point) len-search-string))
        (set-mark (point))
        (goto-char beg))) ;; not into mark-ring
      (setq deactivate-mark nil))))

;; get strings from window
;; 文字列の取得
(defun sc/get-str-from-window (type)
  "Get str from window.  TYPE[search|replace]."
  (interactive)
  (let ((buffer-obj (if (string= type "search")
                        (get-buffer sc/search-str-buffer)
                      (get-buffer sc/replace-str-buffer)))
        (ret ""))

    ;; search or replace
    ;; コンテクストに応じた文字列の取得
    (setq ret (if (buffer-live-p buffer-obj)
                  (save-current-buffer
                    (set-buffer buffer-obj)
                    (buffer-substring-no-properties (point-min) (point-max)))
                nil))

    ;; check global variable
    ;; グローバルな変数にも尋ねる
    (when (and (string= type "search")
               (stringp ret)
               (string-empty-p ret))
      (setq ret nil))

    (if (and (not ret) (string= type "search"))
        (setq ret (if (boundp 'sc/previous-searched-str) sc/previous-searched-str nil)))

    ;; error
    (when (and (not ret) (string= type "search"))
      (error "Error: search word is empty"))

    ret))

;; empty line
;; 空行の検索を特別扱い
(defun sc/replace-empty-line-regex (str)
  "Get str from window.  STR is regex."
  (interactive)
  (if (string= str "^$") "^
" str))

;;; ------------------------------------------------------------
;;; search and replace
;; 検索用バッファの文字列で検索・置換する

;; (declare-function sc/move-region "sc/move-region" ())
(defun sc/search-replace (mode)
  "Do search from other window string.  MODE [next|prev|re-next|re-prev|rep-here|rep-next|rep-prev|re-rep-next|re-rep-prev]."
  (interactive)
  (let* ((search-str "")
         (replace-str "")
         (beg 0)
         (end 0)
         (len-search-string 0)
         (direction (substring mode -4))
         (is-re (if (string= (substring mode 0 3) "re-") t nil))
         (is-next (if (string= direction "next") t nil))
         (is-prev (if (string= direction "prev") t nil))
         (replace-flag (if (>= (length mode) 6) (substring mode 0 6) ""))
         (is-replace (if (or (string= replace-flag "rep-ne")
                             (string= replace-flag "rep-pr")
                             (string= replace-flag "rep-he")
                             (string= replace-flag "re-rep")) t nil))
         (is-replace-here (if (or (string= mode "rep-here")
                                  (string= mode "re-rep-here")) t nil))
         target-str)

    ;; change direction with situation
    ;; 検索方向が変わったら向きを変える
    (when (and mark-active (not (string= sc/previous-searced-direction direction)))
      (exchange-point-and-mark))
    (setq sc/previous-searced-direction direction)

    ;; quit minibuffer
    ;; ミニバッファにいたらまず抜ける
    (when (minibufferp (current-buffer))
      (other-window 1))

    ;; use target window
    ;; 現在のウィンドウが検索・置換編集用ウィンドウだったら、主たるウィンドウに移動する
    (sc/keep-target-buffer)
    (unless (eq (selected-window) sc/target-window)
      (sc/select-target-window))

    ;; get search strings
    ;; 検索用文字列の取得（必須）
    (setq search-str (sc/get-str-from-window "search"))
    (unless search-str
      (setq search-str (if (boundp 'sc/previous-searched-str) sc/previous-searched-str nil)))
    (unless search-str (error "Error: search word is empty"))

    ;; empty line
    ;; 空行表現（^$）を特別扱い
    (when is-re (setq search-str (sc/replace-empty-line-regex search-str)))

    ;; get replace strings
    ;; 置換用文字列の取得（置換時必須）
    (setq replace-str (sc/get-str-from-window "replace"))
    (unless replace-str
      (setq replace-str (if (boundp 'sc/previous-replaced-str) sc/previous-replaced-str nil)))
    (if (and is-replace (not replace-str)) (error "Error: replace word is empty"))

    ;; keep history entry before moving point or replacing text.
    (sc/push-history-entry search-str replace-str is-re)

    ;; main process
    ;; 処理本体
    (cl-labels
        ((move-region ()
           ;; 検索文字列にキャレットを移動しリージョンにする。
           (let (found)
             (setq found
                   (cond
                    ((and (not is-re) is-next)
                     (search-forward search-str nil t))
                    ((and (not is-re) is-prev)
                     (search-backward search-str nil t))
                    ((and is-re is-next)
                     (if sc/is-foreign-regexp
                         (foreign-regexp/search/forward search-str)
                       (re-search-forward search-str nil t)))
                    ((and is-re is-prev)
                     (if sc/is-foreign-regexp
                         (foreign-regexp/search/backward search-str)
                       (re-search-backward search-str nil t)))))
             (if (not found)
                 (progn
                   (message "Not found: %s" search-str)
                   (deactivate-mark)
                   (keyboard-quit))
               ;; ヒットした文字長を取得してリージョン作成する。
               (if is-re
                   (setq len-search-string (length (match-string-no-properties 0)))
                 (setq len-search-string (length search-str)))
               (sc/generate-region (if is-next "next" "prev") len-search-string)))))
      (cond
       ;; rep-nextやrep-prevは、いまの選択範囲を置換してから次に行くようにする
       ((or (and is-replace is-next) (and is-replace is-prev))
        (progn (when mark-active (sc/replace-region search-str replace-str is-re))
               (move-region)))
       ;; その場を置換
       (is-replace-here (sc/replace-region search-str replace-str is-re))
       ;; 通常はただのキャレット移動
       (t (move-region))))

    ;; keep strings
    ;; 今回検索・置換した文字を次回用に保存
    (setq sc/previous-searched-str search-str)
    (setq sc/previous-replaced-str replace-str)))

;;; ------------------------------------------------------------
;;; replace all
;; すべて置換

(defun sc/replace-all (mode &optional opt-search-str opt-replace-str)
  "Replace All.  MODE [re].  OPT-SEARCH-STR and OPT-REPLACE-STR are optional."
  (let
      ((search-str "")
       (replace-str "")
       (beg 1)
       (end 0)
       (cnt 0)
       beg-each
       end-each
       (len-search-string 0)
       (is-re (if (string= mode "re") t nil))
       target-str
       type)

    ;; target window
    ;; 現在のウィンドウが検索・置換編集用ウィンドウだったら、主たるウィンドウに移動する
    (unless (eq (selected-window) sc/target-window)
      (sc/select-target-window))

    ;; get search strings
    ;; 検索用文字列の取得
    (setq search-str (if opt-search-str
                         opt-search-str
                       (sc/get-str-from-window "search")))
    (unless search-str
      (setq search-str (if (boundp 'sc/previous-searched-str)
                           sc/previous-searched-str
                         nil)))
    (unless search-str (error "Error: search word is empty"))

    ;; empty line
    ;; 空行表現（^$）を特別扱い
    (when is-re (setq search-str (sc/replace-empty-line-regex search-str)))

    ;; get replace strings
    ;; 置換用文字列の取得
    (setq replace-str (if opt-replace-str
                          opt-replace-str
                        (sc/get-str-from-window "replace")))
    (unless replace-str
      (setq replace-str (if (boundp 'sc/previous-replaced-str)
                            sc/previous-replaced-str
                          nil)))
    (unless replace-str (error "Error: replace word is empty"))

    ;; keep history entry before replacing.
    (sc/push-history-entry search-str replace-str is-re)

    ;; replace region or whole buffer
    ;; 選択範囲があればそこを対象とする、なければ、すべてを対象にして良いか尋ねる
    (if (region-active-p)
        (progn
          (setq beg (region-beginning))
          (setq end (region-end)))
      (progn
        (setq type (read-string "replace whole buffer?(y, n): " nil))
        (if (string= type "y")
            (progn
              (setq beg (point-min))
              (setq end (point-max)))
          (error "Error: no target region"))))

    ;; replace
    ;; 選択範囲内を置換する
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (cond ((and is-re sc/is-foreign-regexp)
               (foreign-regexp/replace/perform-replace
                search-str replace-str nil nil nil nil nil beg end))
              (is-re
               (perform-replace
                search-str replace-str nil t nil nil nil beg end))
              (t
               (perform-replace
                search-str replace-str nil nil nil nil nil beg end)))))

    ;; keep strings
    ;; 今回検索・置換した文字を次回用に保存
    (setq sc/previous-searched-str search-str)
    (setq sc/previous-replaced-str replace-str)))

;;; ------------------------------------------------------------
;; multi files search
;; マルチファイル検索
;; 対象ディレクトリと拡張子を指定したら検索する。ただのrgrepのラッパー
;; ref: http://d.hatena.ne.jp/IMAKADO/20090225/1235526604

(require 'grep)
;; (require 'gtags)
;; (setq gtags-path-style 'relative)

;; sc/grep
(defun sc/grep (string ext ignore pwd)
  "It asks STRING and EXT for grep command line and IGNORE are for grep-find-ignored-directories and PWD for current directory."
  (interactive
   (progn
     (let* ((default (sc/get-strings "search"))
            (buffer-ext (when buffer-file-name
                          (file-name-extension buffer-file-name)))
            (target-ext (if (and buffer-ext
                                 (not (string-empty-p buffer-ext)))
                            (concat "*." buffer-ext)
                          "*"))
           (target-dir default-directory))
           ;; (target-dir (if (gtags-get-rootpath)
           ;;                 (directory-file-name (gtags-get-rootpath))
           ;;               default-directory)))
       (grep-compute-defaults)
       (list (read-from-minibuffer "Search: " default nil nil 'grep-history (if current-prefix-arg nil default))
             (read-from-minibuffer "Extension (plural available): " target-ext)
             (read-from-minibuffer "Ignored Directories: " "logs caches")
             (read-directory-name "Directory: " target-dir target-dir t)))))
  (when (or (not (stringp string))
            (string-empty-p string))
    (user-error "Error: search word is empty"))
  (let ((grep-find-ignored-directories (copy-sequence grep-find-ignored-directories)))
    (when ignore
      (dolist (ignore-dir (split-string ignore " " t))
        (add-to-list 'grep-find-ignored-directories ignore-dir)))
    (let ((sc/is-renaming-rgrep-buffer t))
      (rgrep string
             (if (and (stringp ext)
                      (not (string-empty-p ext)))
                 ext
               "*")
             pwd
             nil))))

;; name rgrep buffers per search so old results can be kept.
(defun my/rgrep-buffer-name (regexp files dir)
  "Build a descriptive grep buffer name from REGEXP, FILES and DIR."
  (let* ((dir-name (file-name-nondirectory
                    (directory-file-name (expand-file-name dir))))
         (search-label (replace-regexp-in-string "[ \t\n\r]+" " " regexp))
         (search-label (truncate-string-to-width search-label 30 nil nil t)))
    (format "*grep: %s @ %s [%s]*" search-label dir-name files)))

(defun my/rename-rgrep-buffer (regexp files dir &rest _args)
  "Rename the latest rgrep buffer using REGEXP, FILES and DIR."
  (when-let ((grep-buffer (and sc/is-renaming-rgrep-buffer
                               (get-buffer "*grep*"))))
    (with-current-buffer grep-buffer
      (rename-buffer
       (generate-new-buffer-name
        (my/rgrep-buffer-name regexp files dir))
       t))))
(advice-add 'rgrep :after #'my/rename-rgrep-buffer)

;;; ------------------------------------------------------------
;;; experimental area
;; (global-set-key (kbd "C--") 'func)

;;; ------------------------------------------------------------
;;; Provide

(provide 'search-center)

;;; search-center.el ends here
