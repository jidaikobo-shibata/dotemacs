;;; search-center.el --- search center.
;; Copyright (C) 2016 by jidaikobo-shibata
;; Author: jidaikobo-shibata
;; URL: https://github.com/jidaikobo-shibata/dotemacs

;;; Commentary:
;; minor mode for search and replace.
;; 検索置換のためのマイナーモード。

;;; Todo:
;; 補助的依存状態のforeign-regrexとsmartrepがなくても動くようにする
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

;;; Ussage:
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

;;; ------------------------------------------------------------
;;; defvar

(defvar sc/target-window (selected-window))
(defvar sc/target-buffer (get-buffer (buffer-name)))
(defvar sc/search-str-buffer "*search string*")
(defvar sc/replace-str-buffer "*replace string*")
(defvar sc/previous-searched-str)
(defvar sc/previous-replaced-str)
(defvar sc/previous-searced-direction nil)
(defvar sc/previous-direction)
(defvar sc/ignore-delete-window-hook nil)
(defvar sc/modeline-saved nil)
(defvar sc/modeline-background)
(defvar sc/modeline-foreground)
(defvar sc/re-modeline-background "orange")
(defvar sc/re-modeline-foreground "black")
(defvar sc/is-foregin-regexp nil)
(defvar sc/is-use-timer-for-modeline t)
(defvar sc/is-use-timer-for-buffers t)
(defvar sc/split-direction "horizontal")
(defvar search-center-mode-map (make-keymap))
(defvar search-center-re-mode-map (make-keymap))

;;; ------------------------------------------------------------
;;; dependencies

(require 'package)
(package-initialize)

(declare-function package-installed-p "package")
(defvar foreign-regexp/regexp-type nil)

;; Emacs 26でforeign-regexpがエラーを出すので抑止
(defvaralias 'lazy-highlight-face 'isearch-lazy-highlight)

;; foreign-regexp
(when (package-installed-p 'foreign-regexp)
  (with-no-warnings (require 'foreign-regexp))
  (eval-after-load "foreign-regexp"
    (progn
      (custom-set-variables
       '(foreign-regexp/regexp-type 'perl)
       '(reb-re-syntax 'foreign-regexp))
      (setq sc/is-foregin-regexp t)))
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
(defun sc/toggle-mode-line ()
  "Toggle Mode line."
  (interactive)
  ;; keep current color of mode line at the first time
  ;; 最初に現在のモードラインを配色を保存しておく
  (unless sc/modeline-saved
    (setq sc/modeline-background (face-attribute 'mode-line :background)
          sc/modeline-foreground (face-attribute 'mode-line :foreground)
          sc/modeline-saved t))
  (cond
   ;; 正規表現モード
   ((eq search-center-re-mode t)
    (set-face-attribute 'mode-line nil
                        :foreground sc/re-modeline-foreground
                        :background sc/re-modeline-background))
   ;; 標準モード
   ((eq search-center-re-mode nil)
    (set-face-attribute 'mode-line nil
                        :foreground sc/modeline-foreground
                        :background sc/modeline-background)))
  (force-mode-line-update))

;; timer for mode line
;; モードラインの配色を監視するタイマー
(if sc/is-use-timer-for-modeline
    (setq-default global-re-toggle-timer
                  (run-with-idle-timer 0.03 t 'sc/toggle-mode-line)))

;; timer for search/replace buffer
;; 検索置換窓はサクッと消す
(if sc/is-use-timer-for-buffers
    (setq-default
     global-re-toggle-timer
     (run-with-idle-timer
      0.03
      t
      (lambda () (interactive)
        (when (and (not (equal (selected-window) (get-buffer-window sc/search-str-buffer)))
                   (not (equal (selected-window) (get-buffer-window sc/replace-str-buffer)))
                   (or (windowp (get-buffer-window sc/search-str-buffer))
                       (windowp (get-buffer-window sc/replace-str-buffer))))
          (delete-window (get-buffer-window sc/search-str-buffer))
          (delete-window (get-buffer-window sc/replace-str-buffer)))))))

;;; ------------------------------------------------------------
;;; hook

;; compatible with isearch command
;; i-search、i-search-backwardコマンドとの同期
(when sc/is-sync-isearch
  (add-hook 'isearch-update-post-hook 'sc/isearch-update-string)
  (defun sc/isearch-update-string ()
    (when (and (not (or isearch-regexp isearch-regexp-function))
               (eq this-command 'isearch-printing-char))
      (unless (get-buffer sc/search-str-buffer) (get-buffer-create sc/search-str-buffer))
      (sc/keep-target-buffer)
      ;; set strings
      ;; 文字列をセット
      (with-current-buffer sc/search-str-buffer
        (delete-region (point-min) (point-max))
        (insert isearch-string))
      ;; keep strings
      ;; 次回用に文字列を保存
      (setq sc/previous-searched-str isearch-string))))

;; compatible with Anything-occur command
;; Anything-occurで入力した文字は同期し、検索対象にする

(defun my-save-anything-occur-input-to-buffer (input)
  "Save the INPUT string from `anything-occur` to the buffer `sc/search-str-buffer`."
  (when (get-buffer-create sc/search-str-buffer)
    (with-current-buffer sc/search-str-buffer
      (erase-buffer)
      (insert input))))

(advice-add 'anything-occur :around
            (lambda (orig-fun &rest args)
              "Capture the input string of `anything-occur` and save it to the buffer."
              (let ((result (apply orig-fun args)))
                (when anything-input ;; anythingで入力された文字列を取得
                  (my-save-anything-occur-input-to-buffer anything-input))
                result)))

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
  (global-set-key (kbd "s-M") 'sc/grep) ;; s-m is used by Mac OS X
  (global-set-key (kbd "s-h") (lambda () (interactive) (select-window sc/target-window)))
  (global-set-key (kbd "C-g") 'sc/quit-str-windows))

;;; ------------------------------------------------------------
;;; Quit

(defun sc/quit-str-windows ()
  "Quit string windows.  Delete string windows by idle timer."
  (interactive)
  (if (or (equal (selected-window) (get-buffer-window sc/search-str-buffer))
          (equal (selected-window) (get-buffer-window sc/replace-str-buffer)))
      (select-window sc/target-window)
    (keyboard-quit)))

;;; ------------------------------------------------------------
;;; keep target-buffer

(defun sc/keep-target-buffer ()
  "Keep target buffer."
  (unless (or (equal (selected-window) (get-buffer-window sc/search-str-buffer))
              (equal (selected-window) (get-buffer-window sc/replace-str-buffer)))
    (setq sc/target-window (selected-window)
          sc/target-buffer (buffer-name))))

;;; ------------------------------------------------------------
;;; toggle re/search mode

(defun sc/toggle-search-mode ()
  "Toggle search mode."
  (interactive)
  (unless (get-buffer sc/search-str-buffer) (get-buffer-create sc/search-str-buffer))
  (unless (get-buffer sc/replace-str-buffer) (get-buffer-create sc/replace-str-buffer))
  (sc/keep-target-buffer)
  (if (eq search-center-re-mode nil)
      ;; turn into regrex
      ;; 正規表現モードをオン
      (progn
        (with-current-buffer sc/target-buffer
          (search-center-re-mode t))
        (with-current-buffer sc/search-str-buffer
          (search-center-re-mode t))
        (with-current-buffer sc/replace-str-buffer
          (search-center-re-mode t))
        (message "%s" (concat "turned into RE with " sc/target-buffer)))
    ;; turn off regrex
    ;; 正規表現モードをオフ
    (with-current-buffer sc/target-buffer
      (search-center-re-mode -1))
    (with-current-buffer sc/search-str-buffer
      (search-center-re-mode -1))
    (with-current-buffer sc/replace-str-buffer
      (search-center-re-mode -1))
    (message "%s" (concat "turned off RE with " sc/target-buffer))))

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

    (setq sc/ignore-delete-window-hook t)
    (sc/keep-target-buffer)

    ;; search/replace wiondow cannot exists alone
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
    (goto-char (point-max))

    (setq sc/ignore-delete-window-hook t)))

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
          (if sc/is-foregin-regexp
              ;; perform repalce by foreign-regexp
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

;; generate region and controll direction of search
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

(declare-function sc/move-region "sc/move-region" ())
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
      (select-window sc/target-window))

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

    ;; move cursor and generate region
    ;; 検索文字列にキャレットを移動しリージョンにする関数
    (defun sc/move-region ()
      (cond
       ((and (not is-re) is-next)
        (search-forward search-str))
       ((and (not is-re) is-prev)
        (search-backward search-str))
       ((and is-re is-next)
        (if sc/is-foregin-regexp
            (foreign-regexp/search/forward search-str)
          (re-search-forward search-str)))
       ((and is-re is-prev)
        (if sc/is-foregin-regexp
            (foreign-regexp/search/backward search-str)
          (re-search-backward search-str))))
      ;; get length of searched strings
      ;; ヒットした文字長の取得
      (if is-re
          (setq len-search-string (length (match-string-no-properties 0)))
        (setq len-search-string (length search-str)))
      ;; generate region
      ;; リージョン作成
      (sc/generate-region (if is-next "next" "prev") len-search-string))

    ;; main process
    ;; 処理本体
    (cond
     ;; replace and next
     ;; rep-nextやrep-prevは、いまの選択範囲を置換してから次に行くようにする
     ((or (and is-replace is-next) (and is-replace is-prev))
      (progn (when mark-active (sc/replace-region search-str replace-str is-re))
             (sc/move-region)))
     ;; replace here
     ;; その場を置換
     (is-replace-here (sc/replace-region search-str replace-str is-re))
     ;; move corsor
     ;; 通常はただのキャレット移動
     (t (sc/move-region)))

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
      (select-window sc/target-window))

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
    (ignore-errors
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (cond ((and is-re sc/is-foregin-regexp)
                 (foreign-regexp/replace/perform-replace
                  search-str replace-str nil nil nil nil nil beg end))
                (is-re
                 (perform-replace
                  search-str replace-str nil t nil nil nil beg end))
                (t
                 (perform-replace
                  search-str replace-str nil nil nil nil nil beg end))))))

    ;; keep strings
    ;; 今回検索・置換した文字を次回用に保存
    (setq sc/previous-searched-str search-str)
    (setq sc/previous-replaced-str replace-str)))

;;; ------------------------------------------------------------
;; multi files search
;; マルチファイル検索
;; 対象ディレクトリと拡張子を指定したら検索する。ただのrgrepのwrapper
;; thx http://d.hatena.ne.jp/IMAKADO/20090225/1235526604

(require 'grep)
;; (require 'gtags)
;; (setq gtags-path-style 'relative)

;; sc/grep
(defun sc/grep (string ext ignore pwd)
  "It asks STRING and EXT for grep command line and IGNORE are for grep-find-ignored-directories and PWD for current directory."
  (interactive
   (progn
     (let ((default (sc/get-strings "search"))
           (target-ext (concat "*." (ignore-errors
                                      (file-name-extension (buffer-file-name)))))
           (target-dir default-directory))
           ;; (target-dir (if (gtags-get-rootpath)
           ;;                 (directory-file-name (gtags-get-rootpath))
           ;;               default-directory)))
       (grep-compute-defaults)
       (list (read-from-minibuffer "Search: " default nil nil 'grep-history (if current-prefix-arg nil default))
             (read-from-minibuffer "Extension (plural available): " target-ext)
             (read-from-minibuffer "Ignored Directories: " "logs caches")
             (read-directory-name "Directory: " target-dir target-dir t)))))
  (when ignore
    (dolist (ignore-dir (split-string ignore " "))
      (add-to-list 'grep-find-ignored-directories ignore-dir)))
  (rgrep string ext pwd nil))

;; defadvice rgrep
;; thx http://d.hatena.ne.jp/kitokitoki/20101009/p6
(defadvice rgrep (before my-rgrep activate)
  "Confirm delete grep buffer."
  (when (get-buffer "*grep*")
    (when (y-or-n-p "Delete existing *grep* buffer? ")
      (kill-buffer (get-buffer "*grep*")))))

;;; ------------------------------------------------------------
;;; experimental area
;; (global-set-key (kbd "C--") 'func)

;;; ------------------------------------------------------------
;;; Provide

(provide 'search-center)

;;; search-center.el ends here
