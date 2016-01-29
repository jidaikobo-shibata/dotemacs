;; 入力モードが英語の時はカーソルの色をWhiteに、日本語の時はfirebrickにする
;; thx http://masutaka.net/chalow/2015-01-04-1.html
;; これを有効にするためには、Mac OS Xの入力ソースを適切なものにしておくこと
;; thx http://pc-karuma.net/mac-keyboard-input-source/
;; (when (fboundp 'mac-input-source)
;; 	(defun my-mac-selected-keyboard-input-source-chage-function ()
;; 		"Change Cursor Color."
;; 		(let ((mac-input-source (mac-input-source)))
;; 			;; (message mac-input-source)
;; 			(set-cursor-color
;; 			 (if (string-match "\\.US$" mac-input-source)
;; 					 "White" "firebrick"))))
;; 	(add-hook 'mac-selected-keyboard-input-source-change-hook
;; 						'my-mac-selected-keyboard-input-source-chage-function)
;; 	(add-hook 'after-save-hook
;; 					'my-mac-selected-keyboard-input-source-chage-function))

;; (defun my-anything-for-functions ()
;; 	"Anything command for program."
;; 	(interactive)
;; 	(anything-other-buffer
;; 	 '(anything-c-source-imenu
;; 		 anything-c-source-emacs-commands
;; 		 anything-c-source-emacs-functions)
;; 	 "*my-anything-for-functions*"))
;; (bind-key* "C-," (lambda ()
;; 									 (interactive)
;; 									 (when (< (frame-width) 110)
;; 										 (set-frame-size (selected-frame) (+ (frame-width) 100) (frame-height)))
;; 									 (my-anything-for-functions)))

;;; ------------------------------------------------------------
;;; control+shift+cursorでウィンドウ内バッファ履歴
;; thx http://pc12.2ch.net/test/read.cgi/unix/1261307488/

;; (bind-key* "<C-S-left>" 'my-switch-to-prev-buffer)
;; (bind-key* "<C-S-right>" 'my-switch-to-next-buffer)

;; (defun my-switch-to-prev-buffer ()
;; 	"Previous working buffer history."
;; 	(interactive)
;; 	(let ((blist (buffer-list))
;; 				(prev-buffer)
;; 				(buffer))
;; 		(while blist
;; 			(unless (or (string= (substring (buffer-name (car blist)) 0 1) " ")
;; 									(string= (substring (buffer-name (car blist)) 0 1) "*")
;; 									(string= (substring (buffer-name (car blist)) 0 1) "+"))
;; 				(setq prev-buffer (car blist)))
;; 			(setq blist (cdr blist))
;; 			(setq buffer (car blist))
;; 			(if (eq (current-buffer) buffer)
;; 					(progn (switch-to-buffer prev-buffer t)
;; 								 (setq blist nil))))))

;; (defun my-switch-to-next-buffer ()
;; 	"Next working buffer history."
;; 	(interactive)
;; 	(let ((blist (buffer-list))
;; 				(buffer))
;; 		(while blist
;; 			(setq buffer (car blist))
;; 			(setq blist (cdr blist))
;; 			(if (eq (current-buffer) buffer)
;; 					(progn (while (and blist (or (string= (substring (buffer-name (car blist)) 0 1) " ")
;; 																			 (string= (substring (buffer-name (car blist)) 0 1) "*")
;; 																			 (string= (substring (buffer-name (car blist)) 0 1) "+")))
;; 									 (setq blist (cdr blist)))
;; 								 (switch-to-buffer (car blist) t)
;; 								 (setq blist nil))))))

;; (setq hl-line-face 'underline)
;; (global-hl-line-mode)
;; 下線だと、日本語入力時の候補領域がわかりづらいのでやめる。

;;; ------------------------------------------------------------
;; カラーリングをテストする
;; http://d.hatena.ne.jp/buzztaiki/20111209/1323444755

;; (defun font-lock-user-keywords (mode &optional keywords)
;;   "Add user highlighting to MODE to KEYWORDS.
;; See `font-lock-add-keywords' and `font-lock-defaults'."
;;   (unless mode
;;     (error "Mode should be non-nil"))
;;   (font-lock-remove-keywords mode (get mode 'font-lock-user-keywords))
;;   (font-lock-add-keywords mode keywords)
;;   (put mode 'font-lock-user-keywords keywords))

;; rainbow-modeで16進数だけカラーリング
;; (font-lock-remove-keywords
;;    nil
;;    `(,@rainbow-x-colors-font-lock-keywords
;;      ,@rainbow-latex-rgb-colors-font-lock-keywords
;;      ,@rainbow-r-colors-font-lock-keywords
;;      ,@rainbow-html-colors-font-lock-keywords
;;      ,@rainbow-html-rgb-colors-font-lock-keywords))

;; ;;; 接続先Hostを書いた情報源を探して、tramp接続
;; (defvar anything-c-source-my-hosts
;; 	'((name . "hosts")
;; 		(candidates . (lambda ()
;; 										(split-string
;; 										 (with-temp-buffer
;; 											 (insert-file-contents "~/.emacs.d/anything/hosts.txt")
;; 											 (buffer-string)))))
;; 										;; (split-string (shell-command-to-string "find /Users/jidaikobo/FTP/ -name \"*_NON*\" -prune -o -type f -name \"destination.txt\""))))
;; 		(type . file)
;; 		(action . (("Tramp" . anything-tramp-open)))))

;; (defun anything-tramp-open (path)
;; 	"Tramp open.  PATH is path."
;; 	(find-file path))

;; (defun anything-tramp-close (path)
;; 	"Tramp close.  PATH is path."
;; 	(find-file path))

;; (bind-key* "s-t" (lambda () (interactive)
;; 									 (let ((bufname (format-time-string "%y%m%d%H%M%S" (current-time))))
;; 										 (get-buffer-create bufname)
;; 										 (switch-to-buffer bufname)
;; 										 (text-mode))))
	;; (bind-key* "s-t" (lambda () (interactive)
	;; 										 (switch-to-buffer "*scratch*")))
;;; Memo:
;; anything-c-source-google-suggest（面白いのだけど使いどころがない）
;; M-x install-packageで、入らないものがあるが、
;; M-x list-packagesで、C-sで探し、ページ移動の後installでなら入る。

;;; Memo2:
;; tempbuf（idle buffer）を自動的にkill-bufferしてくれるelispだけど、
;; 結構不意に必要なbufferをkillしていることがあるので、使わない方向で。
;; multi-termもよさそうだけど、やっぱりterminalを使う。

;;; Memo3:
;; しばらくauto-install.elを使っていたが、anythingもpackageで入るのでpackageに。
;; auto installはつかわなくてもいける
;; wget http://www.emacswiki.org/emacs/download/auto-install.el
;; M-x byte-compile-file RET ~/.emacs.d/elisp/auto-install.el RET
;; (add-to-list 'load-path "~/.emacs.d/elisp")
;; (require 'auto-install)
;; (setq auto-install-directory "~/.emacs.d/elisp")
;; (auto-install-update-emacswiki-package-name t)
;; (auto-install-compatibility-setup)
;; M-x auto-install-batch RET anything RET

;; wget http://www.ne.jp/asahi/alpha/kazu/pub/emacs/phpdoc.el

;; ;;; ------------------------------------------------------------
;; ;;; 選択範囲がある状態でshiftなしのカーソルが打鍵されたらリージョンを解除
;; ;; macふうの挙動だが、Emacsふうでないので、ちょっと様子見しつつ運用
;; (defcustom is-deactivate-region-by-cursor t
;; 	"*Mac-like behavior."
;; 	:group 'Convenience
;; 	:type 'boolean)

;; (unless is-deactivate-region-by-cursor
;; 	;; regionの解除advice版 - Hookよりこちらのほうが軽い!?
;; 	(defadvice previous-line (before deactivate-region activate)
;; 		"Deactivate Region by cursor."
;; 		(my-deactivate-region))
;; 	(defadvice next-line (before deactivate-region activate)
;; 		"Deactivate Region by cursor."
;; 		(my-deactivate-region))
;; 	(defadvice left-char (before deactivate-region activate)
;; 		"Deactivate Region by cursor."
;; 		(my-deactivate-region))
;; 	(defadvice right-char (before deactivate-region activate)
;; 		"Deactivate Region by cursor."
;; 		(my-deactivate-region))

;; 	;; undo in regionしない
;; 	(defadvice undo-tree-undo (before deactivate-region activate)
;; 		"Deactivate Region when attempt to undo."
;; 		(my-deactivate-region))

;; 	;; リージョン解除関数
;; 	(defun my-deactivate-region ()
;; 		"Logic of deactivate region by cursor."
;; 		(when (and (region-active-p) (not (memq last-input-event '(S-left S-right S-down S-up))))
;; 			(cond
;; 			 ((memq last-input-event '(right down))
;; 				(goto-char (region-end)))
;; 			 ((memq this-command '(left-char previous-line))
;; 				(goto-char (region-beginning))))
;; 			(deactivate-mark))))

;; M-x install-elisp-from-emacswiki RET eldoc-extension.el RET

;; ;;; 新規フレーム作成 (cmd+shift+n)
;; (defun create-new-frame ()
;; 	"Create new frame."
;; 	(interactive)
;; 	(switch-to-buffer-other-frame "*new1*")
;; 	(show-line-number))
;; (bind-key* "s-N" 'create-new-frame)
;; (add-hook 'after-make-frame-functions 'show-line-number)

;;; ------------------------------------------------------------
;;; eww
;; thx http://futurismo.biz/archives/2950

;; duckduckgoの設定
;; (setq eww-search-prefix "https://duckduckgo.com/html/?kl=jp-jp&k1=-1&kc=1&kf=-1&q=")

;; 画像表示
;; (defun eww-disable-images ()
;; 	"Don't show images."
;; 	(interactive)
;; 	(setq-local shr-put-image-function 'shr-put-image-alt)
;; 	(eww-reload))
;; (defun eww-enable-images ()
;; 	"Show images."
;; 	(interactive)
;; 	(setq-local shr-put-image-function 'shr-put-image)
;; 	(eww-reload))
;; (defun shr-put-image-alt (spec alt &optional flags)
;; 	"Show alt instead of image.  SPEC, ALT, FLAGS."
;; 	(insert alt))
;; ;; はじめから非表示
;; (defun eww-mode-hook--disable-image ()
;; 	"Don't show images."
;; 	(setq-local shr-put-image-function 'shr-put-image-alt))
;; ;; (add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)

;;; diredでマークをつけたファイルをviewモードで開く（V）
;; (eval-after-load "dired"
;; 	'(progn
;; 		 (define-key dired-mode-map (kbd "V") 'my-dired-view-marked-files)
;; 		 (defun my-dired-view-marked-files (&optional arg)
;; 			 "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
;; 			 (interactive "P")
;; 			 (let* ((fn-list (dired-get-marked-files nil arg)))
;; 				 (mapc 'view-file fn-list)))))

;;; 釣り合う行カッコが画面外だったらミニバッファに表示
;; thx http://emacswiki.org/emacs/ShowParenMode
;; (defadvice show-paren-function
;; 		(after show-matching-paren-offscreen activate)
;; 	"If the matching paren is offscreen, show the matching line in theecho area.  Has no effect if the character before point is not ofthe syntax class ')'."
;; 	(interactive)
;; 	(let* ((cb (char-before (point)))
;; 				 (matching-text (and cb
;; 														 (char-equal (char-syntax cb) ?\) )
;; 														 (blink-matching-open))))
;; 		(when matching-text (message matching-text))))

;;; 右ボタンの割り当て(押しながらの操作)をはずす。
;;; thx http://cave.under.jp/_contents/emacs.html#60
;; (if window-system
;; 		(progn
;; 			(global-unset-key [down-mouse-3])
;; 			;; マウスの右クリックメニューを出す(押して、離したときにだけメニューが出る)
;; 			(defun bingalls-edit-menu (event)
;; 				(interactive "e")
;; 				(popup-menu menu-bar-edit-menu))
;; 			(bind-key* "<mouse-3>" 'bingalls-edit-menu)))

;; モードラインにカレントディレクトリを表示する
;; (let ((ls (member 'mode-line-buffer-identification mode-line-format)))
;; 	(setcdr ls
;; 					(cons
;; 					 '(:eval (concat " (" (abbreviate-file-name default-directory) ")"))
;; 					 (cdr ls))))

;;; よくあるマイナーモードを非表示
;; thx http://qiita.com/tadsan/items/8b5976682b955788c262
;; これは一通り処理が終わった後呼ぶ必要がある。
;; (setq my/hidden-minor-modes
;; 			'(undo-tree-mode
;; 				eldoc-mode
;; 				magit-auto-revert-mode
;; 				smart-tab-mode
;; 				flycheck-mode
;; 				abbrev-mode
;; 				helm-mode))
;; (mapc (lambda (mode)
;; 				(setq minor-mode-alist
;; 							(cons (list mode "") (assq-delete-all mode minor-mode-alist))))
;; 			my/hidden-minor-modes)

;; ;;; 関数名の表示
;; (which-func-mode 1)

;; ;;; アスタリスクで終わるバッファ名を除いたリストを取得
;; ;;; とりあえず使っていないけど、何かの役に立つかもなので、取っておく。
;; (defun eliminated-buffers ()
;; 	"Eleminate buffers."
;; 	(let (result
;; 				(tmp-buffers (buffer-list)))
;; 		(dolist (buf tmp-buffers result)
;; 			(unless (string= "*" (substring (format "%s" buf) -1 nil))
;; 				(add-to-list 'result buf)))))

;;; ------------------------------------------------------------
;;; バッファ関連

;;; f2キーでmessageと今のバッファをトグル
;; (bind-key* "<f2>" (lambda () (interactive)
;; 											 (let (current-buffer-for-return)
;; 												 (if (eq (selected-window) (get-buffer-window "*Messages*"))
;; 														 (switch-to-buffer current-buffer-for-return)
;; 													 (setq current-buffer-for-return (current-buffer))
;; 													 (switch-to-buffer "*Messages*")))))

;;; ------------------------------------------------------------
;;; ウィンドウ関連

;;; mac like new window (cmd+n)
;; cmd+n でウィンドウを増やす。分割方法は対話式
;; (defun create-new-window-intaractive (act)
;; 	"Mac like new window (cmd+n).  ACT is interactive."
;; 	(interactive "nchoose (holizntal:1, vertical:2):")
;; 	(cond ((eq act 2) (split-window-horizontally))
;; 				(t (split-window-vertically))))
;; (bind-key* "s-n" 'create-new-window-intaractive)

;; ;;; eldoc
;; (require 'eldoc-extension)
;; (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
;; (setq eldoc-idle-delay 0.2)
;; (setq eldoc-minor-mode-string "")

;; ;;; eldoc-php
;; ;; thx http://www.ne.jp/asahi/alpha/kazu/php.html
;; (require 'phpdoc)
;; (add-hook 'php-mode-hook (lambda () (eldoc-mode t)))

;; (dolist (destination
;; 				 (split-string
;; 					(with-temp-buffer
;; 						(insert "%s" (shell-command-to-string "find /Users/jidaikobo/FTP/ -name \"*_NON*\" -prune -o -type f -name \"destination.txt\""))
;; 						(ucs-normalize-NFC-region (point-min) (point-max))
;; 						(buffer-string))))
;; 	(when (file-exists-p destination)
;; 		(insert-file-contents destination)))

;; (defvar anything-c-source-find-by-gtags
;; 	'((name . "Find by gtags or ls")
;; 		(candidates . (lambda ()
;; 										(let
;; 												((default-directory
;; 													 (with-current-buffer anything-current-buffer default-directory))
;; 												 (find-opt " -type f ! -name \"*.png\" ! -name \"*.ico\" ! -name \"*.gif\" ! -name \"*.jpg\""))
;; 											(cond
;; 											 ;; gtags-get-rootpathが返ったらgtagsをあてにして良い
;; 											 ((gtags-get-rootpath)
;; 												(split-string
;; 												 (shell-command-to-string
;; 													(concat "find " (directory-file-name (gtags-get-rootpath)) find-opt)) "\n"))
;; 											 ;; findの負荷が高すぎる場所だったらやりすごす
;; 											 ((member default-directory '("/" "~/"))
;; 												(split-string
;; 												 (shell-command-to-string
;; 													(concat "ls " default-directory)) "\n"))
;; 											 ;; とりあえず自分以下のファイルをfind
;; 											 (t
;; 												(split-string
;; 												 (shell-command-to-string
;; 													(concat "find " (directory-file-name default-directory) find-opt)) "\n"))))))
;; 		(type . file)))
;; 		;; (requires-pattern . 3)
;; 		;; (delayed)))

;;; create-temporary-buffer
;; あたらしい空のバッファを作る (cmd+t)
;; (defun create-temporary-buffer ()
;; 	"Create temporal buffer."
;; 	(interactive)
;; 	(switch-to-buffer (generate-new-buffer "new"))
;; 	(global-auto-complete-mode t))
;; (global-set-key (kbd "s-t") 'create-temporary-buffer) ; (cmd+t)

;;; mac like close window (cmd+w)
;; cmd+wで、開いているウィンドウを閉じる。単一のバッファなら、変更を確認してバッファを閉じる
;; (defun contexual-close-window ()
;; 	"Mac like close window (cmd+w)."
;; 	(interactive)
;; 	(let
;; 			(save-type)
;; 		;; フレームがウインドウ分割をしていない時には、変更の有無を確認
;; 		(if (one-window-p)
;; 				;; 変更があるので振る舞いを尋ねる
;; 				(if (buffer-modified-p)
;; 						(progn
;; 							;; アスタリスクで始まるバッファは何も尋ねず閉じる
;; 							(if (or (string= "*" (substring (buffer-name) 0 1)) buffer-read-only)
;; 									(kill-buffer)
;; 								;; 振る舞いを確認する必要があるウィンドウなので、確認する
;; 								(progn
;; 									(setq save-type (read-string "overwrite? (1:overrite, 2:save as, 3:close anyway): " nil 'my-history))
;; 									(cond
;; 									 ((string-equal save-type "1")
;; 										(save-buffer))
;; 									 ((string-equal save-type "2")
;; 										(progn (call-interactively 'write-file)(save-buffer)))
;; 									 (t
;; 										(kill-buffer-and-window))))))
;; 					;; 変更がないのでkill-buffer-and-window
;; 					(kill-buffer-and-window))
;; 			;; フレームがウィンドウ分割をしているので、delete-windowする
;; 			(delete-window))))
;; (global-set-key (kbd "s-w") 'contexual-close-window)

;;; ------------------------------------------------------------
;;; Macの辞書で検索
;; thx http://moyashi.air-nifty.com/hitori/2007/12/emacscarbon_ema_5f82.html

;; (defun my-search-at-dictionary-app ()
;; 	"Search at dictionary app."
;; 	(interactive)
;; 	(let* ((keyword (read-from-minibuffer
;; 									 " keyword: "
;; 									 (my-get-keyword)))
;; 				 (encoded-keyword (encode-coding-string keyword 'japanese-shift-jis)))
;; 		(unless (string= encoded-keyword "")
;; 			(do-applescript (concat "
;; activate application \"Dictionary\"
;; tell application \"System Events\"
;;     tell application process \"Dictionary\"
;;         set value of text field 1 of group 1 of tool bar 1 of window 1 to \""
;;             encoded-keyword "\"
;;         click button 1 of text field 1 of group 1 of tool bar 1 of window 1
;;     end tell
;; end tell
;; ")))))

;; (defun my-get-keyword ()
;; 	"My get keyword."
;; 	(or (and
;; 			 transient-mark-mode
;; 			 mark-active
;; 			 (buffer-substring-no-properties
;; 				(region-beginning) (region-end)))
;; 			(thing-at-point 'word)))

;; (global-set-key (kbd "s-D") 'my-search-at-dictionary-app)

;;; Rictyを等幅で使う
;; (add-to-list 'default-frame-alist '(font . "ricty-16"))

;; thx http://tam5917.hatenablog.com/entry/20120915/1347688961
;; (if window-system
;; 		(progn
;; 			(create-fontset-from-ascii-font
;; 			 "Ricty:style=regular:spacing=0" nil "ricty")

;; 			(dolist (charset
;; 							 '(unicode
;; 								 japanese-jisx0208
;; 								 japanese-jisx0208-1978
;; 								 japanese-jisx0212
;; 								 japanese-jisx0213-1
;; 								 japanese-jisx0213-2
;; 								 japanese-jisx0213-a
;; 								 japanese-jisx0213.2004-1
;; 								 katakana-jisx0201))
;; 				(set-fontset-font "fontset-ricty"
;; 													charset
;; 													(font-spec :family "Ricty" :size 16) ;16以外のサイズでは等幅にならない :-(
;; 													nil 'prepend))

;; 			(setq default-frame-alist
;; 						(append (list
;; 										 '(font . "fontset-ricty")
;; 										 )
;; 										default-frame-alist))
;; 			(set-face-attribute 'fixed-pitch nil :family "Ricty")
;; 			))

;;; Rictyの準備の仕方
;; thx Rictyなるものがあるらしい | cozy attic
;; https://cozyattic.wordpress.com/2013/07/24/ricty%E3%81%AA%E3%82%8B%E3%82%82%E3%81%AE%E3%81%8C%E3%81%82%E3%82%8B%E3%82%89%E3%81%97%E3%81%84/
; sudo port install fontforge
; で、fontforgeをインストール
; fontforge -version
; で、インストールの確認。
; https://github.com/yascentur/Ricty/tree/3.2.2
; で、Ricty-3.2.2.zipをダウンロード。
; http://levien.com/type/myfonts/inconsolata.html
; で、OpenType fileのInconsolata.otfをダウンロード。
; http://mix-mplus-ipa.sourceforge.jp/migu/
; で、migu-1m-20150712.zipのダウンロード。
; 全て解凍し、Inconsolata.otf、migu-1m-bold.ttf、migu-1m-regular.ttfをRictyのフォルダへ移動
; ターミナルでRictyフォルダに移動
; sh ricty_generator.sh Inconsolata.otf migu-1m-regular.ttf migu-1m-bold.ttf
; とする。僕はデフォルトだと全角スペースが目立ちすぎるので、
; sh ricty_generator.sh -z Inconsolata.otf migu-1m-regular.ttf migu-1m-bold.ttf
; として、全角スペースを不可視にしている。
; https://github.com/yascentur/Ricty/tree/3.2.2
; のREADMEには、このシェルスクリプトのオプションがあるので、参考にすると良い。
