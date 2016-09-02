;; ;;; 現在バッファパスにterminal/iTermでcdする
;; ;; thx http://stackoverflow.com/questions/29404870/change-directory-in-osx-terminal-app-from-emacs-nw
;; (defun my-open-terminal-in-current-dir (&optional command)
;;   "Change directory to current buffer path by Terminal.app.  COMMAND is execute after cd."
;;   (interactive)
;;   (shell-command
;;    (concat "open -b com.apple.terminal " (expand-file-name ".") command)))
;; ;; thx http://qiita.com/ganmacs/items/cfc5f9c2213a6a9e6579
;; (defun cd-on-iterm (&optional command)
;;   "Change directory to current buffer path by iTerm.app.  COMMAND is execute after cd."
;;   (interactive)
;;   (util/execute-on-iterm
;;    (concat (format "cd %s" default-directory) command)))
;; (defun util/execute-on-iterm (command)
;;   "Change directory to current buffer path by iTerm.app.  COMMAND."
;;   (interactive "MCommand: ")
;;   (do-applescript
;;    (format "tell application \"iTerm2\"
;; activate
;; tell current session of current window
;; write text \"%s\"
;; end tell
;; end tell"
;;            command)))

;; ;;; ------------------------------------------------------------
;; ;;; 対になるパーレンに移動
;; ;; thx https://gist.github.com/donghee/3937661

;; (defun jump-match-paren (arg)
;;   "Go to the matching parenthesis.  ARG."
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(\\|\\s\[") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)\\|\\s\]") (forward-char 1) (backward-list 1))
;;         (t (back-to-indentation))))

;; (global-set-key (kbd "s-b") 'jump-match-paren)

;; ;;; ------------------------------------------------------------
;; ;;; 一行目と最終行での上下キーの振る舞い（行末と行頭へ）
;; macらしいけど、Emacsらしくなくview-line-modeと相性が悪いのでいったんやめてみる
;; (defvar prev-line-num (line-number-at-pos))
;; (add-hook 'post-command-hook 'my-goto-the-edge)
;; (defun my-goto-the-edge ()
;;   "Go to the edge of the line."
;;   ;; (message "this-event: %s\nthis-command: %s" last-input-event this-command)
;;   (when (and (eq prev-line-num 1) (memq last-input-event '(up S-up)))
;;     (beginning-of-line))
;;   (when (and (eq prev-line-num (count-lines 1 (point-max)))
;;              (memq last-input-event '(down S-down)))
;;     (end-of-line))
;;   (setq prev-line-num (line-number-at-pos)))

;; 
;; ;;; ------------------------------------------------------------
;; ;;; Mew
;; ;;; ------------------------------------------------------------

;; (autoload 'mew "mew" nil t)
;; (autoload 'mew-send "mew" nil t)

;; ;; Optional setup (Read Mail menu):
;; (setq read-mail-command 'mew)

;; ;; Optional setup (e.g. C-x m for sending a message):
;; (autoload 'mew-user-agent-compose "mew" nil t)
;; (if (boundp 'mail-user-agent)
;;     (setq mail-user-agent 'mew-user-agent))
;; (if (fboundp 'define-mail-user-agent)
;;     (define-mail-user-agent
;;       'mew-user-agent
;;       'mew-user-agent-compose
;;       'mew-draft-send-message
;;       'mew-draft-kill
;;       'mew-send-hook))

;; 
;; ;;; ------------------------------------------------------------
;; ;;; eww
;; ;;; ------------------------------------------------------------

;; (require 'eww)

;; (define-key eww-mode-map (kbd "<backtab>") 'shr-previous-link)
;; (define-key eww-mode-map "r" 'eww-reload)
;; (define-key eww-mode-map "c 0" 'eww-copy-page-url)
;; (define-key eww-mode-map "p" 'scroll-down)
;; (define-key eww-mode-map "n" 'scroll-up)
;; (setq eww-search-prefix "http://www.google.co.jp/search?q=")
;; (setq eww-download-directory "~/Desktop")

;; ;; ewwを複数開く
;; (when (fboundp 'eww)
;;   (progn
;;     (defun xah-rename-eww-hook ()
;;       "Rename eww browser's buffer so sites open in new page."
;;       (rename-buffer "eww" t))
;;     (add-hook 'eww-mode-hook 'xah-rename-eww-hook)))

;;; ------------------------------------------------------------
;;; web-mode

;; (require 'web-mode)
;; (require 'php-mode)

;; (add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.css$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
;; (add-to-list 'web-mode-imenu-regexp-list
;;              '("\\(function\\|class\\) +?\\(.+?\\) *?(" 1 2 " "))
;; (setq-default web-mode-engines-alist '(("php" . "\\.html?$")))
;; (setq-default web-mode-enable-auto-pairing nil)
;; (setq-default web-mode-enable-auto-closing nil)
;; (setq-default web-mode-enable-auto-opening nil)
;; (setq-default web-mode-enable-auto-quoting nil)
;; (setq-default web-mode-enable-auto-indentation nil)
;; (setq-default web-mode-enable-html-entities-fontification t)
;; (setq-default web-mode-enable-element-tag-fontification t)
;; (setq-default web-mode-enable-part-face t)
;; (setq-default web-mode-ignore-ac-start-advice t)

;; ;; thx http://yanmoo.blogspot.jp/2013/06/html5web-mode.html
;; (add-hook 'web-mode-hook
;;           '(lambda()
;;              (setq web-mode-markup-indent-offset 2)
;;              (setq web-mode-css-indent-offset    2)
;;              (setq web-mode-code-indent-offset   2) ; script indent(js,php,etc..)
;;              (define-key web-mode-map "/" 'self-insert-command)))

;;; ------------------------------------------------------------
;;; PHPのimenuの一覧から'All Methods'を取り除く
;; thx http://qiita.com/osamu2001/items/511b558e5280dbf2b218

;; (add-hook
;;   'php-mode-hook
;;     (assq-delete-all
;;       (car (assoc "All Methods" php-imenu-generic-expression))
;;       php-imenu-generic-expression))

;; php-modeにalign-rulesを
;; thx http://d.hatena.ne.jp/Tetsujin/20070614/1181757931
;; (require 'align)
;; (add-to-list 'align-rules-list
;;              '(php-assignment
;;                (regexp . "[^-=!^&*+<>/.| \t\n]\\(\\s-*[.-=!^&*+<>/|]*\\)=>?\\(\\s-*\\)\\([^= \t\n]\\|$\\)")
;;                (justify .t)
;;                (tab-stop . nil)
;;                (modes . '(php-mode))))
;; (add-to-list 'align-dq-string-modes 'php-mode)
;; (add-to-list 'align-sq-string-modes 'php-mode)
;; (add-to-list 'align-open-comment-modes 'php-mode)
;; (setq align-region-separate (concat "\\(^\\s-*$\\)\\|"
;;                                     "\\([({}\\(/\*\\)]$\\)\\|"
;;                                     "\\(^\\s-*[)}\\(\*/\\)][,;]?$\\)\\|"
;;                                     "\\(^\\s-*\\(}\\|for\\|while\\|if\\|else\\|"
;;                                     "switch\\|case\\|break\\|continue\\|do\\)[ ;]\\)"))

;; ;;; ------------------------------------------------------------
;; ;;; FTP by Fetch
;; (defun func-anything-c-source-my-fetch ()
;;   "Anything source."
;;   (let (ret)
;;     (with-temp-buffer
;;       (insert
;;        (shell-command-to-string
;;         (concat "find " my-fetch-app-dir " -name \"*_NON_*\" -prune -o -name \"*.app\"")))
;;       (ucs-normalize-NFC-region (point-min) (point-max))
;;       (setq ret (split-string (buffer-string) "\n")))
;;     ret))
;; ;; 結果がなければたさない
;; (when (func-anything-c-source-my-fetch)
;;   (defvar anything-c-source-my-fetch
;;     '((name . "open Fetch.app")
;;       (candidates . (lambda () (func-anything-c-source-my-fetch)))
;;       (type . file)
;;       (action . (("open Fetch" . anything-fetch-open)))))
;;   (defun anything-fetch-open (app)
;;     "Fetch open.  APP is path."
;;     (shell-command (concat "open " app)))
;;   (add-to-list 'alist-anything-for-files 'anything-c-source-my-fetch t))

;; ;; diredでanythingしたらfindする
;; (defvar anything-c-source-find-at-dired
;;   '((name . "Find file")
;;     (candidates . (lambda ()
;;                     (with-current-buffer anything-current-buffer
;;                       (let* ((host (file-remote-p dired-directory 'localhost))
;;                              (shell-file-name (if (and host (string-match "\\.sakura" host))
;;                                                   "/usr/local/bin/bash"
;;                                                 "/bin/bash"))
;;                              (pwd (string-trim (shell-command-to-string "pwd")))
;;                              (tramp-host (file-remote-p dired-directory 'localhost))
;;                              (tramp-results (list))
;;                              (results (split-string
;;                                        (shell-command-to-string
;;                                         (concat "find "
;;                                                 (replace-regexp-in-string "/$" "" pwd)
;;                                                 (replace-regexp-in-string "\n" " "
;;                                                                           "
;; -type d -name \"logs\" -prune -o
;; -type d -name \"cache\" -prune -o
;; -type d -name \".git\" -prune -o
;; -type f ! -name \"*.png\"
;; ! -name \"*.ico\"
;; ! -name \"*.gif\"
;; ! -name \"*.jpg\"
;; ! -name \".DS_Store\"")))
;;                                        "\n")))
;;                         (if tramp-host
;;                             (progn
;;                               (dolist (result results)
;;                                 (add-to-list 'tramp-results (concat tramp-host result)))
;;                               tramp-results)
;;                           results)))))
;;     (type . file)))

;; 印刷プレビュー
;; /bin/bash xpdf not found?
;; (when (require 'pdf-preview)
;;   (defvar pdf-preview-preview-command "open -a Preview.app")
;;   (global-set-key
;;    (kbd "s-P")
;;    (lambda ()
;;      (interactive)
;;      (when (and
;;             (yes-or-no-p "Show current buffer by Preview.app?")
;;             (or
;;              (<= (length (buffer-string)) 10000)
;;              (and (> (length (buffer-string)) 10000)
;;                   (yes-or-no-p "Large buffer. Preview takes quite time. Preview this?"))))
;;        (pdf-preview-buffer)))))

;; (defun my-anything-for-tramp ()
;;   "Anything command for .ssh/config."
;;   (interactive)
;;   (anything-other-buffer
;;    'anything-c-source-my-hosts
;;    "*my-anything-for-tramp*"))

;; (global-set-key (kbd "C-.") 'my-anything-for-tramp)

;; ;; anything in dired
;; ;; thx http://syohex.hatenablog.com/entry/20120105/1325770778
;; (defun my/anything-dired ()
;;   "Press C-; to into anything mode."
;;   (interactive)
;;   (let ((curbuf (current-buffer)))
;;     (if (anything-other-buffer
;;          '(anything-c-source-files-in-current-dir+)
;;          "*anything-dired*")
;;         (kill-buffer curbuf))))
;; ;; (define-key dired-mode-map (kbd "C-;") 'my/anything-dired)

;; ;; internal directory
;; ;; thx rubikitch
;; (defun my-dirname-to-modeline ()
;;   "Parent directory name."
;;   (concat
;;    "["
;;    ;; current buffer's directory
;;    (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name))))
;;    " "
;;    ;; working internal directory
;;    (if (string-match "/Sites/\\(.+?\\)\\b" default-directory)
;;        (substring default-directory (match-beginning 1) (match-end 1))
;;      (file-name-nondirectory (directory-file-name default-directory)))
;;   "]"))
;; ;; (add-to-list 'default-mode-line-format '(:eval (my-dirname-to-modeline)))

;; ;;; ------------------------------------------------------------
;; ;;; よく使うプロジェクトに対する操作
;; (defun func-anything-c-source-cd-to-projects ()
;;   "Anything source."
;;   (let (ret)
;;     (with-temp-buffer
;;       (insert
;;        (shell-command-to-string (concat "find " my-work-dir " -maxdepth 1 -type d")) "\n")
;;       (ucs-normalize-NFC-region (point-min) (point-max))
;;       (setq ret (split-string (buffer-string) "\n")))
;;     ret))
;; ;; 結果がなければたさない
;; (when (func-anything-c-source-cd-to-projects)
;;   (defvar anything-c-source-cd-to-projects
;;     '((name . "cd to project")
;;       (candidates . (lambda () (func-anything-c-source-cd-to-projects)))
;;       (action . (("Change internal directory" . anything-change-internal-directory)
;;                  ("Dired" . anything-project-dired)
;;                  ("Generate gtags at project" . anything-generate-gtags-at-project)))))
;;   (defun anything-change-internal-directory (dir)
;;     "Change internal directory at Sites.  DIR is path."
;;     (cd dir))
;;   (defun anything-project-dired (dir)
;;     "Dired.  DIR is path."
;;     (dired dir))
;;   (defun anything-generate-gtags-at-project (dir)
;;     "Generate gtags at project.  DIR is path."
;;     (shell-command-to-string (concat "cd " dir " && gtags -v")))
;;   (add-to-list 'alist-anything-for-files 'anything-c-source-cd-to-projects))

;; ;; dired-toggle
;; ;; briefのDiredは使わない
;; (require 'dired-toggle)
;; (setq-default dired-toggle-window-size 40)
;; ;; (global-set-key (kbd "C-x C-d") 'dired-toggle)
;; (define-key dired-toggle-mode-map (kbd "C-g") 'dired-toggle-action-quit)

;; ;; dired-toggleでは簡易表示
;; (defadvice dired-toggle (after dired-toggle-advice activate)
;;   "Into dired-hide-details-mode."
;;   (dired-hide-details-mode t))
;; (defadvice dired-toggle-action-quit (before dired-toggle-action-quit-advice activate)
;;   "Do not leave dired-hide-details-mode."
;;   (when dired-hide-details-mode (dired-hide-details-mode -1))
;;   (kill-buffer dired-toggle-buffer-name))

;; http://xyzzy.s53.xrea.com/reference/wiki.cgi?p=set%2Dsyntax%2Dstart%2Dmulti%2Dcomment
;; (set-syntax-start-multi-comment *c-mode-syntax-table* "/*")

;; ;; URL encode / decode region
;; (defun my-url-decode-region (beg end)
;;   "Decode region as hexified string."
;;   (interactive "r")
;;   (let ((str (buffer-substring beg end)))
;;     (delete-region beg end)
;;     (insert (url-unhex-string str))))
;; (defun my-url-encode-region (beg end)
;;   "Hexify region."
;;   (interactive "r")
;;   (let ((str (buffer-substring beg end)))
;;     (delete-region beg end)
;;     (insert (url-hexify-string str))))

;; dont let the cursor go into minibuffer prompt
;; reference | http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
;; (setq minibuffer-prompt-properties
;;       '(read-only t point-entered minibuffer-avoid-prompt
;;                   face minibuffer-prompt))

;; do not spell-check non-ascii characters
  ;; (add-to-list 'ispell-skip-region-alist '("[^\000-\377]"))

;; ;; ftp settings
;; (setup-after "ange-ftp"
;;   (when my-ftp-executable
;;     (setq ange-ftp-ftp-program-name my-ftp-executable)))

;;; ------------------------------------------------------------
;;; Elispのimenuで、Variablesは表示しない
;; (add-hook
;;   'elisp-mode-hook
;;     (assq-delete-all
;;       (car (assoc "Variables" imenu-generic-expression))
;;       imenu-generic-expression))

;; tramp-default-proxies-alistがおもしろそう
;; http://qiita.com/j_shirota/items/b66edbadf13f15efc13b

;; ;; 選択範囲の文字数を表示
;; ;; thx http://d.hatena.ne.jp/sonota88/20110224/1298557375
;; (defun count-lines-and-chars ()
;;   "Count chars to show modeline."
;;   (if mark-active
;;       (format "%d lines,%d chars "
;;               (count-lines (region-beginning) (region-end))
;;               (- (region-end) (region-beginning)))
;;     ""))
;; (add-to-list 'default-mode-line-format
;;              '(:eval (count-lines-and-chars)))

;; localize
;; (add-to-list 'Info-directory-list "~/info")

;; normalize-chars関係 確実に変換
;; (put-char-code-property ?， 'jisx0208 ?,)
;; (put-char-code-property ?． 'jisx0208 ?.)

;; これやるとむちゃくちゃ打鍵が遅くなるのでやめる
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-symbols t)))

;; ;; ac-anything
;; (when (require 'ac-anything nil t)
;;   (define-key ac-complete-mode-map (kbd "<M-tab>") 'ac-complete-with-anything))

;; いろいろくわしい！
;; https://github.com/d5884/dot-emacs/blob/master/init.el
;; ;; 波ダッシュを全角チルダへ(Windowsスタイル) (U+301C > U+FF5E)
;; (let ((table (make-translation-table-from-alist '((#x301c . #xff5e)))))
;;   (dolist (coding-system '(utf-8 cp932 utf-16le))
;;     (coding-system-put coding-system :decode-translation-table table)
;;     (coding-system-put coding-system :encode-translation-table table)))

;; syntax table
;; http://chiyanop.blogspot.jp/2014/12/emacs-syntax-table.html

;; Variable: (setq inhibit-field-text-motion t)
;; If this variable is non-nil, certain motion functions including forward-word, forward-sentence, and forward-paragraph ignore field boundaries.

;; 単語境界をもうちょっと細かくしたい（シンタックステーブルか？）というか、right-wordなどで行頭行末で引っかかってほしい
;; words-include-escapes,(skip-chars-forward "^$")(skip-chars-backward "^A-Za-z0-9")
;; http://www.fan.gr.jp/~ring/doc/elisp_20/elisp_30.html#SEC464

;; (defun back-to-indentation-or-beginning ()
;;   (interactive)
;;   (if this-command-keys-shift-translated
;;       (unless mark-active (push-mark nil t t))
;;     (when (and mark-active cua--last-region-shifted)
;;       (deactivate-mark)))
;;   (if (= (point) (progn (back-to-indentation) (point)))
;;       (beginning-of-line)))

;; (setq this-command-keys-shift-translated t)
;;   (handle-shift-selection)

;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; ;; new tab
;; (defun my-new-tab ()
;;   "My new tab"
;;   (interactive)
;;   (let ((bufname (format-time-string "%y%m%d%H%M%S" (current-time))))
;;     (get-buffer-create bufname)
;;     (switch-to-buffer bufname)
;;     (text-mode)))

;; 印刷設定
;; thx http://tam5917.hatenablog.com/entry/20120914/1347600433

;; (eval-when-compile (require 'ps-mule nil t))
;; (setq ps-paper-type        'a4 ;paper size
;;       ps-lpr-command       "lpr"
;;       ps-lpr-switches      '("-o Duplex=DuplexNoTumble")
;;       ps-printer-name      "hogehoge"   ; your printer name
;;       ps-multibyte-buffer  'non-latin-printer ;for printing Japanese
;;       ps-n-up-printing     2 ;print n-page per 1 paper

;;       ;; Margin
;;       ps-left-margin       20
;;       ps-right-margin      20
;;       ps-top-margin        20
;;       ps-bottom-margin     20
;;       ps-n-up-margin       20

;;       ;; Header/Footer setup
;;       ps-print-header      t            ;buffer name, page number, etc.
;;       ps-print-footer      nil          ;page number

;;       ;; font
;;       ps-font-size         '(9 . 10)
;;       ps-header-font-size  '(10 . 12)
;;       ps-header-title-font-size '(12 . 14)
;;       ps-header-font-family 'Helvetica    ;default
;;       ps-line-number-font  "Times-Italic" ;default
;;       ps-line-number-font-size 6

;;       ;; line-number
;;       ps-line-number       t ; t:print line number
;;       ps-line-number-start 1
;;       ps-line-number-step  1
;;       )

;; ;; 英語
;; ;; Thx Http://Tech.Basicinc.Jp/Mac/2013/08/04/Linux_Command/
;; (Defvar Ac-English-Cache
;;   (Ac-File-Dictionary "/Usr/Share/Dict/Words"))
;; (Defvar Ac-English-Dict
;;   '((Candidates . Ac-English-Cache)))

;; tramp関係
;; (setq explicit-shell-file-name "bash")
;; (add-to-list 'tramp-remote-path "/usr/local/bin/bash")
;; (shell-command-to-string "/usr/local/bin/bash/pwd")
;; (insert (format "%s" shell-prompt-pattern))

;; ;;; ------------------------------------------------------------
;; ;;; 次/前の空行
;; ;; gist-description: Emacs(Elisp): forward/backward-paragraphだとparagraph判定がおそらくシンタックステーブル依存になり、字義通りの「次の空行」にならないので、別途用意。選択範囲をものぐさして作りたいので、ちょっとfork。thx https://gist.github.com/jewel12/2873112
;; ;; gist-id: ad27b19dd3779ccc1ff2
;; ;; gist-name: move(region)-to-next(previous)-blank-line.el
;; ;; gist-private: nil

;; (defun blank-line? ()
;;   "Check is current line blank."
;;   ;; (message "%s" (substring-no-properties (thing-at-point 'line)))
;;   (string-match "^\n$" (substring-no-properties (thing-at-point 'line))))

;; (defun move-to-next-blank-line ()
;;   "Move to next blank line."
;;   (interactive)
;;   (progn (forward-line 1)
;;          (unless (blank-line?) (move-to-next-blank-line))))

;; (defun region-to-next-blank-line ()
;;   "Make region to next blank line."
;;   (interactive)
;;   (when (and (not (memq last-command '(region-to-next-blank-line)))
;;              (not mark-active))
;;     (set-mark-command nil))
;;   (move-to-next-blank-line))

;; (defun move-to-previous-blank-line ()
;;   "Move to previous blank line."
;;   (interactive)
;;   (progn (forward-line -1)
;;          (unless (blank-line?) (move-to-previous-blank-line))))

;; (defun region-to-previous-blank-line ()
;;   "Make region to previous blank line."
;;   (interactive)
;;   (when (and (not (memq last-command '(region-to-previous-blank-line)))
;;              (not mark-active))
;;     (set-mark-command nil))
;;   (move-to-previous-blank-line))

;; ;; (global-set-key (kbd "<C-up>") 'move-to-previous-blank-line)
;; ;; (global-set-key (kbd "<C-down>") 'move-to-next-blank-line)
;; ;; (global-set-key (kbd "<C-S-up>") 'region-to-previous-blank-line)
;; ;; (global-set-key (kbd "<C-S-down>") 'region-to-next-blank-line)

;; ;; C-aで、開始場所と先頭をトグル
;; ;; thx http://qiita.com/ShingoFukuyama/items/62269c4904ca085f9149
;; (defun my-goto-line-beginning-or-indent (&optional position)
;;   "Goto line beginning or indent.  POSITION is optical."
;;   (interactive)
;;   (or position (setq position (point)))
;;   (let (($starting-position (progn (back-to-indentation) (point))))
;;     (if (eq $starting-position position)
;;         (move-beginning-of-line 1))))

;; (global-set-key (kbd "C-a") 'my-goto-line-beginning-or-indent)

;; update GTAGS
;; thx http://qiita.com/yewton/items/d9e686d2f2a092321e34
;; (defun update-gtags1 (&optional prefix)
;;   "Update gtags.  PREFIX."
;;   (interactive "P")
;;   (let ((rootdir (gtags-get-rootpath))
;;         (args (if prefix "-v" "-iv")))
;;     (when rootdir
;;       (let* ((default-directory rootdir)
;;              (buffer (get-buffer-create "*update GTAGS*")))
;;         (save-excursion
;;           (set-buffer buffer)
;;           (erase-buffer)
;;           (let ((result (process-file "gtags" nil buffer nil args)))
;;             (if (= 0 result)
;;                 (message "GTAGS successfully updated.")
;;               (message "update GTAGS error with exit status %d" result))))))))
;; (add-hook 'after-save-hook 'update-gtags)

;; ;; eww で色を反映しない
;; (defvar eww-disable-colorize t)
;; (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
;;   "Shr colorize region disable.  ORIG START END FG BG (as _)."
;;   (unless eww-disable-colorize
;;     (funcall orig start end fg)))
;; (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
;; (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
;; (defun eww-disable-color ()
;;   "Dsiable color at eww."
;;   (interactive)
;;   (setq-local eww-disable-colorize t)
;;   (eww-reload))

;; ;;; ------------------------------------------------------------
;; (require 'js2-mode)
;; (add-hook 'js2-mode-hook '(lambda ()
;; 														(flycheck-mode t)
;; 														(setq tab-width 2)
;; 														(setq indent-tabs-mode t)))
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (define-key js2-mode-map (kbd "M-up") 'previous-error))
;; (define-key js2-mode-map (kbd "M-down") 'next-error)

;; ;;; ------------------------------------------------------------
;; ;; 関数名をヘッダに表示
;; (setq which-func-modes t)
;; (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
;; (setq-default header-line-format '(which-func-mode ("" which-func-format)))

;; ;;; ------------------------------------------------------------
;; ;; 単語境界を細かく。どうもシンタックステーブルの問題らしい。
;; ;; 文字カテゴリの作成
;; ;; http://smallsteps.seesaa.net/article/123661899.html
;; (define-category ?U "Upper case")
;; (define-category ?L "Lower case")
;; ;; 文字の登録。とりあえずはAからZまでの英字のみ。
;; (modify-category-entry (cons ?A ?Z) ?U)
;; (modify-category-entry (cons ?a ?z) ?L)
;; 小文字に大文字が続く場合を単語境界とする。
;; (add-to-list 'word-separating-categories (cons ?L ?U))

;; ;;; ------------------------------------------------------------
;; ;; my-anything-c-source-buffers-list
;; (defvar my-anything-c-source-buffers-list
;; 	`((name . "Buffers")
;; 		(candidates . (lambda ()
;; 										(buffer-list)))))
;; ;; my-anything-c-source-buffers-list
;; ;; (buffer-list)
;; ;; anything-c-source-buffers-list
;; ;; (anything-c-buffer-list)
;; ;; (anything-c-highlight-buffers)

;; ;;; ------------------------------------------------------------
;; ;; http://d.hatena.ne.jp/tomoya/20101213/1292166026
;; (defun create-hyper-link-at-point-url ()
;;   "カーソル位置のURLを HTML でマークアップする"
;;   (interactive)
;;   (let* ((bounds (bounds-of-thing-at-point 'url))
;;          (start (car bounds))
;;          (end (cdr bounds))
;;          (link (format "<a href=\"%s\"></a>" (thing-at-point 'url))))
;;     (delete-region start end)
;;     (insert link)))

;; ;;; ------------------------------------------------------------
;; ;; 日本語の自動判別
;; ;; thx http://dev.ariel-networks.com/articles/emacs/part1/
;; (setq grep-host-defaults-alist nil)
;; (when mac-carbon-version-string
;; 	(setq grep-template "lgrep <C> -n <R> <F> <N>")
;; 	(setq grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e lgrep <C> -n <R> <N>"))

;; ;; Windows
;; (defvar quote-argument-for-windows-p t "enables `shell-quote-argument' workaround for windows.")
;; (defadvice shell-quote-argument (around shell-quote-argument-for-win activate)
;; 	"workaround for windows."
;; 	(if quote-argument-for-windows-p
;; 			(let ((argument (ad-get-arg 0)))
;; 				(setq argument (replace-regexp-in-string "\\\\" "\\\\" argument nil t))
;; 				(setq argument (replace-regexp-in-string "'" "'\\''" argument nil t))
;; 				(setq ad-return-value (concat "'" argument "'")))
;; 		ad-do-it)
;; 	(setq grep-template "lgrep -Ks -Os <C> -n <R> <F> <N>")
;; 	(setq grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e lgrep -Ks -Os <C> -n <R> <N>")
;; thx http://qiita.com/ybiquitous/items/2f2206ff7a557c4cbc11
;; (setq find-program "\"C:\\Program Files\\Git\\usr\\bin\\find.exe\""
;;       grep-program "\"C:\\Program Files\\Git\\usr\\bin\\grep.exe\""
;;       null-device "/dev/null"))

;; ;;; ------------------------------------------------------------
;; ;;; experiment 検索履歴
;; ;; (global-set-key (kbd "C--") 'sc/anything-grep)
;; ;;; 検索か置換をしたら、候補をファイルに保存する
;; ;;; thx undohist
;; (defcustom sc/history-directory
;; 	(expand-file-name
;; 	 (concat
;; 		(if (boundp 'user-emacs-directory) user-emacs-directory "~/.emacs.d")
;; 		"/es-hist"))
;; 	"A directory being stored searched/replaced history files.")
;; (defvar sc/history-filename (concat sc/history-directory "/es-hist.dat"))
;; ;; (defun sc/initialize ()
;; ;; 	"Initialize editable seacrh directory."
;; ;; 	(interactive)
;; ;; 	(if (not (file-directory-p sc/history-directory))
;; ;; 			(make-directory sc/history-directory t))
;; ;; 	(if (not (file-p (concat sc/history-directory "/es-hist.txt"))
;; ;; 			(make-file sc/history-directory t))))
;; ;; undohistでは、hookを使って、saveしているので、そうするのがよいか。
;; ;;; multibyte-base64-encode-string
;; (defun multibyte-base64-encode-string (str)
;; 	"Multibyte base64 encode string.  STR."
;; 	(interactive)
;; 	(base64-encode-string (encode-coding-string str 'raw-text) t))

;; ;;; multibyte-base64-decode-string
;; (defun multibyte-base64-decode-string (str)
;; 	"Multibyte base64 decode string.  STR."
;; 	(interactive)
;; 	(decode-coding-string (base64-decode-string str) 'utf-8))

;; ;;; sc/hist-load
;; (defun sc/hist-load ()
;; 	"Load es history."
;; 	(interactive)
;; 		(with-temp-buffer
;; 			(insert-file-contents sc/history-filename)
;; 			(if (<= (point-max) 2)
;; 					(list)
;; 				(read (buffer-string)))))

;; ;;; sc/hist-save
;; (defun sc/hist-save ()
;; 	"Save es history."
;; 	(interactive)
;; 	(let
;; 			(search-str
;; 			 replace-str
;; 			 history)
;; 		(when (and (windowp (get-buffer-window sc/search-str-buffer))
;; 							 (windowp (get-buffer-window sc/replace-str-buffer)))
;; 			(setq search-str (multibyte-base64-encode-string (sc/get-str-from-window "search")))
;; 			(setq replace-str (multibyte-base64-encode-string (sc/get-str-from-window "replace")))
;; 			(when search-str
;; 				(with-temp-buffer
;; 					(insert-file-contents sc/history-filename)
;; 					(setq history (sc/hist-load))
;; 					(add-to-list 'history (list search-str replace-str))
;; 					(insert (format "%s" history))
;; 					(write-region (point-min) (point-max) sc/history-filename nil 0))))))
;; ;;; 履歴から検索置換文字列を復活
;; (defun sc/hist-prev ()
;; 	"Call previous set."
;; 	(let (history
;; 				current)
;; 		(with-temp-buffer
;; 			(insert-file-contents sc/history-filename)
;; 			(setq history (sc/hist-load))
;; 			(setq current (car history))
;; 			;; (cdr history)
;; 			(message "%s" current)
;; 			)))
;; ;; 保存すべき文字列がないときの処理
;; ;; すでに保存されているセットの場合。古いものを削除して、一番上に
;; ;; 保存すべき数の上限をdefvarで
;; ;; (sc/hist-save)
;; ;; (sc/hist-prev)
;;   ;; (if (consp buffer-undo-list)
;;   ;;     (let ((file (make-undohist-file-name (buffer-file-name)))
;;   ;;           (contents `((digest . ,(md5 (current-buffer)))
;;   ;;                       (undo-list . ,(undohist-encode buffer-undo-list)))))
;;   ;;       (with-temp-buffer
;;   ;;         (print contents (current-buffer))
;;   ;;         (write-region (point-min) (point-max) file nil 0)
;;   ;;         (set-file-modes file ?\600)))))

;; ;; (defcustom es-is-next-window-by-tab nil
;; ;; 	"*Mac-like behavior."
;; ;; 	:group 'editable-search
;; ;; 	:type 'boolean)
;; ;;; es-is-next-window-by-tab
;; ;; (when es-is-next-window-by-tab
;; ;; 	(define-key editable-search-mode-map [tab] 'es-next-windows-dwim))
;; ;; (defun es-next-windows-dwim ()
;; ;; 	"Next windows dwim."
;; ;; 	(interactive)
;; ;; 	(when (or (equal (selected-window) (get-buffer-window es-search-str-buffer))
;; ;; 					(equal (selected-window) (get-buffer-window es-replace-str-buffer)))
;; ;; 			(select-window (next-window))))

;; ;;; ------------------------------------------------------------
;; ;;; 自分好みのタブの振る舞い
;; ;;; read-onlyバッファではリンクの移動
;; ;;; ミニバッファだったらミニバッファ補完
;; ;;; 文字入力中だったらac-start
;; ;;; なにもなければインデントを試みる
;; ;;; インデントしてキャレットの移動がなければ\tを挿入
;; ;; smart-tab
;; ;; コンテキストに応じたtabキー。auto-completeと共存
;; ;; (require 'smart-tab)
;; ;; (global-smart-tab-mode)

;; (defun my-tab-dwim ()
;; "Insert tab or indentation or jump to link or auto-complete."
;; (interactive)
;; ;; (message "%s" (concat (format "%s" last-input-event) " - " (format "%s" last-command)))
;; ;; (message "%s" (concat (format "%s" initial-point) "-" (format "%s" (point))))
;; (let ((initial-point (point))
;; 			(is-line (if (region-active-p) nil t))
;; 			(beg-point-line (save-excursion
;; 												(beginning-of-line)
;; 												(point))))
;; 	(cond
;; 	 ;; read onlyバッファだったら次のリンク
;; 	 (buffer-read-only
;; 		(forward-button 1 t))
;; 	 ;; ミニバッファだったらミニバッファ補完
;; 	 ((minibufferp (current-buffer))
;; 		(minibuffer-complete))
;; 	 ;; 文字入力の途中（タブ以外）だったらac-startを試みる
;; 	 ((and
;; 		 (require 'auto-complete)
;; 		 (memq last-command '(self-insert-command))
;; 		 (not (memq last-command '(my-tab-dwim))))
;; 		(auto-complete-mode t)
;; 		(ac-start))
;; 	 ;; 直前の操作がタブキーだったらタブを挿入
;; 	 ;; キャレットが先頭でなかったらタブを挿入
;; 	 ((or (memq last-command '(my-tab-dwim))
;; 				(not (eq beg-point-line (point))))
;; 		(insert "\t"))
;; 	 ;; indent-for-tab-commandを試みる
;; 	 (t
;; 		(indent-for-tab-command)
;; 		;; 範囲指定がなく、indent-for-tab-commandでカーソルが移動しないときはメッセージを表示してタブ挿入
;; 		(when (and is-line (eq initial-point (point)))
;; 			(message "No indetentation. Instert Tab.")
;; 			(insert "\t"))))))
;; (bind-key* "<tab>" 'my-tab-dwim)

;; ;;; ------------------------------------------------------------
;; ;;; isearchに文字列をセット
;; ;;; http://blog.livedoor.jp/tek_nishi/archives/4866943.html
;; (defun my-isearch-get-word()
;; 	"Set region to isearch."
;; 	(interactive)
;; 	(if(not isearch-mode)
;; 			(progn
;; 				(call-interactively 'isearch-forward)
;; 				(let ((string
;; 							 (if (and transient-mark-mode mark-active)
;; 									 (buffer-substring (region-beginning) (region-end))
;; 								 (thing-at-point 'symbol))))
;; 					(deactivate-mark)
;; 					(isearch-yank-string string)))))
;; (bind-key* "C-S-s" 'my-isearch-get-word)

;; ;;; ------------------------------------------------------------
;; ;; 入力モードが英語の時はカーソルの色をWhiteに、日本語の時はfirebrickにする
;; ;; thx http://masutaka.net/chalow/2015-01-04-1.html
;; ;; これを有効にするためには、Mac OS Xの入力ソースを適切なものにしておくこと
;; ;; thx http://pc-karuma.net/mac-keyboard-input-source/
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

;; ;;; ------------------------------------------------------------
;; ;;; control+shift+cursorでウィンドウ内バッファ履歴
;; ;; thx http://pc12.2ch.net/test/read.cgi/unix/1261307488/

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

;; ;;; ------------------------------------------------------------
;; ;; (setq hl-line-face 'underline)
;; ;; (global-hl-line-mode)
;; ;; 下線だと、日本語入力時の候補領域がわかりづらいのでやめる。

;; ;;; ------------------------------------------------------------
;; ;; カラーリングをテストする
;; ;; http://d.hatena.ne.jp/buzztaiki/20111209/1323444755

;; (defun font-lock-user-keywords (mode &optional keywords)
;;   "Add user highlighting to MODE to KEYWORDS.
;; See `font-lock-add-keywords' and `font-lock-defaults'."
;;   (unless mode
;;     (error "Mode should be non-nil"))
;;   (font-lock-remove-keywords mode (get mode 'font-lock-user-keywords))
;;   (font-lock-add-keywords mode keywords)
;;   (put mode 'font-lock-user-keywords keywords))

;; ;;; ------------------------------------------------------------
;; ;; rainbow-modeで16進数だけカラーリング
;; (font-lock-remove-keywords
;;    nil
;;    `(,@rainbow-x-colors-font-lock-keywords
;;      ,@rainbow-latex-rgb-colors-font-lock-keywords
;;      ,@rainbow-r-colors-font-lock-keywords
;;      ,@rainbow-html-colors-font-lock-keywords
;;      ,@rainbow-html-rgb-colors-font-lock-keywords))

;; ;;; ------------------------------------------------------------
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

;; ;;; ------------------------------------------------------------
;; ;;; Memo:
;; ;; anything-c-source-google-suggest（面白いのだけど使いどころがない）
;; ;; M-x install-packageで、入らないものがあるが、
;; ;; M-x list-packagesで、C-sで探し、ページ移動の後installでなら入る。

;; ;;; ------------------------------------------------------------
;; ;;; Memo2:
;; ;; tempbuf（idle buffer）を自動的にkill-bufferしてくれるelispだけど、
;; ;; 結構不意に必要なbufferをkillしていることがあるので、使わない方向で。
;; ;; multi-termもよさそうだけど、やっぱりterminalを使う。

;; ;;; ------------------------------------------------------------
;; ;;; Memo3:
;; ;; しばらくauto-install.elを使っていたが、anythingもpackageで入るのでpackageに。
;; ;; auto installはつかわなくてもいける
;; ;; wget http://www.emacswiki.org/emacs/download/auto-install.el
;; ;; M-x byte-compile-file RET ~/.emacs.d/elisp/auto-install.el RET
;; ;; (add-to-list 'load-path "~/.emacs.d/elisp")
;; ;; (require 'auto-install)
;; ;; (setq auto-install-directory "~/.emacs.d/elisp")
;; ;; (auto-install-update-emacswiki-package-name t)
;; ;; (auto-install-compatibility-setup)
;; ;; M-x auto-install-batch RET anything RET
;; ;; wget http://www.ne.jp/asahi/alpha/kazu/pub/emacs/phpdoc.el


;; ;;; ------------------------------------------------------------
;; ;;; ウィンドウ構成を変えようとしたら検索置換窓を閉じる
;; (add-hook 'window-configuration-change-hook 'es-delete-window-fn)
;; (defun es-delete-window-fn ()
;; 	"Delete search mode windows."
;; 	(unless es-ignore-delete-window-hook
;; 		(select-window es-target-window)
;; 		(delete-other-windows)))

;; ;;; ------------------------------------------------------------
;; ;;; 選択範囲がある状態でshiftなしのカーソルが打鍵されたらリージョンを解除
;; ;; macふうの挙動だが、Emacsふうでないので、ちょっと様子見しつつ運用
;; (defcustom is-deactivate-region-by-cursor t
;; 	"*Mac-like behavior."
;; 	:group 'Convenience
;; 	:type 'boolean)

;; ;;; ------------------------------------------------------------
;; ;;; 新規フレーム作成 (cmd+shift+n)
;; (defun create-new-frame ()
;; 	"Create new frame."
;; 	(interactive)
;; 	(switch-to-buffer-other-frame "*new1*")
;; 	(show-line-number))
;; (bind-key* "s-N" 'create-new-frame)
;; (add-hook 'after-make-frame-functions 'show-line-number)

;; ;;; 釣り合う行カッコが画面外だったらミニバッファに表示
;; ;; thx http://emacswiki.org/emacs/ShowParenMode
;; (defadvice show-paren-function
;; 		(after show-matching-paren-offscreen activate)
;; 	"If the matching paren is offscreen, show the matching line in theecho area.  Has no effect if the character before point is not ofthe syntax class ')'."
;; 	(interactive)
;; 	(let* ((cb (char-before (point)))
;; 				 (matching-text (and cb
;; 														 (char-equal (char-syntax cb) ?\) )
;; 														 (blink-matching-open))))
;; 		(when matching-text (message matching-text))))

;; ;;; 右ボタンの割り当て(押しながらの操作)をはずす。
;; ;;; thx http://cave.under.jp/_contents/emacs.html#60
;; (if window-system
;; 		(progn
;; 			(global-unset-key [down-mouse-3])
;; 			;; マウスの右クリックメニューを出す(押して、離したときにだけメニューが出る)
;; 			(defun bingalls-edit-menu (event)
;; 				(interactive "e")
;; 				(popup-menu menu-bar-edit-menu))
;; 			(bind-key* "<mouse-3>" 'bingalls-edit-menu)))

;; ;; モードラインにカレントディレクトリを表示する
;; (let ((ls (member 'mode-line-buffer-identification mode-line-format)))
;; 	(setcdr ls
;; 					(cons
;; 					 '(:eval (concat " (" (abbreviate-file-name default-directory) ")"))
;; 					 (cdr ls))))

;; ;;; よくあるマイナーモードを非表示
;; ;; thx http://qiita.com/tadsan/items/8b5976682b955788c262
;; ;; これは一通り処理が終わった後呼ぶ必要がある。
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

;; ;;; アスタリスクで終わるバッファ名を除いたリストを取得
;; (defun eliminated-buffers ()
;; 	"Eleminate buffers."
;; 	(let (result
;; 				(tmp-buffers (buffer-list)))
;; 		(dolist (buf tmp-buffers result)
;; 			(unless (string= "*" (substring (format "%s" buf) -1 nil))
;; 				(add-to-list 'result buf)))))

;; ;;; ------------------------------------------------------------
;; ;;; f2キーでmessageと今のバッファをトグル
;; (bind-key* "<f2>" (lambda () (interactive)
;; 											 (let (current-buffer-for-return)
;; 												 (if (eq (selected-window) (get-buffer-window "*Messages*"))
;; 														 (switch-to-buffer current-buffer-for-return)
;; 													 (setq current-buffer-for-return (current-buffer))
;; 													 (switch-to-buffer "*Messages*")))))

;; ;;; ------------------------------------------------------------
;; ;;; mac like new window (cmd+n)
;; ;; cmd+n でウィンドウを増やす。分割方法は対話式
;; (defun create-new-window-intaractive (act)
;; 	"Mac like new window (cmd+n).  ACT is interactive."
;; 	(interactive "nchoose (holizntal:1, vertical:2):")
;; 	(cond ((eq act 2) (split-window-horizontally))
;; 				(t (split-window-vertically))))
;; (bind-key* "s-n" 'create-new-window-intaractive)

;; ;;; ------------------------------------------------------------
;; ;;; eldoc
;; ;; 重い……
;; (require 'eldoc-extension)
;; (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
;; (setq eldoc-idle-delay 0.2)
;; (setq eldoc-minor-mode-string "")

;; ;;; ------------------------------------------------------------
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

;; ;;; ------------------------------------------------------------
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

;; ;;; ------------------------------------------------------------
;; ;;; create-temporary-buffer
;; ;; あたらしい空のバッファを作る (cmd+t)
;; (defun create-temporary-buffer ()
;; 	"Create temporal buffer."
;; 	(interactive)
;; 	(switch-to-buffer (generate-new-buffer "new"))
;; 	(global-auto-complete-mode t))
;; (global-set-key (kbd "s-t") 'create-temporary-buffer) ; (cmd+t)

;; ;;; ------------------------------------------------------------
;; ;;; mac like close window (cmd+w)
;; ;; cmd+wで、開いているウィンドウを閉じる。単一のバッファなら、変更を確認してバッファを閉じる
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

;; ;;; ------------------------------------------------------------
;; ;;; Macの辞書で検索
;; ;; thx http://moyashi.air-nifty.com/hitori/2007/12/emacscarbon_ema_5f82.html
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

;; ;;; ------------------------------------------------------------
;; ;;; Rictyを等幅で使う
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

;; ;;; Rictyの準備の仕方
;; ;; thx Rictyなるものがあるらしい | cozy attic
;; ;; https://cozyattic.wordpress.com/2013/07/24/ricty%E3%81%AA%E3%82%8B%E3%82%82%E3%81%AE%E3%81%8C%E3%81%82%E3%82%8B%E3%82%89%E3%81%97%E3%81%84/
;; ; sudo port install fontforge
;; ; で、fontforgeをインストール
;; ; fontforge -version
;; ; で、インストールの確認。
;; ; https://github.com/yascentur/Ricty/tree/3.2.2
;; ; で、Ricty-3.2.2.zipをダウンロード。
;; ; http://levien.com/type/myfonts/inconsolata.html
;; ; で、OpenType fileのInconsolata.otfをダウンロード。
;; ; http://mix-mplus-ipa.sourceforge.jp/migu/
;; ; で、migu-1m-20150712.zipのダウンロード。
;; ; 全て解凍し、Inconsolata.otf、migu-1m-bold.ttf、migu-1m-regular.ttfをRictyのフォルダへ移動
;; ; ターミナルでRictyフォルダに移動
;; ; sh ricty_generator.sh Inconsolata.otf migu-1m-regular.ttf migu-1m-bold.ttf
;; ; とする。僕はデフォルトだと全角スペースが目立ちすぎるので、
;; ; sh ricty_generator.sh -z Inconsolata.otf migu-1m-regular.ttf migu-1m-bold.ttf
;; ; として、全角スペースを不可視にしている。
;; ; https://github.com/yascentur/Ricty/tree/3.2.2
;; ; のREADMEには、このシェルスクリプトのオプションがあるので、参考にすると良い。


;; ;; ファイルが #! から始まる場合、+xを付けて保存する
;; (add-hook 'after-save-hook
;;           'executable-make-buffer-file-executable-if-script-p)

;; [backword|forward]-paragraphだと、syntax table依存で、htmlなどでまどろっこしい
;; (global-set-key (kbd "<C-up>") 'backward-paragraph)
;; (global-set-key (kbd "<C-down>") 'forward-paragraph)
;; (global-set-key (kbd "M-right") 'forward-symbol)
;; (global-set-key (kbd "M-left") (lambda () (interactive) (forward-symbol -1)))
