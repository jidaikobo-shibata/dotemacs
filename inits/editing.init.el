;;; editing.init.el --- init for editing
;;; Commentary:
;; provide editing.init.
;;; Code:

;;; ------------------------------------------------------------
;; リージョンを上書きできるようにする
(delete-selection-mode t)

;;; ------------------------------------------------------------
;; 選択範囲を可視化
(setq transient-mark-mode t)

;;; ------------------------------------------------------------
;; font-lock-mode
(global-font-lock-mode t)

;;; ------------------------------------------------------------
;; sort-linesはcase insensitiveで
(setq-default sort-fold-case t)

;;; ------------------------------------------------------------
;; スクロールを一行ずつにする
(setq scroll-step 1)

;;; ------------------------------------------------------------
;; クリップボードを他のアプリケーションと共用にする
(setq select-enable-clipboard t)

;;; ------------------------------------------------------------
;; オートインデント無効
(when (functionp 'electric-indent-mode) (electric-indent-mode -1))

;;; ------------------------------------------------------------
;; whitespaceによるタブ文字の可視化と自動クリーンアップ
(global-whitespace-mode 1)
(setq whitespace-style '(tabs))
(setq-default whitespace-action '(auto-cleanup))

;;; ------------------------------------------------------------
;; タブ幅
(setq-default tab-width 2)

;;; ------------------------------------------------------------
;; backward-delete-char-untabifyは、タブをバラさない
(setq backward-delete-char-untabify-method nil)

;;; ------------------------------------------------------------
;; cua-modeの設定
(cua-mode t) ; C-RET
(setq-default cua-enable-cua-keys nil)

;;; ------------------------------------------------------------
;; kill-lineがkill ringをnewするのでdelete-lineにする
(global-set-key (kbd "C-k")
                (lambda ()
                  (interactive)
                  (delete-char (- (save-excursion (end-of-line) (point)) (point)))))

;;; ------------------------------------------------------------
;; align-regexp
; (global-set-key (kbd "<backtab>") 'align-regexp) ; Shift-tabはindentにしてみる
(global-set-key (kbd "<C-tab>") 'align-regexp)

;;; ------------------------------------------------------------
;; インデント整形
(global-set-key (kbd "s-}") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "s-]") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-}") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "s-{") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "s-[") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "C-{") 'indent-rigidly-left-to-tab-stop)

;;; ------------------------------------------------------------
;;; 複数箇所選択 - multiple-cursors and smartrep
(require 'multiple-cursors)
(require 'smartrep)

(declare-function smartrep-define-key "smartrep")

(define-key mc/keymap (kbd "<return>")
  (lambda () (interactive) (insert (char-to-string 10))))

(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

(global-unset-key "\C-t")

(smartrep-define-key global-map "C-t"
'(("C-t" . 'mc/mark-next-like-this)
  ("n"   . 'mc/mark-next-like-this)
  ("p"   . 'mc/mark-previous-like-this)
  ("m"   . 'mc/mark-more-like-this-extended)
  ("u"   . 'mc/unmark-next-like-this)
  ("U"   . 'mc/unmark-previous-like-this)
  ("s"   . 'mc/skip-to-next-like-this)
  ("S"   . 'mc/skip-to-previous-like-this)
  ("*"   . 'mc/mark-all-like-this)
  ("d"   . 'mc/mark-all-like-this-dwim)
  ("i"   . 'my/mc/insert-numbers)
  ("o"   . 'mc/sort-regions)
  ("O"   . 'mc/reverse-regions)))

;;; Insert specific serial number
;; thx http://qiita.com/ShingoFukuyama/items/3ad7e24cb2d8f55b4cc5
(defvar my/mc/insert-numbers-hist nil)
(defvar my/mc/insert-numbers-inc 1)
(defvar my/mc/insert-numbers-pad "%01d")

(defun my/mc/insert-numbers (start inc pad)
  "Insert increasing numbers for each cursor specifically.  START, INC, PAD."
  (interactive
   (list (read-number "Start from: " 0)
         (read-number "Increment by: " 1)
         (read-string "Padding (%01d): " nil my/mc/insert-numbers-hist "%01d")))
  (setq mc--insert-numbers-number start)
  (setq my/mc/insert-numbers-inc inc)
  (setq my/mc/insert-numbers-pad pad)
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor
    'my/mc--insert-number-and-increase
    cursor)))

(defun my/mc--insert-number-and-increase ()
  "Insert number and increase."
  (interactive)
  (insert (format my/mc/insert-numbers-pad mc--insert-numbers-number))
  (setq mc--insert-numbers-number (+ mc--insert-numbers-number my/mc/insert-numbers-inc)))

;;; ------------------------------------------------------------
;;; やっぱりキル時にリージョンを残したい……。
;; gist-description: Emacs(Elisp): Preserve region when kill. 他のエディタだと選択範囲を作った後コピーしても選択範囲が解除されないが、Emacsは解除されちゃう。1年以上使っていてもどうしてもこれには慣れることができなかったので、選択範囲をキープするように変更。
;; gist-id: 94f27670afed23696c6a2d0774982b01
;; gist-name: preserve-region-when-kill.el
;; gist-private: nil
(defun f--around--cua-copy-region (cua-copy-region arg)
  "Keep Region at kill.  CUA-COPY-REGION, ARG."
  (let ((beg (region-beginning))
        (end (region-end)))
    (funcall cua-copy-region arg)
    (goto-char beg)
    (set-mark (point))
    (goto-char end)
    (setq deactivate-mark nil)))
(advice-add 'cua-copy-region :around 'f--around--cua-copy-region)

;;; ------------------------------------------------------------
;;; 選択範囲がある状態でshiftなしのカーソルが打鍵されたらリージョンを解除
;; macふうの挙動だが、Emacsふうでないので、ちょっと様子見しつつ運用
;; C-@とどちらをとるか悩ましい

(defvar is-deactivate-region t)
(when is-deactivate-region
  ;; regionの解除advice版
  (defadvice previous-line (before deactivate-region activate)
    "Deactivate Region by cursor."
    (my-deactivate-region))
  (defadvice next-line (before deactivate-region activate)
    "Deactivate Region by cursor."
    (my-deactivate-region))
  (defadvice left-char (before deactivate-region activate)
    "Deactivate Region by cursor."
    (my-deactivate-region))
  (defadvice right-char (before deactivate-region activate)
    "Deactivate Region by cursor."
    (my-deactivate-region))

  ;; リージョン解除関数
  (defun my-deactivate-region ()
    "Logic of deactivate region by cursor."
    ;; (interactive "^")
    ;; (message "l: %s c: %s" last-input-event this-command)
    ;; (message "m:%s r:%s u:%s" mark-active (region-active-p) (use-region-p))
    ;; (message "s:%s e:%s" (region-beginning) (region-end))

    (when (and (not (memq last-input-event '(S-left S-right S-down S-up C-S-left C-S-right C-S-down C-S-up M-S-left M-S-right M-S-down M-S-up)))
               mark-active)
      (cond
       ((memq last-input-event '(right down))
        (goto-char (region-end)))
       ((memq this-command '(left-char previous-line))
        (goto-char (region-beginning))))
      (deactivate-mark))))

;;; ------------------------------------------------------------
;; defadvice-indent-for-tab-command
;; gist-description: Emacs(Elisp): To integrate indent style, delete existing whitespaces before indent-for-tab-command. indent-for-tab-commandの前に存在する行頭ホワイトスペースを削除することでインデントスタイルを統一する
;; gist-id: 604173d11ff376036635fd4811df6abb
;; gist-name: defadvice-indent-for-tab-command.el
;; gist-private: nil

(defadvice indent-for-tab-command (around advise-indent-for-tab-command activate)
  "To integrate indent style, delete existing whitespaces before indentation."
  (let (beg
        end
        (end-line nil))
    (cond
     ((use-region-p)
      (setq beg (region-beginning)
            end (region-end)
            end-line (line-number-at-pos end)))
     (t
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point))))

    (perform-replace "^[\t ]+" "" nil t nil nil nil beg end)
    (goto-char beg)
    (set-mark-command nil)
    (goto-char end)

    ad-do-it

    (when end-line (forward-line -1)) ;; why should i have to do minus?
    (back-to-indentation)))

(global-set-key (kbd "<backtab>") 'indent-for-tab-command)

;;; Auto Setting `indent-tabs-mode' Variable
;; thx https://github.com/moriyamahiroshi/hm-dot-emacs-files/blob/master/init.el

(defvar inside-string-or-comment-p)
(defvar re-search-forward-without-string-and-comments)

(defun inside-string-or-comment-p ()
  "Inside string or comment p."
  (let ((state (parse-partial-sexp (point-min) (point))))
    (or (nth 3 state) (nth 4 state))))

(defun re-search-forward-without-string-and-comments (&rest args)
  "Re search forward without string and comments.  ARGS."
  (let ((value (apply #'re-search-forward args)))
    (if (and value (inside-string-or-comment-p))
        (apply #'re-search-forward-without-string-and-comments args)
      value)))

(defun my-buffer-indent-tabs-code-p (&optional buffer)
  "Check first indent char.  BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (and (re-search-forward-without-string-and-comments "^[ \t]" (point-max) t)
               (string= (match-string 0) "\t")))))))

(defun my-set-indent-tabs-mode ()
  "Set indent tab mode."
  (setq indent-tabs-mode (my-buffer-indent-tabs-code-p)))

(add-hook 'emacs-lisp-mode-hook #'my-set-indent-tabs-mode)
(add-hook 'php-mode-hook #'my-set-indent-tabs-mode)
(add-hook 'conf-mode-hook #'my-set-indent-tabs-mode)
(add-hook 'sh-script-mode-hook #'my-set-indent-tabs-mode)

;;; ------------------------------------------------------------
;; indent-tabs-modeをtoggle
(defun toggle-indent-tabs-mode ()
  "For `which-key'."
  (interactive)
  (if indent-tabs-mode
      (progn (setq indent-tabs-mode nil)
             (message "indent by SPACE"))
    (setq indent-tabs-mode t)
    (message "indent by TAB")))
(global-set-key (kbd "C-c i") 'toggle-indent-tabs-mode)

;;; ------------------------------------------------------------
;;; 行／選択範囲の複製 (cmd+d)
;; gist-description: Emacs(Elisp): duplicate region or line.  if same command repeated, then duplicate sate strings.  選択範囲がある場合は選択範囲を、選択範囲がない場合は、行を複製します。繰り返した場合、同じ文字列を複製し続けます。
;; gist-id: 297fe973cde66b384fa1
;; gist-name: duplicate-region-or-line.el
;; gist-private: nil

(defvar previous-duplicate-region-or-line nil)
(defvar previous-duplicate-region-or-line-was-line nil)

(defun duplicate-region-or-line ()
  "Duplicate the region if active, otherwise duplicate the current line."
  (interactive)
  (let* ((region-active (region-active-p))
         (is-repeat (eq last-command this-command))
         (beg (if region-active (region-beginning) (line-beginning-position)))
         (end (if region-active (region-end) (line-end-position)))
         (strings (buffer-substring-no-properties beg end))
         (is-line (if is-repeat
                      previous-duplicate-region-or-line-was-line
                    (not region-active))))
    ;; Adjust strings for line duplication
    (when is-line
      (setq strings (concat "\n" strings))
      (end-of-line))
    ;; Insert the duplicated text
    (if is-repeat
        (insert previous-duplicate-region-or-line)
      (insert strings)
      (setq previous-duplicate-region-or-line strings))
    ;; Store whether it was a line duplication
    (setq previous-duplicate-region-or-line-was-line is-line)))

(global-set-key (kbd "s-d") 'duplicate-region-or-line)

;;; ------------------------------------------------------------
;;; 選択範囲を[大文字|小文字|キャピタライズ]に

(global-set-key (kbd "s-U") 'upcase-region)
(global-set-key (kbd "s-L") 'downcase-region)
(global-set-key (kbd "s-C") 'capitalize-region)

;;; ------------------------------------------------------------
;;; 全角英数字を半角英数字に、半角カナを全角に、UTF-8の濁点分離を直す
;; http://d.hatena.ne.jp/nakamura001/20120529/1338305696
;; http://www.sakito.com/2010/05/mac-os-x-normalization.html
;; gist-description: Emacs(Elisp): 全角英数字を半角英数字に、半角カナを全角に、UTF-8の濁点分離を直す。
;; gist-id: 08a752b04107dbc50ef5
;; gist-name: normalize-chars.el
;; gist-private: nil

(require 'ucs-normalize)
;;(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)
(defun normalize-chars ()
  "Normarize chars."
  (interactive)
  ;; 選択範囲があればそこを対象にする
  (let (type
        beg
        end)
    (if (region-active-p)
        (progn
          (setq beg (region-beginning))
          (setq end (region-end)))
      (progn
        (setq type (read-string "normalize whole buffer?(y, n): " nil))
        (if (string= type "y")
            (progn
              (setq beg (point-min))
              (setq end (point-max)))
          (error "Error: no target region"))))
    (japanese-zenkaku-region beg end t)
    (japanese-hankaku-region beg end t)
    (ucs-normalize-NFC-region beg end)))

;; 音引、句読点等を除外
(eval-after-load "japan-util"
  '(progn
    (put-char-code-property ?ー 'ascii nil)
    (put-char-code-property ?〜 'ascii nil)
    (put-char-code-property ?〜 'ascii nil)
    (put-char-code-property ?～ 'ascii nil)
    (put-char-code-property ?（ 'ascii nil)
    (put-char-code-property ?） 'ascii nil)
    (put-char-code-property ?、 'ascii nil)
    (put-char-code-property ?。 'ascii nil)
    (put-char-code-property ?＆ 'ascii nil)
    (put-char-code-property ?？ 'ascii nil)
    (put-char-code-property ?！ 'ascii nil)))

(global-set-key (kbd "s-u") 'normalize-chars)

;;; ------------------------------------------------------------
;;; 選択範囲を1行にする。最初のインデントは残す。
;; gist-description: Emacs(Elisp): Join multi lines to one. 選択範囲を1行にまとめます。
;; gist-id: ee6b2f8ef659ed58605d
;; gist-name: join-multi-lines-to-one.el
;; gist-private: nil

(defun join-multi-lines-to-one ()
  "Join multi lines."
  (interactive)
  (let ((beg (region-beginning))
        (end (region-end))
        strings)
    (goto-char beg)
    (back-to-indentation)
    (setq beg (point))
    (goto-char end)
    (goto-char (- (point) 1))
    (end-of-line)
    (setq end (point))
    (setq strings (buffer-substring-no-properties beg end))
    (setq strings (replace-regexp-in-string "\n\\|^>+ *\\|^[\t　 ]+" "" strings))
    (setq strings (replace-regexp-in-string "  +" " " strings))
    (delete-region beg end)
    (insert strings)
    (goto-char beg)))

(global-set-key (kbd "<s-kp-divide>") 'join-multi-lines-to-one)
(global-set-key (kbd "s-/") 'join-multi-lines-to-one)

;;; ------------------------------------------------------------
;;; align-regexpが、indent-tabs-modeがtでも、スペースを詰めるように

(defadvice align-regexp (around advise-align-regexp activate)
  "Let ALIGN-REGEXP indent by spaces."
  (when indent-tabs-mode (setq indent-tabs-mode nil))
  ad-do-it
  (my-set-indent-tabs-mode))

;;; ------------------------------------------------------------
;;; web-beautify

(require 'web-beautify)
(setq-default web-beautify-args
              '("-f"
                "-"
                ;; "--indent_with_tabs"
                ;; "--indent-size 2"
                "--end-with-newline"))

;;; ------------------------------------------------------------
;;; 釣り合いのとれる括弧のハイライト
;; 少々大袈裟だけれど、括弧同士のハイライトがカーソルの邪魔なのでアンダーラインにする
(defun my-force-paren-view-settings ()
  "Settings for my-force-paren-view-mode."
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  (setq show-paren-style 'expression)
  (set-face-attribute 'show-paren-match nil
                      :background 'unspecified :foreground 'unspecified
                      :underline t)
  (set-face-attribute 'show-paren-mismatch nil
                      :background 'unspecified :foreground 'unspecified
                      :strike-through t :weight 'extra-bold))

;;;###autoload
(define-minor-mode my-force-paren-view-mode
  "Toggle my-force-paren-view-mode.
This mode enhances the visibility of matching parentheses."
  :lighter " ParenView"
  :global t
  (if my-force-paren-view-mode
      (my-force-paren-view-settings)
    (show-paren-mode -1)))

;;;###autoload
(define-globalized-minor-mode global-my-force-paren-view-mode
  my-force-paren-view-mode my-force-paren-view-mode)

;; マイナーモードを全てのバッファで有効にする
(global-my-force-paren-view-mode 1)

;;; ------------------------------------------------------------
;;; provides

(provide 'editing.init)

;;; editing.init.el ends here
