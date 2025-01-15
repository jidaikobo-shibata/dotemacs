;;; window.init.el --- init for window-mode
;;; Commentary:
;; provide window.
;;; Code:

;; ------------------------------------------------------------
;; ツールバーを非表示
(when (functionp 'tool-bar-mode) (tool-bar-mode -1))

;; ------------------------------------------------------------
;; 28.2からタイトルバーが真っ黒になってしまったので、全部消す
(setq frame-title-format nil)

;; ------------------------------------------------------------
;;; frame
(add-to-list 'default-frame-alist '(alpha . (1.00 1.00)))
(add-to-list 'default-frame-alist '(width . 105))
(add-to-list 'default-frame-alist '(height . 42))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 800))

;; ------------------------------------------------------------
;; フレームの大きさと位置を変更 (cmd+shift+w)
(defun resize-selected-frame ()
  "Resize frame to jidaikobo's default."
  (interactive)
  ;; フレームが一枚の時には左上に寄せる
  (when (= 1 (safe-length (frame-list)))
    (set-frame-position (selected-frame) 1900 0))
  (set-frame-size (selected-frame) 105 42))
(global-set-key (kbd "s-W") 'resize-selected-frame)

;;; ------------------------------------------------------------
;; 新規フレームを開く
(global-set-key (kbd "s-N") 'make-frame-command)

;;; ------------------------------------------------------------
;; window操作
(global-set-key (kbd "M-o") (lambda () (interactive) (other-window 1))) ; C-o がダメな理由を確認したい
(global-set-key (kbd "C-S-o") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-!") 'delete-other-windows)
(global-set-key (kbd "<C-kp-1>") 'delete-other-windows)
(global-set-key (kbd "C-\"") 'split-window-vertically)
(global-set-key (kbd "<C-kp-2>") 'split-window-vertically)
(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "<C-kp-3>") 'split-window-horizontally)
(global-set-key (kbd "C-0") 'delete-window)

;; 能動的なsplit-windowは、フォーカスを移動してほしい
(defadvice split-window (after split-window-and-select activate)
  "Split window and select."
  (when (memq last-input-event '(50 51 67108914 67108915 C-kp-2 C-kp-3))
  (other-window 1)))

;;; ------------------------------------------------------------
;;; ウィンドウ/スクリーン/フレームを閉じる

(defun my-delete-windows ()
  "Contexual delete windows."
  (interactive)
  (cond
   ;; ミニバッファにいたらまず抜ける
   ((minibufferp (current-buffer))
    (minibuffer-keyboard-quit)
    (other-window 1)
    (my-delete-windows))
   ;; ウィンドウ構成が多ければまず自分を消す
   ((not (one-window-p)) (delete-window))
   ;; 複数のフレームを開いている時には、フレームを閉じる
   ;; ((/= 1 (safe-length (frame-list))) (delete-frame))
   ;; ウィンドウ構成がひとつでバッファに変更があればfoeb/kill-bufferする
   ((or (and (buffer-modified-p)
             ;; read-onlyなら無視
             (not buffer-read-only)
             ;; スクラッチ以外でアスタリスクで始まるバッファ名も保存を尋ねない
             (not (string=
                   (substring (buffer-name (current-buffer)) 0 1)
                   "*")))
        ;; スクラッチバッファでメモ代わりに使っていたらfoeb/kill-bufferする
        (and (buffer-modified-p) (string= (buffer-name) "*scratch*")))
    ;; (unless (yes-or-no-p "Buffer is modified. Close anyway?")
    ;;   (call-interactively (save-buffer)))
    ;; foeb/kill-bufferは、bufferをkillするので、保存の確認が走る
    (foeb/kill-buffer))
   ;; kill-buffer for other situation
   (t (foeb/kill-buffer))))

(global-set-key (kbd "s-w") 'my-delete-windows)
(global-set-key (kbd "C-s-w") 'delete-frame)

;;; ------------------------------------------------------------
;;; カーソル関連

;; cursor-chg
;; カーソルの色と形状を変更（ブロックカーソルが苦手なので）
(require 'cursor-chg)
(change-cursor-mode 1)
(toggle-cursor-type-when-idle 0)
(setq curchg-default-cursor-color "White")
(setq curchg-input-method-cursor-color "firebrick")
(setq curchg-change-cursor-on-input-method-flag t)

;;; ------------------------------------------------------------
;;; 行設定

;; 行カーソル
;; thx http://rubikitch.com/tag/emacs-post-command-hook-timer/
(require 'hl-line)
(defun global-hl-line-timer-function ()
  "Line cursor."
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq-default global-hl-line-timer
              (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

;; 行間隔を少し広げる
(set-default 'line-spacing 3)

;; 行番号を表示する
(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)
(add-hook 'js-mode-hook 'display-line-numbers-mode)
(add-hook 'html-mode-hook 'display-line-numbers-mode)
(add-hook 'php-mode-hook 'display-line-numbers-mode)
(add-hook 'css-mode-hook 'display-line-numbers-mode)

;;; ------------------------------------------------------------
;;; ヘッダーライン設定

(defun show-header-line ()
  "Show Header Line."
  (if (not (active-minibuffer-window))
      (progn
        (setq-default my-header (format "%%b %%* %%f"))
        (setq header-line-format '(:eval (substring my-header
                                                    (min (length my-header)
                                                         (window-hscroll))))))))
(add-hook 'buffer-list-update-hook 'show-header-line)

;;; ------------------------------------------------------------
;;; モードライン設定

;; 関数名の表示
(which-function-mode 1)

;; フレーム情報は不要
(setq-default mode-line-frame-identification "")

;; titleに出ているのでバッファ名は不要
(setq-default mode-line-buffer-identification "")

;; titleに出ているのでバッファの変更状態も不要
(setq-default mode-line-modified "")

;;; 前に行番号、総行数、桁番号を表示
;;; 総行数の計する%記法がないので遅延で計算させる
;; thx rubikitch
(defvar-local mode-line-last-line-number 0)
(defvar-local clnaw-last-tick 0)
(defun calculate-total-line-numbers ()
  "Calculate total line numbers."
  (unless (eq clnaw-last-tick (buffer-modified-tick))
    (setq mode-line-last-line-number (line-number-at-pos (point-max)))
    (setq clnaw-last-tick (buffer-modified-tick))
    (force-mode-line-update)))
(run-with-idle-timer 1 t 'calculate-total-line-numbers)

;; 現在行、総行、文字位置、選択範囲の文字数など
(setq mode-line-position
      '(:eval (format "%d/%d %d/%d %s"
                      (line-number-at-pos)
                      mode-line-last-line-number
                      (point)
                      (point-max)
                      (if mark-active
                          (concat "["
                                  (format "%s" (count-lines (region-end) (region-beginning)))
                                  "-"
                                  (format "%s" (- (region-end) (region-beginning)))
                                  "]")
                        ""))))

;; 改行の種類表示の変更
;; thx https://github.com/moriyamahiroshi/hm-dot-emacs-files/blob/master/init.el
(setq-default eol-mnemonic-unix "(LF)")
(setq-default eol-mnemonic-dos  "(CRLF)")
(setq-default eol-mnemonic-mac  "(CR)")

;; minor-mode名称を省略
;; thx http://syohex.hatenablog.com/entry/20130131/1359646452
(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (yas-minor-mode . " Ys")
    (paredit-mode . " Pe")
    (rainbow-mode . "")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . " Ut")
    ;; (flycheck-mode . " Fc")
    ;; Major modes
    (lisp-interaction-mode . "Li")
    (python-mode . "Py")
    (ruby-mode   . "Rb")
    (php-mode    . "Php")
    (emacs-lisp-mode . "El")
    (markdown-mode . "Md")))

;;; ------------------------------------------------------------
;;; provides

(provide 'window.init)

;;; window.init.el ends here
