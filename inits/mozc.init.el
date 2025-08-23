;;; mozc.init.el --- init for mozc-mode
;;; Commentary:
;; provide mozc.init.
;;; Code:

(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc")
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)

;;; ------------------------------------------------------------
;; henkan/muhenkanキーisearchを抜けないようにする

(with-eval-after-load 'isearch
  ;; IME切替系キーは isearch を抜けず、その場で IME をトグル
  (dolist (k '("<muhenkan>" "<henkan>" "<eisu-toggle>" "<kana>" "<zenkaku-hankaku>"))
    (define-key isearch-mode-map (kbd k) #'isearch-toggle-input-method))

  ;; （任意）変換/無変換を無視したい場合は ignore に
  ;; (define-key isearch-mode-map [henkan]   #'ignore)
  ;; (define-key isearch-mode-map [muhenkan] #'ignore)

  ;; isearchで通常入力時も IME を使えるように（既定オフなので有効化推奨）
  (setq isearch-use-input-method t)
  ;; 念のため M-s (= ESC s) は isearch 中も prefix のまま
  (define-key isearch-mode-map (kbd "M-s") search-map)
  (define-key isearch-mode-map (kbd "ESC") search-map)
  (define-key isearch-mode-map [escape] #'isearch-abort))

(with-eval-after-load 'mozc
  (when (require 'mozc-isearch nil t)
    (mozc-isearch-setup)))

;;; ------------------------------------------------------------
;; muhenkanキーでMozcを抜ける際に、現在の入力を確定する
(defun my-confirm-and-deactivate-input-method ()
  "Confirm the current input and deactivate the input method."
  (interactive)
  ;; Mozcがアクティブだったら入力中の文字を確定
  (when (and (boundp 'mozc-mode) mozc-mode (mozc-input-pending-p))
    (let ((enter-key-event 13))
      (mozc-handle-event enter-key-event)))
  ;; 入力メソッドを無効化
  (deactivate-input-method))

;; 未変換の入力文字があるかどうか確認
(defun mozc-input-pending-p ()
  "Check if Mozc has pending input in the preedit session."
  (and (boundp 'mozc-preedit-in-session-flag) mozc-preedit-in-session-flag))

;; Mozcロード後にキーバインドを設定
(with-eval-after-load 'mozc
  (define-key mozc-mode-map (kbd "<muhenkan>") 'my-confirm-and-deactivate-input-method))
(with-eval-after-load 'anything
  (define-key anything-map (kbd "<muhenkan>") 'my-confirm-and-deactivate-input-method))

;; グローバルキーバインドも設定
(global-set-key (kbd "<muhenkan>") 'my-confirm-and-deactivate-input-method)

;; ミニバッファではmozcをオフに
;; (defun my-confirm-and-deactivate-input-method-on-minibuffer ()
;;   "Automatically confirm and deactivate Mozc when entering the minibuffer."
;;   (when (and (boundp 'mozc-mode) mozc-mode)
;;     (my-confirm-and-deactivate-input-method)))
;; (add-hook 'minibuffer-setup-hook 'my-confirm-and-deactivate-input-method-on-minibuffer)

;;; ------------------------------------------------------------
;; henkanでMozcを起こす
(global-set-key (kbd "<henkan>")
                (lambda () (interactive)
                  (activate-input-method default-input-method)))

;;; ------------------------------------------------------------
;; mozc-modeでもdelete-selection-modeを機能させる
(defun my-mozc-handle-event (orig-fun &rest args)
  "Make `delete-selection-mode` work in mozc-mode.  ORIG-FUN, ARGS."
  (let* ((event (nth 0 args))
         (event-type (and event (event-basic-type event))))
    (if (and (use-region-p)
             event
             (not (or (member event-type '(left right up down henkan muhenkan wheel-up wheel-down escape))
                      (member 'shift (event-modifiers event))
                      (member 'super (event-modifiers event))
                      (member 'meta (event-modifiers event))
                      (member 'control (event-modifiers event))
                      (mouse-event-p event))))
        (delete-region (region-beginning) (region-end)))
    (apply orig-fun args)))

(advice-add 'mozc-handle-event :around #'my-mozc-handle-event)

(defun my-delete-selection-before-yank (&rest _args)
  "Deactivate the selection before yank."
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))))

(advice-add 'yank :before #'my-delete-selection-before-yank)

;;; ------------------------------------------------------------
;;; provides

(provide 'mozc.init)

;;; mozc.init.el ends here
