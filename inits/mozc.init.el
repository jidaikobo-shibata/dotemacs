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
;; search-center 側で `C-s <henkan>' の橋渡しを持つため、
;; ここでは通常の Mozc ON/OFF のみを扱う。

;;; ------------------------------------------------------------
;; ミニバッファや isearch では <muhenkan> 系キーを無害化する
;; 終了確認などで英数キーのつもりで押して `Quit' になるのを避ける。
(defconst my/muhenkan-keys
  '("<muhenkan>" [muhenkan] "<zenkaku-hankaku>" [zenkaku-hankaku] "<eisu-toggle>" [eisu-toggle]))

(defun my/isearch-ignore-muhenkan ()
  "Ignore muhenkan-like keys during isearch, but keep the prompt visible."
  (interactive)
  (when (bound-and-true-p isearch-mode)
    (isearch-update)))

(dolist (map-symbol '(minibuffer-local-map
                      minibuffer-local-ns-map
                      minibuffer-local-completion-map
                      minibuffer-local-must-match-map
                      minibuffer-local-isearch-map
                      minibuffer-inactive-mode-map))
  (when (boundp map-symbol)
    (dolist (key my/muhenkan-keys)
      (define-key (symbol-value map-symbol)
                  (if (stringp key) (kbd key) key)
                  #'ignore))))

(with-eval-after-load 'isearch
  (dolist (key my/muhenkan-keys)
    (define-key isearch-mode-map
                (if (stringp key) (kbd key) key)
                #'my/isearch-ignore-muhenkan)))

;; `save-some-buffers' などの問い合わせは `map-y-or-n-p' / `query-replace-map'
;; を使ってキーを直接読むため、ミニバッファ map の `ignore' は効かない。
;; ここでも <muhenkan> 系キーを無害化して、`Type C-h for help.' ノイズを防ぐ。
(when (boundp 'query-replace-map)
  (dolist (key my/muhenkan-keys)
    (define-key query-replace-map
                (if (stringp key) (kbd key) key)
                #'ignore)))

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

(defun my/deactivate-input-method-command ()
  "Confirm mozc preedit first, then force turn off input method and mozc-mode."
  (interactive)
  ;; 未確定文字があれば、まず確定してから OFF にする。
  (when (and (bound-and-true-p mozc-mode)
             (mozc-input-pending-p))
    (ignore-errors (mozc-handle-event 13)))
  (ignore-errors (deactivate-input-method))
  (when (bound-and-true-p mozc-mode)
    (ignore-errors (mozc-mode -1)))
  ;; Fallback: keep state consistent even if deactivate path failed.
  (when current-input-method
    (setq current-input-method nil
          current-input-method-title nil
          input-method-function nil
          describe-current-input-method-function nil)
    (force-mode-line-update)))

;; 未変換の入力文字があるかどうか確認
(defun mozc-input-pending-p ()
  "Check if Mozc has pending input in the preedit session."
  (and (boundp 'mozc-preedit-in-session-flag) mozc-preedit-in-session-flag))

;; Mozcロード後にキーバインドを設定
(with-eval-after-load 'mozc
  (define-key mozc-mode-map (kbd "<muhenkan>") #'my/deactivate-input-method-command)
  ;; Keep default mouse selection behavior even when mozc-mode keymap is active.
  (define-key mozc-mode-map [down-mouse-1] #'mouse-drag-region)
  (define-key mozc-mode-map [drag-mouse-1] #'mouse-set-region)
  (define-key mozc-mode-map [mouse-1] #'mouse-set-point)
  (define-key mozc-mode-map [double-down-mouse-1] #'mouse-drag-region)
  (define-key mozc-mode-map [double-mouse-1] #'mouse-set-point)
  (define-key mozc-mode-map [triple-down-mouse-1] #'mouse-drag-region)
  (define-key mozc-mode-map [triple-mouse-1] #'mouse-set-point))
(with-eval-after-load 'anything
  (define-key anything-map (kbd "<muhenkan>") #'my/deactivate-input-method-command))

;; グローバルキーバインドも設定
(global-set-key (kbd "<muhenkan>") #'my/deactivate-input-method-command)

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
