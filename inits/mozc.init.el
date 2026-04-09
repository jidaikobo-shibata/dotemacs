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
;; ミニバッファやプロンプトでは <muhenkan> 系キーを無害化する
;;
;; この環境では、英数に切り替えたい気持ちで <muhenkan> を押すことが多い。
;; しかし Emacs の問い合わせ系プロンプトは、そのキーを「未知の入力」や
;; `Quit' として解釈することがあり、かなりストレスになる。
;;
;; ここでは以下の方針に統一する。
;; - 通常バッファでは従来どおり <muhenkan> で Mozc を抜ける
;; - ミニバッファや問い合わせプロンプトでは副作用を起こさず無視する
;; - isearch では完全な `ignore' ではなく `isearch-update' を呼んで
;;   プロンプト表示を保つ
;;
;; 実機ではキーイベント名が環境依存で揺れるため、候補をまとめて扱う。
(defconst my/muhenkan-keys
  '("<muhenkan>" [muhenkan] "<zenkaku-hankaku>" [zenkaku-hankaku] "<eisu-toggle>" [eisu-toggle]))

(defun my/muhenkan-key->kbd (key)
  "Return KEY as a value acceptable to `define-key'.
KEY may be a key description string or a vector event."
  (if (stringp key) (kbd key) key))

(defun my/define-muhenkan-keys (keymap command)
  "Bind every muhenkan-like key in KEYMAP to COMMAND.
This helper keeps all event-name variants in one place so prompt maps,
minibuffer maps, and isearch can be updated consistently."
  (dolist (key my/muhenkan-keys)
    (define-key keymap (my/muhenkan-key->kbd key) command)))

(defun my/isearch-ignore-muhenkan ()
  "Ignore muhenkan-like keys during isearch, but keep the prompt visible."
  (interactive)
  (when (bound-and-true-p isearch-mode)
    (isearch-update)))

;; These minibuffer-local maps cover normal prompts, completion prompts,
;; and inactive minibuffer states.  We explicitly bind muhenkan-like keys
;; to `ignore' here so they do not trigger accidental quit/help behavior.
(dolist (map-symbol '(minibuffer-local-map
                      minibuffer-local-ns-map
                      minibuffer-local-completion-map
                      minibuffer-local-must-match-map
                      minibuffer-local-isearch-map
                      minibuffer-inactive-mode-map))
  (when (boundp map-symbol)
    (my/define-muhenkan-keys (symbol-value map-symbol) #'ignore)))

;; isearch is special: a raw `ignore' can make the echo-area prompt disappear
;; while the search session itself is still alive.  Update the prompt instead.
(with-eval-after-load 'isearch
  (my/define-muhenkan-keys isearch-mode-map #'my/isearch-ignore-muhenkan))

;; `save-some-buffers' などの問い合わせは `map-y-or-n-p' / `query-replace-map'
;; を使ってキーを直接読むため、ミニバッファ map の `ignore' は効かない。
;; ここでも <muhenkan> 系キーを無害化して、`Type C-h for help.' ノイズを防ぐ。
(when (boundp 'query-replace-map)
  (my/define-muhenkan-keys query-replace-map #'ignore))

;; `y-or-n-p' 系の確認でも <muhenkan> を無害化する。
(when (boundp 'y-or-n-p-map)
  (my/define-muhenkan-keys y-or-n-p-map #'ignore))

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
