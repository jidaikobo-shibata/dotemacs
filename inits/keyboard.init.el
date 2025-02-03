;;; keyboard.init.el --- init for keyboard
;;; Commentary:
;; provide keyboard.init.
;;; Code:

;;; ------------------------------------------------------------
;; UbuntuでSuperキーとCtrlキーを入れ替える
(defun my-set-ctrl-key ()
  "Set left Super key to act as Ctrl key in Emacs."
  (interactive)
  (setq x-super-keysym 'ctrl)
  (setq x-ctrl-keysym 'super))
(my-set-ctrl-key)

;;; ------------------------------------------------------------
;; UbuntuでSuperキーとCtrlキーを入れ替える
(defun my-undo-ignore-region ()
  "Undo the entire buffer even if there is a selection."
  (interactive)
  (let ((mark-active nil))
    (undo)))

;;; ------------------------------------------------------------
;; mac-likeなキーボード設定
;; thx http://www.unixuser.org/~euske/doc/emacsref/#file
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'write-file)
(global-set-key (kbd "s-o") 'find-file)
(global-set-key (kbd "s-z") 'my-undo-ignore-region)
(global-set-key (kbd "s-Z") 'undo-redo)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "<s-kp-add>") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "<s-kp-subtract>") 'text-scale-decrease)
(global-set-key (kbd "<s-kp-equal>") (lambda () (interactive) (text-scale-mode 0)))
(global-set-key (kbd "s-=") (lambda () (interactive) (text-scale-mode 0)))
(global-set-key (kbd "<s-kp-0>") (lambda () (interactive) (text-scale-mode 0)))
(global-set-key (kbd "<s-0>") (lambda () (interactive) (text-scale-mode 0)))
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "<backspace>") 'delete-backward-char)
(global-set-key (kbd "<s-up>") (lambda () (interactive "^") (goto-char (point-min))))
(global-set-key (kbd "<s-down>") (lambda () (interactive "^") (goto-char (point-max))))
(global-set-key (kbd "<s-left>") 'beginning-of-line)
(global-set-key (kbd "<s-right>") 'end-of-line)
(global-set-key (kbd "<prior>") 'backward-page)
(global-set-key (kbd "<next>") 'forward-page)

;;; ------------------------------------------------------------
;; M-g or cmd+opt+j で指定行へジャンプ
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-s-j") 'goto-line)

;;; ------------------------------------------------------------
;; escでC-g
(setq-default normal-escape-enabled t)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-inactive-mode-map [escape] 'minibuffer-keyboard-quit)
(define-key isearch-mode-map [escape] 'isearch-abort) ; isearch
(define-key isearch-mode-map "\e" 'isearch-abort) ; \e seems to work better for terminals
(global-set-key (kbd "<escape>") 'keyboard-quit) ; everywhere else
(global-set-key (kbd "M-ESC ESC") 'keyboard-quit)

;;; ------------------------------------------------------------
;;; 次/前の空行
;; gist-description: Emacs(Elisp): forward/backward-paragraphだとparagraph判定がメジャーモードごとで異なり、字義通りの「次の空行」にならないので、別途用意。
;; gist-id: ad27b19dd3779ccc1ff2
;; gist-name: move-to-next(previous)-blank-line.el
;; gist-private: nil

(defun move-to-previous-blank-line ()
  "Go to previous empty lines."
  (interactive "^")
  (goto-char
   (or (save-excursion
         (unless (bobp)
           (backward-char)
           (re-search-backward "^$" nil t)))
       (point-min))))

(defun move-to-next-blank-line ()
  "Go to next empty lines."
  (interactive "^")
  (goto-char
   (or (save-excursion
         (unless (eobp)
           (forward-char)
           (re-search-forward "^$" nil t)))
       (point-max))))

(global-set-key (kbd "<M-up>") 'move-to-previous-blank-line)
(global-set-key (kbd "<M-down>") 'move-to-next-blank-line)
(global-set-key (kbd "<C-up>") 'move-to-previous-blank-line)
(global-set-key (kbd "<C-down>") 'move-to-next-blank-line)

;;; ------------------------------------------------------------
;;; 自分好みのカーソル移動
;; gist-description: Emacs(Elisp): forward/backward-wordだと、移動距離が微妙に大きい。単語境界も微妙だった。ので、カーソル移動をちょっとカスタマイズ。
;; gist-id: 467f4302c002049bfb95511bd21cdbe7
;; gist-name: skip-chars-(forward|backward)-dwim.el
;; gist-private: nil

(defun my-skip-chars (forward)
  "Skip characters based on the direction FORWARD."
  (let ((start (point))
        (char-sets '("a-zA-Z0-9"
                     " "
                     "()"
                     "<>"
                     "ぁ-んー"
                     "ァ-ヶー"
                     "亜-黑ー")))
    (dolist (char-set char-sets)
      (when (eq start (point))
        (if forward
            (skip-chars-forward char-set)
          (skip-chars-backward char-set))))
    ;; Move one character if still at the start point
    (when (eq start (point))
      (if forward
          (forward-char 1)
        (backward-char 1)))))

(defun skip-chars-forward-dwim ()
  "Skip characters forward in a DWIM manner."
  (interactive "^")
  (my-skip-chars t))

(defun skip-chars-backward-dwim ()
  "Skip characters backward in a DWIM manner."
  (interactive "^")
  (my-skip-chars nil))

(global-set-key (kbd "<M-left>") 'skip-chars-backward-dwim)
(global-set-key (kbd "<M-right>") 'skip-chars-forward-dwim)

;;; ------------------------------------------------------------
;;; タブキー

(defvar my-tab-dwim-last-time 0
  "The last time TAB key was pressed in seconds.")

(defun my-tab-dwim ()
  "Insert tab, jump to link, or handle consecutive TAB presses."
  (interactive)
  (let ((current-time (float-time)))
    (cond
     ;; read-onlyバッファだったら次のリンク
     (buffer-read-only
      (forward-button 1 t))
     ;; ミニバッファだったらミニバッファ補完
     ((minibufferp (current-buffer))
      (minibuffer-complete))
     ;; beginning-of-lineだったらモードに応じたインデント移動
     ;; 0.5秒以内の打鍵ならタブを入力
     ((bolp)
      (if (< (- current-time my-tab-dwim-last-time) 0.5)
          (insert-tab-context)
        (indent-according-to-mode)))
     (t
      ;; 選択範囲があったらタブ文字（\t）で上書き
      (when mark-active (delete-region (region-beginning) (region-end)))
      (insert "\t")))
    (setq my-tab-dwim-last-time current-time)))

(defun insert-tab-context ()
  "Insert tab or space."
  (if indent-tabs-mode
      (insert "\t")
    (insert (make-string tab-width ?\s))))

(global-set-key (kbd "<tab>") 'my-tab-dwim)

(add-hook 'php-mode-hook
          #'(lambda()
              (define-key php-mode-map (kbd "TAB") 'my-tab-dwim)
              (define-key php-mode-map (kbd "<tab>") 'my-tab-dwim)))

;;; ------------------------------------------------------------
;;; 対応する括弧を選択

(defun select-enclosing-parens ()
  "Select the text between the current position and its matching parenthesis.
If the cursor is after a closing parenthesis, select the enclosing pair."
  (interactive)
  (let ((start (point)))
    (cond
     ;; キャレットが閉じ括弧の直後にある場合
     ((and (not (bobp)) ; バッファの先頭でない
           (member (char-before) '(?\) ?\] ?\}))) ; 直前が閉じ括弧
      (ignore-errors
        (mark-sexp -1))           ; 対応する開き括弧を選択
      (exchange-point-and-mark))  ; 範囲を確定
     ;; 通常の括弧選択（カーソル位置から始める場合）
     ((ignore-errors (mark-sexp 1) t)
      (exchange-point-and-mark))
     ;; 対応する括弧が見つからない場合
     (t
      (goto-char start)
      (message "No matching parenthesis found")))))

(defun adjust-selection-based-on-context ()
  "Adjust selection or create a selection based on context.
1. If a region is already selected, narrow or expand the selection.
2. If no region is selected, select enclosing parentheses or select quotes."
  (interactive)
  (if (use-region-p)
      ;; 選択範囲がある場合
      (let ((start (region-beginning))
            (end (region-end)))
        (cond
         ;; 両端が括弧または引用符の場合に範囲を狭める
         ((and (member (char-after start) '(?\( ?\[ ?\{ ?\" ?\' ?\`))
               (member (char-before end) '(?\) ?\] ?\} ?\" ?\' ?\`)))
          (goto-char (1+ start)) ; 範囲を狭める
          (set-mark (1- end)))
         ;; 両端の外側が括弧または引用符の場合に範囲を拡大する
         ((and (not (bobp)) (not (eobp))
               (member (char-before start) '(?\( ?\[ ?\{ ?\" ?\' ?\`))
               (member (char-after end) '(?\) ?\] ?\} ?\" ?\' ?\`)))
          (goto-char (1- start)) ; 範囲を拡大する
          (set-mark (1+ end)))
         (t
          (message "Selection does not match enclosing brackets or quotes"))))
    ;; 選択範囲がない場合
    (cond
     ;; キャレットの前後が括弧の場合、括弧を選択
     ((or (and (not (bobp))
               (member (char-before) '(?\) ?\] ?\} ?\( ?\[ ?\{)))
          (and (not (eobp))
               (member (char-after) '(?\) ?\] ?\} ?\( ?\[ ?\{))))
      (select-enclosing-parens))
     ;; 引用符を後方に検索して選択
     ((re-search-backward "[\"'`]" nil t)
      (let ((quote-char (char-after (point))))
        (set-mark (point))
        (forward-char)
        (search-forward (char-to-string quote-char))))
     ;; 何もしない
     (t
      (message "No brackets or quotes found")))))

(global-set-key (kbd "C-s-a") 'adjust-selection-based-on-context)

;;; ------------------------------------------------------------
;;; provides

(provide 'keyboard.init)

;;; keyboard.init.el ends here
