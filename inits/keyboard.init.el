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
;; mac-likeなキーボード設定
;; thx http://www.unixuser.org/~euske/doc/emacsref/#file
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'write-file)
(global-set-key (kbd "s-o") 'find-file)
(global-set-key (kbd "s-z") 'undo)
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

(defun my-tab-dwim ()
  "Insert tab, jump to link etc."
  (interactive)
  (cond
   ;; read onlyバッファだったら次のリンク
   (buffer-read-only
    (forward-button 1 t))
   ;; ミニバッファだったらミニバッファ補完
   ((minibufferp (current-buffer))
    (minibuffer-complete))
   ;; beginning-of-lineだったらモードに応じたインデント移動
   ((bolp)
    (indent-according-to-mode))
   (t
    ;; 選択範囲があったらタブで上書き
    (when mark-active (delete-region (region-beginning) (region-end)))
    (insert "\t"))))

(global-set-key (kbd "<tab>") 'my-tab-dwim)

(add-hook 'php-mode-hook
          #'(lambda()
              (define-key php-mode-map (kbd "TAB") 'my-tab-dwim)
              (define-key php-mode-map (kbd "<tab>") 'my-tab-dwim)))

;;; ------------------------------------------------------------
;;; provides

(provide 'keyboard.init)

;;; keyboard.init.el ends here
