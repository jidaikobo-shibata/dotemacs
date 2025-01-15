;;; auto-complete.init.el --- init for auto-complete-mode
;;; Commentary:
;; provide auto-complete.init.
;;; Code:

(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-dwim t)
(setq ac-auto-show-menu 0.1)
(setq ac-delay 0.2)
(setq ac-auto-start 2)
(setq ac-ignore-case t)
(setq ac-disable-faces nil)
(setq ac-quick-help-delay 1)
(setq ac-use-comphist nil)
(setq ac-use-dictionary-as-stop-words nil)

;; ユーザ辞書設定
(defvar ac-my-dictionary "~/.emacs.d/ac-dict/my-dictionary")
(defvar ac-my-dictionary-dict '((candidates . (ac-file-dictionary ac-my-dictionary))))
(setq-default ac-sources '(ac-my-dictionary-dict
                           ac-source-words-in-same-mode-buffers))

;; 条件の追加
;; global-auto-complete-modeで足されていないものたち
(add-to-list 'ac-modes 'conf-mode)
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'html-mode)
(add-to-list 'ac-modes 'yaml-mode)
(add-to-list 'ac-modes 'default-generic-mode)

;;; 辞書に文字列を足して、git commit
(defun add-strings-to-ac-my-dictionary (dict-path)
  "Add strings to ac my dictionary.  DICT-PATH."
  (interactive)
  (let* ((beg (if mark-active (region-beginning) nil))
         (end (if mark-active (region-end) nil))
         (strings (if mark-active
                      (buffer-substring-no-properties beg end)
                    (read-string " String: " ""))))
    (with-temp-buffer
      (insert-file-contents dict-path)
      (goto-char (point-min))
      (if (re-search-forward (concat "^" strings "$") nil t)
          (message (concat "strings was already exists: " strings))
        (goto-char (point-max))
        (insert (concat "\n" strings))
        (sort-lines nil (point-min) (point-max))
        (delete-duplicate-lines (point-min) (point-max))
        (write-file dict-path)
        (shell-command (concat "git commit " dict-path " -m \"dictionary update.\""))
        (ac-clear-dictionary-cache)
        (message (concat "Add \"" strings "\"and git commit."))))))

;;; 辞書から文字列を削除して、git commit
(defun remove-strings-from-ac-my-dictionary (dict-path)
  "Remove strings from ac my dictionary.  DICT-PATH."
  (interactive)
  (let* ((beg (if mark-active (region-beginning) nil))
         (end (if mark-active (region-end) nil))
         (strings (if mark-active
                      (buffer-substring-no-properties beg end)
                    (read-string " String: " ""))))
    (with-temp-buffer
      (insert-file-contents dict-path)
      (goto-char (point-min))
      (if (re-search-forward (concat "^" strings "$") nil t)
          (if (yes-or-no-p (concat "Remove?:" strings))
              (progn
                (beginning-of-line)
                (kill-whole-line)
                (sort-lines nil (point-min) (point-max))
                (delete-duplicate-lines (point-min) (point-max))
                (write-file dict-path)
                (shell-command (concat "git commit " dict-path " -m \"dictionary update.\""))
                (ac-clear-dictionary-cache)
                (message (concat "Remove \"" strings "\"and git commit.")))
            (message (concat "Did nothing with: " strings)))
        (message (concat "Not found: " strings))))))

(defun add-strings-to-ac-my-dictionary-f ()
  "For `which-key'."
  (interactive)
  (add-strings-to-ac-my-dictionary ac-my-dictionary))

(defun remove-strings-from-ac-my-dictionary-f ()
  "For `which-key'."
  (interactive)
  (remove-strings-from-ac-my-dictionary ac-my-dictionary))

(global-set-key (kbd "C-c a") 'add-strings-to-ac-my-dictionary-f)
(global-set-key (kbd "C-c r") 'remove-strings-from-ac-my-dictionary-f)

;; 候補と入力文字が完全に一致している時にRETでac-completeするとnewlineしてしまうので抑止
(defadvice ac-complete (after advice-ac-complete-to-avoid-newline activate)
  "Inhibit newline when full string was matched with candidate."
  (when (memq this-command '(newline))
    (delete-char -1)))

;;; ------------------------------------------------------------
;;; provides

(provide 'auto-complete.init)

;;; auto-complete.init.el ends here
