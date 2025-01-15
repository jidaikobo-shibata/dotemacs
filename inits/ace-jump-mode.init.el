;;; ace-jump-mode.init.el --- init for ace-jump-mode-mode
;;; Commentary:
;; provide ace-jump-mode.
;;; Code:

(require 'ace-jump-mode)
(setq ace-jump-mode-move-keys
      (append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))
(setq ace-jump-word-mode-use-query-char nil)
(defun do-ace-jump-mode (chr)
  "Do ace jump mode.  CHR."
  (interactive (list (read-char "Query Char:")))
  ;; (mac-auto-ascii-select-input-source)
  (ace-jump-char-mode chr))
(global-set-key (kbd "M-z") 'do-ace-jump-mode)

;;; ------------------------------------------------------------
;;; provides

(provide 'ace-jump-mode.init)

;;; ace-jump-mode.init.el ends here
