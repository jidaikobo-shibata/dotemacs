;;; migemo.init.el --- init for migemo
;;; Commentary:
;; provide migemo.
;;; Code:

(add-to-list 'load-path "~/.emacs.d/elisp/migemo")
(require 'migemo)
(setq migemo-command "cmigemo") ;; cmigemoのパスを指定
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict") ;; 辞書ファイルのパスを指定
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-coding-system 'utf-8-unix)
(setq migemo-use-default-isearch-keybinding nil)
(migemo-init)

;; anything.init.elでもmigemoの設定を行っている

;;; ------------------------------------------------------------
;;; provides

(provide 'migemo.init)

;;; migemo.init.el ends here
