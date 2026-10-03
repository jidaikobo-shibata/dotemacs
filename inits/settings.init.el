;;; settings.init.el --- Shared settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Load local/settings.el before modules perform initialization.
;;; Code:
(defgroup my/settings nil "Shared dotemacs settings." :group 'environment)
(defcustom my/font-families '("MyricaM M" "Menlo" "DejaVu Sans Mono")
  "Preferred available fonts, in order." :type '(repeat string))
(defcustom my/japanese-font-families '("MyricaM M" "Hiragino Sans" "Noto Sans CJK JP")
  "Preferred available Japanese fonts." :type '(repeat string))
(defcustom my/font-height 160 "Default font height." :type 'integer)
(defcustom my/use-mozc (eq system-type 'gnu/linux)
  "Whether to enable Mozc integration." :type 'boolean)
(defcustom my/swap-control-super (eq system-type 'gnu/linux)
  "Whether to swap Control and Super on X." :type 'boolean)
(defcustom my/mac-command-modifier 'super "Command modifier on macOS." :type 'symbol)
(defcustom my/mac-option-modifier 'meta "Option modifier on macOS." :type 'symbol)
(defcustom my/junk-directory (expand-file-name ".tmp/junk/" user-emacs-directory)
  "Directory for new notes." :type 'directory)
(defcustom my/junk-auto-delete t "Whether managed notes expire." :type 'boolean)
(defcustom my/tmp-retention-days 30 "Retention for managed temporary files." :type 'integer)
(defun my/load-local-file (name)
  "Load optional personal file NAME; report errors without losing shared startup."
  (let ((file (expand-file-name (concat "local/" name) user-emacs-directory)))
    (when (file-exists-p file)
      (condition-case err (load file nil t)
        (error (display-warning 'my/settings
                                (format "%s: %s" file (error-message-string err))))))))
(provide 'settings.init)
;;; settings.init.el ends here
