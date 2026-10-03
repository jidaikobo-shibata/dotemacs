;;; Copy to local/overrides.el only if you prefer these movements.
;; Personal choices, never applied automatically on macOS.
(global-set-key (kbd "s-<left>") #'beginning-of-visual-line)
(global-set-key (kbd "s-<right>") #'end-of-visual-line)
(global-set-key (kbd "C-a") #'move-beginning-of-line)
(global-set-key (kbd "C-e") #'move-end-of-line)
(global-set-key (kbd "M-<left>") #'skip-chars-backward-dwim)
(global-set-key (kbd "M-<right>") #'skip-chars-forward-dwim)
