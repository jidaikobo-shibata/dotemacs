;;; gc.init.el --- init for garbage collector
;;; Commentary:
;; provide better garbage collector
;;; Code:

;; 1. M-x profiler-start
;; 2. cpu を選択（memory も気になるなら cpu+memory）
;; 3. Emacsが重くなったら M-x profiler-report
;; 4. q で抜けて M-x profiler-stop

;; GCの閾値を50MBに増やす（デフォルトは16MB）
(setq gc-cons-threshold (* 50 1000 1000))
(setq gc-cons-percentage 0.6)

;; Emacsのアイドル時にGCを実行する
(defun my-defer-gc ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-restore-gc ()
  (run-at-time
   1 nil
   (lambda ()
     (setq gc-cons-threshold (* 50 1000 1000)))))

(add-hook 'minibuffer-setup-hook #'my-defer-gc)
(add-hook 'minibuffer-exit-hook #'my-restore-gc)

;; *Messages* バッファにGCの発生をログ出力
(setq garbage-collection-messages t)

;; redisplay_internal (C function)を軽減
(setq redisplay-skip-fontification-on-input t)
(setq jit-lock-defer-time 0.05)

;;; ------------------------------------------------------------
;;; provides

(provide 'gc.init)

;;; gc.init.el ends here
