;;; gc.init.el --- init for garbage collector
;;; Commentary:
;; provide better garbage collector
;;; Code:

;; 1. M-x profiler-start
;; 2. cpu を選択（memory も気になるなら cpu+memory）
;; 3. Emacsが重くなったら M-x profiler-report
;; 4. q で抜けて M-x profiler-stop

;; 起動後のGCの回数を見る
;; (message "GC count: %d" gcs-done)

;; 確認用 *Messages* でログを見る
;; ある程度見たら、コメントアウトすること
(defun my-log-gc (orig-fun &rest args)
  (let ((start (float-time)))
    (prog1
        (apply orig-fun args)
      (message "[GC] %.3fs at %s"
               (- (float-time) start)
               (format-time-string "%H:%M:%S")))))
(advice-add 'garbage-collect :around #'my-log-gc)

;; GCの閾値を50MBに増やす（デフォルトは16MB）
(setq gc-cons-threshold (* 50 1000 1000))
(setq gc-cons-percentage 0.6)

;; Emacsのアイドル時にGCを実行する
(defvar my-gc-cons-threshold-default (* 50 1000 1000))
(defvar my-gc-idle-timer nil)

(defun my-defer-gc ()
  (setq gc-cons-threshold most-positive-fixnum)
  (when my-gc-idle-timer
    (cancel-timer my-gc-idle-timer))
  (setq my-gc-idle-timer
        (run-with-idle-timer
         1 nil
         #'my-restore-gc)))

(defun my-restore-gc ()
  (setq gc-cons-threshold my-gc-cons-threshold-default)
  (setq my-gc-idle-timer nil))

(add-hook 'minibuffer-setup-hook #'my-defer-gc)

;; *Messages* バッファにGCの発生をログ出力
;; (setq garbage-collection-messages t)
(setq garbage-collection-messages nil)

;; redisplay_internal (C function)を軽減
(setq redisplay-skip-fontification-on-input t)
(setq jit-lock-defer-time 0.05)

;;; ------------------------------------------------------------
;;; provides

(provide 'gc.init)

;;; gc.init.el ends here
