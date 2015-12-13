;;; jidaikobo-dark-theme.el --- theme
;; Copyright (C) 2015 by jidaikobo-shibata
;; Author: nicr-takahata
;; URL: https://github.com/nicr-takahata/dotemacs
;; Fork: jidaikobo-shibata
;; URL: https://github.com/jidaikobo-shibata/dotemacs

;;; Commentary:
;; 黒っぽい背景のテーマ
;; M-x list-color-displayでカラーコード一覧表表示
;; 変更したい場所を見つけたらカーソルを持って行ってdescribe-face

;;; Code:

(deftheme jidaikobo-dark "jidaikobo-dark color theme")

;;; ------------------------------------------------------------
;;; whitespace関連設定
;;; thx http://qiita.com/itiut@github/items/4d74da2412a29ef59c3a
(require 'whitespace)
(setq whitespace-style '(face          ; faceで可視化
												trailing       ; 行末
												tabs           ; タブ
												spaces         ; スペース
												empty          ; 先頭/末尾の空行
												space-mark     ; 表示のマッピング
												tab-mark
												))
(setq whitespace-display-mappings
			'((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
				;;(space-mark ?\u3000 [?\u25a1])
				))

;;; ------------------------------------------------------------
;;; color palette
(let ((class '((class color) (min-colors 89) (background dark)))
			(jidaikobo-fg                "#ffffff")
			(jidaikobo-bg                "#201f1f")
			(jidaikobo-red-1             "#f87373")
			(jidaikobo-red               "#ff0000")
			(jidaikobo-pink              "#f92672")
			(jidaikobo-orange-1          "#fd971f")
			(jidaikobo-orange            "#e96529")
			(jidaikobo-yellow            "#ffff00")
			(jidaikobo-darkgoldenrod     "#e6db74")
			(jidaikobo-wheat             "#c4be89")
			(jidaikobo-olive             "#808000")
			(jidaikobo-chartreuse        "#a6e22e")
			(jidaikobo-lime              "#00ff00")
			(jidaikobo-yellowgreen       "#83a300")
			(jidaikobo-green             "#008000")
			(jidaikobo-darkwine          "#1e0010")
			(jidaikobo-maroon            "#800000")
			(jidaikobo-wine              "#960050")
			(jidaikobo-teal              "#008080")
			(jidaikobo-aqua              "#00ffff")
			(jidaikobo-blue-3            "#88ddff")
			(jidaikobo-blue-2            "#77e9ef")
			(jidaikobo-blue-1            "#66d9ef")
			(jidaikobo-blue              "#0064ff")
			(jidaikobo-dodgerblue        "#13354a")
			(jidaikobo-slateblue         "#7070f0")
			(jidaikobo-purple-1          "#7f58c6")
			(jidaikobo-purple            "#7b4eff")
			(jidaikobo-palevioletred     "#d33682"))

;;; ------------------------------------------------------------
;;; custom-theme-set-faces
	(custom-theme-set-faces
	 'jidaikobo-dark

;;; ------------------------------------------------------------
;;; base
	 `(default
			((t (:foreground ,jidaikobo-fg
					 :background ,jidaikobo-bg))))
	 `(cursor
		 ((t (:foreground ,jidaikobo-bg
					:background ,jidaikobo-fg))))
	 `(fringe
		 ((t (:foreground ,jidaikobo-bg
					:background ,jidaikobo-bg)))) ;; フレームの余白
	 `(highlight
		 ((t (:background "grey"))))
	 `(region
		 ((t (:background "grey50"))
			(t :inverse-video t)))
	 `(warning
		 ((t (:foreground ,jidaikobo-palevioletred
					:weight bold))))

;;; ------------------------------------------------------------
;;; font lock

	 ;; コメント
	 `(font-lock-comment-face
		 ((t (:foreground ,jidaikobo-orange-1
					:background ,jidaikobo-bg))))
	 `(font-lock-comment-delimiter-face
		 ((t (:foreground ,jidaikobo-orange-1
					:background ,jidaikobo-bg))))

	 ;; ビルトイン関数
	 `(font-lock-builtin-face
		 ((t (:foreground ,jidaikobo-aqua))))

	 ;; 関数名
	 `(font-lock-function-name-face
		 ((t (:foreground ,jidaikobo-chartreuse))))

	 ;; 変数名
	 `(font-lock-variable-name-face
		 ((t (:foreground ,jidaikobo-yellowgreen))))
	 `(font-lock-warning-face
		 ((t (:foreground ,jidaikobo-palevioletred
					:weight bold))))

	 ;; 定数
	 `(font-lock-constant-face
		 ((t (:foreground ,jidaikobo-wheat))))
	 `(font-lock-doc-string-face
		 ((t (:foreground ,jidaikobo-darkgoldenrod))))

	 ;; キーワード
	 `(font-lock-keyword-face
		 ((t (:foreground ,jidaikobo-blue-1))))

	 ;; for easily-overlooked negation characters.
	 `(font-lock-negation-char-face
		 ((t (:foreground ,jidaikobo-wine))))

	 ;; 数値
	 `(font-lock-number-face
		 ((t (:foreground ,jidaikobo-blue))))
	 `(font-lock-preprocessor-face
		 ((t (:inherit (font-lock-builtin-face)))))
	 `(font-lock-regexp-grouping-backslash
		 ((t (:inherit (bold)))))
	 `(font-lock-regexp-grouping-construct
		 ((t (:inherit (bold)))))

	 ;; 文字列
	 `(font-lock-string-face
		 ((t (:foreground ,jidaikobo-red-1))))

	 ;; データ型名
	 `(font-lock-type-face
		 ((t (:foreground ,jidaikobo-blue))))

;;; ------------------------------------------------------------
;;; mode line
	 `(mode-line
		 ((t (:foreground "black"
					:background ,jidaikobo-wheat
					:box nil))))
	 `(mode-line-buffer-id
		 ((t (:weight bold))))
	 `(mode-line-inactive
		 ((t (:foreground "grey30"
	 		  :background ,jidaikobo-bg
					:box nil))))
	 `(mode-line-read-only-face
		 ((t (:foreground "black"
				  :background ,jidaikobo-red
					:box nil))))

;;; ------------------------------------------------------------
;;; header-line
	 `(header-line
		 ((t (:foreground ,jidaikobo-bg
					:background ,jidaikobo-wheat
					:weight bold))))

;;; ------------------------------------------------------------
;;; isearch
	 `(isearch
		 ((t (:foreground "black"
					:background ,jidaikobo-wheat
					:weight bold))))
	 `(isearch-fail
		 ((t (:foreground ,jidaikobo-wine
					:background ,jidaikobo-darkwine))))

;;; ------------------------------------------------------------
;;; linum
	 `(linum
		 ((t (:foreground "grey30"
					:background ,jidaikobo-bg))))

;;; ------------------------------------------------------------
;;; hl-line-mode
	 `(hl-line-face
		 ((,class (:background "grey60"))
			(t :weight bold)))
	 `(hl-line
		 ((,class (:foreground nil
							 :background "grey30"))
			(t :weight bold
				 :underline nil)))

;;; ------------------------------------------------------------
;;; whitespace
	 `(whitespace-trailing
		 ((t (:background ,jidaikobo-bg
					:underline t))))
	 `(whitespace-space
		 ((t (:background ,jidaikobo-bg
					:underline nil))))
	 `(whitespace-empty
		 ((t (:background ,jidaikobo-bg
					:underline nil))))
	 `(whitespace-tab
		 ((t (:foreground "grey20"
					:background ,jidaikobo-bg
					:underline nil))))

;;; ------------------------------------------------------------
;;; auto-complete and popup
	 `(ac-completion-face
		 ((t (:foreground "orange"
					:background "grey10"))))
	 `(ac-candidate-face
		 ((t (:foreground "grey50"))))
	 `(ac-selection-face
		 ((t (:background "grey40"))))
	 `(popup-face
		 ((t (:foreground "grey40"
					:background "grey20"))))

;;; ------------------------------------------------------------
;;; tabbar
	`(tabbar-default
		((t (:family (face-attribute 'default :family)
				 :background "black"
				 :height 0.9))))
	`(tabbar-unselected
		((t (:foreground "grey50"
				 :background "black"
				 :box nil))))
	`(tabbar-selected
		((t (:foreground "black"
				 :background "grey90"
				 :box nil))))
	`(tabbar-modified
		((t (:foreground "grey50"
				 :background "black"
				 :box nil))))

	 ;; TODO
	 ;; ido-mode
	 ;; flycheck
	 ;; show-paren
	 ;; rainbow-delimiters
	 ;; highlight-symbols
	 ))

;;; ------------------------------------------------------------
;;; Provide

;;;###autoload
(when load-file-name
	(add-to-list 'custom-theme-load-path
							 (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'jidaikobo-dark)

;;; jidaikobo-dark-theme.el ends here
