;;; dired の a で開くやつ
(put 'dired-find-alternate-file 'disabled nil)

;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;;; splash screenを無効にする
(setq inhibit-splash-screen t)

;;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;; C-u C-SPC C-SPC …でどんどん過去のマークを遡る
(setq set-mark-command-repeat-pop t)

;;; インデントにTABを使わないようにする
(setq-default indent-tabs-mode nil)

;;; tab 幅は4
(setq-default tab-width 4)

;;; 現在行に色をつける
;(global-hl-line-mode 1)

;;; リージョンを削除できるように
(delete-selection-mode t)

;; 自動で対応するカッコ入力する
(electric-pair-mode t)

;; 対応カッコ強調
(show-paren-mode 1)

 ;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;;; モードラインに時刻を表示する
(display-time)

 ;;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;; 履歴をたくさん保存する
(setq history-length 1000)

;;; メニューバーとツールバーとスクロールバーを消す
;(menu-bar-mode -1)
;(tool-bar-mode -1)
;(scroll-bar-mode -1)

;; 位置を出すようにする
(column-number-mode 1)

;; utf-8 .. ??
(prefer-coding-system 'utf-8)

;; 補完
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially   ;
        try-complete-file-name             ;
;        try-expand-all-abbrevs             ; 略語展開
;        try-expand-list                    ; 括弧内容
;        try-expand-line                    ; 行
        try-expand-dabbrev                 ;
        try-expand-dabbrev-all-buffers     ;
        try-expand-dabbrev-from-kill       ;
;        try-complete-lisp-symbol-partially ; Emacs lisp シンボル
;        try-complete-lisp-symbol           ; Emacs lisp シンボル all
        ))

;; 警告音鳴らさない
(setq ring-bell-function 'ignore)

;; no auto save file
(setq auto-save-default nil)

;; Backup file
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/.backup"))
            backup-directory-alist))

;;; シェルに合わせるため、C-hは後退に割り当てる
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key key-translation-map (kbd "C-?") (kbd "C-h"))

(global-set-key "\C-z" 'scroll-down)
(global-set-key "\C-\M-y" 'insert-register)
(global-set-key "\C-\M-k" 'kill-buffer)

;; C-, ウィンドウ切り替え
(global-set-key (kbd "C-,") 'other-window)
;;  => 95_additional にも np 同時押し

;; インデント
;(global-set-key "\C-c\C-q" 'indent-region)

;; 対応する括弧を削除
(global-set-key "\C-c\C-d" 'delete-pair)

;; Mac の Cmd は meta key として使う
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; タイトルバーにフルパス
(setq frame-title-format "emacs %f")

;; ido
(ido-mode 1)

;; 行番号とか
(global-display-line-numbers-mode)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; hs-mode
(define-key global-map (kbd "C-{") 'hs-hide-block)
(define-key global-map (kbd "C-}") 'hs-show-block)
