;;; 複数のディレクトリで同じファイル名のファイルを開いたときのバッファ名を調整する
;(require 'uniquify)

;;; filename<dir> 形式のバッファ名にする
;(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;(setq uniquify-ignore-buffers-re "[^*]+")

;;; ログの記録行数を増やす
;(setq message-log-max 10000)


;; ;; 以下 http://emacs.rubikitch.com/imenus/ のやつ
;; (require 'imenus)
;; ;;; ido化: imenus/with-ido imenus-mode-buffers/with-idoを定義
;; (with-ido-completion imenus)
;; ;;; エラー対策
;; (defun imenu-find-default--or-current-symbol (&rest them)
;;   (condition-case nil
;;       (apply them)
;;     (error (thing-at-point 'symbol))))
;; (advice-add 'imenu-find-default :around 'imenu-find-default--or-current-symbol)
;; ;; C-M-s C-M-sで現在のシンボルをhelm-multi-swoopできるよ！
;; (global-set-key (kbd "C-M-s") (with-ido-completion imenus-mode-buffers))


;; ;;; M-oでのmulti-occurをシンボル正規表現にするよう改良
;; (push '(occur . imenus-ido-multi-occur) imenus-actions)
;; (defun imenus-ido-multi-occur (buffers input)
;;   (multi-occur buffers
;;                (format "\\_<%s\\_>"
;;                        (regexp-quote (replace-regexp-in-string "^.*|" "" input)))))

;; ;;; C-M-sで関数呼び出しをhelm-multi-swoopできるようにした
;; (push '(helm-multi-swoop . imenus-helm-multi-swoop) imenus-actions)
;; (defun imenus-helm-multi-swoop (buffers input)
;;   (helm-multi-swoop (replace-regexp-in-string "^.*|" "" input)
;;                     (mapcar 'buffer-name buffers)))
;; (define-key imenus-minibuffer-map (kbd "C-M-s") 'imenus-exit-to-helm-multi-swoop)
;; (defun imenus-exit-to-helm-multi-swoop ()
;;   "Exit from imenu prompt; start `helm-multi-swoop' with the current input."
;;   (interactive)
;;   (setq imenus-exit-status 'helm-multi-swoop)
;;   (imenus-exit-minibuffer))

;;;;;

;; ;; open-junk-file
;; (require 'open-junk-file)
;; ;; (setq open-junk-file-format "~/junk/%Y-%m-%d.")
;; (global-set-key "\C-xj" 'open-junk-file)

;; (cond ((file-directory-p "~/OneDrive/memos")
;;        (setq open-junk-file-format "~/OneDrive/memos/%Y-%m."))
;;       ((file-directory-p "~/win/OneDrive/memos")
;;        (setq open-junk-file-format "~/win/OneDrive/memos/%Y-%m."))
;;       (t
;;        (setq open-junk-file-format "~/junk/%Y-%m."))
;;       )
