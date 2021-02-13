(use-package helm :ensure t)
(require 'helm-config)
(helm-mode t)

;; Emulate 'kill-line' in helm minibuffer
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  "Emulate 'kill-line' in helm minibuffer"
  (kill-new (buffer-substring (point) (field-end))))

(global-set-key (kbd "M-[") 'helm-M-x)
(global-set-key (kbd "M-x") 'helm-M-x)
;(global-set-key (kbd "C-;") 'helm-mini)
(global-set-key (kbd "C-;") 'helm-for-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(use-package helm-cmd-t
  :ensure t
  :config
  (global-set-key (kbd "C-:") 'helm-C-x-b)
  )
(setq helm-for-files-preferred-list
      '(helm-source-buffers-list
        helm-source-recentf
        helm-source-cmd-t-caches
        helm-source-files-in-current-dir
        ;helm-source-file-cache
        ;helm-source-bookmarks
        ;; 必要とあれば
        ;helm-source-bookmark-set
        ;helm-source-locate)
        ))

(use-package helm-ghq
  :ensure t
  :config
  (when (executable-find "ghq")
    (setq helm-for-files-preferred-list
          (append helm-for-files-preferred-list '(helm-source-ghq))))
  )

;; search kaizen
;; ace-isearch (isearch + ace-jump
;; ace-isearch か下記の isearch-.. かどっちかを有効に
;; (global-ace-isearch-mode +1)
;; (custom-set-variables
;; ; '(ace-isearch-input-length 7)
;; ; '(ace-isearch-jump-delay 0.25)
;; ; '(ace-isearch-function 'avy-goto-char)
;;  '(ace-isearch-use-jump 'printing-char))

;; ace-isearch かこれかどっちかを有効に
(use-package helm-swoop  :ensure t)
(cl-defun helm-swoop-nomigemo (&key $query ($multiline current-prefix-arg))
  (interactive)
  (let (helm-migemo-mode)
    (helm-swoop :$query $query :$multiline $multiline)))
(defun isearch-forward-or-helm-swoop-or-helm-occur (use-helm-swoop)
  (interactive "p")
  (let (current-prefix-arg
        (helm-swoop-pre-input-function 'ignore))
    (call-interactively
     (case use-helm-swoop
       (1 'isearch-forward)
       ;; C-u C-sを押した場合
       ;; 1000000以上のバッファサイズならばhelm-occur、
       ;; それ以下ならばhelm-swoop
       (4 (if (< 1000000 (buffer-size)) 'helm-occur 'helm-swoop))
       ;; C-u C-u C-sでmigemoなしのhelm-swoop
       (16 'helm-swoop-nomigemo)))))
(global-set-key (kbd "C-s") 'isearch-forward-or-helm-swoop-or-helm-occur)
;; isearch 中に C-o で occur(or swoop) に移行
(define-key isearch-mode-map (kbd "C-o") (if (< 1000000 (buffer-size)) 'helm-occur 'helm-swoop))
;; search kaizen ここまで

;;; C-M-g で helm-ag (grep), C-u C-M-g で helm-cmd-t-grep (git grep)
(use-package helm-ag :ensure t
  :config
  (setq helm-ag-base-command "rg --vimgrep --no-heading")
  (mykie:global-set-key "C-M-g"
    :default    helm-ag
    :C-u!       helm-cmd-t-grep
    )
  )
(global-set-key (kbd "C-M-k") 'backward-kill-sexp) ;推奨
