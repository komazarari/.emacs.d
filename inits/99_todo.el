;; Cfn Lint https://www.emacswiki.org/emacs/CfnLint
;; Set up a mode for JSON based templates

(define-derived-mode cfn-json-mode js-mode
    "CFN-JSON"
    "Simple mode to edit CloudFormation template in JSON format."
    (setq js-indent-level 2))

(add-to-list 'magic-mode-alist
             '("\\({\n *\\)? *[\"']AWSTemplateFormatVersion" . cfn-json-mode))

;; Set up a mode for YAML based templates if yaml-mode is installed
;; Get yaml-mode here https://github.com/yoshiki/yaml-mode
(when (featurep 'yaml-mode)

  (define-derived-mode cfn-yaml-mode yaml-mode
    "CFN-YAML"
    "Simple mode to edit CloudFormation template in YAML format.")

  (add-to-list 'magic-mode-alist
               '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode)))

;; Set up cfn-lint integration if flycheck is installed
;; Get flycheck here https://www.flycheck.org/
(when (featurep 'flycheck)
  (flycheck-define-checker cfn-lint
    "AWS CloudFormation linter using cfn-lint.

Install cfn-lint first: pip install cfn-lint

See `https://github.com/aws-cloudformation/cfn-python-lint'."

    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns ((warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end))
    :modes (cfn-json-mode cfn-yaml-mode))

  (add-to-list 'flycheck-checkers 'cfn-lint)
  (add-hook 'cfn-json-mode-hook 'flycheck-mode)
  (add-hook 'cfn-yaml-mode-hook 'flycheck-mode))

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
