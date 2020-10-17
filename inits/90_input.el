(when (executable-find "mozc_emacs_helper.sh")
  ;; mozc from WSL
  (setq mozc-helper-program-name "mozc_emacs_helper.sh")
  (use-package mozc-im :ensure t)
  (use-package mozc-popup :ensure t)
  ;(use-package mozc-cursor-color :ensure t)

  ;; 試しに mozc-temp
  (use-package mozc-temp :ensure t
    :config
    (global-set-key (kbd "M-n") #'mozc-temp-convert)
    (global-set-key (kbd "C-S-SPC") #'mozc-temp-convert-dwim)
    (global-set-key (kbd "C-\"") #'mozc-temp-convert-dwim)
    (setq mozc-temp-remove-pre-space nil)
    )
  (setq default-input-method "japanese-mozc-im")

  ;; popupスタイル を使用する
  (setq mozc-candidate-style 'popup)
  ;; カーソルの点滅を OFF にする
  (blink-cursor-mode 0)

  ;; C-' で IME をトグルする
;  (global-set-key (kbd "C-'") 'toggle-input-method)
  (global-set-key (kbd "C-'") 'mozc-temp-convert-dwim)
  (define-key isearch-mode-map (kbd "C-'") 'isearch-toggle-input-method)
  (define-minor-mode overriding-minor-mode
    "強制的にC-'を割り当てる"             ;説明文字列
    t                                     ;デフォルトで有効にする
    ""                                    ;モードラインに表示しない
;    `((,(kbd "C-'") . toggle-input-method)))
    `((,(kbd "C-'") . mozc-temp-convert-dwim)))
  ;; mozc-cursor-color を利用するための対策
  (make-variable-buffer-local 'mozc-im-mode)
  (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
  (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))

  (advice-add 'mozc-cursor-color-update
              :around (lambda (orig-fun &rest args)
                        (let ((mozc-mode mozc-im-mode))
                          (apply orig-fun args))))

  ;; isearch を利用する前後で IME の状態を維持するための対策
  (add-hook 'isearch-mode-hook
            (lambda ()
              (setq im-state mozc-im-mode)))
  (add-hook 'isearch-mode-end-hook
            (lambda ()
              (unless (eq im-state mozc-im-mode)
                (if im-state
                    (activate-input-method default-input-method)
                  (deactivate-input-method)))))
  ;; wdired 終了時に IME を OFF にする
  ;; (require 'wdired)
  ;; (advice-add 'wdired-finish-edit
  ;;             :after (lambda (&rest args)
  ;;                      (deactivate-input-method)))

  ;; mozc 関連 helm
  ;; helm でミニバッファの入力時に IME の状態を継承しない
  (setq helm-inherit-input-method nil)

  ;; helm の検索パターンを mozc を使って入力した場合にエラーが発生することがあるのを改善する
  (advice-add 'mozc-helper-process-recv-response
              :around (lambda (orig-fun &rest args)
                        (cl-loop for return-value = (apply orig-fun args)
                                 if return-value return it)))

  ;; helm の検索パターンを mozc を使って入力する場合、入力中は helm の候補の更新を停止する
  (advice-add 'mozc-candidate-dispatch
              :before (lambda (&rest args)
                        (when (and (fboundp 'helm-alive-p)
                                   (helm-alive-p))
                          (cl-case (nth 0 args)
                            ('update
                             (unless helm-suspend-update-flag
                               (helm-kill-async-processes)
                               (setq helm-pattern "")
                               (setq helm-suspend-update-flag t)))
                            ('clean-up
                             (when helm-suspend-update-flag
                               (setq helm-suspend-update-flag nil)))))))

  ;; helm で候補のアクションを表示する際に IME を OFF にする
  (advice-add 'helm-select-action
              :before (lambda (&rest args)
                        (deactivate-input-method)))

  ;; Windows の mozc では、セッション接続直後 directモード になるので hiraganaモード にする
  (advice-add 'mozc-session-execute-command
              :after (lambda (&rest args)
                       (when (eq (nth 0 args) 'CreateSession)
                         ;; (mozc-session-sendkey '(hiragana)))))
                         (mozc-session-sendkey '(Hankaku/Zenkaku)))))

  )
