(use-package exec-path-from-shell
  :unless (equal system-type 'windows-nt)
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )
(use-package helm
  :ensure t
  )
(use-package mykie
  :ensure t
  )

(use-package ace-jump-mode
  :ensure t
  :config
  (setq ace-jump-mode-gray-background nil)
  (setq ace-jump-word-mode-use-query-char nil)
  (setq ace-jump-mode-move-keys
        (append "asdfghjkl;wertyuio" nil))
  (mykie:global-set-key "C-o"
    :default    ace-jump-word-mode
    :C-u!       ace-jump-line-mode
    :C-u*2!     ace-jump-char-mode
    )
  )

;; company に移行してみる?
(use-package auto-complete
  :disabled
;  :ensure t
  :config
  (ac-config-default)
  ;(ac-set-trigger-key "TAB")
  ;(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  ;(define-key ac-mode-map (kbd "<C-tab>") 'auto-complete)
  (setq ac-auto-start 4)
  (setq ac-use-fuzzy t)
  (add-to-list 'ac-user-dictionary-files "~/.emacs.d/ac-dict")
  (setq ac-auto-show-menu 0.2)
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  (add-to-list 'ac-modes 'haml-mode)
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'fundamental-mode)
  )

(use-package company
  :ensure t
  :bind (("<tab>" . company-indent-or-complete-common)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("M-/" . company-dabbrev)
         )
  :config
  (global-company-mode)
  )

;; 現在位置からいい感じに region 選択
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C-M-=") 'er/contract-region)
  )

;; カーソル分身周り
(use-package multiple-cursors
  :ensure t
  :config
  (multiple-cursors-mode)
  )
(use-package region-bindings-mode
  :ensure t
  :config
  (region-bindings-mode-enable)
  :bind (:map region-bindings-mode-map
              ("a" . 'mc/mark-all-like-this)
              ("p" . 'mc/mark-previous-lines)
              ("n" . 'mc/mark-next-lines)
              ("P" . 'mc/mark-previous-like-this)
              ("N" . 'mc/mark-next-like-this)
              ("s" . 'mc/skip-to-next-like-this)
              ("S" . 'mc/skip-to-previous-like-this)
              ("m" . 'mc/mark-more-like-this-extended)
              ("q" . 'query-replace-regexp)
              )
  )

;; C-a, C-e で先頭, 末尾
(use-package sequential-command
  :ensure t
  :config
  (require 'sequential-command-config)
  (sequential-command-setup-keys)
  )

;; 繰り返しを快適に
(use-package smartrep :ensure t)
(smartrep-define-key global-map "C-x"
  '(("o" . other-window)
    ("0" . delete-window)
    ("1" . delete-other-windows)
    ("2" . split-window-below)
    ("3" . split-window-right)
    ("{" . shrink-window-horizontally)
    ("}" . enlarge-window-horizontally)
    ("+" . balance-windows)
    ("^" . enlarge-window)
    ("-" . shrink-window)))

(use-package comment-dwim-2
  :ensure t
  :config
  (global-set-key (kbd "M-;") 'comment-dwim-2)
  )
