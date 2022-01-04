;;; init.el --- My init.el  -*- lexical-binding: t; -*-
;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(require 'package)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
; (package-initialize)

(eval-when-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  )

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (setq use-package-enable-imenu-support t)
  (require 'use-package)
  )

(eval-and-compile
;  (package-initialize t)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  (leaf leaf-keywords
    :ensure t
    ;;   :init
    ;;   ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    ;;   (leaf hydra :ensure t)
    ;;   (leaf el-get :ensure t)
    ;;   (leaf blackout :ensure t)

    :config
    ;;   ;; initialize leaf-keywords.el
    (leaf-keywords-init)
    )
  )

;; (unless package-archive-contents
;;   (package-refresh-contents))
;;;;;;;;;;;;

(leaf cus-edit
  :tag "builtin" "faces"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((user-full-name . "Takuto Komazaki")
            (user-mail-address . "komazarari@gmail.com")
            (user-login-name . "komazarari")
            (create-lockfiles . nil)
            (debug-on-error . t)
            (init-file-debug . t)
            (frame-resize-pixelwise . t)
            (enable-recursive-minibuffers . t)
            (history-length . 2000)
            (history-delete-duplicates . t)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively . 100)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (text-quoting-style . 'straight)
            (truncate-lines . t)
            ;; (use-dialog-box . nil)
            ;; (use-file-dialog . nil)
            ;; (menu-bar-mode . t)
            ;; (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil)

            ;; C-u C-SPC C-SPC …でどんどん過去のマークを遡る
            (set-mark-command-repeat-pop . t)
            ;; リージョンを削除できるように
            (delete-selection-mode . t)
            ;; 現在行に色をつける
            (global-hl-line-mode . t)
            ;; 自動で対応するカッコ入力する
            (electric-pair-mode . t)
            ;; 対応カッコ強調
            (show-paren-mode . t)
            ;; ミニバッファ履歴を次回Emacs起動時にも保存する
            (savehist-mode . 1)
            ;; 位置を出すようにする
            (column-number-mode . 1)
            ;; utf-8 .. ??
            (prefer-coding-system . 'utf-8)
            ;; タイトルバーにフルパス
            (frame-title-format . "Emacs %f")
            )
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq-default tab-width 4)
  ;; 右から左に読む言語に対応させないことで描画高速化
  (setq-default bidi-display-reordering nil)
  ;; モードラインに時刻を表示する
  (display-time)
  ;; ido
  (ido-mode 1)

  ;; 行番号とか
  (global-display-line-numbers-mode)
  (setq-default indicate-empty-lines t)
  (setq-default indicate-buffer-boundaries 'left)

  (keyboard-translate ?\C-h ?\C-?)
  )

;; ;;; dired の a で開くやつ
(put 'dired-find-alternate-file 'disabled nil)

;; ;;; splash screenを無効にする
;; (setq inhibit-splash-screen t)

;;; インデントにTABを使わないようにする
;; (setq-default indent-tabs-mode nil)

;;; メニューバーとツールバーとスクロールバーを消す
;(menu-bar-mode -1)
;(tool-bar-mode -1)
;(scroll-bar-mode -1)

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
    
;; Backup file
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/.backup"))
            backup-directory-alist))

;; ;;; シェルに合わせるため、C-hは後退に割り当てる
;; (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;; (define-key key-translation-map (kbd "C-?") (kbd "C-h"))
(global-set-key "\C-z" 'scroll-down)
(global-set-key "\C-\M-y" 'insert-register)
(global-set-key "\C-\M-k" 'kill-buffer)

;; GCを減らして軽くする
;(gc-cons-threshold (* 10 gc-cons-threshold)) ;

;; C-, ウィンドウ切り替え
(global-set-key (kbd "C-,") 'other-window)
;;  => 95_additional にも np 同時押し

;; インデント
;(global-set-key "\C-c\C-q" 'indent-region)

;; 対応する括弧を削除
(global-set-key "\C-c\C-d" 'delete-pair)

;; ;; Mac の Cmd は meta key として使う
;; (when (eq system-type 'darwin)
;;   (setq ns-command-modifier (quote meta)))

;; hs-minor-mode
(leaf hs-minor-mode
;  :commands hs-minor-mode
  :bind (
         ("C-{" . hs-hide-block)
         ("C-}" . hs-show-block)
         )
  :hook
  (emacs-lisp-mode-hook 'hs-minor-mode)
  )
;
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

(leaf cc-mode
  :doc "major mode for editing C and similar languages"
  :tag "builtin"
  :defvar (c-basic-offset)
  :bind (c-mode-base-map
         ("C-c c" . compile))
  :mode-hook
  (c-mode-hook . ((c-set-style "gnu")
                  ;; (setq c-basic-offset 4)
                  ))
  (c++-mode-hook . ((c-set-style "gnu")
                    ;; (setq c-basic-offset 4)
                    )))

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom-face
  (show-paren-match . '((nil (:background "#44475a" :foreground "#f1fa8c"))))

  :custom
   (show-paren-delay . 0.1)
   (show-paren-style . 'mixed)
   (show-paren-when-point-inside-paren . t)
   (show-paren-when-point-in-periphery . t)
  :global-minor-mode show-paren-mode)

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

(leaf files
  :doc "file input and output commands for Emacs"
  :tag "builtin"
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)))

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
;  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))
  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
 ;   :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t)
  )

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)
  
(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
;  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

(leaf doom-themes
  :added "2021-05-29"
  :ensure t
  :custom (
           (doom-themes-enable-bold . t)
           (doom-themes-enable-italic . t)
           )
  :config
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(leaf magit
  :ensure t)

(leaf git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

(leaf all-the-icons
  :ensure t)

(leaf doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  ;; :custom (
  ;;          (line-number-mode . t)
  ;;          (column-number-mode . t)
  ;;          )
  ;; (line-number-mode 0)
  ;; ;;(column-number-mode 0)
  ;; (doom-modeline-def-modeline 'main
  ;;                             '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
  ;;                             '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))
  )

(leaf yaml-mode
  :ensure t
  :config
  )

(leaf highlight-indent-guides
  :ensure t
  :hook ((prog-mode-hook
          yaml-mode-hook
          )
         . highlight-indent-guides-mode)
  :custom (
           (highlight-indent-guides-auto-enabled . t)
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-method . 'character) ; column
           )
)

;;;;;; ToDo: migration
;; init-loader
;; (use-package init-loader
;;   :ensure t
;;   :config
;;   (init-loader-load (locate-user-emacs-file "./inits"))
;;   )

(use-package exec-path-from-shell
  :unless (equal system-type 'windows-nt)
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
)

(provide 'init)
;;; init.el ends here
