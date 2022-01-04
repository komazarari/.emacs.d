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

(use-package mykie  :ensure t)

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
   ;; :bind (("C-s" . swiper))
    :config
    (mykie:global-set-key "C-s"
      :default    isearch-forward
      :C-u!       swiper
      )
    )

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
           (company-minimum-prefix-length . 2)
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

(use-package ace-jump-mode
  :ensure t
  :config
  (setq ace-jump-mode-gray-background nil)
  (setq ace-jump-word-mode-use-query-char nil)
  (setq ace-jump-mode-move-keys
;        (append "asdfghjkl;wertyuio" nil))
        (append "azwsxedcrfvtgyhnujikol;p[]0/" nil))
  (mykie:global-set-key "C-o"
    :default    ace-jump-word-mode
    :C-u!       ace-jump-line-mode
    :C-u*2!     ace-jump-char-mode
    )
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

;; 空白とかを可視化
(use-package whitespace
  :ensure t
  :config
  (setq whitespace-style '(
                           face           ; faceで可視化
                           trailing       ; 末尾
                           tabs           ; タブ
                           space
                           empty          ; 先頭/末尾の空行
                           space-mark     ; 表示のマッピング
                           tab-mark
                           ))
                                        ; from http://cloverrose.hateblo.jp/entry/2013/04/12/041758
  (setq whitespace-display-mappings
        '(
          ;; (space-mark   ?\     [?\u00B7]     [?.]) ; space - centered dot
          (space-mark   ?\xA0  [?\u00A4]     [?_]) ; hard space - currency
          (space-mark   ?\x8A0 [?\x8A4]      [?_]) ; hard space - currency
          (space-mark   ?\x920 [?\x924]      [?_]) ; hard space - currency
          (space-mark   ?\xE20 [?\xE24]      [?_]) ; hard space - currency
          (space-mark   ?\xF20 [?\xF24]      [?_]) ; hard space - currency
          (space-mark ?\u3000 [?\u25a1] [?_ ?_]) ; full-width-space - square
          ;; NEWLINE is displayed using the face `whitespace-newline'
          ;; (newline-mark ?\n    [?$ ?\n])  ; eol - dollar sign
          ;; (newline-mark ?\n    [?\u21B5 ?\n] [?$ ?\n])	; eol - downwards arrow
          ;; (newline-mark ?\n    [?\u00B6 ?\n] [?$ ?\n])	; eol - pilcrow
          ;; (newline-mark ?\n    [?\x8AF ?\n]  [?$ ?\n])	; eol - overscore
          ;; (newline-mark ?\n    [?\x8AC ?\n]  [?$ ?\n])	; eol - negation
          ;; (newline-mark ?\n    [?\x8B0 ?\n]  [?$ ?\n])	; eol - grade
          ;;
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; the next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t]) ; tab - left quote mark
          ))
  (defvar my/bg-color "#232323")
  (set-face-attribute 'whitespace-trailing nil
                      :background my/bg-color
                      :foreground "DeepPink"
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :background my/bg-color
                      :foreground "LightSkyBlue"
                      :underline t)
  (set-face-attribute 'whitespace-space nil
                      :background my/bg-color
                      :foreground "GreenYellow"
                      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
                      :background my/bg-color)
  (setq whitespace-action '(auto-cleanup))
  (global-whitespace-mode 1)
  )


(provide 'init)
;;; init.el ends here
