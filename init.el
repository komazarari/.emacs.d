;;(require 'package)
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (use-package leaf :ensure t)

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init)))

(leaf leaf-convert
  :doc "Convert many format to leaf format"
  :ensure t)

(leaf leaf-tree
  :doc "tree view for init.el leaves"
  :ensure t)

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el")))
  :config (load custom-file))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))
  :leaf-defer nil
  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((user-full-name . "Takuto Komazaki")
            (user-mail-address . "komazarari@gmail.com")
            (user-login-name . "komazarari")
            (create-lockfiles . nil)
            (tab-width . 4)
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
            (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil)
            (global-hl-line-mode . t)
            (electric-pair-mode . t)
            (savehist-mode . 1)
            (column-number-mode . 1)
            (prefer-coding-system . 'utf-8)
            (frame-title-format . "Emacs %f"))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq-default bidi-display-reordering nil)
  (display-time)
  (global-display-line-numbers-mode)
  (setq-default indicate-empty-lines t)
  (setq-default indicate-buffer-boundaries 'left)
  (keyboard-translate ?\C-h ?\C-?))

(leaf simple
  :doc "basic editing commands for Emacs - mark and kill ring settings"
  :tag "builtin"
  :custom ((set-mark-command-repeat-pop . t)
           (kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :global-minor-mode global-auto-revert-mode)

(leaf del-selection
  :doc "delete selection if you insert"
  :global-minor-mode delete-selection-mode)

(leaf ido
  :doc "ido-mode"
  :global-minor-mode ido-mode)

(leaf paren
  :doc "highlight matching paren"
  :global-minor-mode show-paren-mode)

(leaf which-key
  :doc "Display available keybindings in popup"
  :ensure t
  :global-minor-mode t)

(leaf files
  :doc "file input and output commands for Emacs - backup and auto-save settings"
  :tag "builtin"
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)))

(leaf startup
  :doc "process Emacs shell arguments - auto-save list file location"
  :tag "builtin" "internal"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf savehist
  :doc "save minibuffer history"
  :tag "builtin"
  :custom (
           (savehist-additional-variables . '(search-ring regexp-search-ring))
           )
  :global-minor-mode savehist-mode)

(leaf recentf
  :doc "setup to save you a lot of time when switching between files"
  :tag "builtin"
  :custom ((recentf-max-saved-items . 500)
           (recentf-exclude . '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:"
                                "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/")))
  :config
  (defvar recentf-auto-save-timer)
  (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  :global-minor-mode recentf-mode)

(leaf vertico
  :doc "VERTical Interactive COmpletion - enhanced minibuffer completion"
  :ensure t
  :custom ((vertico-count . 20)
           (vertico-resize . t)
           (vertico-cycle . t))
  :global-minor-mode vertico-mode)

(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :ensure t
  :custom ((completion-styles . '(orderless basic))
           (completion-category-defaults . nil)
           (completion-category-overrides . '((file (styles partial-completion))))))

(leaf hippie-exp
  :doc "expand text trying various ways to find its expansion"
  :tag "builtin"
  :bind (("M-/" . hippie-expand)
         ("C-M-/" . hippie-expand-undo))
  :custom ((hippie-expand-try-functions-list . '(try-complete-file-name-partially
                                                  try-complete-file-name
                                                  try-expand-dabbrev
                                                  try-expand-dabbrev-all-buffers
                                                  try-expand-dabbrev-from-kill)))
  :config
  (defun hippie-expand-undo ()
    "Undo the expansion done by hippie-expand."
    (interactive)
    (undo)))

;; marginalia:
;; - marginalia-mode: 補完候補に詳細情報を表示するモードを有効化
;; - M-A: 注釈の表示レベルを切り替え（詳細⇔簡潔⇔なし）
;; - ミニバッファ内でもM-A: 補完中にも注釈レベルを変更可能
;; Marginaliaで表示される情報例:
;; - ファイル: サイズ、更新日時、パーミッション
;; - バッファ: モード、サイズ、ファイルパス
;; - コマンド: キーバインド、説明
;; - 変数: 値、説明
;; - 関数: 引数、説明
;; テスト
;; 1. M-x find-file - ファイル情報が右側に表示
;; 2. C-x b (switch-to-buffer) - バッファ情報が表示
;; 3. M-x - コマンドの説明とキーバインドが表示
;; 4. 補完中にM-Aを押すと注釈レベルが変わります
(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         (minibuffer-local-map
          ("M-A" . marginalia-cycle)))
  :global-minor-mode marginalia-mode)
