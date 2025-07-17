;;; init.el --- My init.el  -*- lexical-binding: t; -*-
;;; Commentary:

;; My init.el.

;;; Code:
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
            (history-length . 1800)
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
  (set-language-environment 'utf-8)
  (keyboard-translate ?\C-h ?\C-?))

(leaf ddskk
  :ensure t
  :custom ((skk-sticky-key . ";"))
  :config
  (setopt
   default-input-method "japanese-skk")
  )

(leaf simple
  :doc "basic editing commands for Emacs - mark and kill ring settings"
  :tag "builtin"
  :custom ((set-mark-command-repeat-pop . t)
           (kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . nil)))

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

(leaf consult
  :doc "Consulting completing-read"
  :ensure t
  :init
  ;; Replace some built-in commands with consult alternatives
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind (;; Navigation
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; Search
;; ToDo         ;; ("C-s" . consult-line)
         ("C-M-s" . consult-ripgrep)
         ;; Buffer and file navigation
         ("C-x b" . consult-buffer)
         ;; History
         ("M-y" . consult-yank-pop))
  :config
  ;; Configure project root detection
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(leaf consult-ghq
  :doc "Consult interface for ghq"
  :ensure t
  :after consult
  :bind (("C-c g" . consult-ghq-find)
         ("C-c G" . consult-ghq-switch-project))
  :custom ((consult-ghq-find-function . #'find-file))
  :config
  ;; Add ghq source to consult-buffer (use correct source name)
  (with-eval-after-load 'consult
    (when (boundp 'consult-ghq--source)
      ;; (add-to-list 'consult-buffer-sources 'consult-ghq--source 'append))))
      (add-to-list 'consult-buffer-sources 'consult--source-ghq 'append))))

(leaf keybindings
  :doc "Basic key bindings for improved productivity"
  :config
  ;; Window operations
  (global-set-key (kbd "C-,") 'other-window)

  ;; Buffer operations
  (global-set-key (kbd "C-M-k") 'kill-buffer)

  ;; Scrolling
  (global-set-key (kbd "C-z") 'scroll-down)

  ;; Text editing
  (global-set-key (kbd "C-c C-d") 'delete-pair)
  (global-set-key (kbd "C-M-y") 'insert-register)
  )

(leaf hs-minor-mode
  :doc "Hide/Show minor mode for code folding"
  :tag "builtin"
  :bind (;; VSCode-like keybindings for macOS
         ("M-s-[" . hs-hide-block)
         ("M-s-]" . hs-show-block)
         ;; Alternative keybindings for non-Mac or preference
         ("C-c C-h" . hs-hide-block)
         ("C-c C-s" . hs-show-block)
         ("C-c C-M-h" . hs-hide-all)
         ("C-c C-M-s" . hs-show-all))
  :hook ((emacs-lisp-mode-hook . hs-minor-mode)
         (lisp-mode-hook . hs-minor-mode)
         (python-mode-hook . hs-minor-mode)
         (javascript-mode-hook . hs-minor-mode)
         (js-mode-hook . hs-minor-mode)
         (c-mode-hook . hs-minor-mode)
         (c++-mode-hook . hs-minor-mode)))

(leaf multiple-cursors
  :doc "Multiple cursors for Emacs"
  :ensure t
  :bind (;; Option-based keybindings (preserve C-d for delete-char)
         ("M-s-d" . mc/mark-next-like-this)
         ("M-s-S-d" . mc/mark-previous-like-this)
         ("M-C-s-d" . mc/mark-all-like-this)
         ;; Line editing
         ("C-S-l" . mc/edit-lines)
         ;; Mouse support
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)
         ;; Additional useful bindings
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this)
         ("C-c m l" . mc/edit-lines)))

(leaf expand-region
  :doc "Increase selected region by semantic units"
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/contract-region)))

(leaf comment-dwim-2
  :doc "An all-in-one comment command to rule them all"
  :ensure t
  :bind (("M-;" . comment-dwim-2)))

;; C-a, C-e で先頭, 末尾
(leaf sequential-command
  :config
  (leaf sequential-command-config
	:hook (emacs-startup-hook . sequential-command-setup-keys)))

(leaf modus-themes
  :ensure t
  :custom ((modus-themes-itaric-constructs . t)
           (modus-themes-bold-constructs . nil)
           (modus-themes-region . '(bg-only no-extend))
           (modus-themes-subtle-line-numbers . t)
           (modus-themes-headings ; this is an alist: read the manual or its doc string
            . '((1 . (overline background))
                (2 . (rainbow overline))
                (t . (no-bold)))
            )
           )
  :config
  (load-theme 'modus-vivendi-tinted)
  )

;; (leaf telephone-line
;;   :ensure t)

;; (leaf doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   )

(leaf moody
  :ensure t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(leaf nerd-icons
  :ensure t)

;; (leaf doom-themes
;;   :doc "An opinionated pack of modern color-themes"
;;   :ensure t
;;   :custom ((doom-themes-enable-bold . t)
;;            (doom-themes-enable-italic . t))
;;   :config
;;   ;; Load theme
;;   (load-theme 'doom-one t)
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Improve org-mode fontification
;;   (doom-themes-org-config))

;; (leaf all-the-icons
;;   :doc "A library for inserting developer icons"
;;   :ensure t
;;   :config
;;   ;; Run M-x all-the-icons-install-fonts after first installation
;;   (unless (find-font (font-spec :name "all-the-icons"))
;;     (message "Install fonts with: M-x all-the-icons-install-fonts")))

(leaf font-settings
  :doc "Font configuration for better readability"
  :config
  (when (eq system-type 'darwin)
    ;; Font priority order for macOS
    (cond
     ;; First choice: UDEV Gothic 35NFLG (excellent for programming)
     ((find-font (font-spec :name "UDEV Gothic 35NFLG"))
      (set-face-attribute 'default nil :family "UDEV Gothic 35NFLG" :height 180))
     ;; Second choice: SF Mono (macOS standard monospace)
     ((find-font (font-spec :name "SF Mono"))
      (set-face-attribute 'default nil :family "SF Mono" :height 180))
     ;; Fallback options
     ((find-font (font-spec :name "Monaco"))
      (set-face-attribute 'default nil :family "Monaco" :height 180))
     ((find-font (font-spec :name "Menlo"))
      (set-face-attribute 'default nil :family "Menlo" :height 180))
     ((find-font (font-spec :name "Consolas"))
      (set-face-attribute 'default nil :family "Consolas" :height 180)))
    
    ;; Japanese font settings (UDEV Gothic handles Japanese well, but fallback)
    (unless (find-font (font-spec :name "UDEV Gothic 35NFLG"))
      (when (find-font (font-spec :name "Hiragino Kaku Gothic ProN"))
        (set-fontset-font t 'japanese-jisx0208
                          (font-spec :family "Hiragino Kaku Gothic ProN"))))
    
    ;; Font smoothing on macOS
    (setq mac-allow-anti-aliasing t)

    ;; Spacing
    (setopt line-spacing 0.2)
    ))

(leaf magit
  :doc "A Git porcelain inside Emacs"
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom ((magit-diff-refine-hunk . t)))

(leaf git-gutter
  :doc "Show git diff in gutter"
  :ensure t
  :custom ((git-gutter:modified-sign . "~")
           (git-gutter:added-sign . "+")
           (git-gutter:deleted-sign . "-"))
  :global-minor-mode global-git-gutter-mode)

(leaf dired-sidebar
  :doc "Tree style file explorer sidebar"
  :ensure t
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :custom ((dired-sidebar-subtree-line-prefix . " ")
           ;; (dired-sidebar-theme . 'all-the-icons)
           (dired-sidebar-theme . 'nerd-icons)
           (dired-sidebar-use-term-integration . t)
           (dired-sidebar-use-custom-font . t))
  :config
  (leaf all-the-icons-dired
    :ensure t
	:hook (dired-mode-hook . all-the-icons-dired-mode))
  
  ;; Auto-open dired-sidebar on startup (except for tmp directories)
  (defun auto-open-dired-sidebar ()
    "Open dired-sidebar automatically if not in tmp directory"
    (when (and (not (string-match-p "tmp" default-directory))
               (not (string-match-p "/tmp/" default-directory)))
      (dired-sidebar-show-sidebar)))
  
  (add-hook 'emacs-startup-hook 'auto-open-dired-sidebar))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

(leaf dmacro
  :doc "Dynamic macro - repeat complex operations"
  :ensure t
  :bind (("C-S-e" . dmacro-exec))
  :global-minor-mode global-dmacro-mode)

(leaf markdown-mode
  :doc "Major mode for markdown"
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setopt
   markdown-header-scaling t
   markdown-fontify-code-blocks-natively  t)
  )

;; End:
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
