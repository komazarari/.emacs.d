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
            (set-mark-command-repeat-pop . t)
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
