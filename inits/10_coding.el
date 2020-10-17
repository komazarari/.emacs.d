;; Go
(defun lsp-go-install-save-hooks()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organaize-imports t t)
  )

(use-package go-mode
  :bind (("M-." . godef-jump)
         )
  :config
  ;(add-hook 'before-save-hook 'gofmt-before-save)
  )
(use-package company-go
  )
(use-package go-eldoc
  )

;; LSP
(use-package lsp-mode
  :hook
  (go-mode . lsp-deferred)
  (javascript-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  )
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package company-lsp
  :commands company-lsp)
(use-package dap-mode
  :after lsp-mode
  :custom
  (dap-auto-configure-features '(sessions locals breakpoints expressions repl controls tooltip))
  :config
  (dap-mode 1)
  (dap-auto-configure-mode 1)
  (require 'dap-hydra) ; hydraでDAPの操作を楽にするもの(Optional)
  (require 'dap-go))


(use-package groovy-mode
  :config
  (setq groovy-indent-offset 2)
  )

;; Docker
(use-package dockerfile-mode
  :ensure t)
