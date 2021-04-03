;;; add execute permission
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; fish
(use-package fish-mode
  :ensure t)

;;; flycheck
(use-package flycheck
  :ensure t
  :hook
  (js-mode . global-flycheck-mode)
  (js2-mode . global-flycheck-mode)
  ;; :config
  ;; (custom-set-variables)
  )

;; C
;;; Enable helm-gtags-mode
(custom-set-variables
 ;; '(helm-gtags-prefix-key "\C-t")
 '(helm-gtags-suggested-key-mapping t))

(add-hook 'c-mode-hook 'helm-gtags-mode)

;; Go
(defun lsp-go-install-save-hooks()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organaize-imports t t)
  )

;; (use-package go-mode
;;   :bind (("M-." . godef-jump)
;;          )
;;   :config
;;   ;(add-hook 'before-save-hook 'gofmt-before-save)
;;   )
(use-package company-go
  :ensure t
  )
(use-package go-eldoc
  :ensure t
  )

;; Javascript
(setq js-indent-level 2)
(use-package add-node-modules-path
  :ensure t
  :hook
  (js-mode . add-node-modules-path)
  (js2-mode . add-node-modules-path)
  )

(use-package typescript-mode
  :ensure t)

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
  :ensure t
  :config
  (setq groovy-indent-offset 2)
  )

;; Docker
(use-package dockerfile-mode
  :ensure t)

;; terraform
(use-package company-terraform
  :ensure t
  :config
  (company-terraform-init)
  :hook
  (terraform-mode . terraform-format-on-save-mode)
  )
