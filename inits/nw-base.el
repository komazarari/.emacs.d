;;; 現在行に色をつける
(global-hl-line-mode 1)

;; ido
;(setq ido-enable-flex-matching 1)
(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1) ; GUI は重いので
  ;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  ;; (ido-everywhere 1)
  )
