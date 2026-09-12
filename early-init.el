;;; early-init.el --- Early init  -*- lexical-binding: t; -*-
;;; Commentary:

;; Loaded before package initialization and GUI setup.

;;; Code:
(setq debug-on-error t)
(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  )

(provide 'early-init)
;;; early-init.el ends here
