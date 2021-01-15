(require 'package)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

 ;(setq package-user-dir "~/.emacs.d/elisp/elpa/")
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	;; ("marmalade" . "http://marmalade-repo.org/packages/")
    ))

(unless package-archive-contents
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; init-loader
(use-package init-loader
	     :ensure t
	     :config
	     (init-loader-load (locate-user-emacs-file "./inits"))
	     )
