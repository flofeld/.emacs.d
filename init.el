;; Start Emacs in Fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Documents/orgstuff/1.org")))
 '(package-selected-packages
   (quote
	(flycheck-objc-clang flycheck-irony company-irony-c-headers rtags yaml-mode projectile lsp-java dap-mode lsp-ui company-lsp use-package-ensure-system-package solarized-theme scss-mode rainbow-delimiters paredit org-bullets moody go-errcheck flycheck-package exec-path-from-shell diff-hl company-go coffee-mode auto-compile ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
