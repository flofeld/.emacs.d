(local-set-key (kbd "TAB") 'tab-to-tab-stop)
(setq indent-tabs-mode t)
(setq tab-width 4)
(setq backward-delete-char-untabify-method 'hungry)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(use-package auto-package-update
:config
(setq auto-package-update-delete-old-versions t)
(setq auto-package-update-hide-results t)
(auto-package-update-maybe))

(load-file "~/.emacs.d/sensible-defaults.el/sensible-defaults.el")
(sensible-defaults/use-all-settings)

(setq user-full-name "Florian Feldmann"
      user-mail-address "kontakt@flofeld.tech"
)

(add-to-list 'load-path "~/.emacs.d/resources/")

(defun hrs/rename-file (new-name)
  (interactive "FNew name: ")
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (when (buffer-modified-p)
             (save-buffer))
          (rename-file filename new-name t)
          (kill-buffer (current-buffer))
          (find-file new-name)
          (message "Renamed '%s' -> '%s'" filename new-name))
      (message "Buffer '%s' isn't backed by a file!" (buffer-name)))))

(defun hrs/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun hrs/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun hrs/visit-last-migration ()
  "Open the most recent Rails migration. Relies on projectile."
  (interactive)
  (let ((migrations
         (directory-files
          (expand-file-name "db/migrate" (projectile-project-root)) t)))
    (find-file (car (last migrations)))))

(defun hrs/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun hrs/find-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defun hrs/region-or-word ()
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word)))

(defun hrs/append-to-path (path)
  "Add a path both to the $PATH variable and to Emacs' exec-path."
  (setenv "PATH" (concat (getenv "PATH") ":" path))
  (add-to-list 'exec-path path))

(set-window-scroll-bars (minibuffer-window) nil nil)

(setq frame-title-format '((:eval (projectile-project-name))))

(global-prettify-symbols-mode t)

(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t)

  (setq solarized-use-variable-pitch nil
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)

  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))



(defun hrs/apply-theme ()
  "Apply the `solarized-dark' theme and make frames just slightly transparent."
  (interactive)
  (load-theme 'solarized-dark t))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (hrs/apply-theme))))
  (hrs/apply-theme))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(setq ring-bell-function 'ignore)

(setq scroll-conservatively 100)

(global-hl-line-mode)

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package ag)

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "M-/") 'company-complete-common)

(use-package magit)

(setq-default tab-width 4)

(use-package subword
  :config (global-subword-mode 1))

(setq compilation-scroll-output t)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package lsp-mode)
(use-package hydra)
(use-package company-lsp)
(use-package lsp-ui)
(use-package lsp-java :after (lsp)
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))
(require 'dap-java)

(use-package lsp-treemacs)

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package scss-mode
  :config
  (setq scss-compile-at-save nil))

(use-package less-css-mode)

(use-package go-mode)
(use-package go-errcheck)
(use-package company-go)
(add-to-list 'company-backends 'company-go)

(setenv "GOPATH" "/home/flofeld/go")
(hrs/append-to-path (concat (getenv "GOPATH") "/bin"))

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook
          (lambda ()
            (set
            (company-mode)
            (if (not (string-match "go" compile-command))
                (set (make-local-variable 'compile-command)
                     "go build -v && go test -v && go vet"))
            (flycheck-mode))))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;For company-go
(require 'company)
(require 'company-go)

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (company-tooltip-limit 20)
  (company-begin-commands '(self-insert-command))
  (global-company-mode t))

(add-hook 'go-mode-hook
      (lambda ()
        (company-mode)))

;  (use-package company-rtags
;	:config (add-to-list 'company-backends 'company-rtags))
  (use-package company-c-headers
	:config (add-to-list 'company-backends 'company-c-headers))
;  (use-package flycheck-rtags)
  (use-package irony)
  (use-package flycheck-irony)
  (eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  (use-package company-irony
	:config (add-to-list 'company-backends 'company-irony))

  (add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(use-package coffee-mode)

(setq js-indent-level 2)

(add-hook 'coffee-mode-hook
          (lambda ()
            (yas-minor-mode 1)
            (setq coffee-tab-width 2)))

(use-package paredit)

(use-package rainbow-delimiters)

(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   (setq show-paren-style 'expression)
                   (paredit-mode)
                   (rainbow-delimiters-mode))))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package flycheck-package)

(eval-after-load 'flycheck
  '(flycheck-package-setup))

(require 'org-agenda)
(global-set-key "\C-ca" 'org-agenda)

(use-package org)

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(setq org-log-done 'time)

(require 'ox-md)
(require 'ox-beamer)

(setq org-export-with-smart-quotes t)

(use-package htmlize)
(setq org-html-postamble nil)

;(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)

(customize-set-value 'org-latex-with-hyperref nil)


(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq TeX-parse-self t)

(setq TeX-PDF-mode t)

(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "okular %s"))))

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'gfm-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(hrs/append-to-path "/usr/local/bin")

(save-place-mode t)

(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)

(use-package smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)

(use-package yasnippet)
(use-package yasnippet-snippets)
(yas-minor-mode t)

(use-package flycheck)
(global-flycheck-mode t)
