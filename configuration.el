(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

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

(use-package flycheck)

(setq-default tab-width 4)

(use-package subword
  :config (global-subword-mode 1))

(setq compilation-scroll-output t)

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

(setenv "GOPATH" "/home/flofeld/go")
(hrs/append-to-path (concat (getenv "GOPATH") "/bin"))

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-go))
            (company-mode)
            (if (not (string-match "go" compile-command))
                (set (make-local-variable 'compile-command)
                     "go build -v && go test -v && go vet"))
            (flycheck-mode)))

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
        (set (make-local-variable 'company-backends) '(company-go))
        (company-mode)))

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

(customize-set-value 'org-latex-with-hyperref nil)


;(require 'ox-latex)
;(unless (boundp 'org-latex-classes)
;  (setq org-latex-classes nil))
;(add-to-list 'org-latex-classes
;             '("article"
;               "\\documentclass{article}"
;               ("\\section{%s}" . "\\section*{%s}")))

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
