
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;;;Personal preferences

;;Delete select text when typing
(delete-selection-mode t)

;;Spaces over tabs
(setq-default indent-tabs-mode nil)

;;Widen line spacing
(setq-default line-spacing 0.2)

;;Enable prettify symbol mode
(global-prettify-symbols-mode t)

;;Use ibuffer as the default
(defalias 'list-buffers 'ibuffer)

;;Disable UI component
(scroll-bar-mode -1)
(tool-bar-mode 0)

;;Kill current buffer
;;From pragmaticemacs.com
(defun kill-this-buffer ()
  "Kill the currrent buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;Highlight line
(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :underline t)

;;Show matching parens
(show-paren-mode t)

;;Change default font face
(set-frame-font "Hack-12" nil t)

;;;;Start Package configuration

;;;Install use-package if not already
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;Enable package updates
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;;Enable Emacs Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5))))

;;;Enable expand region plugin
(use-package expand-region
  :ensure t
  :bind ("C-z" . 'er/expand-region))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<". 'mc/mark-all-like-this)))

;;;Enable projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/Projects/"))
  (setq projectile-completion-system 'ido)
  (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

;;;Enable paren-face
(use-package paren-face
  :ensure t
  :config
  (global-paren-face-mode))

;;;Enable Ido
(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (ido-everywhere t)
  (ido-mode 1))

;;;Enable ido vertical mode
(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1))

;;;Enable smex
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;;;Enable uptimes
(use-package uptimes
  :ensure t)

;;;Configure org-mode
(use-package org
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-startup-indented t))

;;;Enable org-bullets
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;;;Enable smartparens
(use-package smartparens-config
  :ensure smartparens
  :hook (python-mode . turn-on-smartparens-mode))
  

;;;Enable paredit
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))


(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :bind ("RET" . electrify-return-if-match))

;;;Enable ace-jump
(use-package ace-jump-mode
  :ensure t
  :bind ("C-c SPC" . ace-jump-mode)
  :bind ("C-M-SPC" . ace-jump-mode-pop-mark)
  :config
  (ace-jump-mode-enable-mark-sync))

;;Enable which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;Enable yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;;;Enable Companymode
(use-package company
  :ensure t
  :bind ("<C-tab>" . company-complete-common)
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-limit 20)                       ; bigger popup window
  (setq company-idle-delay 0)                           ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                           ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command))) ; start completition right after typing

;;;Enable company-quickhelp
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

;;;Enable web-mode
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist '(("django" . "\\.html?\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t))

;;;Enable company-web-html
(use-package company-web-html
  :ensure company-web
  :hook (web-mode . (lambda ()
                      (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))))

;;;Enable emmet
(use-package emmet-mode
  :ensure t
  :hook (web-mode . emmet-mode))

;;;Enable FlyCkeck
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint))

;;;Enable flycheck-prospector
(use-package flycheck-prospector
  :ensure t)

;;;Enable autopep8
(use-package py-autopep8
  :ensure t
  :hook (python-mode . py-autopep8-enable-on-save)
  :config
  (setq py-autopep8-options '("--max-line-length=79")))

;;;Set Lisp compiler to sbcl
(use-package slime
  :ensure t
  :hook (lisp-mode . slime-mode)
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"))

;;;Enable geiser for Scheme
;; (use-package geiser
;;   :ensure t
;;   :config
;;   (setq geiser-repl-query-on-kill-p nil)
;;   (setq geiser-repl-skip-version-check-p t)
;;   (setq geiser-active-implementations '(mit)))

;;;Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;;;Enable pyenv-mode

;;enable pyenv for projectile
(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (progn
          (setenv "VIRTUAL_ENV" (pyenv-mode-full-path project))
          (pyenv-mode-set project))
      (pyenv-mode-unset))))


(use-package pyenv-mode
  :ensure t
  :if (eq system-type 'gnu/linux)
  :init
  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)
  :config
  (pyenv-mode))

;;;Enable company-anaconda
(use-package company-anaconda
  :ensure t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)
         (python-mode . (lambda ()
                          (add-to-list 'company-backends 'company-anaconda)))))

;;;Enable smart-mode-line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

;;;Enable volatile highlights
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

;;;Enable restclient
(use-package restclient
  :ensure t)

;;;Enable eink-theme
(use-package eink-theme
  :ensure t
  :config
  (load-theme 'eink t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-bullets yasnippet-snippets which-key web-mode volatile-highlights use-package uptimes smex smartparens smart-mode-line slime restclient pyenv-mode py-autopep8 projectile paren-face paredit org multiple-cursors magit ido-vertical-mode flycheck-prospector expand-region emmet-mode eink-theme dashboard company-web company-quickhelp company-jedi company-anaconda auto-package-update ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
