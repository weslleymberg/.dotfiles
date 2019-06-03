
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;;;Personal preferences

;;Set emacs home (needed on windows 10 wsl).
(setq default-directory (getenv "HOME"))

;;Delete select text when typing
(delete-selection-mode t)

;;Reduces time spent with gc
(setq gc-cons-threshold 20000000)

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
(tool-bar-mode -1)
(menu-bar-mode -1)

;;Dired
(setq dired-dwim-target t)

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

;;Enable auto-save mode
(auto-save-visited-mode)
(setq auto-save-visited-interval 3)

;;Use roswel slime helper
(load (expand-file-name "~/.roswell/helper.el"))

;;Load slime for local development
(defun slime-qlot (directory)
  "Start slime on DIRECTORY for local development."
  (interactive (list (read-directory-name "Project directory: ")))
  (slime-start :program "qlot"
               :program-args '("exec" "ros" "-S" "." "run")
               :directory directory
               :name 'qlot
               :env (list (concat "PATH=" (mapconcat 'identity exec-path ":")))))

;;Set default browser to eww
(setq browse-url-browser-function 'eww-browse-url)

;;;Personal functions
(defun choose-programming-subject ()
  "Choose programming subject from list."
  (interactive)
  (let* ((subjects '("3D Renderer" "Augmented Reality" "BitTorrent Client" "Blockchain / Cryptocurrency" "Bot" "Command-Line Tool" "Database" "Docker" "Emulator / Virtual Machine" "Front-end Framework / Library" "Game" "Git" "Network Stack" "Neural Network" "Operating System" "Physics Engine" "Programming Language" "Regex Engine" "Search Engine" "Shell" "Template Engine" "Text Editor" "Visual Recognition System" "Voxel Engine" "Web Search Engine" "Web Server" "Uncategorized"))
         (subjects-length (length subjects))
         (choice (random subjects-length)))
    (message "%s" (elt subjects choice))))

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

;;;Enable all-the-icons
(use-package all-the-icons
  :ensure t)

;;;Enable Emacs Dashboard
(use-package dashboard
  :ensure t
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
                          (projects . 5))))

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
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

;;;Enable pdf-tools
(use-package pdf-tools
  :ensure t
  :init
  (pdf-loader-install))

;;;Enable paren-face
(use-package paren-face
  :ensure t
  :config
  (global-paren-face-mode))

;;;Enable ivy related packages
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :ensure t
  :bind (("C-c g" . 'counsel-git-grep)
         ("C-x l" . 'counsel-locate))
  :config
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package flx
  :ensure t)

;;needed by swiper
(use-package smex
  :ensure t)

;;;Enable org-bullets
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;;;Enable smartparens
(use-package smartparens-config
  :ensure smartparens
  :hook (c-mode . turn-on-smartparens-mode))

;;;Enable picolisp-mode
;; Clone picolisp-mode from https://github.com/tj64/picolisp-mode
(use-package picolisp
  :load-path "site-lisp/picolisp-mode"
  :mode ("\\.l$" . picolisp-mode)
  :hook (picolisp-mode-hook . tsm-mode))

;;;Enable paredit
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor do an \"electric\" return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor match `electrify-return-match' then
open and indent an empty line between the cursor and the text.
Move the cursor to the new line."
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
  (add-hook 'picolisp-mode-hook         #'enable-paredit-mode)
  (add-hook 'inferior-picolisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
  :bind ("RET" . electrify-return-if-match))

;;;Enable avy
(use-package avy
  :ensure t
  :bind (("C-:" . 'avy-goto-word-1)
         ("M-g g" . 'avy-goto-line)))

;;;Enable ace-window
(use-package ace-window
  :ensure t
  :bind (("M-o" . 'ace-window)
         ("C-x o" . 'ace-window)))

;;;Enable which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;Enable Companymode
(use-package company
  :ensure t
  :bind ("<C-tab>" . company-complete-common)
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-limit 20)                       ; bigger popup window
  (setq company-echo-delay 0)                           ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command))) ; start completition right after typing

;;;Enable company-quickhelp
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

;;;Enable FlyCkeck
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

;;;Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;;;Enable smart-mode-line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light)
  (sml/setup))

;;;Hide minor modes
(use-package minions
  :ensure t
  :config (minions-mode 1))

;;;Enable volatile highlights
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(ansi-term-color-vector
   [unspecified "#2e2e2e" "#bc8383" "#7f9f7f" "#d0bf8f" "#6ca0a3" "#dc8cc3" "#8cd0d3" "#b6b6b6"] t)
 '(custom-safe-themes
   (quote
    ("f738c3eb5cfc7e730fea413f9cd8ba0624bd8b4837451660fe169f13f77c7814" "39fe48be738ea23b0295cdf17c99054bb439a7d830248d7e6493c2110bfed6f8" "5b52a4d0d95032547f718e1574d3a096c6eaf56117e188945ae873bdb3200066" "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c" "de1f10725856538a8c373b3a314d41b450b8eba21d653c4a4498d52bb801ecd2" "5ed25f51c2ed06fc63ada02d3af8ed860d62707e96efc826f4a88fd511f45a1d" "94e31993c54782f0db8494c42dc7ddd4195f9e3e43b9caff63f3f7f6ad6c8693" "1dacaddeba04ac1d1a2c6c8100952283b63c4b5279f3d58fb76a4f5dd8936a2c" "6c5a5c47749e7992b4da3011595f5470f33e19f29b10564cd4f62faebbe36b91" "8150ded55351553f9d143c58338ebbc582611adc8a51946ca467bd6fa35a1075" default)))
 '(fci-rule-color "#f8fce8")
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (sudo-edit monotropic-theme punpun-theme plan9-theme plain-theme brutalist-theme goose-theme grayscale-theme greymatters-theme ido-completing-read+ magit-svn php-mode org-bullets yasnippet-snippets which-key web-mode volatile-highlights use-package uptimes smex smartparens smart-mode-line slime restclient pyenv-mode py-autopep8 projectile paren-face paredit org multiple-cursors magit ido-vertical-mode flycheck-prospector expand-region emmet-mode eink-theme dashboard company-web company-quickhelp company-jedi company-anaconda auto-package-update ace-jump-mode)))
 '(picolisp-documentation-directory "/usr/share/doc/picolisp/doc/" t)
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
