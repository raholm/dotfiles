;; TODO: Check god-mode
;; TODO: Setting C-M-m key-binding seems to not override in all modes to perhaps change multiple-cursors key-bindings
;; TODO: Rewrite to use use-package to automatically install all dependencies
;; NOTE: Keybindings left that are noticable are C-ä C-M-å C-M-ä
;; Resources: https://github.com/pierre-lecocq/emacs4developers https://github.com/emacs-tw/awesome-emacs

;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Key bindings for navigation
(global-set-key (kbd "C-p") 'next-line)
(global-set-key (kbd "C-o") 'previous-line)
(global-set-key (kbd "C-l") 'backward-word)
(global-set-key (kbd "C-ö") 'forward-word)
(global-set-key (kbd "M-l") 'backward-char)
(global-set-key (kbd "M-ö") 'forward-char)

(global-set-key (kbd "C-M-p") 'windmove-down)
(global-set-key (kbd "C-M-o") 'windmove-up)
(global-set-key (kbd "C-M-ö") 'windmove-right)
(global-set-key (kbd "C-M-l") 'windmove-left)

(global-set-key (kbd "M-p") 'next-buffer)
(global-set-key (kbd "M-o") 'previous-buffer)

(global-set-key (kbd "C-i") 'forward-paragraph)
(global-set-key (kbd "C-u") 'backward-paragraph)

(global-set-key (kbd "M-i") 'forward-page)
(global-set-key (kbd "M-u") 'backward-page)

(global-set-key (kbd "C-f") 'projectile-find-file)
(global-set-key (kbd "M-f") 'xref-find-definitions)
(global-set-key (kbd "C-M-f") 'find-name-dired)

(global-set-key (kbd "C-6") 'comment-or-uncomment-region)
(global-set-key (kbd "<C-tab>") 'indent-region)

(global-set-key (kbd "C-å") 'goto-line)

(global-set-key (kbd "M-r") 'replace-string)

;; Mode
(defun rh/add-to-mode (mode lst)
  (dolist (file lst)
    (add-to-list 'auto-mode-alist
                 (cons file mode))))

(rh/add-to-mode 'c++-mode (list
                           "\\.h$"
                           "\\.hpp$"
                           "\\.cpp$"
                           "\\.c$"))

(use-package opencl-mode
  :init
  (rh/add-to-mode 'opencl-mode (list "\\.cl$")))

(use-package cuda-mode
  :init
  (rh/add-to-mode 'cuda-mode (list "\\.cu$")))

(use-package glsl-mode
  :init
  (rh/add-to-mode 'glsl-mode (list
			      "\\.vert$"
			      "\\.frag$")))

;; Global Hooks
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Edit
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(setq c-default-style "linux"
      c-basic-offset 4)
(setq indent-tabs-mode nil)
(infer-indentation-style)

;; General
(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

(show-paren-mode t)
(delete-selection-mode t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq inhibit-startup-screen t)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-linum-mode t)
(line-number-mode t)
(column-number-mode t)

(add-hook 'find-file-hook (lambda () (toggle-truncate-lines)))

(setq gc-cons-threshold 20000000) ;; 20MB allocated before running GC

(setq visible-bell 1)

;; Matlab
;; (require 'matlab)
;; (rh/add-to-mode 'matlab-mode (list
;;                               "\\.m$"))

;; Magit
(use-package magit
  :config
  (global-set-key (kbd "C-c g s") 'magit-status)
  (global-set-key (kbd "C-c g p") 'magit-push)
  (global-set-key (kbd "C-c g l") 'magit-pull)
  (global-set-key (kbd "C-c g c") 'magit-commit)
  (global-set-key (kbd "C-c g b") 'magit-branch)
  (global-set-key (kbd "C-c g a") 'magit-commit-amend)
  (global-set-key (kbd "C-c g h") 'magit-stash))

;; SmartParens
(use-package smartparens
  :config
  (smartparens-global-mode t))

;; Yasnippet
(use-package yasnippet
  :config
  (global-set-key (kbd "C-c C-i") 'yas-insert-snippet)
  (yas-global-mode t))
(use-package yasnippet-snippets)

;; Grep
(defun rh/grep-hooks ()
  (local-set-key (kbd "C-o") 'previous-line)
  (local-set-key (kbd "C-p") 'next-line)
  (local-set-key (kbd "M-p") 'next-buffer)
  (local-set-key (kbd "M-o") 'previous-buffer))

(add-hook 'grep-mode-hook 'rh/grep-hooks)

;; Dired
(defun rh/dired-hooks ()
  (local-set-key (kbd "C-o") 'previous-line)
  (local-set-key (kbd "C-p") 'next-line))

(add-hook 'dired-mode-hook 'rh/dired-hooks)

;; Org
(use-package org-beautify-theme)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; org-capture
(global-set-key (kbd "C-c C-,") 'org-capture)
(setq org-capture-file "~/git/tella/docs/todo.org")

(setq
 org-capture-templates
 (quote
  (("t" "Todo" entry
    (file+headline org-capture-file "TODO")
    "* TODO %?\n %i\n\n Added: %U @ %a")
   ("n" "Note" entry
    (file+headline org-capture-file "Notes")
    "* %?\n %i\n\n Added: %U @ %a")
   ("i" "Ideas" entry
    (file+headline org-capture-file "Ideas")
    "* %?\n %i\n\n Added: %U @ %a")
   ("l" "Life Pro Tips" entry
    (file+headline org-capture-file "Life Pro Tips")
    "* %?\n %i\n\n Added: %U @ %a")
   ("s" "Notes on Stories" entry
    (file+headline org-capture-file "Notes on Stories")
    "* %?\n %i\n\n Added: %U @ %a")
   ("e" "Education" entry
    (file+headline org-capture-file "Education")
    "* %?\n %i\n\n Added: %U @ %a"))))

;; Buffer Menu
(defun rh/buffer-menu-hooks ()
  (local-set-key (kbd "C-o") 'previous-line)
  (local-set-key (kbd "C-p") 'next-line))

(add-hook 'buffer-menu-mode-hook 'rh/buffer-menu-hooks)

;; OpenCL
(use-package opencl-mode
  :config
  (rh/add-to-mode 'opencl-mode (list
                                "\\.cl$")))

;; Projectile
(use-package projectile
  :config
  (projectile-mode t))

(global-set-key (kbd "C-M-d") 'projectile-switch-project)

;; Helm Swoop
(use-package helm-swoop
  :config
  (global-set-key (kbd "C-s") 'helm-swoop)
  (global-set-key (kbd "M-s") 'helm-multi-swoop-all)

  (define-key helm-swoop-map (kbd "C-o") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-p") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-o") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-p") 'helm-next-line))

;; Helm Buffers List
(use-package helm
  :config
  (helm-mode t)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (define-key helm-map (kbd "C-o") 'helm-previous-line)
  (define-key helm-map (kbd "C-p") 'helm-next-line))

;; Helm Projectile
(use-package helm-projectile
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

;; Helm Find Files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-map (kbd "C-l") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "C-ö") 'helm-execute-persistent-action)

(define-key helm-find-files-map (kbd "<C-backspace>") #'backward-kill-word)
(define-key helm-projectile-find-file-map (kbd "<C-backspace>") #'backward-kill-word)

;; Helm Org
(use-package helm-org-rifle
  :config
  (global-set-key (kbd "C-c C-s") 'helm-org-rifle))

;; Company
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "C-o") #'company-select-previous)
  (define-key company-active-map (kbd "C-p") #'company-select-next))

(setq company-dabbrev-downcase nil) ;; Preserve capital letters.

;; Highlighting
(use-package highlight-symbol
  :config
  (defun rh/highlight-hooks ()
    (highlight-symbol-mode t))

  (add-hook 'find-file-hook 'rh/highlight-hooks))

;; Auto Complete
(use-package auto-complete
  :config
  (global-auto-complete-mode t)
  (setq ac-ignore-case t))

;; ido
(defun rh/bind-ido-keys ()
  (define-key ido-completion-map (kbd "C-p") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-o") 'ido-prev-match))

(add-hook 'ido-setup-hook #'rh/bind-ido-keys)

;; Drag stuff
(use-package drag-stuff
  :config
  (global-set-key (kbd "M-å") 'drag-stuff-up)
  (global-set-key (kbd "M-ä") 'drag-stuff-down))

;; ws butler - removes whitespace only in edited lines
(use-package ws-butler
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))

;; Dimmer
(use-package dimmer
  :config
  (dimmer-mode)
  (setq dimmer-fraction 0.35))

;; Multiple cursors
(use-package multiple-cursors
  :config
  (global-set-key (kbd "M-n") 'mc/mark-previous-like-this-word)
  (global-set-key (kbd "M-m") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "C-M-n") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-M-m") 'mc/mark-next-like-this))

;; Common file/dir navigations
(defun rh/get-home-dir()
  (file-name-as-directory (getenv "HOME")))

(defun rh/get-dropbox-dir()
  (file-name-as-directory (concat (rh/get-home-dir) "Dropbox/VirtualBox")))

(defun rh/get-dropbox-emacs-config-dir()
  (file-name-as-directory (concat (rh/get-dropbox-dir) "emacs")))

(defun rh/get-config-file()
  (setq dir-path (concat (rh/get-home-dir) ".emacs.d"))
  (setq file-path (concat (file-name-as-directory dir-path) "init.el"))
  file-path)

(defun rh/open-config-file()
  (interactive)
  (find-file (rh/get-config-file)))

(defun rh/open-inbox-file()
  (interactive)
  (setq dir-path (concat (rh/get-dropbox-dir) "docs"))
  (setq file-path (concat (file-name-as-directory dir-path) "inbox.org"))
  (find-file file-path))

(defun rh/get-timestamp()
  (format-time-string "%Y%m%d_%H%M%S"))

(defun rh/backup-config()
  (interactive)
  (setq filename (concat "init_" (rh/get-timestamp) ".el"))
  (setq destination (concat (rh/get-dropbox-emacs-config-dir) filename))
  (copy-file (rh/get-config-file) destination))

(global-set-key (kbd "C-c c") 'rh/open-config-file)
(global-set-key (kbd "C-c i") 'rh/open-inbox-file)
(global-set-key (kbd "C-c b") 'rh/backup-config)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" default)))
 '(global-auto-complete-mode t)
 '(package-selected-packages
   (quote
    (helm-org-rifle helm-projectile multiple-cursors dimmer ws-butler drag-stuff flycheck projectile org-bullets matlab-mode auto-complete highlight-symbol zenburn-theme yasnippet smartparens magit helm-swoop diffview company autopair)))
 '(semantic-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
