;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Theme
(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

;; Dimmer
(use-package dimmer
  :config
  (dimmer-mode)
  (setq dimmer-fraction 0.35))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

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

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (call-interactively 'compile)))

;; Show whitespaces
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; Company
(use-package company
  :init
  (global-company-mode 1)
  (delete 'company-semantic company-backends)
  (define-key company-mode-map (kbd "C-M-k") 'company-select-previous)
  (define-key company-mode-map (kbd "C-M-j") 'company-select-next))

;; Projectile
(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (global-set-key (kbd "C-M-d") 'projectile-switch-project))

;; Drag stuff
(use-package drag-stuff
  :config
  (global-set-key (kbd "M-å") 'drag-stuff-up)
  (global-set-key (kbd "M-ä") 'drag-stuff-down))

;; Multiple cursors
(use-package multiple-cursors
  :config
  (global-set-key (kbd "M-n") 'mc/mark-previous-like-this-word)
  (global-set-key (kbd "M-m") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "C-M-n") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-M-m") 'mc/mark-next-like-this))

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

;; rtags
(use-package rtags)
  ;; :config
  ;; (define-key rtags-mode-map (kbd "C-o") 'rtags-previous-match)
  ;; (define-key rtags-mode-map (kbd "C-p") 'rtags-next-match))

;; ggtags
;; (use-package ggtags
;;   :init
;;   (ggtags-mode 1)
;;   (add-hook 'c-mode-common-hook
;; 	    (lambda ()
;; 	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;; 		(ggtags-mode 1))))

;;   (dolist (map (list ggtags-mode-map dired-mode-map))
;;     (define-key map (kbd "C-c f s") 'ggtags-find-other-symbol)
;;     (define-key map (kbd "C-c f h") 'ggtags-view-tag-history)
;;     (define-key map (kbd "C-c f r") 'ggtags-find-reference)
;;     (define-key map (kbd "C-c f f") 'ggtags-find-file)
;;     (define-key map (kbd "C-c f c") 'ggtags-create-tags)
;;     (define-key map (kbd "C-c f u") 'ggtags-update-tags)
;;     (define-key map (kbd "C-c f a") 'helm-gtags-tags-in-this-function)
;;     ;; (define-key map (kbd "M-.") 'ggtags-find-tag-dwim)
;;     ;; (define-key map (kbd "M-,") 'pop-tag-mark)
;;     ;; (define-key map (kbd "C-c <") 'ggtags-prev-mark)
;;     ;; (define-key map (kbd "C-c >") 'ggtags-next-mark)))
;;     ))

;; Helm
(use-package helm
  :config
  (helm-mode t)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (define-key helm-map (kbd "C-o") 'helm-previous-line)
  (define-key helm-map (kbd "C-p") 'helm-next-line)
  (define-key helm-map (kbd "C-l") 'helm-find-files-up-one-level)
  (define-key helm-map (kbd "C-ö") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "<C-backspace>") #'backward-kill-word)

  (use-package helm-swoop
    :config
    (global-set-key (kbd "C-s") 'helm-swoop)
    (global-set-key (kbd "M-s") 'helm-multi-swoop-all)

    (define-key helm-swoop-map (kbd "C-o") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-p") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-o") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-p") 'helm-next-line))

  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (define-key helm-projectile-find-file-map (kbd "<C-backspace>") #'backward-kill-word))

  (use-package helm-org-rifle
    :config
    (global-set-key (kbd "C-c C-s") 'helm-org-rifle))

  ;; (use-package helm-gtags
  ;;   :init
  ;;   (progn
  ;;     (setq helm-gtags-ignore-case t
  ;;           helm-gtags-auto-update t
  ;;           helm-gtags-use-input-at-cursor t
  ;;           helm-gtags-pulse-at-cursor t
  ;;           helm-gtags-prefix-key "\C-cf"
  ;;           helm-gtags-suggested-key-mapping t)

  ;;     ;; Enable helm-gtags-mode in Dired so you can jump to any tag
  ;;     ;; when navigate project tree with Dired
  ;;     (add-hook 'dired-mode-hook 'helm-gtags-mode)

  ;;     ;; Enable helm-gtags-mode in Eshell for the same reason as above
  ;;     (add-hook 'eshell-mode-hook 'helm-gtags-mode)

  ;;     ;; Enable helm-gtags-mode in languages that GNU Global supports
  ;;     (add-hook 'c-mode-hook 'helm-gtags-mode)
  ;;     (add-hook 'c++-mode-hook 'helm-gtags-mode)

  ;;     ;; key bindings
  ;;     (with-eval-after-load 'helm-gtags
  ;;       (define-key helm-gtags-mode-map (kbd "C-c f a") 'helm-gtags-tags-in-this-function)
  ;;       ;; (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  ;;       ;; (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  ;;       ;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  ;;       ;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  ;;       ;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))
  ;;       )))
  )

;; CEDET
(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)

(semantic-mode 1)

(defun rh/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'rh/cedet-hook)
(add-hook 'c-mode-hook 'rh/cedet-hook)
(add-hook 'c++-mode-hook 'rh/cedet-hook)

;; SmartParens
(use-package smartparens
  :config
  (smartparens-global-mode t))

;; Yasnippet
(use-package yasnippet
  :config
  (global-set-key (kbd "C-c C-i") 'yas-insert-snippet)

  (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package yasnippet-classic-snippets)
(use-package yasnippet-snippets)

;; Indentation
;; (use-package dtrt-indent
;;   :init
;;   (dtrt-indent-mode 1)
;;   (setq dtrt-indent-verbosity 0))

;; (use-package clean-aindent-mode
;;   :init
;;   (add-hook 'prog-mode-hook 'clean-aindent-mode))

;; Highlighting
(use-package volatile-highlights
  :init
  (volatile-highlights-mode t))

;; (use-package highlight-symbol
;;   :config
;;   (defun rh/highlight-hooks ()
;;     (highlight-symbol-mode t))

;;   (add-hook 'find-file-hook 'rh/highlight-hooks))

;; ws butler - removes whitespace only in edited lines
(use-package ws-butler
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode 'ws-butler-mode)
  (add-hook 'fundamental-mode 'ws-butler-mode))

(provide 'setup-general)
