;; TODO: Check god-mode
;; TODO: Setting C-M-m key-binding seems to not override in all modes to perhaps change multiple-cursors key-bindings
;; NOTE: Keybindings left that are noticable are C-ä C-M-å C-M-ä
;; Resources: https://github.com/pierre-lecocq/emacs4developers https://github.com/emacs-tw/awesome-emacs

;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-utility-functions)
(require 'setup-general)
(require 'setup-c++-mode)
(require 'setup-rust-mode)
(require 'setup-org-mode)
(require 'setup-key-bindings)

;; (require 'setup-matlab-mode)

;; Auto Complete
;; (use-package auto-complete
;;   :config
;;   (global-auto-complete-mode t)
;;   (setq ac-ignore-case t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rtags-xref cmake-ide helm-gtags gtags company-c-headers volatile-highlights clean-aindent-mode dtrt-indent zenburn-theme yasnippet ws-butler use-package smartparens org-bullets org-beautify-theme opencl-mode multiple-cursors magit irony-eldoc highlight-symbol helm-swoop helm-projectile helm-org-rifle flycheck-irony drag-stuff dimmer cuda-mode company-irony)))
 '(safe-local-variable-values
   (quote
    ((company-c-headers-path-user "/home/rasmus/git/tella/tella/include/")
     (company-clang-arguments "-I/home/rasmus/git/tella/tella/include" "-I/home/rasmus/git/tella/apps/maze/include")
     (company-clang-arguments "-I/home/rasmus/git/trixie/include/")
     (eval setq flycheck-clang-include-path
	   (list
	    (expand-file-name "~/git/trixie/include/")))
     (company-clang--arguments "-I/home/rasmus/git/trixie/include")
     (company-clang--arguments "-I/home/rasmus/git/trixie/trixie")
     (company-clang--arguments "-I/home/rasmus/git/trixie/trixie/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
