(setq c-default-style "linux"
      c-basic-offset 4
      indent-tabs-mode nil)

(require 'ede)
(global-ede-mode)

(rh/add-to-mode 'c++-mode (list
                           "\\.h$"
                           "\\.hpp$"
                           "\\.cpp$"
                           "\\.c$"
			   "\\.cuh$"
			   "\\.clh$"
			   "\\.inl$"))

(defun rh/unmap-from-c++-mode-map ()
  (define-key c++-mode-map "\C-i" nil))

(add-hook 'c++-mode-hook 'rh/unmap-from-c++-mode-map)
(add-hook 'c-mode-hook 'rh/unmap-from-c++-mode-map)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; CMake
(use-package cmake-mode)

;; Shaders
(use-package glsl-mode
  :init
  (rh/add-to-mode 'glsl-mode (list
                              "\\.frag$"
                              "\\.vert$")))

;; Opencl
(use-package opencl-mode
  :init
  (rh/add-to-mode 'opencl-mode (list "\\.cl$")))

;; CUDA
(use-package cuda-mode
  :init
  (rh/add-to-mode 'cuda-mode (list "\\.cu$")))

;; Company
(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))

(use-package cc-mode
  :init
  (define-key c-mode-map [(tab)] 'company-complete)
  (define-key c++-mode-map [(tab)] 'company-complete))

;; Flycheck
(use-package flycheck
  :config
  (global-flycheck-mode))

;; Eldoc
(use-package eldoc
  :config
  (add-hook 'c++-mode-hook 'eldoc-mode)
  (add-hook 'c-mode-hook 'eldoc-mode))

;; Irony
(use-package irony
  :config
  ;; If irony server was never installed, install it.
  (unless (irony--find-server-executable) (call-interactively #'irony-install-server))

  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)

  ;; Use compilation database first, clang_complete as fallback.
  (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
						  irony-cdb-clang-complete))

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  ;; Use irony with company to get code completion.
  (use-package company-irony
    :config
    (eval-after-load 'company '(add-to-list 'company-backends 'company-irony)))

  ;; Use irony with flycheck to get real-time syntax checking.
  (use-package flycheck-irony
    :config
    (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

  ;; Eldoc shows argument list of the function you are currently writing in the echo area.
  (use-package irony-eldoc
    :config
    (add-hook 'irony-mode-hook #'irony-eldoc)))

(provide 'setup-c++-mode)
