(global-set-key (kbd "C-p") 'next-line)
(global-set-key (kbd "C-o") 'previous-line)
(global-set-key (kbd "C-l") 'backward-word)
(global-set-key (kbd "C-ö") 'forward-word)
(global-set-key (kbd "M-l") 'backward-char)
(global-set-key (kbd "M-ö") 'forward-char)

(global-set-key (kbd "C-s-p") 'windmove-down)
(global-set-key (kbd "C-s-o") 'windmove-up)
(global-set-key (kbd "C-s-ö") 'windmove-right)
(global-set-key (kbd "C-s-l") 'windmove-left)

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

;; Buffer Menu
(defun rh/buffer-menu-hooks ()
  (local-set-key (kbd "C-o") 'previous-line)
  (local-set-key (kbd "C-p") 'next-line))

(add-hook 'buffer-menu-mode-hook 'rh/buffer-menu-hooks)

;; ido
(defun rh/bind-ido-keys ()
  (define-key ido-completion-map (kbd "C-p") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-o") 'ido-prev-match))

(add-hook 'ido-setup-hook #'rh/bind-ido-keys)

(provide 'setup-key-bindings)
