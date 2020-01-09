(use-package org-beautify-theme)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun rh/get-home-dir()
  (file-name-as-directory (getenv "HOME")))

(defun rh/get-config-file()
  (setq dir-path (concat (rh/get-home-dir) ".emacs.d"))
  (setq file-path (concat (file-name-as-directory dir-path) "init.el"))
  file-path)

(defun rh/open-config-file()
  (interactive)
  (find-file (rh/get-config-file)))

(defun rh/get-local-docs-dir()
  (file-name-as-directory (concat (getenv "HOME") "/.docs")))

(defun rh/get-local-todo-file()
  (concat (rh/get-local-docs-dir) "todo.org"))

(defun rh/get-local-note-file()
  (concat (rh/get-local-docs-dir) "note.org"))

(defun rh/open-local-todo-file()
  (interactive)
  (find-file (rh/get-local-todo-file)))

(defun rh/open-local-note-file()
  (interactive)
  (find-file (rh/get-local-note-file)))

(global-set-key (kbd "C-c c") 'rh/open-config-file)
(global-set-key (kbd "C-c t") 'rh/open-local-todo-file)
(global-set-key (kbd "C-c n") 'rh/open-local-note-file)

;; org-capture
(global-set-key (kbd "C-c C-,") 'org-capture)
(setq org-capture-todo-file (rh/get-local-todo-file))
(setq org-capture-note-file (rh/get-local-note-file))

(setq
 org-capture-templates
 (quote
  (("t" "Todo" entry
    (file+headline org-capture-todo-file "Backlog")
    "* TODO %?\n %i\n\n Added: %U @ %a")
   ("n" "Note" entry
    (file+headline org-capture-note-file "Notes")
    "* NOTE %?\n %i\n\n Added: %U @ %a"))))
;; ("i" "Ideas" entry
;;  (file+headline org-capture-file "Ideas")
;;  "* %?\n %i\n\n Added: %U @ %a")
;; ("l" "Life Pro Tips" entry
;;  (file+headline org-capture-file "Life Pro Tips")
;;  "* %?\n %i\n\n Added: %U @ %a")
;; ("s" "Notes on Stories" entry
;;  (file+headline org-capture-file "Notes on Stories")
;;  "* %?\n %i\n\n Added: %U @ %a")
;; ("e" "Education" entry
;;  (file+headline org-capture-file "Education")
;;  "* %?\n %i\n\n Added: %U @ %a"))))

(provide 'setup-org-mode)
