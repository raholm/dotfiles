(defun rh/add-to-mode (mode lst)
  (dolist (file lst)
    (add-to-list 'auto-mode-alist
                 (cons file mode))))

(defun rh/indent-files (directory extension)
  (interactive (list (read-directory-name "Directory: ")
		     (read-string "File extension: ")))
  (dolist (file (directory-files-recursively directory extension))
    (find-file file)
    (indent-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))

(provide 'setup-utility-functions)
