(when (maybe-require-package 'projectile)
  
  (add-hook 'ruby-mode-hook 'projectile-mode)
  
  (add-hook 'after-init-hook 'projectile-global-mode)

  ;; The following code means you get a menu if you hit "C-c p" and wait
  (after-load 'guide-key
    (add-to-list 'guide-key/guide-key-sequence "C-c p"))

  (after-load 'projectile
	      (setq-default
	       projectile-mode-line
	       '(:eval
		 (if (file-remote-p default-directory)
		     " Pr"
		   (format " Pr[%s]" (projectile-project-name)))))))
		 

(provide 'init-projectile)
