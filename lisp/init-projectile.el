(straight-use-package 'projectile)

(projectile-mode t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

(provide 'init-projectile)
