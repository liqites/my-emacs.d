(use-package projectile
  :ensure t
  :defer t)

(projectile-mode t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

(provide 'init-projectile)
