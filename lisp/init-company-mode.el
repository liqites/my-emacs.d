(use-package company
  :ensure t
  :demand t)

;; To use company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company-mode)
