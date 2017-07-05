(require-package 'flycheck)
(require-package 'use-package)

(use-package flycheck
	     :ensure t
	     :init (global-flycheck-mode))

(provide 'init-flycheck)
