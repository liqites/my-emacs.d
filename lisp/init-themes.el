;; (require-package 'dracula-theme)
;; (load-theme 'dracula t)

;;; Theme Ample ---
;; (use-package ample-theme
;;   :init (progn (load-theme 'ample t t)
;;                (load-theme 'ample-flat t t)
;;                (load-theme 'ample-light t t)
;;                (enable-theme 'ample-light))
;;   :defer t
;;   :ensure t)

;;; Theme Afternoon ---
;; (use-package afternoon-theme
;;   :init (progn (load-theme 'afternoon t))
;;   :defer t
;;   :ensure t)

(straight-use-package 'spacemacs-theme)
(load-theme 'spacemacs-light t)

(provide 'init-themes)
