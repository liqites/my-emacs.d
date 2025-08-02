;; (load "~/.emacs.d/init-packages.el")
(load (expand-file-name "init-packages.el" user-emacs-directory))
(require 'init-packages)

;; Load custom variables and faces from a separate file for clarity.
(load (expand-file-name "lisp/init-custom-variables.el" user-emacs-directory))

;; It is recommended to move custom-set-variables and custom-set-faces to init-custom-variables.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
