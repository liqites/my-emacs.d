
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(load "~/.emacs.d/init-packages")

(require 'init-utils)

(require 'init-elpa)
(require 'init-editor)
(require 'init-themes)
(require 'init-fonts)

(require 'init-ivy)
(require 'init-ido)
(require 'init-projectile)
(require 'init-rails)
(require 'init-spaceline)
(require 'init-auto-complete)

;;
(require 'init-ruby-mode)

;;=======

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages (quote (solarized-theme auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
