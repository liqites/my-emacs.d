(load (expand-file-name "init-packages.el" user-emacs-directory))

;; Load custom variables and faces from a separate file for clarity.
(load (expand-file-name "lisp/init-custom-variables.el" user-emacs-directory))
(require 'init-custom-variables)


 ;; Set Proxy
 (setq url-proxy-services
       '(("http"  . "0.0.0.0:6152")
         ("https" . "0.0.0.0:6152")))

 ;;
 (setq mac-option-modifier 'meta)
