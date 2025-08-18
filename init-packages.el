;; Add lisp directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Package management is handled by straight.el (bootstrapped in init.el).
;; Avoid using package.el here to speed startup and prevent duplicate installs.
;; If you still need GNU ELPA packages via package.el, wire them explicitly.

;; (require 'init-elpa) ; Not needed with straight.el
;; (require 'init-mirrors) ; Only needed if you use package.el archives
(require 'init-editor)
(require 'init-auto-complete)
(require 'init-ido-mode)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-rails)
(require 'init-company-mode)
;; (require 'init-lsp-mode)
;; (require 'init-lsp-ui)
(require 'init-web-mode)
(require 'init-frames)
(require 'init-formatter)
(require 'init-treesitter)
(require 'init-mmm)
(require 'init-docker)  ;; Docker integration: CLI, Dockerfile, Compose

(provide 'init-packages)
;;;init-packages.el ends here
