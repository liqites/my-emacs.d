(require 'package)

;; Add lisp directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; List of packages to ensure are installed
(setq package-list '(use-package))

(require 'init-mirrors)
(set-package-mirror my/current-mirror)

(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'init-elpa)
(require 'init-editor)
(require 'init-auto-complete)
(require 'init-dired-sidebar)
(require 'init-ido-mode)
(require 'init-projectile)
; (require 'init-ivy-mode)
(require 'init-flycheck)
(require 'init-rails)
;; (require 'init-ruby-mode)
(require 'init-company-mode)
(require 'init-lsp-mode)
(require 'init-lsp-ui)
(require 'init-web-mode)
(require 'init-frames)
(require 'init-formatter)
(require 'init-treesitter)
(require 'init-mmm)

(provide 'init-packages)
;;;init-packages.el ends here
