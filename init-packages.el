(require 'package)

;; Set package archives (using Tsinghua mirrors for speed in China)
(setq package-archives
      '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; Add lisp directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; List of packages to ensure are installed
(setq package-list '(use-package))

(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(message "Initializing auto complete module...")
(require 'init-auto-complete)
(message "Initializing dired sidebar module...")
(require 'init-dired-sidebar)
(message "Initializing ido mode module...")
(require 'init-ido-mode)
(message "Initializing projectile module...")
(require 'init-projectile)
(message "Initializing ivy mode module...")
(require 'init-ivy-mode)
(message "Initializing flycheck module...")
(require 'init-flycheck)
(message "Initializing company mode module...")
(require 'init-company-mode)
(message "Initializing lsp mode module...")
(require 'init-lsp-mode)
(message "Initializing web mode module...")
(require 'init-web-mode)
(message "Initializing frames module...")
(require 'init-frames)
(message "Initializing formatter module...")
(require 'init-formatter)
;; (require 'init-custom-variables) ; Only load from init.el to avoid duplication
(message "Initializing editor module...")
(require 'init-editor)

(provide 'init-packages)
;;;init-packages.el ends here
