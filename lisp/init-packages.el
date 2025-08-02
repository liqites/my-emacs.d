(require 'package)

;; Set package archives (using Tsinghua mirrors for speed in China)
(setq package-archives
      '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; Add lisp directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Initialize and refresh package contents if needed
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages
(setq package-list '(use-package))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Load core configuration modules
(require 'init-auto-complete)
(require 'init-dired-sidebar)
(require 'init-ido-mode)
(require 'init-projectile)
(require 'init-ivy-mode)
(require 'init-flycheck)
(require 'init-company-mode)
(require 'init-lsp-mode)
(require 'init-web-mode)
(require 'init-frames)
(require 'init-formatter)
(require 'init-custom-variables)
(require 'init-editor)

(provide 'init-packages)
;;;init-packages.el ends here
