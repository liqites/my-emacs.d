(require 'package)

(let* ((no-ssl (and (memq system-type '(windwos-nt ms-dots))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There arsze two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
)

;; load all find in ./lisp folder
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'package-archives
	     '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;; (add-to-list 'package-archives
;; 	     '("marmalade" . "http://marmalade-repo.org/packages/"))


(setq package-list
'(use-package
))

;; activate all packages
(package-initialize)

;; fetch list of packages available
(when (not package-archive-contents)
    (package-refresh-contents))

;; fetch list of packages available
(unless package-archive-contents
(package-refresh-contents))

;; install missing packages
(dolist (package package-list)
(unless (package-installed-p package)
(package-install package)))

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
