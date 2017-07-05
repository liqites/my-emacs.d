(require 'package)
(add-to-list 'package-archives
	     '("popkit" . "https://elpa.popkit.org/packages/"))
(when (< emacs-major-version 24)
  ;; For importnat compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; list the packages you want
(setq package-list
      '(projectile spaceline solarized-theme)
      )

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; (provide 'init-packages)
