;; 禁用默认启动界面
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;; Set Proxy (do this early so straight.el bootstrap can use it)
(setq url-proxy-services
      '(("http"  . "127.0.0.0:6152")
        ("https" . "127.0.0.0:6152")))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate use-package with straight so existing modules keep working
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; Make :ensure a no-op so legacy blocks don't pull from package.el
(defvar use-package-ensure-function 'ignore)
(setq use-package-always-ensure nil)

(load (expand-file-name "init-packages.el" user-emacs-directory))

;; Custom settings: write to custom.el to avoid polluting init
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)


;; macOS modifiers
(setq mac-option-modifier 'meta)
