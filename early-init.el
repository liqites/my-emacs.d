;;; early-init.el --- Emacs 27+ early initialization

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Faster startup; will be reset later by gcmh/your config
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reduce UI jank during init
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Precompute autoloads for built-ins
(setq package-quickstart t)

(provide 'early-init)
