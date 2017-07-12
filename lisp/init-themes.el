;; (require-package 'dash)
;; (require-package 'solarized-theme)

;; (require 'solarized)

;; (deftheme solarized-dark "The dark variant of the Solarized colour theme")

;; (create-solarized-theme 'dark 'solarized-dark)

;; (provide-theme 'solarized-dark)

;; (when (maybe-require-package 'monokai-theme)
;;   (load-theme 'monokai t)
;;  )

(require-package 'dracula-theme)
(load-theme 'dracula t)


(provide 'init-themes)
