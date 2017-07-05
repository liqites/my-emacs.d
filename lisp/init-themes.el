(require-package 'dash)
(require-package 'solarized-theme)

(require 'solarized)

(deftheme solarized-dark "The dark variant of the Solarized colour theme")

(create-solarized-theme 'dark 'solarized-dark)

(provide-theme 'solarized-dark)

(provide 'init-themes)
