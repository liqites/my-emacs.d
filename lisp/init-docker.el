;;; init-docker.el --- Docker integration
;;; Commentary:
;; Setup Docker CLI integration, Dockerfile editing, and Docker Compose support.
;;; Code:

;; Install Docker packages
(straight-use-package 'docker)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'docker-compose-mode)

(defun init-docker ()
  "Initialize Docker integration: CLI, Dockerfile, and Compose modes."
  ;; Docker CLI commands
  (require 'docker)
  ;; Keybinding to open Docker buffer
  (global-set-key (kbd "C-c d") #'docker)
  ;; Enable Dockerfile mode for Dockerfile
  (add-hook 'dockerfile-mode-hook #'dockerfile-mode)
  ;; Enable Docker Compose mode for docker-compose files
  (add-hook 'docker-compose-mode-hook #'docker-compose-mode))

(provide 'init-docker)
;;; init-docker.el ends here
