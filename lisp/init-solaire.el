;;; init-solaire.el --- Solaire-mode configuration
;;; Commentary:
;; This configuration sets up solaire-mode to give file buffers a slightly different,
;; polished background and aligns the fringe with the main buffer.
;;; Code:

(straight-use-package 'solaire-mode)

;; Enable solaire-mode in graphical sessions.
(when (display-graphic-p)
  (solaire-global-mode +1))

;; Remap fringe so that it uses the same background as solaire-mode buffers.
(setq solaire-mode-remap-fringe t)

;; Optionally customize solaire-mode faces.
(custom-set-faces
 ;; Adjust the background for standard solaire buffers.
 '(solaire-default-face ((t (:background "#fafafa"))))
 ;; Optionally adjust the background for comments.
 '(solaire-comment-face ((t (:background "#e8e8e8" :slant italic)))))

;; If you want solaire-mode disabled in certain major modes (like dired or neotree):
(setq solaire-mode-disable-modes '(dired-mode
                                     neotree-mode
                                     treemacs-mode))

(provide 'init-solaire)
;;; init-solaire.el ends here