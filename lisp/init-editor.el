;; disable toolbar
(tool-bar-mode -1)

;; fullscreen
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; cursor type
(setq-default cursor-type '(bar . 2))

;; 
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq-default tab-stop-list (number-sequence 4 120 4))

;; shortcut
(global-set-key (kbd "C-M-\;") 'comment-line)

(provide 'init-editor)
