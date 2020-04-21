;; (set-face-attribute 'default nil :family "consolas")
;; (set-face-attribute 'default nil :height 140)
;; (set-face-attribute 'default nil :weight 'light)


;; (add-to-list 'default-frame-alist '(width  . 90))
;; (add-to-list 'default-frame-alist '(height . 40))
;; (add-to-list 'default-frame-alist '(font . "Monospace-10"))

;; Font
(add-to-list 'default-frame-alist
	     '(font . "JetBrains Mono"))
(set-face-attribute 'default nil :height 130)

;; Themes
;; (use-package color-theme-modern
;;   :ensure t)
;; (load-theme 'pierson t t)
;; (enable-theme 'pierson)

; Atom One Dark Theme
;; (use-package atom-one-dark-theme
  ;; :ensure t)
;; (load-theme 'atom-one-dark t t)
;; (enable-theme 'atom-one-dark)

;; Hide Toolbar
(tool-bar-mode -1)

;; Spacemacs Theme
;; (use-package spacemacs-theme
  ;; :ensure t)

;; Smooth Scroll
;; (use-package smooth-scrolling)
;; (smooth-scrolling-mode 1)

(setq-default tab-width 3)
;;
;; (use-package color-identifiers-mode)
;; (add-hook 'after-init-hook 'global-color-identifiers-mode)


;; Keybindings
(global-set-key (kbd "C-;") 'comment-line)

;; Cursor Type
(setq-default cursor-type 'bar)
(set-cursor-color "#FF9800") 



(provide 'init-editor)
