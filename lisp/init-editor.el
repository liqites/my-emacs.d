(set-face-attribute 'default nil :family "Input Sans Narrow")
(set-face-attribute 'default nil :height 130)
;; (set-face-attribute 'default nil :weight 'light)


;; (add-to-list 'default-frame-alist '(width  . 90))
;; (add-to-list 'default-frame-alist '(height . 40))
;; (add-to-list 'default-frame-alist '(font . "Monospace-10"))


;; Themes
;; (use-package color-theme-modern
;; 	:init
;; 	(load-theme 'pierson t)
;; 	(enable-theme 'pierson)
;;   :ensure t)

; Atom One Dark Theme
;; (use-package atom-one-dark-theme
  ;; :ensure t)
;; (load-theme 'atom-one-dark t t)
;; (enable-theme 'atom-one-dark)

;; Hide Toolbar
(tool-bar-mode -1)

;; Spacemacs Theme
;; (use-package spacemacs-theme
;;   :init (progn (load-theme 'spacemacs-dark t))
;;   :defer t
;;   :ensure t)

;; Dracula Theme
(use-package dracula-theme
  :init (load-theme 'dracula t)
  :defer t
  :ensure t
  :demand t)

;; solarized-theme
;; (use-package solarized-theme
;;   :init (load-theme 'solarized-light t)
;;   :defer t
;;   :ensure t
;;   )

;; Smooth Scroll
(use-package smooth-scrolling
	:init
	(smooth-scrolling-mode 1)
  :ensure t
  :demand t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;;
;; (use-package color-identifiers-mode)
;; (add-hook 'after-init-hook 'global-color-identifiers-mode)


;; Keybindings
(global-set-key (kbd "C-;") 'comment-line)

;; Cursor Type
(setq-default cursor-type 'bar)
(set-cursor-color "#29B6F6")


(setq inhibit-startup-echo-area-message "YOUR-USER-NAME")

;; Font
;; (add-to-list 'default-frame-alist
;; 				 ;; '(font . "Input Serif Narrow"))
;; 				 '(font . "JetBrains Mono"))
;; 				 ;; '(font . "Victor Mono"))
;; 				 ;; '(font . "Input Sans Narrow"))
;; 				 ;; '(font . "Input Sans Narrow"))
;; 				 (set-face-attribute 'default nil :height 140)


(provide 'init-editor)
