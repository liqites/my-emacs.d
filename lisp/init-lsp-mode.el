;; (setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternative - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode
			;; (ruby-mode . lsp-deferred)
			(ruby-mode . lsp-deferred)
			;; if you want which-key integration
			(lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :ensure t
  :defer t)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; (use-package company-lsp
  ;; :ensure t
  ;; :commands company-lsp)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
(use-package dap-ruby)

(use-package which-key
  :config
  (which-key-mode))

(provide 'init-lsp-mode)
;;; init-lsp-mode ends here
