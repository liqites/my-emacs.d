;;; init-lsp-ui.el --- LSP UI enhancements

(straight-use-package 'lsp-ui)
(add-hook 'lsp-mode-hook #'lsp-ui-mode)  ; 自动启用
;; Custom variables
(setq lsp-ui-doc-enable t
  lsp-ui-doc-position 'top
  lsp-ui-doc-max-width 100
  lsp-ui-doc-max-height 30
  lsp-ui-sideline-enable t
  lsp-ui-sideline-show-diagnostics t
  lsp-ui-sideline-show-hover t
  lsp-ui-sideline-show-code-actions t
  lsp-ui-peek-enable t
  lsp-ui-peek-list-width 60
  lsp-ui-peek-peek-height 25
  lsp-ui-doc-delay 1.0)
(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map (kbd "M-?") #'lsp-ui-peek-find-references))

(provide 'init-lsp-ui)
;;; init-lsp-ui.el ends here
