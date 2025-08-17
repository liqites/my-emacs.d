;;; init-ruby-mode.el --- Ruby mode enhancements

;; Enable Enhanced Ruby Mode for *.rb, Rakefiles, etc.
(straight-use-package 'enh-ruby-mode)
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|rake\\|ru\\|gemspec\\)\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (setq-local ruby-indent-level 2
                        indent-tabs-mode nil)
            (electric-pair-local-mode 1)))

;; LSP support for Ruby via Solargraph
;; Ensure lsp-mode is available (installed in init-lsp-mode)
(add-hook 'enh-ruby-mode-hook #'lsp-deferred)
(add-hook 'ruby-mode-hook #'lsp-deferred)
;; Solargraph settings
(with-eval-after-load 'lsp-mode
  (setq lsp-solargraph-use-bundler t
        lsp-solargraph-autoformat t
        lsp-solargraph-diagnostics t))

;; lsp-ui integration provided by init-lsp-ui

(provide 'init-ruby-mode)
;;; init-ruby-mode.el ends here