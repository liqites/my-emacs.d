;;; init-ruby-mode.el --- Ruby mode enhancements

;; Enable Enhanced Ruby Mode for *.rb, Rakefiles, etc.
(use-package enh-ruby-mode
  :ensure t
  :mode (("\\.\\(rb\\|rake\\|ru\\|gemspec\\)\\'" . enh-ruby-mode))
  :interpreter ("ruby" . enh-ruby-mode)
  :hook (enh-ruby-mode . (lambda ()
                           ;; Use 2 spaces for indentation
                           (setq-local ruby-indent-level 2
                                       indent-tabs-mode nil)
                           ;; Auto-pair braces, parens, quotes
                           (electric-pair-local-mode 1))))

;; LSP support for Ruby via Solargraph
(use-package lsp-mode
  :ensure t
  :hook ((enh-ruby-mode ruby-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :init
  ;; prefix for lsp-command-keymap (few alternatives: "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; use bundler to start solargraph if Gemfile present
  (setq lsp-solargraph-use-bundler t
        ;; format on save
        lsp-solargraph-autoformat t
        ;; show diagnostics from server
        lsp-solargraph-diagnostics t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t))

(provide 'init-ruby-mode)
;;; init-ruby-mode.el ends here