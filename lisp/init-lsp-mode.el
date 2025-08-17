;;; init-lsp-mode.el --- Core LSP configuration

(straight-use-package 'lsp-mode)
(setq lsp-keymap-prefix "C-c l")        ; 统一前缀键
(setq lsp-auto-configure t)             ; 自动配置
(setq lsp-log-io nil)                   ; 禁用日志（调试时可设为 t）
(setq lsp-restart 'auto-restart)        ; 自动重启语言服务器
(setq lsp-enable-symbol-highlighting t) ; 高亮当前符号
(setq lsp-enable-text-document-color t) ; 支持颜色高亮

;; Hooks
(dolist (mode-hook '(ruby-mode-hook js-mode-hook typescript-mode-hook python-mode-hook go-mode-hook))
  (add-hook mode-hook #'lsp-deferred))

(add-hook 'lsp-mode-hook
          (lambda ()
            (lsp-enable-which-key-integration)
            (setq lsp-headerline-breadcrumb-enable t)))

;; LSP UI 增强（由 init-lsp-ui.el 独立配置）
(straight-use-package 'lsp-ui)

;; 调试支持（可选）
;; (straight-use-package 'dap-mode)
;; (with-eval-after-load 'lsp-mode
;;   (require 'dap-hydra)
;;   (dap-mode +1)
;;   (dap-ui-mode +1))

;; 语言特定配置示例（Python）
(straight-use-package 'lsp-pyright)
(add-hook 'python-mode-hook
          (lambda ()
            (require 'lsp-pyright)
            (lsp-deferred)))

(provide 'init-lsp-mode)
;;; init-lsp-mode ends here
