;;; init-lsp-mode.el --- Core LSP configuration

(use-package lsp-mode
  :ensure t
  :demand t
  :init
  (setq lsp-keymap-prefix "C-c l")        ; 统一前缀键
  (setq lsp-auto-configure t)             ; 自动配置
  (setq lsp-log-io nil)                   ; 禁用日志（调试时可设为 t）
  (setq lsp-restart 'auto-restart)        ; 自动重启语言服务器
  (setq lsp-enable-symbol-highlighting t) ; 高亮当前符号
  (setq lsp-enable-text-document-color t) ; 支持颜色高亮
  :hook
  ((ruby-mode       . lsp-deferred)       ; Ruby 支持
   (js-mode         . lsp-deferred)       ; JavaScript 支持
   (typescript-mode . lsp-deferred)       ; TypeScript 支持
   (python-mode     . lsp-deferred)       ; Python 支持
   (go-mode         . lsp-deferred)       ; Go 支持
   (lsp-mode        . (lambda ()
                       (lsp-enable-which-key-integration) ; which-key 支持
                       (setq lsp-headerline-breadcrumb-enable t)))) ; 显示路径面包屑
  :commands (lsp lsp-deferred))

;; LSP UI 增强（由 init-lsp-ui.el 独立配置）
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode)

;; 调试支持（可选）
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (require 'dap-hydra)
  (dap-mode +1)
  (dap-ui-mode +1))

;; 语言特定配置示例（Python）
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lsp-deferred))))

(provide 'init-lsp-mode)
;;; init-lsp-mode ends here
