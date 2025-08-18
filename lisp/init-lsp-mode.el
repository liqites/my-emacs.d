;;; init-lsp-mode.el --- Core LSP configuration

(straight-use-package 'lsp-mode)
(setq lsp-keymap-prefix "C-c l")        ; 统一前缀键
(setq lsp-auto-configure t)             ; 自动配置
(setq lsp-log-io nil)                   ; 禁用日志（调试时可设为 t）
(setq lsp-restart 'auto-restart)        ; 自动重启语言服务器
(setq lsp-enable-symbol-highlighting t) ; 高亮当前符号
(setq lsp-enable-text-document-color t) ; 支持颜色高亮

;; 指定 Ruby LSP 服务器为 Solargraph（需先 `gem install solargraph`）
(setq lsp-ruby-server 'solargraph)

;; 不要自动提示安装 LSP 服务器（已手动安装）
(setq lsp-auto-install-servers nil
  lsp-install-server-no-confirm t)

;; Hooks
(dolist (mode-hook '(ruby-mode-hook js-mode-hook typescript-mode-hook python-mode-hook go-mode-hook))
  (add-hook mode-hook #'lsp-deferred))

(add-hook 'lsp-mode-hook
          (lambda ()
            (lsp-enable-which-key-integration)
            (setq lsp-headerline-breadcrumb-enable t)))

;; LSP UI 增强（由 init-lsp-ui.el 独立配置）
(straight-use-package 'lsp-ui)

;; rbenv integration: use project-specific Ruby versions via rbenv
(straight-use-package 'rbenv)
(require 'rbenv)
(global-rbenv-mode)
(add-hook 'ruby-mode-hook #'rbenv-use-corresponding)

;; Solargraph 配置: 使用项目的 Bundler 环境
(setq lsp-ruby-server 'solargraph
  lsp-solargraph-use-bundler t
  lsp-solargraph-bundler-path "bundle"
  lsp-solargraph-formatting-provider 'rubocop)

;; Solargraph 命令：通过 Bundler 执行本地安装的 solargraph 并使用 STDIO 通信
(setq lsp-solargraph-command '("bundle" "exec" "solargraph" "stdio"))

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
