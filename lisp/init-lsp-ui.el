;;; init-lsp-ui.el --- LSP UI enhancements

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)  ; 自动启用
  :custom
  (lsp-ui-doc-enable t)           ; 启用悬浮文档
  (lsp-ui-doc-position 'top)      ; 文档显示在顶部（可选：'bottom'）
  (lsp-ui-doc-max-width 100)      ; 文档最大宽度
  (lsp-ui-doc-max-height 30)      ; 文档最大高度
  (lsp-ui-sideline-enable t)      ; 启用右侧边栏提示
  (lsp-ui-sideline-show-diagnostics t) ; 显示诊断信息
  (lsp-ui-sideline-show-hover t)  ; 显示悬停信息
  (lsp-ui-sideline-show-code-actions t) ; 显示代码操作
  (lsp-ui-peek-enable t)          ; 启用代码跳转预览
  (lsp-ui-peek-list-width 60)     ; 预览列表宽度
  (lsp-ui-peek-peek-height 25)    ; 预览内容高度
  :bind (:map lsp-ui-mode-map
         ("M-." . lsp-ui-peek-find-definitions)   ; 跳转到定义
         ("M-?" . lsp-ui-peek-find-references))    ; 查找引用
  :config
  ;; 优化性能：延迟显示悬浮文档
  (setq lsp-ui-doc-delay 1.0))

(provide 'init-lsp-ui)
;;; init-lsp-ui.el ends here
