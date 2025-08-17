;;; init-mmm.el --- Configuration for multiple major modes support

;; ==================== 基本配置 ====================
(straight-use-package 'mmm-mode)
(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)

;; ==================== 错误处理 ====================
(defun my-mmm-error-handler (err)
  (message "[MMM] 子模式解析错误: %s" err)
  (when (fboundp 'flycheck-add-error)
    (flycheck-add-error :error (point) (point) (format "%s" err))))

(advice-add 'mmm-parse-region :around #'my-mmm-error-handler)

;; ==================== 提供模块 ====================
(provide 'init-mmm)
