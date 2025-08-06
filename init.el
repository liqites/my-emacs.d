;; 禁用默认启动界面
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;; 自定义欢迎 Buffer
(defun my-custom-startup-buffer ()
  "创建带包加载状态的自定义欢迎界面"
  (let ((buf (get-buffer-create "*Welcome*")))
  (with-current-buffer buf
    (erase-buffer)
    ;; 固定欢迎信息
    (insert (propertize "
  ╭──────────────────────────────────────╮
  │                                      │
  │       WELCOME TO MY EMACS            │
  │       ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾            │
  │                                      │
  ╰──────────────────────────────────────╯
" 'face '(:height 1.2 :foreground "#61afef")))
    (insert (format "\n  Startup time: %s\n\n" (current-time-string)))
    (setq buffer-read-only t)  ; 设为只读
    (switch-to-buffer buf))))

; ;; 包加载状态监控
; (defun my-package-load-status (package &optional desc)
;   "在欢迎界面更新包加载状态"
;   (when-let ((buf (get-buffer "*Welcome*")))
;     (with-current-buffer buf
;       (save-excursion
;         (goto-char (point-max))
;         (let ((line (format "  %-20s %s"
;                             (or desc (symbol-name package))
;                             (if (featurep package) "✓" "⌛"))))
;           (if (re-search-forward (format "^  %s" (regexp-quote (symbol-name package))) nil t)
;             (insert line "\n")
;           (replace-match line nil nil)))
;       (setq buffer-read-only t)))))

; ;; 在包加载时触发状态更新
; (advice-add 'require :after #'my-package-load-status)

;; 启动时显示欢迎界面
(add-hook 'emacs-startup-hook #'my-custom-startup-buffer)


(load (expand-file-name "init-packages.el" user-emacs-directory))

;; Load custom variables and faces from a separate file for clarity.
(load (expand-file-name "lisp/init-custom-variables.el" user-emacs-directory))
(require 'init-custom-variables)


 ;; Set Proxy
 (setq url-proxy-services
       '(("http"  . "0.0.0.0:6152")
         ("https" . "0.0.0.0:6152")))

 ;;
 (setq mac-option-modifier 'meta)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
