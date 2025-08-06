;;; init-editor.el --- Core editor configuration

;; ==================== 基础设置（保留原有） ====================
(set-face-attribute 'default nil :family "Input Sans Narrow")
(set-face-attribute 'default nil :height 130)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq inhibit-startup-echo-area-message "Hi, Welcome to the world of bit.")
(global-set-key (kbd "C-;") 'comment-line)
(setq-default cursor-type 'bar)
(set-cursor-color "#29B6F6")

;; ==== 流畅度优化 ====
;; 垃圾回收优化（减少卡顿）
(use-package gcmh
  :ensure t
  :demand t
  :config
  (setq gcmh-high-cons-threshold (* 32 1024 1024))  ; 32MB
  (gcmh-mode +1))

;; 处理超长行文件（防卡死）
(use-package so-long
  :ensure t
  :demand t
  :config (global-so-long-mode +1))

;; 异步操作支持
(use-package async
  :ensure t
  :demand t
  :commands (async-start async-start-process))

;; 内置性能分析工具（无需安装）
(defun my-run-profiler ()
  "Quickly start CPU profiler"
  (interactive)
  (profiler-start 'cpu)
  (message "Profiler started! Use M-x profiler-report to view results."))

;; ==== 界面精简 ====
;; 关闭菜单栏和工具栏（保留原有 toolbar-mode -1）
(menu-bar-mode -1)
(scroll-bar-mode -1)  ; 可选：关闭滚动条

;; ==== 字体与渲染优化 ====
(when (display-graphic-p)
  ;; 图形界面专属优化
  (setq-default line-spacing 0.2)                  ; 行间距
  (set-fontset-font t 'unicode "Noto Color Emoji") ; Emoji支持
  (setq-default bidi-paragraph-direction 'left-to-right) ; 提升渲染性能
  (setq inhibit-compacting-font-caches t))         ; 字体缓存不压缩

;; ==== 性能监控工具 ====
;; 启动时间分析
; (use-package benchmark-init
;   :ensure t
;   :hook (after-init . benchmark-init/deactivate)
;   :config
;   (add-hook 'emacs-startup-hook
;             (lambda ()
;               (message "Emacs loaded in %.2f seconds"
;                        (benchmark-init/get-total-time)))))

;; 内存监控
; (use-package memory-usage
;   :ensure t
;   :commands (memory-usage memory-usage-toggle)
;   :config
;   (setq memory-usage-poll-interval 5))  ; 每5秒更新

;; ==================== 新增主题与图标 ====================
;; 图标库（需先于其他插件加载）
(use-package all-the-icons
  :ensure t
  :demand t
  :if (display-graphic-p)  ; 仅在图形界面加载
  :config
  (when (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (unless (display-graphic-p)
                    (all-the-icons-ivy-setup)))))))

;; Doom 主题集合
(use-package doom-themes
  :ensure t
  :demand t
  :config
  (setq doom-themes-enable-bold t    ; 启用粗体
        doom-themes-enable-italic t) ; 启用斜体
  ;; 可选：加载特定主题（注释掉 dracula-theme 后启用）
  ;; (load-theme 'doom-one t)
  )

;; 现代化模式行
(use-package moody
  :ensure t
  :demand t
  :config
  (setq moody-slant-function #'moody-slant-apple-rgb) ; 风格设置
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; 高亮 TODO 注释
(use-package hl-todo
  :ensure t
  :demand t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF8C00")
          ("HACK"   . "#FFD700")
          ("NOTE"   . "#1E90FF")
          ("DEPRECATED" . "#9400D3"))))

;; ==================== 保留原有插件 ====================
;; Dracula 主题（与 doom-themes 二选一）
(use-package dracula-theme
  :ensure t
  :demand t
  :init (load-theme 'dracula t))

;; 平滑滚动
(use-package smooth-scrolling
  :ensure t
  :demand t
  :init (smooth-scrolling-mode 1))

;; Git 修改状态指示器
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-update))
  :config
  (global-diff-hl-mode +1)
  (setq diff-hl-side 'right)
  (setq diff-hl-draw-borders nil))

;; ==================== 效率工具 ====================
;; 快捷键提示
(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.3))

;; 可视化撤销历史
(use-package undo-tree
  :ensure t
  :demand t
  :config
  (global-undo-tree-mode +1)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :bind (("C-x u" . undo-tree-visualize)))

;; 最近文件记录
(use-package recentf
  :demand t
  :config
  (recentf-mode +1)
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 200)
  (add-to-list 'recentf-exclude "~/.emacs.d/.*"))

;; Minibuffer 历史记录
(use-package savehist
  :demand t
  :config
  (savehist-mode +1)
  (setq savehist-additional-variables '(search-ring regexp-search-ring))
  (setq savehist-autosave-interval 60))

;; 备份文件设置
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))  ; 备份文件存储到指定目录
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))  ; 自动保存文件存储到指定目录


(provide 'init-editor)
