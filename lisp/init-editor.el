;;; init-editor.el --- Core editor configuration
;;; Commentary:
;; Core editor configuration: basic editing, performance, UI, themes, tools, VC integration,
;; backups & autosave handling, completion & search, dashboard, file & window management,
;; usability enhancements, help system, tabs (optional), and Hydra workflows.

;; ============================================================
;;                       1. 基础编辑器设置
;; ============================================================
(set-face-attribute 'default nil :family "Input Sans Narrow")
(set-face-attribute 'default nil :height 130)
(tool-bar-mode -1)                  ; 关闭工具栏
(setq-default indent-tabs-mode nil) ; 禁用 Tab 缩进，使用空格
(setq-default tab-width 2)          ; 设置 Tab 宽度为 2 个空格
(setq inhibit-startup-echo-area-message "Hi, Welcome to the world of bit.") ; 启动信息
(global-set-key (kbd "C-;") 'comment-line) ; 全局绑定 C-; 为注释行
(setq-default cursor-type 'bar)     ; 设置光标类型为竖线
(set-cursor-color "#29B6F6")       ; 设置光标颜色
(electric-pair-mode 1)          ; 自动括号配对
(show-paren-mode 1)             ; 高亮显示匹配括号
(setq show-paren-delay 0)       ; 立即高亮匹配括号
(global-display-line-numbers-mode 1) ; 显示行号
;; ============================================================
;;                     2. 性能与流畅度优化
;; ============================================================
;; 垃圾回收优化（减少卡顿）
(use-package gcmh
  :ensure t
  :demand t
  :config
  (setq gcmh-high-cons-threshold (* 32 1024 1024))  ; 32MB 垃圾回收阈值
  (gcmh-mode +1)) ; 启用垃圾回收监控模式

;; 处理超长行文件（防卡死）
(use-package so-long
  :ensure t
  :demand t
  :config (global-so-long-mode +1)) ; 启用全局超长行模式

;; 异步操作支持
(use-package async
  :ensure t
  :demand t
  :commands (async-start async-start-process)) ; 提供异步启动命令

;; 内置性能分析工具（无需安装）
(defun my-run-profiler ()
  "Quickly start CPU profiler"
  (interactive)
  (profiler-start 'cpu) ; 启动 CPU 性能分析
  (message "Profiler started! Use M-x profiler-report to view results."))

;; ============================================================
;;                       3. 界面与渲染优化
;; ============================================================
;; 关闭菜单栏和工具栏
(menu-bar-mode -1)    ; 关闭菜单栏
(scroll-bar-mode -1)  ; 关闭滚动条

;; 图形界面专属优化
(when (display-graphic-p)
  (setq-default line-spacing 0.2)                  ; 设置行间距
  (set-fontset-font t 'unicode "Noto Color Emoji") ; 支持 Emoji
  (setq-default bidi-paragraph-direction 'left-to-right) ; 提升双向文本渲染性能
  (setq inhibit-compacting-font-caches t))         ; 不压缩字体缓存

;; ============================================================
;;                       4. 主题与图标
;; ============================================================
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

;; Dired 文件管理器图标
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Doom 主题集合
(use-package doom-themes
  :ensure t
  :demand t
  :config
  (setq doom-themes-enable-bold t    ; 启用粗体
        doom-themes-enable-italic t) ; 启用斜体
  ;; 可选：加载特定主题
  ;; (load-theme 'doom-one t)
  )

;; Dracula 主题（与 doom-themes 二选一，当前默认启用 Dracula）
(use-package dracula-theme
  :ensure t
  :demand t
  :init (load-theme 'dracula t))

;; 现代化模式行
(use-package moody
  :ensure t
  :demand t
  :config
  (setq moody-slant-function #'moody-slant-apple-rgb) ; 风格设置
  (moody-replace-mode-line-buffer-identification)      ; 替换缓冲区标识
  (moody-replace-vc-mode))                             ; 替换版本控制显示

;; ============================================================
;;                       5. 效率工具
;; ============================================================
;; 高亮 TODO 注释
(use-package hl-todo
  :ensure t
  :demand t
  :hook (prog-mode . hl-todo-mode) ; 在编程模式下启用
  :config
  (setq hl-todo-highlight-punctuation ":" ; 设置高亮标点
        hl-todo-keyword-faces              ; 定义高亮关键字及颜色
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF8C00")
          ("HACK"   . "#FFD700")
          ("NOTE"   . "#1E90FF")
          ("DEPRECATED" . "#9400D3"))))

;; 快捷键提示
(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode +1)             ; 启用 which-key 模式
  (setq which-key-idle-delay 0.3)) ; 设置提示延迟

;; 可视化撤销历史
(use-package undo-tree
  :ensure t
  :demand t
  :config
  (global-undo-tree-mode +1)      ; 全局启用 undo-tree
  (setq undo-tree-auto-save-history t) ; 自动保存历史记录
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))) ; 历史记录目录
  :bind (("C-x u" . undo-tree-visualize))) ; 绑定可视化快捷键

;; 最近文件记录
(use-package recentf
  :demand t
  :config
  (recentf-mode +1)              ; 启用最近文件模式
  (setq recentf-max-menu-items 100) ; 菜单最大项目数
  (setq recentf-max-saved-items 200) ; 保存最大项目数
  (add-to-list 'recentf-exclude "~/.emacs.d/.*")) ; 排除指定目录

;; Minibuffer 历史记录
(use-package savehist
  :demand t
  :config
  (savehist-mode +1)             ; 启用保存历史模式
  (setq savehist-additional-variables '(search-ring regexp-search-ring)) ; 额外变量
  (setq savehist-autosave-interval 60)) ; 自动保存间隔

;; 平滑滚动
(use-package smooth-scrolling
  :ensure t
  :demand t
  :init (smooth-scrolling-mode 1)) ; 启用平滑滚动

;; ============================================================
;;                      6. 版本控制集成
;; ============================================================
;; Git 修改状态指示器
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)         ; 在编程模式下启用
         (magit-post-refresh . diff-hl-update)) ; Magit 更新后更新
  :config
  (global-diff-hl-mode +1) ; 全局启用 diff-hl
  (setq diff-hl-side 'right) ; 显示在右侧
  (setq diff-hl-draw-borders nil)) ; 不绘制边框

;; ============================================================
;;                    7. 备份与自动保存设置
;; ============================================================
;; 备份文件存储到指定目录
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; 自动保存文件存储到指定目录
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;; 不生成备份文件和自动保存文件
(setq make-backup-files nil    ; 禁用 ~ 备份
  auto-save-default nil)   ; 禁用 #autosave#

;; ============================================================
;;               8. 外部文件更改监控与提示 (File Change Watch)
;; ============================================================
(require 'filenotify)

(defvar my/file-notify-watches nil
  "List of file-notify watch descriptors for open buffers.")

(defun my/file-change-callback (event)
  "Prompt to reload buffer when file changes on disk (EVENT)."
  (let ((type (cadr event))
        (file (caddr event)))
    (when (eq type 'changed)
      (when-let ((buf (get-file-buffer file)))
        (with-current-buffer buf
          (unless (buffer-modified-p)
            (when (yes-or-no-p (format "File changed on disk: %s. Reload? " file))
              (revert-buffer t t t))))))))

(defun my/add-file-watch ()
  "Add file-notify watch for the current buffer's file."  
  (when-let ((file (buffer-file-name)))
    (push (file-notify-add-watch file '(change) #'my/file-change-callback)
          my/file-notify-watches)))

(add-hook 'find-file-hook #'my/add-file-watch)

;; ============================================================
;;                    9. 补全与搜索框架 (Vertico + Consult)
;; ============================================================
;; 1. Vertico: 最小的 Emacs 交互式完成框架
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; 启用 Vertico 后，默认的 `completion-styles` 会被改变
  ;; 可以根据需求调整 `completion-styles`
  ;; (setq completion-styles '(basic substring))
  ;; (setq completion-category-defaults nil)
  ;; (setq completion-category-overrides nil)
  )

;; 2. Marginalia: 为 Vertico (或其他补全框架) 添加额外信息 (例如文件大小，日期等)
(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

;; 3. Orderless: 高级无序匹配风格，用于更灵活的模糊搜索
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles . (partial-completion))))) ; 对文件路径使用部分匹配
  (setq orderless-matching-styles
        '(orderless-regexp
          orderless-literal
          orderless-initialism
          orderless-flex))
  )

;; 4. Consult: 提供一组强大的交互式命令，基于 Vertico / Embark
(use-package consult
  :ensure t
  :after (vertico) ; 确保在 vertico 之后加载
  :init
  ;; 明确禁用预览键，避免 'key-valid-p' 错误
  ;; 明确禁用 Consult 的预览功能
  (setq consult-preview-function nil)
  :config
  ;; 快捷键绑定 (可以根据个人习惯调整)
  (global-set-key (kbd "C-x b") 'consult-buffer) ; 替换 C-x b 为 consult-buffer
  (global-set-key (kbd "C-x C-f") 'consult-find) ; 替换 C-x C-f 为 consult-find (强大的文件查找)
  (global-set-key (kbd "C-s") 'consult-line)    ; 替换 C-s 为 consult-line (行内搜索)
  (global-set-key (kbd "M-y") 'consult-yank-pop) ; 替换 M-y 为 consult-yank-pop
  (global-set-key (kbd "M-s g") 'consult-grep)   ; 全局 grep 搜索
  (global-set-key (kbd "M-s r") 'consult-ripgrep) ; ripgrep (如果已安装)
  (global-set-key (kbd "M-s l") 'consult-locate) ; locate 搜索
  (global-set-key (kbd "M-g M-g") 'consult-goto-line) ; 跳转到行
  (global-set-key (kbd "M-g f") 'consult-fd) ; fdfind (如果已安装)

  ;; 启用预览模式 (实时显示结果)


  ;; 确保 consult-project-root 总是使用 project.el 的项目根
  (setq consult-project-root-function 'project-root)
  )

;; ============================================================
;;                       10. 启动界面 (Dashboard)
;; ============================================================
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)   ; 显示最近5个文件
                          (bookmarks . 5) ; 显示5个书签
                          (projects . 5)  ; 显示5个项目
                          (agenda . 5)))  ; 显示5个议程项

  ;; 使用官方横幅并设置欢迎语
  (setq dashboard-startup-banner 'official) ; 使用官方Emacs横幅
  (setq dashboard-banner-logo-title "Welcome to The EMACS") ; 设置欢迎语

  ;; 其他基本设置
  (setq dashboard-center-content t) ; 内容居中显示
  (setq dashboard-show-shortcuts t) ; 显示快捷键提示
  (setq dashboard-set-init-info t)) ; 显示启动时间
  ; (setq dashboard-startup-banner nil) ; 不显示任何图片


;; ============================================================
;;                       11. 文件管理增强
;; ============================================================
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode) ; 在 dired 模式下启用
  :config
  (setq diredfl-global-modes '(dired-mode))) ; 确保全局作用于 dired

;; ============================================================
;;                       12. 窗口管理
;; ============================================================
;; 快速窗口切换
(use-package ace-window
  :ensure t
  :init
  (global-set-key (kbd "M-o") 'ace-window) ; 全局绑定 M-o 快速切换窗口
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ; 设置快捷键字符
  (setq aw-scope 'frame) ; 仅在当前框架内切换
  (setq aw-display-mode 'top-left)) ; 标签显示在左上角

;; 撤销/重做窗口布局
(winner-mode 1) ; 启用 winner-mode
;; 快捷键绑定 (可选，winner-mode 默认有 C-c <left>/C-c <right>)
;; (global-set-key (kbd "C-c C-l") 'winner-undo)
;; (global-set-key (kbd "C-c C-r") 'winner-redo)

;; ============================================================
;;                       13. 界面与可用性
;; ============================================================
;; 光标高亮信标
(use-package beacon
  :ensure t
  :init
  (beacon-mode 1)) ; 启用 beacon 模式

;; 自动高亮当前符号
(use-package highlight-symbol
  :ensure t
  :hook (prog-mode . highlight-symbol-mode))

;; 彩虹括号高亮
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================
;;                       14. 帮助系统增强
;; ============================================================
;; 更友好的帮助系统
(use-package helpful
  :ensure t
  :commands (helpful-at-point helpful-callable helpful-key helpful-variable helpful-symbol)
  :config
  (global-set-key (kbd "C-h f") 'helpful-callable) ; C-h f (function)
  (global-set-key (kbd "C-h v") 'helpful-variable) ; C-h v (variable)
  (global-set-key (kbd "C-h k") 'helpful-key)      ; C-h k (key)
  (global-set-key (kbd "C-h F") 'helpful-at-point)) ; C-h F (full help at point)

;; ============================================================
;;                      15. 标签页管理 (Centaur Tabs)
;; ============================================================
; (use-package centaur-tabs
;   :ensure t
;   :demand t
;   :config
;   (centaur-tabs-mode 1) ; 启用 centaur-tabs
;   (setq centaur-tabs-set-bar-height 25) ; 设置标签栏高度
;   (setq centaur-tabs-set-icons t) ; 显示图标
;   (setq centaur-tabs-set-modified-marker t) ; 显示修改标记
;   (setq centaur-tabs-show-new-tab-button t) ; 显示新建标签按钮
;   (setq centaur-tabs-style "rounded") ; 标签风格
;   ;; 自动隐藏标签栏 (仅剩一个标签时)
;   (setq centaur-tabs-auto-hide-tabs 't)
;   (setq centaur-tabs-height 24)
;   (setq centaur-tabs-bar-position 'top) ; 标签栏位置
;   (setq centaur-tabs-close-button "x") ; 关闭按钮样式
;   ;; 窗口切换时自动激活对应标签页
;   (add-hook 'window-configuration-change-hook 'centaur-tabs-buffer-groups-update)
;   (setq centaur-tabs-set-close-button 'right)
;   (setq centaur-tabs-set-font "Input Sans Narrow-13") ; 设置字体和大小
;   )

;; ============================================================
;;                       16. 模式增强 (Hydra)
;; ============================================================
(use-package hydra
  :ensure t
  :config
  ;; 字体大小调整 Hydra
  (defhydra hydra-font-size (:color blue :hint nil)
    "font"
    ("j" text-scale-increase "更大")
    ("k" text-scale-decrease "更小")
    ("0" (lambda () (interactive) (text-scale-set 0)) "重置")
    ("q" nil "退出")))

;; 绑定 Hydra 到 C-c f
(global-set-key (kbd "C-c f") 'hydra-font-size/font)

(provide 'init-editor)
