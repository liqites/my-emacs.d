;;; init-editor.el --- Core editor configuration
;;; Commentary:
;; Core editor configuration: basic editing, performance, UI, themes, tools, VC integration,
;; backups & autosave handling, completion & search, dashboard, file & window management,
;; usability enhancements, help system, tabs (optional), and Hydra workflows.
;;; Code:

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
;; gcmh: Optimize Emacs garbage collection to reduce UI pauses
(straight-use-package 'gcmh)
(setq gcmh-high-cons-threshold (* 32 1024 1024))  ; 32MB 垃圾回收阈值
(gcmh-mode +1) ; 启用垃圾回收监控模式

;; so-long: Automatically handle extremely long lines to prevent freezes
(straight-use-package 'so-long)
(global-so-long-mode +1) ; 启用全局超长行模式

;; async: Provide asynchronous processing commands for better responsiveness
(straight-use-package 'async)
;; Commands provided: async-start, async-start-process

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
(straight-use-package 'all-the-icons)
;; only meaningful in GUI; optional frame hook kept minimal

;; Dired 文件管理器图标
(straight-use-package 'all-the-icons-dired)
(add-hook 'dired-mode-hook #'all-the-icons-dired-mode)

;; Doom 主题集合
(straight-use-package 'doom-themes)
(setq doom-themes-enable-bold t
  doom-themes-enable-italic t)
;; (load-theme 'doom-dark+ t)
;; (load-theme 'doom-one t)
;; (load-theme 'doom-city-lights t)
(load-theme 'doom-acario-light t)

(straight-use-package 'circadian)
(setq calendar-latitude 31.230391)
(setq calendar-longitude 121.473701)
(setq circadian-themes '((:sunrise . doom-acario-light)
                          (:sunset . doom-one)))
(setq circadian-verbose t)
(circadian-setup)

;; 现代化模式行
(straight-use-package 'moody)
(setq moody-slant-function #'moody-slant-apple-rgb)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)

;; ============================================================
;;                       5. 效率工具
;; ============================================================
;; 高亮 TODO 注释
(straight-use-package 'hl-todo)
(add-hook 'prog-mode-hook #'hl-todo-mode)
(setq hl-todo-highlight-punctuation ":"
      hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF8C00")
        ("HACK"   . "#FFD700")
        ("NOTE"   . "#1E90FF")
        ("DEPRECATED" . "#9400D3")))

;; 快捷键提示
(straight-use-package 'which-key)
(which-key-mode +1)
(setq which-key-idle-delay 0.3)

;; 可视化撤销历史
(straight-use-package 'undo-tree)
(global-undo-tree-mode +1)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(global-set-key (kbd "C-x u") 'undo-tree-visualize)

;; 最近文件记录
(recentf-mode +1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 200)
(add-to-list 'recentf-exclude "~/.emacs.d/.*")

;; Minibuffer 历史记录
(savehist-mode +1)
(setq savehist-additional-variables '(search-ring regexp-search-ring))
(setq savehist-autosave-interval 60)

;; 平滑滚动
(straight-use-package 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; ============================================================
;;                      6. 版本控制集成
;; ============================================================
;; Git 修改状态指示器
(straight-use-package 'diff-hl)
(add-hook 'prog-mode-hook #'diff-hl-mode)
(add-hook 'magit-post-refresh-hook #'diff-hl-update)
(global-diff-hl-mode +1)
(setq diff-hl-side 'right)
(setq diff-hl-draw-borders nil)

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

;;; ============================================================
;;; 8. 外部文件更改监控与提示 (File Change Watch)
;;; ============================================================
(require 'filenotify)

(defvar my/file-notify-watches nil
  "List of file-notify watch descriptors for open buffers.")

(defvar my/file-notify-saved-files nil
  "List of files recently saved by Emacs to ignore notifications.")

;; Record files you save so we can ignore their own notifications
(add-hook 'after-save-hook
          (lambda ()
            (when buffer-file-name
              (push buffer-file-name my/file-notify-saved-files))))

;; File change callback: automatically revert buffer on disk change
(defun my/file-change-callback (event)
  "Automatically reload buffer when file changes on disk (EVENT) without prompting.
Ignores file saves initiated by Emacs."
  (let ((type (cadr event))
        (file (caddr event)))
    (when (eq type 'changed)
      (if (member file my/file-notify-saved-files)
          (setq my/file-notify-saved-files
                (delete file my/file-notify-saved-files))
        (when-let ((buf (get-file-buffer file)))
          (with-current-buffer buf
            (unless (buffer-modified-p)
              (revert-buffer t t t))))))))

;; (defun my/file-change-callback (event)
;;   "Prompt to reload buffer when file changes on disk (EVENT), ignoring Emacs saves."
;;   (let ((type (cadr event))
;;         (file (caddr event)))
;;     (when (eq type 'changed)
;;       (if (member file my/file-notify-saved-files)
;;           ;; our own save → drop it
;;           (setq my/file-notify-saved-files
;;                 (delete file my/file-notify-saved-files))
;;         ;; external change → prompt
;;         (when-let ((buf (get-file-buffer file)))
;;           (with-current-buffer buf
;;             (unless (buffer-modified-p)
;;               (when (yes-or-no-p
;;                      (format "File changed on disk: %s. Reload? " file))
;;                 (revert-buffer t t t)))))))))

(defun my/add-file-watch ()
  "Add a file-notify watch for the current buffer’s file."
  (when-let ((file (buffer-file-name)))
    (let ((watch (file-notify-add-watch
                  file
                  '(change)
                  #'my/file-change-callback)))
      (push watch my/file-notify-watches))))

(add-hook 'find-file-hook #'my/add-file-watch)

;; ============================================================
;;                    9. 补全与搜索框架 (Vertico + Consult)
;; ============================================================
;; 1. Vertico: 最小的 Emacs 交互式完成框架
(straight-use-package 'vertico)
(vertico-mode 1)

;; 2. Marginalia: 为 Vertico (或其他补全框架) 添加额外信息 (例如文件大小，日期等)
(straight-use-package 'marginalia)
(marginalia-mode 1)

;; 3. Orderless: 高级无序匹配风格，用于更灵活的模糊搜索
(straight-use-package 'orderless)
(setq completion-styles '(orderless basic)
  completion-category-overrides '((file (styles . (partial-completion))))
  orderless-matching-styles
  '(orderless-regexp orderless-literal orderless-initialism orderless-flex))

;; 4. Consult: 提供一组强大的交互式命令，基于 Vertico / Embark
(straight-use-package 'consult)
;; 明确禁用 Consult 的预览功能
(setq consult-preview-function nil)
;; 快捷键绑定
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x C-f") 'consult-find)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "M-s g") 'consult-grep)
(global-set-key (kbd "M-s r") 'consult-ripgrep)
(global-set-key (kbd "M-s l") 'consult-locate)
(global-set-key (kbd "M-g M-g") 'consult-goto-line)
(global-set-key (kbd "M-g f") 'consult-fd)
(setq consult-project-root-function 'project-root)

;; ============================================================
;;                       10. 启动界面 (Dashboard)
;; ============================================================
(straight-use-package 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((recents . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))
(setq dashboard-startup-banner 'official)
(setq dashboard-banner-logo-title "Welcome to The EMACS")
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts t)
(setq dashboard-set-init-info t)


;; ============================================================
;;                       11. 文件管理增强
;; ============================================================
(straight-use-package 'diredfl)
(add-hook 'dired-mode-hook #'diredfl-mode)
(setq diredfl-global-modes '(dired-mode))

;; ============================================================
;;                       12. 窗口管理
;; ============================================================
;; 快速窗口切换
(straight-use-package 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-scope 'frame)
(setq aw-display-mode 'top-left)

;; 撤销/重做窗口布局
(winner-mode 1) ; 启用 winner-mode
;; 快捷键绑定 (可选，winner-mode 默认有 C-c <left>/C-c <right>)
;; (global-set-key (kbd "C-c C-l") 'winner-undo)
;; (global-set-key (kbd "C-c C-r") 'winner-redo)

;; ============================================================
;;                       13. 界面与可用性
;; ============================================================
;; 光标高亮信标
(straight-use-package 'beacon)
(beacon-mode 1)

;; 自动高亮当前符号
(straight-use-package 'highlight-symbol)
(add-hook 'prog-mode-hook #'highlight-symbol-mode)

;; 彩虹括号高亮
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ============================================================
;;                       14. 帮助系统增强
;; ============================================================
;; 更友好的帮助系统
(straight-use-package 'helpful)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h F") 'helpful-at-point)

;; ============================================================
;;                      15. 标签页管理 (Centaur Tabs)
;; ============================================================
;; (straight-use-package 'centaur-tabs)
;; (centaur-tabs-mode 1)
;; ;; 基本样式与行为
;; (setq centaur-tabs-style "rounded"
;;       centaur-tabs-set-icons t
;;       centaur-tabs-set-modified-marker t
;;       centaur-tabs-show-new-tab-button t
;;       centaur-tabs-auto-hide-tabs 't
;;       centaur-tabs-height 24
;;       centaur-tabs-bar-position 'top)
;; ;; 可选：高度/关闭按钮等（若变量存在则生效）
;; (with-eval-after-load 'centaur-tabs
;;   (when (boundp 'centaur-tabs-set-bar-height)
;;     (setq centaur-tabs-set-bar-height 25))
;;   (when (boundp 'centaur-tabs-close-button)
;;     (setq centaur-tabs-close-button "x"))
;;   (when (fboundp 'centaur-tabs-buffer-groups-update)
;;     (add-hook 'window-configuration-change-hook 'centaur-tabs-buffer-groups-update))
;;   (when (boundp 'centaur-tabs-set-close-button)
;;     (setq centaur-tabs-set-close-button 'right))
;;   (when (boundp 'centaur-tabs-set-font)
;;     (setq centaur-tabs-set-font "Input Sans Narrow-13")))

;; ============================================================
;;                       16. 模式增强 (Hydra)
;; ============================================================
(straight-use-package 'hydra)
;;
  ;; 字体大小调整 Hydra
  (defhydra hydra-font-size (:color blue :hint nil)
    "font"
    ("j" text-scale-increase "更大")
    ("k" text-scale-decrease "更小")
    ("0" (lambda () (interactive) (text-scale-set 0)) "重置")
    ("q" nil "退出"))

;; 绑定 Hydra 到 C-c f
(global-set-key (kbd "C-c f") 'hydra-font-size/font)

;;; ============================================================
;;; 17. 状态栏美化 
;;; ============================================================
;; 使用 straight.el 安装 moody 和 minions（推荐一起用，隐藏 minor modes）
(straight-use-package 'moody)
(straight-use-package 'minions)

;; 启用 moody + minions
(use-package moody
  :straight t
  :config
  ;; 启用左右弯曲的样式（moody 提供的亮点）
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package minions
  :straight t
  :config
  (minions-mode 1))

;; ------------------------------
;; NeoTree
;; ------------------------------
(use-package neotree
  :straight t
  :bind ([f8] . neotree-toggle)  ;; F8 打开/关闭
  :config
  ;; 设置项目根目录自动切换
  (setq neo-smart-open t)
  ;; 启用 all-the-icons（可选，性能影响小于 nerd-icons）
  (use-package all-the-icons
    :straight t)
  ;; NeoTree 只显示文件夹图标
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  ;; 自动跟随当前 buffer
  (setq neo-autorefresh t))

;; filepath: ~/.emacs.d/lisp/init-editor.el

;;; ============================================================
;;; 18. 更多 Doom 风格 UI (solaire, doom-themes extras)
;;; ============================================================
(straight-use-package 'solaire-mode)
(add-hook 'after-init-hook #'solaire-global-mode)
(solaire-mode +1)

;; doom-themes extras
(with-eval-after-load 'doom-themes
  (doom-themes-org-config)
  (doom-themes-neotree-config)
  (doom-themes-visual-bell-config))

;;; ============================================================
;;; 19. 额外 UI 美化
;;; ============================================================

;;  Dimmer — 自动淡化非活动窗口
(straight-use-package 'dimmer)
;; 在非焦点窗口中降低亮度，并排除部分临时缓冲区
(setq dimmer-fraction 0.25
  dimmer-exclusion-regexp "\\` \\*\\(which-key\\|NeoTree\\|Minibuffer\\)\\*\\'")
(dimmer-mode +1)

;;  Golden-Ratio — 自动调整活动窗口尺寸
(straight-use-package 'golden-ratio)
(golden-ratio-mode +1)
(setq golden-ratio-adjust-factor .8)

;;  Fill-Column-Indicator — 在指定列画一条竖线
(straight-use-package 'fill-column-indicator)
(add-hook 'prog-mode-hook #'fci-mode)
(setq fill-column 80)


;;  Minimap — 编辑区侧边预览
(straight-use-package 'minimap)
(setq minimap-window-location 'right
  minimap-width-fraction 0.05)
(global-set-key (kbd "C-c m") 'minimap-create)

(straight-use-package 'pulsar)
(add-hook 'after-init-hook #'pulsar-global-mode)
(setq pulsar-pulse t
  pulsar-delay 0.05
  pulsar-iterations 8
  pulsar-face 'pulsar-green)

(straight-use-package 'highlight-indent-guides)
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character
  highlight-indent-guides-character ?\│
  highlight-indent-guides-responsive 'top)

(straight-use-package 'vertico-posframe)
(vertico-posframe-mode 1)
(setq vertico-posframe-border-width 1
  vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))

;; Built-in tab-line
(add-hook 'after-init-hook #'tab-line-mode)
(setq tab-line-new-button-show nil
  tab-line-close-button-show nil
  tab-line-separator " | ")

(straight-use-package 'page-break-lines)
(add-hook 'prog-mode-hook #'page-break-lines-mode)

(straight-use-package 'eyebrowse)
(eyebrowse-mode t)
(setq eyebrowse-new-workspace t)

;; (provide 'init-editor)
;;   :config (setq eyebrowse-new-workspace t))

;; 增大 GC 阈值，减少垃圾回收次数
(setq gc-cons-threshold 100000000  ;; 100 MB
      gc-cons-percentage 0.6)

;; 启动时临时关闭 GC，加快加载
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 100000000)))

;; 提高滚动流畅度
(setq redisplay-dont-pause t
      fast-but-imprecise-scrolling t
      jit-lock-defer-time 0.05)

;; ------------------------------
;; 字体优化
;; ------------------------------
;; 设置固定宽度字体，提高绘制效率
(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)


;; YAML 语言支持
(straight-use-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook (lambda () (setq yaml-indent-offset 2)))

(provide 'init-editor)
