;; filepath: /Users/teslalee/.emacs.d/lisp/init-mirrors.el
;;; init-mirrors.el --- ELPA 源镜像配置
;;; Commentary:
;; 定义多套 package-archives 镜像 (官方、清华、USTC)，
;; 并提供切换函数 `set-package-mirror`。
;;; Code:

(require 'package)

(defconst my/package-mirrors-alist
  '((official
     ("gnu"          . "https://elpa.gnu.org/packages/")
     ("melpa"        . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/"))
    (tsinghua
     ("gnu"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
     ("melpa"        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
     ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/"))
    (ustc
     ("gnu"          . "https://mirrors.ustc.edu.cn/elpa/gnu/")
     ("melpa"        . "https://mirrors.ustc.edu.cn/elpa/melpa/")
     ("melpa-stable" . "https://mirrors.ustc.edu.cn/elpa/stable-melpa/")))

  "Alist of mirror profiles and their `package-archives` alists.")

(defvar my/current-mirror 'ustc
  "当前使用的 ELPA 镜像配置，参见 `my/package-mirrors-alist`。")

(defun set-package-mirror (mirror)
  "切换 `package-archives` 到 MIRROR 并刷新包列表。
MIRROR 是 `official`、`tsinghua` 或 `ustc`。"
  (interactive
   (list (intern
          (completing-read
           "选择镜像 (official/tsinghua/ustc): "
           (mapcar (lambda (x) (symbol-name (car x)))
                   my/package-mirrors-alist)
           nil t nil nil (symbol-name my/current-mirror)))))
  (let ((archives (cdr (assoc mirror my/package-mirrors-alist))))
    (if archives
        (progn
          (setq package-archives archives
                my/current-mirror mirror)
          (message "已切换到 ELPA 镜像: %s" mirror)
          (package-refresh-contents))
      (error "未知的镜像：%s" mirror))))

;; 启动时默认使用 `my/current-mirror`
(when (not package--initialized)
  (setq package-archives
        (cdr (assoc my/current-mirror my/package-mirrors-alist)))
  (package-initialize))

(provide 'init-mirrors)
;;; init-mirrors.el ends here
