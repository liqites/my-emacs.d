;;; init-rails.el --- Rails development configuration

(straight-use-package 'diminish)

(straight-use-package 'rinari)
(global-rinari-mode)

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command
     (concat "ctags -a -e -f " rinari-tags-file-name
             " --tag-relative -R app lib vendor test"))))

(straight-use-package 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(with-eval-after-load 'guide-key
  (add-to-list 'guide-key/guide-key-sequence "C-c r"))

(straight-use-package 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
(add-hook 'haml-mode-hook
          (lambda ()
            (when (derived-mode-p 'ruby-mode)
              (rinari-minor-mode))))

(provide 'init-rails)
;;; init-rails.el ends here