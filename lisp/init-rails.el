;;; init-rails.el --- Rails development configuration

(use-package rinari
  :ensure t
  :config
  (diminish 'rinari-minor-mode "Rin")
  (global-rinari-mode))

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command (concat "ctags -a -e -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))

(use-package projectile-rails
  :ensure t
  :after projectile
  :config
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  (with-eval-after-load 'guide-key
    (add-to-list 'guide-key/guide-key-sequence "C-c r")))

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'"
  :hook (haml-mode . (lambda ()
                      (when (derived-mode-p 'ruby-mode)
                        (rinari-minor-mode))))

(provide 'init-rails)
;;; init-rails.el ends here
