;; ripgrep integration via rg.el
(straight-use-package 'rg)
(require 'rg)
;; Default flags: include hidden files, no headings, show line numbers, smart case
(setq rg-command-line-flags '("--hidden" "--no-heading" "--line-number" "--smart-case"))
;; Auto-hide the *rg* buffer after jumping to a result
(setq rg-hide-command t)
;; Keybinding: C-c r to launch an rg search
(global-set-key (kbd "C-c r") #'rg)

(provide 'init-