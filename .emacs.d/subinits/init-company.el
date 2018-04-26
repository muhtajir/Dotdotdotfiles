(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  :config
  (push 'company-tng-frontend company-frontends)
  (add-hook 'prog-mode-hook 'company-mode))

(use-package company-quickhelp
  :after company
  :init
  ;; set pos-tip theme
  (setq pos-tip-foreground-color (plist-get base16-generic-colors :base07))
  (setq pos-tip-background-color (plist-get base16-generic-colors :base02))
  (setq company-quickhelp-delay 0))

(use-package company-jedi
  :after company
  :config
  (defun my/python-company-mode ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-company-mode))

(provide 'init-company)
