(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  :config
  (push 'company-tng-frontend company-frontends)
  (add-hook 'prog-mode-hook 'company-mode))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 1.5)
  (add-hook 'prog-mode-hook 'company-quickhelp-mode))


(provide 'init-company)
