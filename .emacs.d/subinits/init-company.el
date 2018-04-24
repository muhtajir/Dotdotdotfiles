(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  :config
  (push 'company-tng-frontend company-frontends))
(push-button)

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 1.5))

(provide 'init-company)
