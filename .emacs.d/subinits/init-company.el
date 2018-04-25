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
  (setq pos-tip-foreground-color (plist-get base16-generic-colors :base00))
  (setq pos-tip-background-color (plist-get base16-generic-colors :base04))
  (setq company-quickhelp-delay 0))

(provide 'init-company)
