;; language specific major modes
(use-package fish-mode)

(use-package tex
  :ensure auctex
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  :config
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'company-auctex-init))

(use-package go-mode)

;; autocompletion
(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  :config
  (push 'company-tng-frontend company-frontends)
  (add-hook 'prog-mode-hook 'company-mode))

(use-package company-flx
  :after company
  :init
  (setq company-flx-limit 250)
  :config
  (company-flx-mode 1))

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
    (add-to-list 'company-backends 'company-jedi)
    (jedi:setup))
  (add-hook 'python-mode-hook (lambda ()
                                (add-to-list 'company-backends 'company-jedi)
                                (jedi:setup))))

(use-package company-go
  :after (company go-mode)
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (add-to-list 'company-backends 'company-go))))

(use-package company-auctex
  :after company)

(provide 'init-language-specific)
