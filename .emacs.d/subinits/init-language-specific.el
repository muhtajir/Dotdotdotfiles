;; language specific major modes
(use-package fish-mode
  :defer t)

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
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (push 'company-tng-frontend company-frontends))

(use-package company-flx
  :after company
  :config
  (setq company-flx-limit 250)
  (company-flx-mode 1))

(use-package company-quickhelp
  :after company
  :config
  ;; set pos-tip theme
  (setq pos-tip-foreground-color (plist-get base16-generic-colors :base07))
  (setq pos-tip-background-color (plist-get base16-generic-colors :base02))
  (setq company-quickhelp-delay 0))

(use-package company-jedi
  :hook (python-mode . (lambda ()
                         (add-to-list 'company-backends 'company-jedi)
                         (jedi:setup))))

(use-package company-go
  :hook (go-mode . (lambda ()
                     (add-to-list 'company-backends 'company-go))))

(use-package company-auctex
  :after (company tex))

;; syntax checking
(use-package flycheck
  :hook ((emacs-lisp-mode python-mode go-mode LaTeX-mode) . flycheck-mode)
  :config
  (defun my/flycheck-upon-normal-entry ()
    (when (bound-and-true-p flycheck-mode)
      (flycheck-buffer)
      (setq flycheck-check-syntax-automatically
            (append flycheck-check-syntax-automatically '(idle-change)))))
  (defun my/flycheck-upon-normal-exit()
    (when (bound-and-true-p flycheck-mode)
      (setq flycheck-check-syntax-automatically
            (delq 'idle-change flycheck-check-syntax-automatically))))
  (setq flycheck-display-errors-delay 0.1)
  (setq flycheck-check-syntax-automatically '(save idle-change))
  (setq flycheck-idle-change-delay 0.1)
  (add-hook 'evil-normal-state-entry-hook 'my/flycheck-upon-normal-entry)
  (add-hook 'evil-normal-state-exit-hook 'my/flycheck-upon-normal-exit))

(provide 'init-language-specific)
