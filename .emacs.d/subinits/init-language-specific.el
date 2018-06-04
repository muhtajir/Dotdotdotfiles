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

(use-package pkgbuild-mode
  :commands pkgbuild-mode)

(use-package go-mode
  :commands go-mode)

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

;; pos-tip setup for use by both company and flycheck
(use-package pos-tip
  :after (:any company flycheck)
  :config
  (setq x-gtk-use-system-tooltips nil)
  (setq pos-tip-foreground-color (plist-get base16-generic-colors :base07))
  (setq pos-tip-background-color (plist-get base16-generic-colors :base02)))

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
  :after (company pos-tip)
  :config
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
  ;; flycheck buffer when entering normal state
  (defun my/flycheck-upon-normal-entry ()
    (when (bound-and-true-p flycheck-mode)
      (flycheck-pos-tip-mode 1)
      (flycheck-buffer)
      (setq flycheck-check-syntax-automatically
            (append flycheck-check-syntax-automatically '(idle-change)))))
  (defun my/flycheck-upon-normal-exit()
    (when (bound-and-true-p flycheck-mode)
      (flycheck-pos-tip-mode 0)
      (flycheck-clear)
      (setq flycheck-check-syntax-automatically
            (delq 'idle-change flycheck-check-syntax-automatically))))
  (setq flycheck-check-syntax-automatically '(save idle-change))
  (setq flycheck-idle-change-delay 0.1)
  (add-hook 'evil-normal-state-entry-hook 'my/flycheck-upon-normal-entry)
  (add-hook 'evil-normal-state-exit-hook 'my/flycheck-upon-normal-exit)

  ;; hack for actions that aren't considered changes by flycheck
  (defun my/flycheck-idleize (&rest args)
    (flycheck--handle-idle-change-in-buffer (current-buffer)))
  (advice-add 'insert-for-yank :after #'my/flycheck-idleize)
  (advice-add 'undo-tree-undo :after #'my/flycheck-idleize))

(use-package flycheck-pos-tip
  :after (flycheck pos-tip)
  :config
  (flycheck-pos-tip-mode))

;; (use-package flycheck-gometalinter
;;   :hook (go-mode . flycheck-gometalinter-setup)
;;   :config
;;   (setq flycheck-gometalinter-fast t))

(use-package yasnippet
  :hook ((text-mode prog-mode) . yas-minor-mode))

(provide 'init-language-specific)
