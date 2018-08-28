;; pos-tip setup for use by both company and flycheck
(use-package pos-tip
  :after company
  :config
  (setq x-gtk-use-system-tooltips nil)
  (setq pos-tip-foreground-color (plist-get base16-generic-colors :base07))
  (setq pos-tip-background-color (plist-get base16-generic-colors :base02)))

;; autocompletion
(use-package company
  :hook (prog-mode . company-mode)
  :config
  ;; workaround for compatibility with fill-column-indicator
  (defun on-off-fci-before-company(command)
    (when (string= "show" command)
      (turn-off-fci-mode))
    (when (string= "hide" command)
      (turn-on-fci-mode)))
  (advice-add 'company-call-frontends :before #'on-off-fci-before-company)

  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
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
      (flycheck-buffer)
      (setq flycheck-check-syntax-automatically
            (append flycheck-check-syntax-automatically '(idle-change)))))
  (defun my/flycheck-upon-normal-exit()
    (when (bound-and-true-p flycheck-mode)
      (flycheck-clear)
      (setq flycheck-check-syntax-automatically
            (delq 'idle-change flycheck-check-syntax-automatically))))
  (setq flycheck-check-syntax-automatically '(save idle-change))
  (setq flycheck-idle-change-delay 0.1)
  (setq flycheck-display-errors-delay 0.07)
  (add-hook 'evil-normal-state-entry-hook 'my/flycheck-upon-normal-entry)
  (add-hook 'evil-normal-state-exit-hook 'my/flycheck-upon-normal-exit)

  ;; hack for actions that aren't considered changes by flycheck
  (defun my/flycheck-idleize (&rest args)
    (flycheck--handle-idle-change-in-buffer (current-buffer)))
  (advice-add 'insert-for-yank :after #'my/flycheck-idleize)
  (advice-add 'undo-tree-undo :after #'my/flycheck-idleize)

  (mapc 'evil-declare-motion (list 'flycheck-next-error 'flycheck-previous-error))

  ;; create a right fringe if there are any errors
  (setq flycheck-indication-mode 'right-fringe)
  (defun my/flycheck-create-fringe ()
    (if (> (length flycheck-current-errors) 0)
        (set-window-fringes nil nil 9)
      (set-window-fringes nil nil 0)))
  (add-hook 'flycheck-after-syntax-check-hook 'my/flycheck-create-fringe)

  ;; select different fringe bitmaps for flycheck error levels
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'left-triangle
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'left-triangle
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'left-triangle
    :fringe-face 'flycheck-fringe-info))

(use-package yasnippet
  :hook ((text-mode prog-mode) . yas-minor-mode)
  :config
  (yas-reload-all)

  ;; bind this here because yas-maybe-expanded needs to be loaded first
  (general-def
    :states  'insert
    :keymaps 'yas-minor-mode-map
    "SPC"    yas-maybe-expand))


;; language specific major modes
(use-package fish-mode
  :defer t
  :config
  (setq fish-enable-auto-indent t))

(use-package tex
  :straight auctex
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

(provide 'init-language-specific)
