(use-package flx)

(use-package counsel
  :after flx
  :init
  (setq ivy-use-virtual-buffers t)
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-mode 1)
  (counsel-mode 1))

(use-package counsel-projectile
  :after counsel)

(provide 'init-ivy)
