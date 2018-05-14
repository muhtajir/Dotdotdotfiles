(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-mode 1)
  (counsel-mode 1))

(use-package flx
  :after counsel)

(use-package counsel-projectile
  :defer t)

(provide 'init-ivy)
