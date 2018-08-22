(use-package flx)

(use-package counsel
  :after flx
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  ;; don't show lockfiles
  (setq counsel-find-file-ignore-regexp "^.#")
  (ivy-mode 1)
  (counsel-mode 1))

(use-package projectile
  :hook (prog-mode . projectile-mode))

(use-package counsel-projectile
  :defer t)

(provide 'init-ivy)
