;; cursor coloring to use with base16 package
(defvar my/base16-colors base16-generic-colors)
(setq evil-emacs-state-cursor   `(,(plist-get my/base16-colors :base0D) box)
      evil-insert-state-cursor  `(,(plist-get my/base16-colors :base05) bar)
      evil-motion-state-cursor  `(,(plist-get my/base16-colors :base0E) box)
      evil-normal-state-cursor  `(,(plist-get my/base16-colors :base05) box)
      evil-replace-state-cursor `(,(plist-get my/base16-colors :base08) hollow)
      evil-visual-state-cursor  `(,(plist-get my/base16-colors :base05) box))

;; undo-tree is a dependency but not available in melpa so get it from elpa
(use-package undo-tree
  :pin gnu)

(use-package evil
  :after (undo-tree nlinum-relative)
  :init
  (setq evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1))

(use-package vertigo
  :init
  (setq vertigo-home-row '(?a ?s ?d ?f ?g ?h ?j ?k ?k ?l ?ö)))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-replace-with-register
  :after evil)

(use-package evil-goggles
  :after evil
  :init
  (setq evil-goggles-duration 0.500)
  (setq evil-goggles-enable-shift nil)
  (setq evil-goggles-enable-undo nil)
  (setq evil-goggles-enable-paste nil)
  (setq evil-goggles-enable-commentary nil)
  (setq evil-goggles-enable-surround nil)
  (setq evil-goggles-enable-delete nil)
  :config
  (add-hook 'after-init-hook 'evil-goggles-mode))

(provide 'init-evil)
