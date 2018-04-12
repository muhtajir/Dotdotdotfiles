;; cursor coloring to use with base16 package
(defvar my/base16-colors base16-generic-colors)
(setq evil-emacs-state-cursor   `(,(plist-get my/base16-colors :base0D) box)
      evil-insert-state-cursor  `(,(plist-get my/base16-colors :base0D) bar)
      evil-motion-state-cursor  `(,(plist-get my/base16-colors :base0E) box)
      evil-normal-state-cursor  `(,(plist-get my/base16-colors :base05) box)
      evil-replace-state-cursor `(,(plist-get my/base16-colors :base08) bar)
      evil-visual-state-cursor  `(,(plist-get my/base16-colors :base09) box))

;; undo-tree is a dependency but not available in melpa so get it from elpa
(use-package undo-tree
             :pin elpa)

(use-package evil-goggles
             :init
             (setq evil-goggles-duration 0.600)
             (setq evil-goggles-enable-delete nil)
             (setq evil-goggles-enable-undo nil)
             :config
             (evil-goggles-mode 1))

(use-package evil
             :ensure t
             :init
             (setq evil-want-Y-yank-to-eol t)
             :config
             (evil-mode 1))

(use-package evil-surround
             :config
             (global-evil-surround-mode 1))

(provide 'init-evil)
