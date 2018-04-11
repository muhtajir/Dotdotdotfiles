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

(use-package evil
             :config
             (require 'evil)
             (evil-mode 1))

(use-package general)

(provide 'init-evil)
