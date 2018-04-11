(add-to-list 'load-path (expand-file-name "subinits" user-emacs-directory))

(setq inhibit-startup-message t)

;; unclutter the ui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; turn of blinking cursor
(blink-cursor-mode 0)

;; get rid of the custom blabla by using custom-file
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load custom-file)

(show-paren-mode 1)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; why isn't everyone setting this one?
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; counsel pulls in ivy and swiper as dependencies
(use-package counsel
             :config
             (counsel-mode 1))

(use-package try)

(use-package powerline)
(use-package powerline-evil
             :config
             (powerline-evil-vim-theme))

(use-package nlinum-relative
             :config
             (nlinum-relative-setup-evil)
             (add-hook 'prog-mode-hook 'nlinum-relative-mode)
             (setq nlinum-relative-redisplay-delay 0)
             (setq nlinum-relative-current-symbol ""))

(use-package smartparens
             :config
             (add-hook 'prog-mode-hook 'smartparens-mode))

(require 'init-base16-generic-theme)

(require 'init-evil)

(use-package general)

(require 'init-keybinds)
