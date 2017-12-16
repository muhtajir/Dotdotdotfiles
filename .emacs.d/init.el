(add-to-list 'load-path (expand-file-name "subinits" user-emacs-directory))

(setq inhibit-startup-message t)

;; unclutter the ui
(tool-bar-mode -1)
(menu-bar-mode -1)

;; get rid of the custom blabla
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(show-paren-mode 1)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

(use-package ivy
             :config
             (ivy-mode 1))

(use-package base16-theme
             :init
             :config
             (load-theme 'base16-gruvbox-dark-pale t))

(require 'init-evil)
