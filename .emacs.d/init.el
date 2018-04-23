;; -*- no-byte-compile: t; -*-
;; enable sourcing from init scripts in emacs.d/subinits
(add-to-list 'load-path (expand-file-name "subinits" user-emacs-directory))

;; get rid of the custom blabla by using custom-file
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load custom-file)

;; use-package setup with auto-package-update
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; why isn't everyone setting this one?
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package auto-package-update
  :config
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; GUI and Highlighting settings
(setq inhibit-startup-message t)
(fringe-mode '(8 . 0))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq show-paren-delay 0)
(show-paren-mode 1)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(setq-default cursor-in-non-selected-windows nil)
(setq echo-keystrokes .01)
(require 'init-mode-line)
(require 'init-base16-generic-theme)

;; Indentation settings (no TABs)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(use-package helm
  :init
  ;; set fuzzy matching wherever it's supported
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)
  (setq helm-session-fuzzy-match t)
  (setq helm-etags-fuzzy-match t)
  ;; don't make helm expand the current window
  (setq helm-split-window-in-side-p t)
  :config
  (helm-autoresize-mode 1)
  (helm-mode 1))

;; Parens matching
(use-package highlight-parentheses
  :init
  (setq hl-paren-colors (list
                         (plist-get base16-generic-colors :base0D)
                         (plist-get base16-generic-colors :base0F)
                         (plist-get base16-generic-colors :base0E)
                         (plist-get base16-generic-colors :base0A)))
  (setq hl-paren-delay 0))

;; relative linenumbers
(use-package nlinum-relative
  :config
  (setq nlinum-format "%3d")
  (setq nlinum-relative-redisplay-delay 0)
  (setq nlinum-relative-current-symbol ""))

(use-package try)

(require 'init-evil)

(require 'init-company)

(require 'init-keybinds)

;; set mode-dependent hooks
(defun my/prog-mode-hooks ()
  (company-mode 1)
  (electric-pair-local-mode 1)
  (highlight-parentheses-mode 1)
  (nlinum-relative-mode 1))
(defun my/text-mode-hooks ()
  (nlinum-relative-mode 1))

(add-hook 'prog-mode-hook 'my/prog-mode-hooks)
(add-hook 'text-mode-hook 'my/text-mode-hooks)
