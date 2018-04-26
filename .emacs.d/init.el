;; -*- no-byte-compile: t; -*-
;; enable sourcing from init scripts in emacs.d/subinits
(add-to-list 'load-path (expand-file-name "subinits" user-emacs-directory))

;; get rid of the custom blabla by using custom-file
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load custom-file)

;; set up a separate location for backup and temp files
(defconst emacs-tmp-dir (expand-file-name "auto-save" user-emacs-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

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

;; various mode setting options
(add-to-list 'auto-mode-alist '(".gitignore" . text-mode))
(add-hook 'python-mode-hook 'jedi:setup)

;; delimiter highlighting and matching
(add-hook 'prog-mode-hook 'electric-pair-local-mode)
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; mark column 80 with line
(use-package fill-column-indicator
  :init
  (setq fci-rule-color (plist-get base16-generic-colors :base01))
  :config
  (add-hook 'python-mode-hook 'fci-mode))

;; vimperator-style link-hints
(use-package link-hint)

;; relative linenumbers
(use-package nlinum-relative
  :config
  (setq nlinum-format "%3d")
  (setq nlinum-relative-redisplay-delay 0)
  (setq nlinum-relative-current-symbol "")
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (add-hook 'text-mode-hook 'nlinum-relative-mode))

(use-package try)

(require 'init-ivy)

(require 'init-evil)

(require 'init-company)

(require 'init-keybinds)
