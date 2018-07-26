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
      `((".*" ,(concat emacs-tmp-dir "/\\1") t)))
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
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(setq-default cursor-in-non-selected-windows nil)
(setq echo-keystrokes .01)
(setq eldoc-idle-delay .03)
(setq-default fill-column 80)
(require 'init-mode-line)
(require 'init-base16-generic-theme)

;; Indentation settings (no TABs)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; load-up org-mode
(use-package org
  :ensure nil
  :commands org-mode
  :config
  (setq org-log-done 'time)
  (add-hook 'org-mode-hook (lambda ()
                             (setq evil-auto-indent nil))))

;; various mode setting options
(add-to-list 'auto-mode-alist '(".gitignore" . prog-mode))

;; eshell settings
(setq eshell-banner-message "")
(add-hook 'eshell-exit-hook (lambda ()
                              (when
                                  (string= (buffer-name (window-buffer (selected-window)))
                                           "*eshell*")
                                (delete-window))))

;; delimiter highlighting and matching
(electric-pair-mode 1)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; mark column 80 with line
(use-package fill-column-indicator
  :hook ((python-mode go-mode) . fci-mode)
  :config
  (setq fci-rule-color (plist-get base16-generic-colors :base01)))

;; sexier builtin help
(use-package helpful
  :defer t
  :config
  (defun my/helpful-buffer-other-window (buf)
    "Custom function to open helpful buffers;
Replace buffer/window if in helpful-mode, lazy-open otherwise."
    (let (sw)
      (if (eq major-mode 'helpful-mode)
          (progn
            (quit-window)
            (pop-to-buffer buf))
        (progn (setq sw (selected-window))
               (switch-to-buffer-other-window buf)))
      (helpful-update)
      (when sw (select-window sw))))
  (setq helpful-switch-buffer-function 'my/helpful-buffer-other-window)
  (setq helpful-max-buffers 2))

;; vimperator-style link-hints
(use-package link-hint
  :commands link-hint-open-link)

;; relative linenumbers
(use-package nlinum-relative
  :hook ((prog-mode text-mode conf-mode) . nlinum-relative-mode)
  :config
  (setq nlinum-format "%3d")
  (setq nlinum-relative-redisplay-delay 0)
  (setq nlinum-relative-current-symbol ""))

(use-package pcre2el
  :defer t)

(use-package quickrun
  :commands quickrun
  :config
  (setq quickrun-focus-p nil)
  (quickrun-add-command "go/go"
    '((:exec . ((lambda ()
                 (if (string-match-p "_test\\.go\\'" (buffer-name))
                     "%c test %o"
                   "%c run %o %d/*.go %a")))))
    :override t))

(use-package shackle
  :config
  (shackle-mode 1)
  (setq shackle-rules '(("\\*eshell\\*"
                         :regexp t :select t :popup t :align 'below :size 0.2))))

(use-package try
  :commands try)

(use-package visual-regexp-steroids
  :commands (vr/replace vr/query-replace vr/isearch-forward vr/isearch-backward)
  :after pcre2el
  :config
  (setq vr/engine 'pcre2el))

(require 'init-ivy)

(require 'init-evil)

(require 'init-language-specific)

(require 'init-keybinds)

;; open todo.org on startup
(add-hook 'after-init-hook (lambda ()
                             (find-file (expand-file-name "~/Nextcloud/Diverses/todo.org" (getenv "HOME")))
                             (org-cycle)))
