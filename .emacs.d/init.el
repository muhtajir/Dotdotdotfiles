;; enable sourcing from init scripts in emacs.d/subinits
(add-to-list 'load-path (expand-file-name "subinits" user-emacs-directory))

; get rid of the custom blabla by using custom-file
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load custom-file)

;; use-package setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

; why isn't everyone setting this one?
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; UI settings
(require 'init-powerline)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
; turn of blinking cursor
(blink-cursor-mode 0)
(show-paren-mode 1)

(require 'init-base16-generic-theme)
(require 'init-evil)

(use-package try)

;; define a global mode for nlinum-relative because linenumbers are super
;; everywhere
(define-globalized-minor-mode global-nlinum-relative-mode
                              nlinum-relative-mode
                              (lambda() (nlinum-relative-mode 1)))

(use-package nlinum-relative
             :config
             (nlinum-relative-setup-evil)
             (setq nlinum-relative-redisplay-delay 0)
             (setq nlinum-relative-current-symbol "")
             (global-nlinum-relative-mode 1))

(use-package smartparens
             :config
             (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package general)

(use-package helm
             :config
             (helm-mode 1))

(require 'init-keybinds)
