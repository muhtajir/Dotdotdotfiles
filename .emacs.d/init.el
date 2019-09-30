;; get rid of the custom blabla by using custom-file
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))

;; set up default browser
(setq browse-url-generic-program "qutebrowser")
(setq browse-url-browser-function 'browse-url-generic)

;; set up a separate location for backup and temp files
(defconst emacs-tmp-dir (expand-file-name "auto-save" user-emacs-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,(concat emacs-tmp-dir "/\\1") t)))
    (setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; enable sourcing from init scripts in emacs.d/subinits
(push (expand-file-name "subinits" user-emacs-directory) load-path)

;; use more conservative sentence definition
(setq sentence-end-double-space nil)

(require 'init-package-management)

;; custom functions
(defun my/add-hooks (func &rest hooks)
  "Add FUNC to multiple HOOKS at once."
  (mapc
   (lambda (hook)
     (add-hook hook func))
   hooks))

;; and everything that can be deferred goes in here
(use-package init-my-functions
  :straight nil
  :commands (my/get-line
             my/sudo-find-file
             my/dired-mark-toggle
             my/eshell
             my/eval-at-point
             my/eval-visual-region
             my/eval-normal-line
             my/evil-dry-open-below
             my/evil-dry-open-above
             my/evil-lisp-first-non-blank
             my/evil-lisp-append-line
             my/evil-lisp-insert-line
             my/evil-lisp-open-below
             my/evil-paste-with-newline-above
             my/evil-paste-with-newline-below
             my/evil-search-visual-selection
             my/python-remove-breakpoints
             my/python-test
             my/source-ssh-env
             my/split-window-sensibly
             my/straight-update
             my/toggle-scratch-buffer
             my/window-clear-side
             my/split-window-and-do))

;; setup gui early to avoid modeline troubles
(require 'init-gui-setup)

;; load up org-mode with workarounds
(require 'init-org-mode)

;; various mode setting options
(push '(".gitignore" . prog-mode) auto-mode-alist)

;; mu4e
(require 'init-mu4e)

(require 'init-ivy)

(require 'init-evil)

(require 'init-emacs-extensions)

(require 'init-general-programming)

(require 'init-language-specific)

(require 'init-keybinds)

;; load custom file late so it can make use of previously defined references
(load (expand-file-name custom-file user-emacs-directory))

;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-set-file-icons t
        dashboard-set-heading-icons t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  (add-hook 'window-setup-hook (lambda ()
                                 (evil-emacs-state)
                                 (dashboard-next-section)
                                 (dashboard-next-line 1)
                                 (beacon-blink)))
  ;; dashboard should ONLY be a starting point
  (run-at-time 120 nil #'kill-buffer dashboard-buffer-name))
