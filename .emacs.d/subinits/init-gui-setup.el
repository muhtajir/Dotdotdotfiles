(require 'f)

(use-package base16-theme
  :config
  (defvar base16-generic-colors
  `(:base00     ,(concat "#" (getenv "__BASE00"))
    :base01     ,(concat "#" (getenv "__BASE01"))
    :base02     ,(concat "#" (getenv "__BASE02"))
    :base03     ,(concat "#" (getenv "__BASE03"))
    :base04     ,(concat "#" (getenv "__BASE04"))
    :base05     ,(concat "#" (getenv "__BASE05"))
    :base06     ,(concat "#" (getenv "__BASE06"))
    :base07     ,(concat "#" (getenv "__BASE07"))
    :base08     ,(concat "#" (getenv "__BASE08"))
    :base09     ,(concat "#" (getenv "__BASE09"))
    :base0A     ,(concat "#" (getenv "__BASE0A"))
    :base0B     ,(concat "#" (getenv "__BASE0B"))
    :base0C     ,(concat "#" (getenv "__BASE0C"))
    :base0D     ,(concat "#" (getenv "__BASE0D"))
    :base0E     ,(concat "#" (getenv "__BASE0E"))
    :base0F     ,(concat "#" (getenv "__BASE0F"))))

  (deftheme base16-generic)
  (base16-theme-define 'base16-generic base16-generic-colors)
  ;; additional theming here:
  (set-face-font 'default "Source Code Pro 11")
  (setq evil-emacs-state-cursor   `(,(plist-get base16-generic-colors :base0D) box)
        evil-insert-state-cursor  `(,(plist-get base16-generic-colors :base05) bar)
        evil-motion-state-cursor  `(,(plist-get base16-generic-colors :base0E) box)
        evil-normal-state-cursor  `(,(plist-get base16-generic-colors :base05) box)
        evil-replace-state-cursor `(,(plist-get base16-generic-colors :base08) hollow)
        evil-visual-state-cursor  `(,(plist-get base16-generic-colors :base05) box)))

;; GUI and Highlighting settings
(setq inhibit-startup-message t)
(fringe-mode '(0 . 0))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(blink-cursor-mode 0)
(setq-default cursor-in-non-selected-windows nil)
(setq echo-keystrokes .01)
(setq eldoc-idle-delay .8)
(setq-default fill-column 80)
(my/add-hooks 'hl-line-mode '(prog-mode-hook text-mode-hook conf-mode-hook))

(use-package nswbuff
  :commands nswbuff-switch-to-next-buffer
  :config
  (setq nswbuff-delay-switch t)
  (add-to-list 'nswbuff-exclude-buffer-regexps "^\\*.*\\*$"))

(use-package telephone-line
  :config
  (telephone-line-defsegment telephone-line-my-projectile-segment ()
    (if projectile-project-name
      (let*
          ((branch (car (vc-git-branches)))
           (branch-str (unless (string= branch "master")
                         (concat "[" branch "]"))))
        (concat (projectile-project-name) branch-str))
      (file-name-base (directory-file-name (file-name-directory (buffer-file-name))))
))

  (telephone-line-defsegment telephone-line-my-flycheck-segment ()
    (when (bound-and-true-p flycheck-mode)
      (pcase flycheck-last-status-change
        ('finished (if flycheck-current-errors
                       (let-alist (flycheck-count-errors flycheck-current-errors)
                         (concat
                          (when .error
                            (propertize (prin1-to-string .error) 'face 'telephone-line-error))
                          (when (and .error .warning)
                            "/")
                          (when .warning
                            (propertize (prin1-to-string .warning) 'face 'telephone-line-warning))))
                     ""))
        ('running    "")
        ('no-checker (propertize "" 'face 'telephone-line-unimportant))
        ('not-checked "")
        ('interrupted (propertize "" 'face 'telephone-line-error))
        ('error       (propertize "" 'face 'telephone-line-error))
        ('suspicious  ""))))

  (telephone-line-defsegment telephone-line-my-buffer-segment ()
    (if (and (buffer-file-name)
             (fboundp 'projectile-project-name)
             (fboundp 'projectile-project-p)
             (projectile-project-p))
        (let*
            ((file-path
              (abbreviate-file-name (file-relative-name buffer-file-name (projectile-project-root))))
             (dir-list
              (f-split file-path))
             (first-part-str
              (apply 'concat
                     (mapcar (lambda (dir)
                               (file-name-as-directory
                                (if (> (length dir) 4)
                                    (concat (substring dir 0 2)
                                            (substring dir -2))
                                  dir)))
                             (subseq dir-list 0 -2))))
          (last-part-str
           (concat
            (file-name-as-directory (car (last dir-list 2)))
            (car (last dir-list))))
          (mode-line-str (concat first-part-str last-part-str)))
          (if (buffer-modified-p)
              (propertize mode-line-str 'face 'telephone-line-warning)
            mode-line-str))
      (telephone-line-raw mode-line-buffer-identification)))

  (setq telephone-line-lhs
        '((evil      . (telephone-line-evil-tag-segment))
          (accent    . (telephone-line-my-projectile-segment))
          (nil       . (telephone-line-my-buffer-segment))))

  (setq telephone-line-rhs
        '((nil       . (telephone-line-misc-info-segment
                        telephone-line-my-flycheck-segment))
          (accent    . (telephone-line-major-mode-segment))
          (evil      . (telephone-line-airline-position-segment))))
  (setq telephone-line-primary-left-separator     'telephone-line-cubed-left
        telephone-line-secondary-left-separator   'telephone-line-abs-left
        telephone-line-primary-right-separator    'telephone-line-cubed-right
        telephone-line-secondary-right-separator  'telephone-line-abs-right)
  (telephone-line-mode t))

(my/add-hooks (lambda ()
                (setq display-line-numbers 'relative
                      display-line-numbers-widen t
                      display-line-numbers-current-absolute t))
              '(prog-mode-hook text-mode-hook conf-mode-hook))

(use-package sublimity
  :commands sublimity-mode
  :init
  (my/add-hooks (lambda ()
                  (if (> (count-lines 1 (point-max)) 80)
                      (sublimity-mode 1)
                    (sublimity-mode 0)))
                '(prog-mode-hook text-mode-hook))
  :config
  (add-to-list 'sublimity-disabled-major-modes 'term-mode)
  (require 'sublimity-scroll)
  (setq sublimity-scroll-weight 8)
  (setq sublimity-scroll-drift-length 2))

(provide 'init-gui-setup)
