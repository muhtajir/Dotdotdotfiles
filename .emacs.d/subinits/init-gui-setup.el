(require 'init-base16-generic-theme)

;; GUI and Highlighting settings
(setq inhibit-startup-message t)
(fringe-mode '(0 . 0))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(setq-default cursor-in-non-selected-windows nil)
(setq echo-keystrokes .01)
(setq eldoc-idle-delay .8)
(setq-default fill-column 80)

(use-package telephone-line
  :init
  (defvar telephone-line-lhs
    '((evil      . (telephone-line-evil-tag-segment))
      (accent    . (telephone-line-vc-segment
                    telephone-line-erc-modified-channels-segment
                    telephone-line-process-segment))
      (nil       . (telephone-line-buffer-segment))))
  (defvar telephone-line-rhs
    '((nil       . (telephone-line-misc-info-segment))
      (accent    . (telephone-line-major-mode-segment))
      (evil      . (telephone-line-airline-position-segment))))
  (defvar telephone-line-primary-left-separator     'telephone-line-cubed-left)
  (defvar telephone-line-secondary-left-separator   'telephone-line-cubed-hollow-left)
  (defvar telephone-line-primary-right-separator    'telephone-line-cubed-right)
  (defvar telephone-line-secondary-right-separator  'telephone-line-cubed-hollow-right)
  :config
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
                  (when (> (count-lines 1 (point-max)) 120)
                    (sublimity-mode 1)))
                '(prog-mode-hook text-mode-hook))
  :config
  (require 'sublimity-scroll)
  (setq sublimity-scroll-weight 8)
  (setq sublimity-scroll-drift-length 2)
  (require 'sublimity-map)
  (cancel-timer sublimity-map--timer)
  (mapc
   (lambda (x)
     (advice-add x :around (lambda (orig-func &rest args)
                             (let ((return-value (apply orig-func args)))
                               (sublimity-map-show)
                               return-value))))
   '(evil-scroll-down
     evil-scroll-up
     evil-scroll-page-down
     evil-scroll-page-up)))

(provide 'init-gui-setup)
