(use-package telephone-line
             :init
             (setq telephone-line-lhs
                   '((evil      . (telephone-line-evil-tag-segment))
                     (accent    . (telephone-line-vc-segment
                                   telephone-line-erc-modified-channels-segment
                                   telephone-line-process-segment))
                     (nil       . (telephone-line-buffer-segment))))
             (setq telephone-line-rhs
                   '((nil       . (telephone-line-misc-info-segment))
                     (accent    . (telephone-line-major-mode-segment))
                     (evil      . (telephone-line-airline-position-segment))))
             (setq telephone-line-primary-left-separator    'telephone-line-cubed-left
                   telephone-line-secondary-left-separator    'telephone-line-cubed-hollow-left
                   telephone-line-primary-right-separator    'telephone-line-cubed-right
                   telephone-line-secondary-right-separator    'telephone-line-cubed-hollow-right)
             :config
             (telephone-line-mode t))

(provide 'init-mode-line)
