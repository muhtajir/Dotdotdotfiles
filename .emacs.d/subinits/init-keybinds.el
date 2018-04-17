(defun close-current-window ()
    (interactive)
    (delete-window))

(general-auto-unbind-keys)
(general-create-definer general-def-leader
    :prefix ",")

;; normal state keybinds
(general-def
    :states 'normal
    "M-x"   'helm-M-x
)

(general-def-leader
    :states 'normal
    "TAB"       'next-buffer
    "backtab"   'previous-buffer
)

;; motion state keybinds
(general-def
    :states 'motion
    "M-h"   'evil-window-left
    "M-j"   'evil-window-down
    "M-k"   'evil-window-up
    "M-l"   'evil-window-right
    "M-c"   'delete-window
    "C-u"   'evil-scroll-up
)

(provide 'init-keybinds)
