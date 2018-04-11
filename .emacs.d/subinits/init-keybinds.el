(general-auto-unbind-keys)
(general-create-definer general-def-leader
    :prefix ",")

(general-def-leader
    :states 'normal
    "TAB" 'next-buffer
    "backtab" 'previous-buffer
    )

(provide 'init-keybinds)
