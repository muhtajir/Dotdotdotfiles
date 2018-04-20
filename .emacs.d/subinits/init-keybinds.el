(use-package general
  :config
  (general-auto-unbind-keys)

  (general-create-definer general-def-leader
                          :prefix "SPC")

  (general-create-definer general-def-goleader
                          :prefix "g")

  ;; helper functions to bind keys to
  (defun my/evil-dry-open-below(line)
    (interactive "P")
    (let ((pos (evil-column)))
      (evil-open-below line)
      (evil-normal-state nil)
      (evil-previous-line line)
      (evil-move-to-column pos)))

  (defun my/evil-dry-open-above(line)
    (interactive "P")
    (let ((pos (evil-column)))
      (evil-open-above line)
      (evil-normal-state nil)
      (evil-next-line)
      (evil-move-to-column pos)))


  ;; normal state keybinds
  (general-def
    :states 'normal
    "ö"         'my/evil-dry-open-below
    "Ö"         'my/evil-dry-open-above
    "M-y"       'helm-show-kill-ring
    "C-t"       'helm-find-files)

  (general-def-goleader
    :states 'normal
    "r"         'evil-replace-with-register)


  ;; motion state keybinds
  (general-def
    :states 'motion
    "M-h"       'evil-window-left
    "M-j"       'evil-window-down
    "M-k"       'evil-window-up
    "M-l"       'evil-window-right
    "M-x"       'helm-M-x
    "M-c"       'delete-window
    "("         'evil-backward-paragraph
    ")"         'evil-forward-paragraph
    "{"         'evil-backward-sentence-begin
    "}"         'evil-forward-sentence-begin
    "C-u"       'evil-scroll-up)

  (general-def-leader
    :states 'motion
    "rc"    (general-lambda ()
                            (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))
    "b"           'helm-mini
    "o"           'delete-other-windows
    "TAB"         'next-buffer
    "<backtab>"   'previous-buffer)


  ;; insert keybinds
  (general-def
    :states 'insert
    "<backtab>"  'indent-relative)


  ;; helm keybinds
  (general-def
    :keymaps 'helm-map
    "<escape>"  'keyboard-escape-quit
    "C-u"       'helm-previous-page
    "C-d"       'helm-next-page
    "M-k"       'helm-previous-line
    "M-j"       'helm-next-line))

;; end of keybinds


(provide 'init-keybinds)
