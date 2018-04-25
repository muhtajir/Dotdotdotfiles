(use-package general
  :config
  (general-auto-unbind-keys)

  (general-create-definer general-def-leader
                          :prefix "spc")

  (general-create-definer general-def-goleader
                          :prefix "g")

  ;; helper functions to bind keys to
  (defun my/describe-function-or-variable ()
    (interactive)
    (ignore-errors
      (let ((func-or-var (intern (thing-at-point 'symbol t))))
        (cond ((fboundp func-or-var)
               (describe-function func-or-var))
              ((boundp func-or-var)
               (describe-variable func-or-var))
              (t
               (message "No known variable or function under cursor."))))))

  (defun my/evil-dry-open-below (line)
    (interactive "p")
    (let ((pos (evil-column)))
      (evil-open-below line)
      (evil-normal-state nil)
      (evil-previous-line line)
      (evil-move-to-column pos)))

  (defun my/evil-dry-open-above (line)
    (interactive "p")
    (let ((pos (evil-column)))
      (evil-open-above line)
      (evil-normal-state nil)
      (evil-next-line)
      (evil-move-to-column pos)))

  (defun my/eval-visual-region ()
    (interactive)
    (when (> (mark) (point))
      (exchange-point-and-mark))
    (eval-region (mark) (point))
    (evil-normal-state))

  (defun my/eval-normal-line ()
    (interactive)
    (let ((pos (evil-column)))
      (evil-end-of-line)
      (eval-last-sexp nil)
      (evil-move-to-column pos)))

  (defun my/company-select-next ()
      "Navigate company-mode and also open the quickhelp popup."
    (interactive)
    (company-quickhelp-manual-begin)
    (company-select-next))

  (defun my/company-select-previous ()
      "Navigate company-mode and also open the quickhelp popup."
    (interactive)
    (company-quickhelp-manual-begin)
    (company-select-previous))

  ;; normal state keybinds
  (general-def
    :states 'normal
    "ö"         'my/evil-dry-open-below
    "ö"         'my/evil-dry-open-above)

  (general-def-leader
    :states 'normal
    "e"     'my/eval-normal-line
    "E"     'eval-buffer)

  (general-def-goleader
    :states 'normal
    "r"         'evil-replace-with-register)


  ;; motion state keybinds
  (general-def
    :states 'motion
    "C-h x"     'my/describe-function-or-variable
    "C-p"       'counsel-git
    "C-t"       'find-file-wildcards
    "C-u"       'evil-scroll-up
    "M-h"       'evil-window-left
    "M-j"       'evil-window-down
    "M-k"       'evil-window-up
    "M-l"       'evil-window-right
    "M-c"       'delete-window
    "("         'evil-backward-paragraph
    ")"         'evil-forward-paragraph
    "{"         'evil-backward-sentence-begin
    "}"         'evil-forward-sentence-begin
    "-"         'goto-last-change
    "+"         'goto-last-change-reverse)

  (general-def-leader
    :states 'motion
    "rc"    (general-lambda ()
                            (find-file (substitute-in-file-name "$home/.emacs.d/init.el")))
    "j"           'vertigo-jump-down
    "k"           'vertigo-jump-up
    "b"           'ivy-switch-buffer
    "o"           'delete-other-windows
    "h"           'next-buffer
    "l"           'previous-buffer
    "s"           'evil-window-split
    "v"           'evil-window-vsplit)


  ;; visual keybinds
  (general-def-leader
    :states 'visual
    "e"     'my/eval-visual-region)


  ;; insert keybinds
  (general-def
    :states 'insert
    "C-n"        nil
    "C-p"        nil
    "<backtab>"  'indent-relative)


  ;; ivy keybinds
  (general-def
    :keymaps 'ivy-minibuffer-map
    "<S-return>"    'ivy-call
    "<escape>"      'keyboard-escape-quit
    "C-u"           'ivy-scroll-down-command
    "C-d"           'ivy-scroll-up-command
    "M-k"           'ivy-previous-line
    "M-j"           'ivy-next-line)

  ;; company keybinds
  (general-def
    :keymaps 'company-active-map
    "C-n"    'my/company-select-next
    "C-p"    'my/company-select-previous))

  ;; end of keybinds

(provide 'init-keybinds)
