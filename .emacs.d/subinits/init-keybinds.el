(use-package general
  :config
  (general-auto-unbind-keys)

  (general-create-definer general-def-leader
                          :prefix "SPC")

  (general-create-definer general-def-goleader
                          :prefix "g")

  (defun my/evil-dry-open-below (line)
    (interactive "P")
    (let ((pos (evil-column)))
      (evil-open-below line)
      (evil-normal-state nil)
      (evil-previous-line line)
      (evil-move-to-column pos)))

  (defun my/evil-dry-open-above (line)
    (interactive "P")
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
    "Ö"         'my/evil-dry-open-above)

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
    "C-h x"     'helpful-at-point
    "C-h f"     'helpful-callable
    "C-h F"     'helpful-function
    "C-h v"     'helpful-variable
    "C-h k"     'helpful-key
    "C-q"       'counsel-projectile-switch-project
    "Q"         'counsel-projectile-find-file
    "C-u"       'evil-scroll-up
    "M-h"       'evil-window-left
    "M-j"       'evil-window-down
    "M-k"       'evil-window-up
    "M-l"       'evil-window-right
    "M-c"       'delete-window
    "C-l"       'link-hint-open-link
    "C-S-l"     'link-hint-copy-link
    "("         'evil-backward-paragraph
    ")"         'evil-forward-paragraph
    "{"         'evil-backward-sentence-begin
    "}"         'evil-forward-sentence-begin
    "-"         'goto-last-change
    "+"         'goto-last-change-reverse)

  (general-def-leader
    :states 'motion
    "rc"    (general-lambda ()
                  (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))
    "Q"     'find-file
    "SPC"   'vertigo-set-digit-argument
    "b"     'ivy-switch-buffer
    "h"     'next-buffer
    "I"     'ivy-resume
    "l"     'previous-buffer
    "o"     'delete-other-windows
    "s"     'evil-window-split
    "v"     'evil-window-vsplit
    "S"     (general-lambda ()
                            (evil-window-split) (evil-window-down 1))
    "V"     (general-lambda ()
                            (evil-window-vsplit) (evil-window-right 1))
    "<tab>" 'evil-switch-to-windows-last-buffer)


  ;; visual keybinds
  (general-def-leader
    :states 'visual
    "e"     'my/eval-visual-region)


  ;; insert keybinds
  (general-def
    :states         'insert
    "C-n"           nil
    "C-p"           nil
    "<backtab>"     'indent-relative)


  ;; dired-keybinds
  (general-def
    :keymaps    'dired-mode-map
    "SPC"       nil)


  ;; ivy keybinds
  (general-def
    :keymaps        'ivy-minibuffer-map
    "<S-return>"    'ivy-call
    "<escape>"      'keyboard-escape-quit
    "C-u"           'ivy-scroll-down-command
    "C-d"           'ivy-scroll-up-command
    "M-k"           'ivy-previous-line
    "M-j"           'ivy-next-line)


  ;; company keybinds
  (general-def
    :keymaps    'company-active-map
    ;; don't use return for anything in company
    "<return>"  'newline
    "C-n"       'my/company-select-next
    "C-p"       'my/company-select-previous)


  ;; go-mode-keybinds
  (general-def
    :states     'normal
    :keymaps    'go-mode-map
    "C-h x"     'godoc-at-point)

  (general-def-goleader
    :states     'normal
    :keymaps    'go-mode-map
    "d"         'godef-jump
    "D"         'godef-jump-other-window)

  ;; jedi-keybinds
  (general-def
    :states     'normal
    :keymaps    'jedi-mode-map
    "C-h x"     'jedi:show-doc)
  
  (general-def-goleader
    :states     'normal
    :keymaps    'jedi-mode-map
    "d"         'jedi:goto-definition
    "D"         'jedi:goto-definition-pop-marker))

  ;; end of keybinds

(provide 'init-keybinds)
