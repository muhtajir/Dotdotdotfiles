(use-package general
  :config
  (general-auto-unbind-keys)

  (general-create-definer general-def-leader
    :prefix "SPC")

  (general-create-definer general-def-goleader
    :prefix "g")

  (general-create-definer general-def-local
    :prefix "C-c")


  ;; normal state keybinds
  (general-def
    :states         'normal
    "M-p"           'evil-paste-pop
    "M-P"           'evil-paste-pop-next
    "C-a"           'evil-numbers/inc-at-pt
    "C-x"           'evil-numbers/dec-at-pt
    "ö"             'my/evil-dry-open-below
    "Ö"             'my/evil-dry-open-above
    "_"             'goto-last-change
    "-"             'goto-last-change-reverse)

  (general-def-leader
    :states         'normal
    "RET"           'quickrun
    "e"             'my/eval-normal-line
    "E"             'eval-buffer
    "P"             'my/evil-paste-with-newline-above
    "p"             'my/evil-paste-with-newline-below)

  (general-def-goleader
    :states         'normal
    "\""            'counsel-evil-registers
    "r"             nil
    "r"             'evil-replace-with-register)


  ;; motion state keybinds
  (general-def
    :states         'motion
    "("             'evil-backward-paragraph
    ")"             'evil-forward-paragraph
    "+"             'goto-last-change-reverse
    "-"             'goto-last-change
    "C-S-l"         'link-hint-copy-link
    "C-S-n"         'evil-mc-skip-and-goto-next-match
    "C-l"           'link-hint-open-link
    "C-n"           'evil-mc-make-and-goto-next-match
    "C-S-p"         (general-lambda
                     (evil-mc-undo-cursor-at-pos (point))
                     (evil-mc-skip-and-goto-prev-cursor))
    "C-p"           'evil-mc-skip-and-goto-prev-cursor
    "C-q"           'counsel-projectile-switch-project
    "C-u"           'evil-scroll-up
    "C-´"           'evil-ex-nohighlight
    "C-/"           'vr/isearch-forward
    "C-?"           'vr/isearch-backward
    "M-c"           'delete-window
    "M-h"           'evil-window-left
    "M-j"           'evil-window-down
    "M-k"           'evil-window-up
    "M-l"           'evil-window-right
    "Q"             'counsel-projectile-find-file
    "{"             'evil-backward-sentence-begin
    "}"             'evil-forward-sentence-begin
    "Ä"             'evil-mc-undo-all-cursors
    "ä"             'evil-mc-make-all-cursors)

  (general-def-leader
    :states         'motion
    "rc"            (general-lambda
                     (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))
    "hx"            'helpful-at-point
    "hf"            'helpful-callable
    "hF"            'helpful-command
    "hv"            'helpful-variable
    "hk"            'helpful-key
    "hm"            'describe-mode
    "hX"            'helpful-kill-buffers
    "SPC"           'vertigo-set-digit-argument
    "$"             'my/eshell
    "b"             (general-lambda
                     (let ((ivy-use-virtual-buffers nil))
                       (ivy-switch-buffer)))
    "B"             (general-lambda
                     (let ((ivy-use-virtual-buffers t))
                       (ivy-switch-buffer)))
    "k"             'kill-this-buffer
    "o"             'delete-other-windows
    "v"             'evil-window-split
    "s"             'evil-window-vsplit
    "X"             'evil-window-delete
    "I"             'ivy-resume
    "q"             'find-file
    "Q"             'my/sudo-find-file
    "S"             (general-lambda
                     (evil-window-split) (evil-window-down 1))
    "V"             (general-lambda
                     (evil-window-vsplit) (evil-window-right 1))
    "<tab>"         'evil-switch-to-windows-last-buffer
    "Yn"            'yas-new-snippet
    "Ye"            'yas-visit-snippet-file)


  ;; visual keybinds
  (general-def
    :states         'visual
    "*"             (lambda (count)
                      (interactive "P")
                      (my/evil-search-visual-selection 'forward count))
    "#"             (lambda (count)
                      (interactive "P")
                      (my/evil-search-visual-selection 'backward count)))

  (general-def-leader
    :states         'visual
    "e"             'my/eval-visual-region)


  ;; insert keybinds
  (general-def
    :states         'insert
    "C-n"           nil
    "C-p"           nil
    "C-f"           'forward-word
    "C-b"           'backward-word
    "C-a"           'move-beginning-of-line
    "C-e"           'move-end-of-line
    "M-f"           'forward-char
    "M-b"           'backward-char
    "<backtab>"     'indent-relative
    "<return>"      'newline
    "C-j"           'newline)
  

  ;;  evil-ex keybinds
  (general-def
    :keymaps        '(evil-ex-completion-map evil-ex-search-keymap)
    "M-k"           'previous-line-or-history-element
    "M-j"           'next-line-or-history-element)

  ;; minibuffer keybinds
  (general-def
    :keymaps        'minibuffer-local-map
    "C-v"           'yank
    "C-M-v"         'yank-pop
    "<escape>"      'minibuffer-keyboard-quit)

  ;; dired keybinds
  (general-def
    :keymaps        'dired-mode-map
    "SPC"           nil)

  ;; ivy keybinds
  (general-def
    :keymaps        'ivy-minibuffer-map
    "<S-return>"    'ivy-call
    "<escape>"      'keyboard-escape-quit
    "C-S-p"         'ivy-beginning-of-buffer
    "C-S-n"         'ivy-end-of-buffer
    "C-u"           'ivy-scroll-down-command
    "C-d"           'ivy-scroll-up-command
    "M-k"           'ivy-previous-history-element
    "M-j"           'ivy-next-history-element)

  ;; simple escape for multiple modes
  (general-def
    :states         'normal
    :keymaps        '(helpful-mode-map flycheck-error-list-mode-map godoc-mode-map
                                       quickrun--mode-map)
    "q"             'quit-window)

  ;; workaround for disabling evil-mc-key-map
  (general-def
    :states         '(normal motion)
    :keymaps        'evil-mc-key-map
    "gr"            nil
    "M-P"           nil
    "M-p"           nil)

  ;; keymap/mode-specific keybinds:
  ;;
  ;; company keybinds
  (general-def
    :keymaps       'company-active-map
    ;; insert newline with return even with open completions
    "<tab>"        nil
    "<return>"     (general-lambda
                    (company-complete)
                    (company-pseudo-tooltip-hide)
                    (newline 1 t))
    "S-<return>"   (general-lambda
                    (company-abort)
                    (newline 1 t))
    "C-n"          'my/company-select-next
    "C-p"          'my/company-select-previous)


  ;; eshell keybinds (eshell-mode-keymap is buffer-local and only gets
  ;; initialized after eshell is started - why?)
  (defun my/eshell-set-keys ()
    (general-def
      :states         'insert
      :keymaps        'eshell-mode-map
      "<return>"      'eshell-send-input
      "M-k"           'eshell-previous-matching-input-from-input
      "M-j"           'eshell-next-matching-input-from-input))
  (add-hook 'eshell-first-time-mode-hook 'my/eshell-set-keys)

  ;; fish-mode keybinds
  (general-def-leader
    :states         'normal
    :keymaps        'fish-mode-map
    "hx"            'man-follow)

  ;; flycheck-mode keybinds
  (general-def
    :states         'normal
    :keymaps        'flycheck-mode-map
    "M--"           'flycheck-next-error
    "M-_"           'flycheck-previous-error)

  (general-def-goleader
    :states         'normal
    :keymaps        'flycheck-mode-map
    "!"             'flycheck-list-errors
    "/"             'vr/replace
    "C-/"           'vr/query-replace)

  ;; flycheck-list-mode keybinds
  (general-def
    :states         'normal
    :keymaps        'flycheck-error-list-mode-map
    "j"             'flycheck-error-list-next-error
    "k"             'flycheck-error-list-previous-error
    "f"             'flycheck-error-list-set-filter
    "F"             'flycheck-error-list-reset-filter)

  ;; go-mode keybinds
  (general-def-leader
    :states         'normal
    :keymaps        'go-mode-map
    "hx"            'godoc-at-point
    "hf"            'godoc)

  (general-def-goleader
    :states         'normal
    :keymaps        'go-mode-map
    "d"             'godef-jump
    "D"             'godef-jump-other-window)

  (general-def-local
    :states          'normal
    :keymaps         'go-mode-map
    "i"              'go-import-add)

  ;; ivy keybindings
  (general-def
    :keymaps        'ivy-switch-buffer-map
    "C-k"           'ivy-switch-buffer-kill)

  ;; jedi keybinds
  (general-def-leader
    :states         'normal
    :keymaps        'jedi-mode-map
    "hf"            'jedi:doc-mode
    "hx"            'jedi:show-doc
    "S-<return>"    'my/python-test)
  
  (general-def-goleader
    :states         'normal
    :keymaps        'jedi-mode-map
    "d"             'jedi:goto-definition
    "D"             'jedi:goto-definition-pop-marker)

  ;; visual regexp keybinds
  (general-def
    :keymaps        'vr/minibuffer-keymap
    "<escape>"      'minibuffer-keyboard-quit)

  ;; yasnippet keybinds
  ;; yas-maybe-expand must be bound after the package is loaded because it's a var
  (general-def
    :keymaps        'yas-keymap
    :states         'insert
    "S-SPC"         (general-lambda
                     (insert " "))
    "C-<tab>"       'yas-skip-and-clear-field)

  (general-def-leader
    :keymaps        'snippet-mode-map
    :states         'normal
    "YY"            'yas-load-snippet-buffer-and-close
    "Yy"            'yas-load-snippet-buffer))

(provide 'init-keybinds)
