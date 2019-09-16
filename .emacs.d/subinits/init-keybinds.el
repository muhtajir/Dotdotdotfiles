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
    "<escape>"      (general-lambda
                     (evil-ex-nohighlight)
                     (evil-force-normal-state))
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
    "C-s"           'vr/replace
    "C-S-s"         'vr/query-replace
    "r"             nil
    "r"             'evil-replace-with-register
    "G"             'magit-status)

  (general-def-goleader
    :states         'motion
    "s"             (general-lambda
                     (if (string= (buffer-name) "*scratch*")
                         (evil-switch-to-windows-last-buffer)
                      (switch-to-buffer "*scratch*"))))

  ;; motion state keybinds
  (general-def
    :states             'motion
    "("                 'evil-backward-paragraph
    ")"                 'evil-forward-paragraph
    "+"                 'goto-last-change-reverse
    "-"                 'goto-last-change
    "C-S-l"             'link-hint-copy-link
    "C-S-n"             'evil-mc-skip-and-goto-next-match
    "C-l"               'link-hint-open-link
    "C-n"               'evil-mc-make-and-goto-next-match
    "C-S-p"             (general-lambda
                         (evil-mc-undo-cursor-at-pos (point))
                          (evil-mc-skip-and-goto-prev-cursor))
    "C-p"               'evil-mc-skip-and-goto-prev-cursor
    "C-q"               'counsel-projectile-switch-project
    "C-u"               'evil-scroll-up
    "<escape>"          (general-lambda
                         (evil-ex-nohighlight)
                         (evil-force-normal-state))
    "C-s"               'vr/isearch-forward
    "C-S-s"             'vr/isearch-backward
    "M-o"               'delete-other-windows
    "M-c"               'delete-window
    "M-h"               'evil-window-left
    "M-j"               'evil-window-down
    "M-k"               'evil-window-up
    "M-l"               'evil-window-right
    "M-H"               'helpful-kill-buffers
    "Q"                 'counsel-projectile-find-file
    "{"                 'evil-backward-sentence-begin
    "}"                 'evil-forward-sentence-begin
    "Ä"                 'evil-mc-undo-all-cursors
    "ä"                 'evil-mc-make-all-cursors
    "<S-SPC><S-SPC>"    'my/vertigo-reuse-last-arg)

  (general-def-leader
    :states         'motion
    "rc"            (general-lambda
                     (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))
    "rf"            'counsel-recentf
    "hx"            'helpful-at-point
    "hf"            'helpful-callable
    "hF"            'helpful-command
    "hv"            'helpful-variable
    "hk"            'helpful-key
    "hm"            'describe-mode
    "SPC"           'vertigo-set-digit-argument
    "$"             'my/vterm
    "C-$"           'my/eshell
    "%"             'my/counsel-ag-projectile
    "C-%"           'my/counsel-ag-prompt-path
    "b"             'nswbuff-switch-to-next-buffer
    "B"             (general-lambda
                     (let ((ivy-use-virtual-buffers t))
                       (ivy-switch-buffer)))
    "k"             'kill-this-buffer
    "v"             'evil-window-split
    "s"             'evil-window-vsplit
    "S"             (general-lambda
                     (evil-window-vsplit) (evil-window-right 1))
    "V"             (general-lambda
                     (evil-window-split) (evil-window-down 1))
    "X"             'evil-window-delete
    "I"             'ivy-resume
    "q"             'find-file
    "Q"             'my/sudo-find-file
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
    "C-a"           'move-beginning-of-line
    "C-e"           'move-end-of-line
    "C-S-f"         'forward-word
    "C-S-b"         'backward-word
    "<backtab>"     'indent-relative
    "<return>"      'newline
    "C-j"           'newline)
  

  ;; isearch keybinds
  (general-def
    :keymaps        'isearch-mode-map
    "C-S-s"         'isearch-repeat-backward)

  ;;  evil-ex and minibuffer keybinds
  (general-def
    :keymaps        '(evil-ex-completion-map evil-ex-search-keymap read-expression-map
                                             minibuffer-local-map)
    "C-h k"         'helpful-key
    "C-a"           'move-beginning-of-line
    "C-e"           'move-end-of-line
    "C-f"           'forward-char
    "C-b"           'backward-char
    "C-S-f"         'forward-word
    "C-S-b"         'backward-word
    "C-d"           'delete-char
    "C-S-d"         'kill-word
    "M-k"           'previous-line-or-history-element
    "M-j"           'next-line-or-history-element
    "C-v"           'yank
    "C-M-v"         'yank-pop
    "<escape>"      'minibuffer-keyboard-quit)

  ;; simple escape for multiple modes
  (general-def
    :states         'normal
    :keymaps        '(helpful-mode-map flycheck-error-list-mode-map godoc-mode-map
                                       quickrun--mode-map magit-mode-map)
    "q"             'quit-window)

  (general-def
    :states         'normal
    :keymaps        'view-mode-map
    "q"             'View-quit)
  ;; i don't know why this is necessary...?
  (add-hook 'view-mode-hook (lambda ()
                              (use-local-map view-mode-map)))
  

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

  ;; dired keybinds
  (general-def
    :keymaps        'dired-mode-map
    "SPC"           nil
    "t"             'my/dired-mark-toggle
    "T"             'dired-toggle-marks)

  ;; eshell keybinds (eshell-mode-keymap is buffer-local and only gets
  ;; initialized after eshell is started - why?)
  (defun my/eshell-set-keys ()
    (general-def
      :states           'insert
      :keymaps          'eshell-mode-map
      "<return>"        'eshell-send-input
      "M-k"             'eshell-previous-matching-input-from-input
      "M-j"             'eshell-next-matching-input-from-input)

    (general-def
      :states           'normal
      "^"               'eshell-bol
      "S"               (general-lambda
                         (eshell-bol)
                         (kill-line)
                         (evil-insert-state))))
  (add-hook 'eshell-first-time-mode-hook 'my/eshell-set-keys)

  ;; ivy keybinds
  (general-def
    :keymaps        'ivy-minibuffer-map
    "<S-return>"    'ivy-call
    "<C-return>"    'ivy-immediate-done
    "<escape>"      'keyboard-escape-quit
    "C-S-p"         'ivy-beginning-of-buffer
    "C-S-n"         'ivy-end-of-buffer
    "C-u"           'ivy-scroll-down-command
    "C-d"           'ivy-scroll-up-command
    "M-k"           'ivy-previous-history-element
    "M-j"           'ivy-next-history-element)

  (general-def
    :keymaps        'ivy-switch-buffer-map
    "C-k"           'ivy-switch-buffer-kill)


  ;; term keybinds
  (general-def
    :states         'insert
    :keymaps        'vterm-mode-map
    "<return>"      'vterm--self-insert
    "C-h k"         'helpful-key)

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
    "!"             'flycheck-list-errors)

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
    "hF"            'godoc)

  (general-def-goleader
    :states         'normal
    :keymaps        'go-mode-map
    "d"             'godef-jump
    "D"             'godef-jump-other-window)

  (general-def-local
    :states          'normal
    :keymaps         'go-mode-map
    "i"              'go-import-add)

  ;; markdown-mode keybinds
  (general-def-leader
    :states          'normal
    :keymaps         'flymd-map
    "RET"            'flymd-flyit)

  ;; magit keybindings
  (general-def
    :states         'emacs
    :keymaps        'magit-mode-map
    "M-h"           'evil-window-left
    "M-j"           'evil-window-down
    "M-k"           'evil-window-up
    "M-l"           'evil-window-right
    "j"             'magit-section-forward
    "k"             'magit-section-backward
    "p"             'magit-push
    "d"             'magit-delete-thing
    "D"             'magit-diff
    "Ju"            'magit-jump-to-unstaged
    "Js"            'magit-jump-to-staged
    "Jn"            'magit-jump-to-untracked
    "Jz"            'magit-jump-to-stashes
    "Jt"            'magit-jump-to-tracked)

  ;; nswbuff keybindings
  (general-def
    :keymaps        'nswbuff-override-map
    "j"             'nswbuff-switch-to-previous-buffer
    "k"             'nswbuff-switch-to-next-buffer
    "h"             'nswbuff-switch-to-previous-buffer
    "l"             'nswbuff-switch-to-next-buffer)

  ;; python and jedi keybinds
  (general-def-leader
    :states         'normal
    :keymaps        'python-mode-map
    "C-$"           'run-python
    "cB"            'my/python-remove-breakpoints)

  (general-def
    :states         'insert
    :keymaps        'inferior-python-mode-map
    "<return>"      'comint-send-input)

  (general-def-leader
    :states         'normal
    :keymaps        'jedi-mode-map
    "hF"            'jedi:doc-mode
    "hx"            'jedi:show-doc
    "S-<return>"    (general-lambda
                     (if (string-match-p "^test_" (buffer-file-name))
                         'my/python-test
                       'quickrun)))
  
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
    :keymaps        '(yas-keymap yas/keymap)
    "<tab>"         nil
    "TAB"           nil
    "<backtab>"     nil
    "M-j"           'yas-next-field-or-maybe-expand
    "M-k"           'yas-prev-field
    "M-S-j"         'yas-skip-and-clear-field)

  (general-def-leader
    :keymaps        'snippet-mode-map
    :states         'normal
    "YY"            'yas-load-snippet-buffer-and-close
    "Yy"            'yas-load-snippet-buffer))

(provide 'init-keybinds)
