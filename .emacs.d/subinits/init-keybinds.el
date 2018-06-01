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

  (defun my/search-visual-selection (direction count)
    "Search for visually selected text in buffer."
    (when (> (mark) (point))
      (exchange-point-and-mark))
    (when (eq direction 'backward)
      (setq count (+ (or count 1) 1)))
    (let ((regex (format "\\<%s\\>" (regexp-quote (buffer-substring (mark) (point))))))
      (setq evil-ex-search-count count
            evil-ex-search-direction direction
            evil-ex-search-pattern
            (evil-ex-make-search-pattern regex)
            evil-ex-search-offset nil
            evil-ex-last-was-search t)
      ;; update search history unless this pattern equals the
      ;; previous pattern
      (unless (equal (car-safe evil-ex-search-history) regex)
        (push regex evil-ex-search-history))
      (evil-push-search-history regex (eq direction 'forward))
      (evil-ex-delete-hl 'evil-ex-search)
      (evil-exit-visual-state)
      (when (fboundp 'evil-ex-search-next)
        (evil-ex-search-next count))))

  ;; normal state keybinds
  (general-def
    :states     'normal
    "M-p"       'evil-paste-pop
    "M-S-p"     'evil-paste-pop-next
    "C-a"       'evil-numbers/inc-at-pt
    "C-x"       'evil-numbers/dec-at-pt
    "ö"         'my/evil-dry-open-below
    "Ö"         'my/evil-dry-open-above)

  (general-def-leader
    :states 'normal
    "c"     (general-lambda (when (bound-and-true-p flycheck-mode)
                              (sleep-for .5)
                              (flycheck-display-error-at-point)))
    "RET"   'quickrun
    "e"     'my/eval-normal-line
    "E"     'eval-buffer)

  (general-def-goleader
    :states 'normal
    "r"     nil
    "r"     'evil-replace-with-register)


  ;; motion state keybinds
  (general-def
    :states     'motion
    "("         'evil-backward-paragraph
    ")"         'evil-forward-paragraph
    "+"         'goto-last-change-reverse
    "-"         'goto-last-change
    "C-S-l"     'link-hint-copy-link
    "C-S-n"     'evil-mc-skip-and-goto-next-match
    "C-l"       'link-hint-open-link
    "C-n"       'evil-mc-make-and-goto-next-match
    "C-S-p"     (general-lambda ()
                                (evil-mc-undo-cursor-at-pos (point))
                                (evil-mc-skip-and-goto-prev-cursor))
    "C-p"       'evil-mc-skip-and-goto-prev-cursor
    "C-q"       'counsel-projectile-switch-project
    "C-u"       'evil-scroll-up
    "C-´"       'evil-ex-nohighlight
    "M-c"       'delete-window
    "M-h"       'evil-window-left
    "M-j"       'evil-window-down
    "M-k"       'evil-window-up
    "M-l"       'evil-window-right
    "Q"         'counsel-projectile-find-file
    "{"         'evil-backward-sentence-begin
    "}"         'evil-forward-sentence-begin
    "Ä"         'evil-mc-undo-all-cursors
    "ä"         'evil-mc-make-all-cursors)

  (general-def-leader
    :states 'motion
    "rc"    (general-lambda ()
                  (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))
    "hx"     'helpful-at-point
    "hf"     'helpful-callable
    "hF"     'helpful-function
    "hv"     'helpful-variable
    "hk"     'helpful-key
    "SPC"    'vertigo-set-digit-argument
    "b"      'ivy-switch-buffer
    "j"      'next-buffer
    "k"      'previous-buffer
    "o"      'delete-other-windows
    "v"      'evil-window-split
    "s"      'evil-window-vsplit
    "I"      'ivy-resume
    "K"      (general-lambda ()
                             (kill-buffer nil))
    "Q"      'find-file
    "S"      (general-lambda ()
                             (evil-window-split) (evil-window-down 1))
    "V"      (general-lambda ()
                             (evil-window-vsplit) (evil-window-right 1))
    "<tab>"                                  'evil-switch-to-windows-last-buffer)


  ;; visual keybinds
  (general-def
    :states 'visual
    "*"     (lambda (count)
              (interactive "P")
              (my/search-visual-selection 'forward count))
    "#"     (lambda (count)
              (interactive "P")
              (my/search-visual-selection 'backward count)))

  (general-def-leader
    :states 'visual
    "e"     'my/eval-visual-region)


  ;; insert keybinds
  (general-def
    :states         'insert
    "C-n"           nil
    "C-p"           nil
    "<backtab>"     'indent-relative)
  

  ;; dired keybinds
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


  ;; simple escape for multiple modes
  (general-def
    :states     'normal
    :keymaps    '(helpful-mode-map flycheck-error-list-mode-map godoc-mode-map)
    "q"         'quit-window)

  ;; workaround for disabling evil-mc-key-map
  (general-def
    :states     '(normal motion)
    :keymaps    'evil-mc-key-map
    "gr"        nil)

  ;; keymap/mode-specific keybinds:
  ;;
  ;; company keybinds
  (general-def
    :keymaps    'company-active-map
    ;; insert newline with return even with open completions
    "<return>"  (general-lambda ()
                                (company-complete)
                                (newline 1 t))
    "C-n"       'my/company-select-next
    "C-p"       'my/company-select-previous)


  ;; flycheck-mode keybinds
  (general-def
    :states     'normal
    :keymaps    'flycheck-mode-map
    "C-j"       'flycheck-next-error
    "C-k"       'flycheck-previous-error)

  (general-def-goleader
    :states     'normal
    :keymaps    'flycheck-mode-map
    "!"         'flycheck-list-errors)

  ;; flycheck-list-mode keybinds
  (general-def
    :states     'normal
    :keymaps    'flycheck-error-list-mode-map
    "j"         'flycheck-error-list-next-error
    "k"         'flycheck-error-list-previous-error
    "f"         'flycheck-error-list-set-filter
    "F"         'flycheck-error-list-reset-filter)

  ;; go-mode keybinds
  (general-def-leader
    :states     'normal
    :keymaps    'go-mode-map
    "hx"        'godoc-at-point
    "hf"        'godoc)

  (general-def-goleader
    :states     'normal
    :keymaps    'go-mode-map
    "d"         'godef-jump
    "D"         'godef-jump-other-window)

  ;; ivy keybindings
  (general-def
    :keymaps    'ivy-switch-buffer-map
    "C-k"       'ivy-switch-buffer-kill)

  ;; jedi keybinds
  (general-def-leader
    :states     'normal
    :keymaps    'jedi-mode-map
    "hf"        'jedi:doc-mode
    "hx"        'jedi:show-doc)
  
  (general-def-goleader
    :states     'normal
    :keymaps    'jedi-mode-map
    "d"         'jedi:goto-definition
    "D"         'jedi:goto-definition-pop-marker))

(provide 'init-keybinds)
