;; undo-tree is a dependency but not available in melpa so get it from elpa (straight takes care of this automatically)
(use-package undo-tree
  :config
  (setq undo-tree-enable-undo-in-region nil))

(use-package evil
  :after undo-tree
  :init
  (setq evil-search-module 'evil-search)
  :config
  (setq-default evil-symbol-word-search t)
  ;; workaround for view-mode keybinding behavior
  (add-hook 'view-mode-hook (lambda ()
                              (general-def :states 'normal :keymaps 'local
                                "q" nil)))
  (evil-mode 1)
  ;; sensible Y behavior
  (customize-set-variable 'evil-want-Y-yank-to-eol t)

  ;; evil-related-functions
  (defun my/evil-dry-open-below (&optional line)
    "Open LINE number of lines below but stay in current line."
    (interactive "p")
    (save-excursion
      (end-of-line)
      (open-line line)))

  (defun my/evil-dry-open-above (line)
    "Open LINE number of lines above but stay in current line."
    (interactive "p")
    (save-excursion
      (my/open-line-above line)))

  (defun my/evil-paste-with-newline-above (count)
    "Paste COUNT times into a newly opened line above."
    (interactive "p")
    (evil-with-single-undo
      (my/open-line-above 1)
      (evil-paste-after count)
      (indent-according-to-mode)))

  (defun my/evil-paste-with-newline-below (count)
    "Paste COUNT times into a newly opened line above."
    (interactive "p")
    (evil-with-single-undo
      (evil-open-below 1)
      (evil-normal-state nil)
      (evil-paste-after count)
      (indent-according-to-mode)))

  (defun my/evil-search-visual-selection (direction count)
    "Search for visually selected text in buffer.
DIRECTION can be forward or backward.  Don't know what COUNT does."
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
        (evil-ex-search-next count)))))

(use-package vertigo
  :commands vertigo-set-digit-argument
  :config
  (setq vertigo-home-row '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?ö))
  (setq vertigo-cut-off 9)
  (evil-declare-motion #'vertigo-set-digit-argument)
  (evil-add-command-properties #'vertigo-set-digit-argument :jump t)
  (defun my/vertigo--remember-arg (func num)
    (setq-local my/vertigo--last-arg num)
    (funcall func num))
  (advice-add #'vertigo--set-digit-argument :around #'my/vertigo--remember-arg)
  (defun my/vertigo-reuse-last-arg ()
    (interactive)
    (if (boundp 'my/vertigo--last-arg)
        (vertigo--set-digit-argument my/vertigo--last-arg)
      (message "No previously used vertigo."))))

(use-package evil-commentary
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-line)
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :commands evil-surround-operation
  :config
  (global-evil-surround-mode 1))

(use-package evil-replace-with-register
  :commands evil-replace-with-register)

(use-package evil-goggles
  :hook (after-init . evil-goggles-mode)
  :init
  (setq evil-goggles-duration 0.500)
  (setq evil-goggles-blocking-duration 0.001)
  (setq evil-goggles-enable-shift nil)
  (setq evil-goggles-enable-undo nil)
  (setq evil-goggles-enable-paste nil)
  (setq evil-goggles-enable-commentary nil)
  (setq evil-goggles-enable-surround nil)
  (setq evil-goggles-enable-delete nil))

(use-package evil-mc
  :commands (evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-make-all-cursors)
  :config
  (global-evil-mc-mode 1)
  (setq evil-mc-custom-known-commands
        '((indent-relative ((:default . evil-mc-execute-default-call))))))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

;; evil commands and ex-commands
(evil-define-command my/mv-buf-and-file (new-filename)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "<a>")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-filename)
          (message "A buffer named '%s' already exists!" new-filename)
        (progn
          (rename-file filename new-filename 1)
          (rename-buffer new-filename)
          (set-visited-file-name new-filename)
          (set-buffer-modified-p nil))))))

(evil-ex-define-cmd "mv" 'my/mv-buf-and-file)

(provide 'init-evil)
