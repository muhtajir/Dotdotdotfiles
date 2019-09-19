(require 'cl)

;; make shell scripts executable after save if they include a shebang
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; pos-tip setup for use by both company and flycheck
(use-package pos-tip
  :after company
  :config
  (setq x-gtk-use-system-tooltips nil)
  (add-hook 'focus-out-hook #'pos-tip-hide))

;; autocompletion
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (push 'company-tng-frontend company-frontends)

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
  (evil-declare-not-repeat #'my/company-select-next)
  (evil-declare-not-repeat #'my/company-select-previous))


(use-package company-flx
  :after company
  :config
  (setq company-flx-limit 250)
  (company-flx-mode 1))

(use-package company-quickhelp
  :after (company pos-tip)
  :config
  (setq company-quickhelp-delay 0))

(use-package company-jedi
  :hook (python-mode . (lambda ()
                         (add-to-list 'company-backends #'company-jedi)
                         (jedi:setup)
                         (evil-add-command-properties #'jedi:goto-definition :jump t)
                         (setq jedi:tooltip-method nil))))

(use-package company-go
  :hook (go-mode . (lambda ()
                     (add-to-list 'company-backends 'company-go))))

(use-package company-auctex
  :after (company tex))

;; syntax checking
(use-package flycheck
  :hook ((emacs-lisp-mode python-mode go-mode LaTeX-mode) . flycheck-mode)
  :config
  ;; flycheck buffer when entering normal state
  (defun my/flycheck-upon-normal-entry ()
    (when (bound-and-true-p flycheck-mode)
      (flycheck-buffer)
      (setq flycheck-check-syntax-automatically
            (append flycheck-check-syntax-automatically '(idle-change)))))
  (defun my/flycheck-upon-normal-exit()
    (when (bound-and-true-p flycheck-mode)
      (flycheck-clear)
      (setq flycheck-check-syntax-automatically
            (delq 'idle-change flycheck-check-syntax-automatically))))
  (setq flycheck-check-syntax-automatically '(save idle-change))
  (setq flycheck-idle-change-delay 0.1)
  (setq flycheck-display-errors-delay 1)
  (add-hook 'evil-normal-state-entry-hook 'my/flycheck-upon-normal-entry)
  (add-hook 'evil-normal-state-exit-hook 'my/flycheck-upon-normal-exit)

  ;; hack for actions that aren't considered changes by flycheck
  (defun my/flycheck-idleize (&rest args)
    (cl-pushnew 'idle-change flycheck--idle-trigger-conditions))
  (advice-add 'insert-for-yank :after #'my/flycheck-idleize)
  (advice-add 'undo-tree-undo :after #'my/flycheck-idleize)

  (mapc 'evil-declare-motion (list 'flycheck-next-error 'flycheck-previous-error))

  ;; create a right fringe if there are any errors
  (setq flycheck-indication-mode 'right-fringe)
  (defun my/flycheck-create-fringe ()
    (if (> (length flycheck-current-errors) 0)
        (set-window-fringes nil nil 9)
      (set-window-fringes nil nil 0)))
  (add-hook 'flycheck-after-syntax-check-hook 'my/flycheck-create-fringe)

  ;; select different fringe bitmaps for flycheck error levels
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'left-triangle
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'left-triangle
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'left-triangle
    :fringe-face 'flycheck-fringe-info)

  ;; checker specific settings
  (setq flycheck-flake8-maximum-line-length 200))

(use-package yasnippet
  :hook ((text-mode prog-mode) . yas-minor-mode)
  :config
  (yas-reload-all)
  ;; bind this here because yas-maybe-expanded needs to be loaded first
  (general-def
    :states  'insert
    :keymaps 'yas-minor-mode-map
    "SPC"    yas-maybe-expand)

  ;; expansion for some python snippets
  (general-def
    :keymaps    'python-mode-map
    :states     'insert
    ":"         yas-maybe-expand)


  ;; yas related functions
  (defun* my/yas-func-padding (count &optional down)
    "Add COUNT empty lines above current position.

If DOWN is non-nil, then add lines below instead."
    (let ((counter count)
          (non-break t)
          (fillstr "")
          (direction (if down 1 -1))
          (current-line (line-number-at-pos)))
      ;; do nothing if we're already at the end or beginning of the file
      (when (or
             (= current-line 1)
             (>= current-line (- (line-number-at-pos (max-char)) 1)))
        (return-from my/yas-func-padding))
      (save-excursion
        (while (and (> counter 0) non-break)
          (forward-line direction)
          (if (string= "" (my/get-line))
              (setq counter (1- counter))
            (setq non-break nil)))
        (make-string counter ?\n))))

  (defun my/yas-indented-p (line)
    "Return t if LINE is indented, else return nil."
    (if (string-match-p "^\s" line) t nil))

  (defun my/yas-snippet-key ()
    "Retrieve the key of the snippet that's currently being edited."
    (save-excursion
      (goto-char 0)
      (search-forward-regexp "# key:[[:space:]]*")
      (thing-at-point 'symbol t)))

  (defun my/yas-python-class-field-splitter (arg-string)
    "Return ARG-STRING as a conventional Python class field assignment block."
    (if (= (length arg-string) 0)
        ""
      (let ((clean-string)
            (field-list))
        (setq clean-string
              (string-trim-left (replace-regexp-in-string " ?[:=][^,]+" "" arg-string) ", "))
        (setq field-list (split-string clean-string ", +"))
        (string-join (mapcar (lambda (s) (concat "self." s " = " s "\n")) field-list)))))

  (defun my/yas-python-doc-wrapper (docstring side)
    "Wrap DOCSTRING in quotes on either left or right SIDE."
    (let* ((line-length (+ (python-indent-calculate-indentation) 6 (length docstring)))
           (nl ""))
      (when (> (+ (python-indent-calculate-indentation) 6 (length docstring)) fill-column)
        (setq nl "\n"))
      (apply 'concat
             (cond ((eq side 'left)
                    `("\"\"\"" ,nl))
                   ((eq side 'right)
                    `(,nl "\"\"\""))))))

  (defun my/yas-python-func-padding (indent &optional down)
    "Use Python INDENT to determine necessary padding for class or function declaration.
If decorator syntax is found a line above the current, don't do any padding."
    (let ((decorated nil))
      (unless down
        (save-excursion
          (forward-line -1)
          (setq decorated (string-match-p "^[ \t]*@" (my/get-line)))))
      ;; exit without any padding here if this is a decorated function
      (if decorated
          ""
        (my/yas-func-padding (if (> indent 0) 1 2) down)))))

;; ;; mark text after column 80 in prog-modes (but not elisp because headaches)
(use-package column-enforce-mode
  :hook ((python-mode go-mode) . column-enforce-mode))


;; language specific major modes and their settings
(use-package fish-mode
  :defer t
  :config
  (setq fish-enable-auto-indent t))

(use-package tex
  :straight auctex
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  :config
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'company-auctex-init))

(use-package markdown-mode
  :defer t)
(use-package flymd
  :after markdown-mode
  :config
  (setq flymd-output-directory temporary-file-directory))

(use-package pkgbuild-mode
  :commands pkgbuild-mode)

;; python settings
(add-hook
 'python-mode-hook
 (lambda ()
   ;; auto-fill
   (auto-fill-mode)
   (setq-local comment-auto-fill-only-comments t)
   (setq python-fill-docstring-style 'symmetric)
   ;; width settings
   (setq-local fill-column 79)
   (setq-local column-enforce-column 79)
   (setq-local electric-pair-open-newline-between-pairs nil)))

(use-package go-mode
  :commands go-mode)

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

(provide 'init-language-specific)
