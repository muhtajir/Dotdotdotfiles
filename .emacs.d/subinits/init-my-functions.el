(require 'cl)

(defun my/add-hooks (func hooks)
  "Add FUNC to multiple HOOKS at once."
  (mapc (lambda (hook)
          (add-hook hook func))
        hooks))

(defun my/get-line ()
  "Uniform way to get content of current line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun my/sudo-find-file ()
  "Open 'find-file' with sudo prefix."
  (interactive)
  (let ((default-directory "/sudo::/"))
    (command-execute 'find-file)))

(defun my/dired-mark-toggle ()
  "Toggle mark for currently selected file."
  (interactive)
  (let ((inhibit-read-only t))
    (when (not (dired-between-files))
      (save-excursion
        (beginning-of-line)
        (apply 'subst-char-in-region
               (point) (1+ (point))
               (if (eq (following-char) ?\040)
                   (list ?\040 dired-marker-char)
                 (list dired-marker-char ?\040)))))))

(defun my/eshell ()
  "Open or bring eshell to front if it isn't already.
Otherwise kill the eshell buffer and window."
  (interactive)
  (if (get-buffer-window "*eshell*")
      (progn
        (select-window (get-buffer-window "*eshell*"))
        (delete-window))
    (eshell)))

(defun my/eval-visual-region ()
  (interactive)
  (when (> (mark) (point))
    (exchange-point-and-mark))
  (eval-region (mark) (point) t)
  (evil-normal-state))

(defun my/eval-normal-line ()
  (interactive)
  (let ((pos (current-column)))
    (end-of-line)
    (eval-last-sexp nil)
    (move-to-column pos)))

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

(defun my/helpful-buffer-other-window (buf)
    "Custom function to open helpful buffers;
Replace buffer/window if in helpful-mode, lazy-open otherwise."
    (let (sw)
      (if (eq major-mode 'helpful-mode)
          (progn
            (quit-window)
            (pop-to-buffer buf))
        (progn (setq sw (selected-window))
               (switch-to-buffer-other-window buf)))
      (helpful-update)
      (when sw (select-window sw))))

(defun my/open-line-above (line)
  "Really open LINE lines above instead of just prepending them to the beginning of the line or something."
  (interactive "p")
  (forward-line -1)
  (end-of-line)
  (open-line line)
  (forward-line line))

(defun my/python-remove-breakpoints ()
  "Remove all breakpoint declarations in buffer."
  (interactive)
  (let ((counter 0))
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "^[[:space:]]*breakpoint()[[:space:]]*\n" nil t)
        (replace-match "")
        (setq counter (1+ counter))))
    (message "%s breakpoint%s removed." counter (if (= counter 1) "" "s"))))

(defun my/python-test ()
  (interactive)
  (let ((old-py-path (getenv "PYTHONPATH"))
        (new-py-path (projectile-project-root)))
    (setenv "PYTHONPATH" new-py-path)
    (quickrun :source `((:command . "pytest")
                        (:default-directory . ,new-py-path)
                        (:exec . ("pytest"))))
    (setenv "PYTHONPATH" old-py-path)))

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
      (my/yas-func-padding (if (> indent 0) 1 2) down))))

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
      (evil-ex-search-next count))))

(provide 'init-my-functions)
