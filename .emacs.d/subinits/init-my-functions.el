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

(defun my/eshell ()
  "Open or bring eshell to front if it isn't already.
Otherwise kill the eshell buffer and window."
  (interactive)
  (if (get-buffer-window "*eshell*")
      (progn
        (select-window (get-buffer-window "*eshell*"))
        (kill-buffer))
    (eshell)))

(defun my/eval-visual-region ()
  (interactive)
  (when (> (mark) (point))
    (exchange-point-and-mark))
  (eval-region (mark) (point))
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

(defun my/open-line-above (line)
  "Really open LINE lines above instead of just prepending them to the beginning of the line or something."
  (interactive "p")
  (forward-line -1)
  (end-of-line)
  (open-line line)
  (forward-line line))

;; running tests via quickrun
(defun my/python-test ()
  (interactive)
  (let* ((old-py-path (getenv "PYTHONPATH"))
         (new-py-path (projectile-project-root)))
    (setenv "PYTHONPATH" new-py-path)
    (quickrun :source `((:command . "pytest")
                        (:default-directory . ,new-py-path)
                        (:exec . ("pytest"))))
    (setenv "PYTHONPATH" old-py-path)))


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
