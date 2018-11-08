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
  "Hide or show eshell window.
Start eshell if it isn't running already."
  (interactive)
  (if (get-buffer-window "*eshell*")
      (progn
        (select-window (get-buffer-window "*eshell*"))
        (delete-window))
    (eshell)))

(defun my/eval-visual-region ()
  "Evaluate region."
  (interactive)
  (when (> (mark) (point))
    (exchange-point-and-mark))
  (eval-region (mark) (point) t)
  (ignore-errors
   (evil-normal-state)))

(defun my/eval-normal-line ()
  "Evaluate current line."
  (interactive)
  (let ((pos (current-column)))
    (end-of-line)
    (eval-last-sexp nil)
    (move-to-column pos)))

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
  "Run pytest."
  (interactive)
  (let ((old-py-path (getenv "PYTHONPATH"))
        (new-py-path (projectile-project-root)))
    (setenv "PYTHONPATH" new-py-path)
    (quickrun :source `((:command . "pytest")
                        (:default-directory . ,new-py-path)
                        (:exec . ("pytest"))))
    (setenv "PYTHONPATH" old-py-path)))

(defun my/source-ssh-env ()
  "Read environment variables for the ssh environment from '~/.ssh/environment'."
  (let (pos1 pos2 (var-strs '("SSH_AUTH_SOCK" "SSH_AGENT_PID")))
    (unless (some 'getenv var-strs)
      (with-temp-buffer
        (ignore-errors
          (insert-file-contents "~/.ssh/environment")
          (mapc
           (lambda (var-str)
             (goto-char 0)
             (search-forward var-str)
             (setq pos1 (+ (point) 1))
             (search-forward ";")
             (setq pos2 (- (point) 1))
             (setenv var-str (buffer-substring-no-properties pos1 pos2)))
           var-strs))))))

(defun my/fcitx-init ()
  "Enable fcitx input support."
  (interactive)
    (fcitx-default-setup)
    (when (eq system-type 'gnu/linux)
      (setq fcitx-use-dbus t)))

(defun my/term ()
  "Hide or show term window.
Start terminal if it isn't running already."
  (interactive)
  (let* ((term-buf "*ansi-term*")
         (term-win (get-buffer-window term-buf)))
    (if term-win
        (progn
          (select-window term-win)
          (delete-window))
      (unless (get-buffer term-buf)
        (let ((cur-buf (buffer-name)))
          (ansi-term "/bin/bash")
          (switch-to-buffer cur-buf)))
      (pop-to-buffer term-buf)
      (evil-insert-state))))

(provide 'init-my-functions)
