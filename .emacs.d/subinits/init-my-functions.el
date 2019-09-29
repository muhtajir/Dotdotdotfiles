;; macros
(defmacro my/split-window-and-do (&rest funcs)
  `(progn
     (ignore-errors
       (select-window (funcall split-window-preferred-function)))
     ,@funcs))

;; functions
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

(defun my/eval-line ()
  "Evaluate current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (eval-last-sexp nil)))

(defun my/eval-at-point ()
  "Move out to closest sexp and evaluate."
  (interactive)
  (let ((point-char (thing-at-point 'char))
        (reg-start)
        (reg-end))
    (save-excursion
      (while (not (or (string= point-char "(")
                      (string= point-char ")")))
        (ignore-errors
            (backward-sexp))
          (backward-char)
        (setq point-char (thing-at-point 'char)))
      (if (string= point-char "(")
          (setq reg-start (point))
        (setq reg-end (+ (point) 1)))
      (evil-jump-item)
      (if reg-start
          (setq reg-end (+ (point) 1))
        (setq reg-start (point))))
    (eval-region reg-start reg-end t)))

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
  ;; this does not work with save-excursion if it's done at the beginning of
  ;; the buffer
  (let ((col (current-column)))
    (beginning-of-line)
    (open-line line)
    (forward-line line)
    (move-to-column col)))

(defun my/evil-paste-with-newline-above (count)
  "Paste COUNT times into a newly opened line above."
  (interactive "p")
  (evil-with-single-undo
    (evil-save-state
      (evil-open-above 1)
      (evil-paste-after count)
      (indent-according-to-mode))))

(defun my/evil-paste-with-newline-below (count)
  "Paste COUNT times into a newly opened line above."
  (interactive "p")
  (evil-with-single-undo
    (evil-save-state
      (evil-open-below 1)
      (evil-paste-after count)
      (indent-according-to-mode))))

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
    (unless (cl-some 'getenv var-strs)
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

(defun my/split-window-sensibly (&optional window)
  "Copied from standard function but with preference for horizontal split."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below))) (and
             ;; If WINDOW is the only usable window on its frame (it is
             ;; the only one or, not being the only one, all the other
             ;; ones are dedicated) and is not the minibuffer window, try
             ;; to split it vertically disregarding the value of
             ;; `split-height-threshold'.
             (let ((frame (window-frame window)))
               (or
                (eq window (frame-root-window frame))
                (catch 'done
                  (walk-window-tree (lambda (w)
                                      (unless (or (eq w window)
                                                  (window-dedicated-p w))
                                        (throw 'done nil)))
                                    frame)
                  t)))
             (not (window-minibuffer-p window))
             (let ((split-height-threshold 0))
               (when (window-splittable-p window)
                 (with-selected-window window
                   (split-window-below))))))))

(defun my/straight-update ()
  "Fetch, merge and rebuild all straight packages."
  (interactive)
  (straight-pull-all)
  (straight-rebuild-all))

(defun my/toggle-scratch-buffer ()
  "Go back and forth between scratch buffer and most recent other buffer."
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (evil-switch-to-windows-last-buffer)
    (switch-to-buffer "*scratch*")))

(defun my/window-clear-side ()
  "Clear selected pane from vertically split windows."
  (interactive)
  (cl-flet ((clear
             (direction)
             (while
                 (ignore-errors
                   (funcall (intern (concat "windmove-" direction))))
               (delete-window))))
    (mapc #'clear (list "up" "down"))))

(provide 'init-my-functions.el)
