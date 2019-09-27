;; smtp
(use-package smtpmail
  :straight nil
  :after mu4e
  :config
  (setq message-send-mail-function 'smtpmail-send-it))

(use-package mu4e
  :commands mu4e
  :straight nil
  :config
  ;; define smtp settings here as well in case they need to be made part of a mu4e context later
  (setq smtpmail-stream-type      'ssl
        smtpmail-smtp-server      "posteo.de"
        smtpmail-smtp-service     465
        smtpmail-queue-dir        "~/.local/share/mail/queued_mail")
  (setq mu4e-maildir              (expand-file-name "~/.local/share/mail/posteo")
        mu4e-sent-folder          "/Sent"
        mu4e-drafts-folder        "/Drafts"
        mu4e-trash-folder         "/Trash"
        mu4e-refile-folder        "/Archiv"
        mu4e-get-mail-command     "mbsync posteo"
        mu4e-user-mail-address-list '("{{MAIL_ADDRESS}}"))
  ;; maildir shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/Arbeit+Bildung/"    . ?b)
          ("/Archiv/"            . ?a)
          ("/Drafts/"            . ?d)
          ("/Following/"         . ?f)
          ("/Inbox/"             . ?i)
          ("/Privat/"            . ?p)
          ("/Sent/"              . ?s)
          ("/Transaktionen/"     . ?t)))
  ;; headers view
  (setq mu4e-use-fancy-chars t
        mu4e-headers-include-related nil
        mu4e-headers-fields '((:human-date . 12)
                             (:flags . 6)
                             (:from-or-to . 22)
                             (:subject)))

  ;; message view
  (setq mu4e-view-scroll-to-next t)
  (setq mu4e-confirm-quit nil
        mu4e-change-filenames-when-moving t
        mu4e-view-show-images t
        mu4e-html2text-command "lynx -stdin -dump")
  (when (image-type-available-p 'imagemagick)
    (imagemagick-register-types))
  (add-to-list 'mu4e-view-actions
               '("bopen in browser" . mu4e-action-view-in-browser))

  (defun my/mu4e-headers-mark-toggle ()
    (interactive)
    (if (mu4e-mark-docid-marked-p mu4e~highlighted-docid)
        (mu4e-headers-mark-and-next 'unmark)
      (mu4e-headers-mark-and-next 'something)))

  (defun my/mu4e-headers-mark-pattern ()
    (interactive)
    (cl-letf (((symbol-function 'mu4e~mark-get-markpair)
               (lambda (&rest args) '(something))))
      (mu4e-headers-mark-pattern)))

  (defun my/mu4e-view-mark-toggle ()
    (interactive)
    (mu4e~view-in-headers-context
     (if (mu4e-mark-docid-marked-p mu4e~highlighted-docid)
        (mu4e-headers-mark-and-next 'unmark)
      (mu4e-headers-mark-and-next 'something))))

  (defun my/mu4e-view-mark-pattern ()
    (interactive)
    (mu4e~view-in-headers-context
     (my/mu4e-headers-mark-pattern)))

  (defun my/mu4e-save-attachment (filename mime)
    (let (dir)
      (setq dir (read-directory-name "Save to: " (xdg-user-dir "DOWNLOAD")))
      (cond
       ((and (file-exists-p) (not (file-directory-p dir)))
        (message "%s is not a directory.")
        nil)
       ((file-directory-p dir)
        dir)
       (t
        (when (y-or-n-p "Directory doesn't exist. Create? ")
          (make-directory dir t))
        dir))))
  (setq mu4e-attachment-dir #'my/mu4e-save-attachment)

  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (setq-local evil-emacs-state-cursor nil)
              (setq-local cursor-type nil))))

(provide 'init-mu4e)
