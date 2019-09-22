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
        mu4e-get-mail-command     "mbsync -V posteo"))

(provide 'init-mu4e)
