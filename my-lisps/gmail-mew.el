(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;; Optional setup (Read Mail menu for Emacs 21):
(if (boundp 'read-mail-command)
    (setq read-mail-command 'mew))

;; Optional setup (e.g. C-xm for sending a message):
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

(defvar mew-cite-fields '("From:" "Subject:" "Date:"))
(defvar mew-cite-format "From: %snSubject: %snDate: %snn")
(defvar mew-cite-prefix "> ")

(setq mew-ssl-verify-level 0)
(setq mew-use-cached-passwd t)

;; Setting MailBox
(setq mew-config-alist
      '(
        ;; Default fetch mailbox is IMAP
        (default
         (mailbox-type          imap)
         (proto                 "%")
         (prog-ssl "/usr/bin/mewstunnel")
         (imap-server           "imap.gmail.com")
         (imap-user             "wxjeacen@gmail.com")
         (name                  "wxjeacen")
         (user                  "wxjeacen")
         (mail-domain           "gmail.com")
         (imap-ssl t)
         (imap-size             0)
         (imap-delete           t)
         (imap-queue-folder     "%queue")
         (imap-trash-folder     "%Trash")
         (imap-ssl-port "993")
         ;; This must be in concile with your IMAP box setup
         (smtp-ssl t)
         (smtp-auth-list        ("PLAIN" "LOGIN" "CRAM-MD5"))
         (smtp-user             "wxjeacen@gmail.com")
         (smtp-server           "smtp.gmail.com")
         (smtp-ssl-port "465")
        )

        ;; (Xiang Wang
        ;;  (mailbox-type          imap)
        ;;  (proto                 "%")
        ;;  (prog-ssl "/usr/bin/mewstunnel")
        ;;  (imap-server           "imap.gmail.com")
        ;;  (imap-user             "wxjeacen@gmail.com")
        ;;  (name                  "wxjeacen")
        ;;  (user                  "wxjeacen")
        ;;  (mail-domain           "gmail.com")
        ;;  (imap-ssl t)
        ;;  (imap-size             0)
        ;;  (imap-delete           t)
        ;;  (imap-queue-folder     "%queue")
        ;;  (imap-trash-folder     "%Trash")
        ;;  (imap-ssl-port "993")
        ;;  ;; This must be in concile with your IMAP box setup
        ;;  (smtp-ssl t)
        ;;  (smtp-auth-list        ("PLAIN" "LOGIN" "CRAM-MD5"))
        ;;  (smtp-user             "wxjeacen@gmail.com")
        ;;  (smtp-server           "smtp.gmail.com")
        ;;  (smtp-ssl-port "465")
        ;; )
))

(setq mew-signature-file "~/Mail/signature")
(setq mew-signature-as-lastpart t)
(setq mew-signature-insert-last t)
(add-hook 'mew-before-cite-hook 'mew-header-goto-body)
(add-hook 'mew-draft-mode-newdraft-hook 'mew-draft-insert-signature)

;; (setq mew-refile-guess-alist
;;       '(("To:"
;;          ("@octave.org"                       . "+math/octave")
;;          ("@freebsd.org"                      . "+unix/freebsd"))
;;         ("Cc:"
;;          ("@octave.org"                       . "+math/octave")
;;          ("@freebsd.org"                      . "+unix/freebsd"))
;;         (nil . "+inbox")))
(setq mew-refile-guess-control
      '(mew-refile-guess-by-folder
        mew-refile-guess-by-alist))

(setq mew-summary-form
      '(type (5 date) " " (14 from) " " t (0 subj)))
(setq mew-summary-form-extract-rule '(name))

(provide 'gmail-mew)
