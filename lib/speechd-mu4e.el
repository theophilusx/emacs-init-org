;;; speechd-mu4e --- A simple extension of speechd-el for mu4e
;;; Commentary:
;;; This is a simple extension for speechd-el which enhances the
;;; speech output for mu4e

;;; Code:
(require 'mu4e)
(require 'speechd-out)

(defun speechd-mu4e-msg-status (flags)
  (mapconcat (lambda (e)
               (case e
                 ('draft "draft message")
                 ('flagged "which is starred")
                 ('new "new message")
                 ('seen "seen message")
                 ('passed ", has been forwarded")
                 ('replied "you have replied to")
                 ('trashed "and is marked deleted")
                 ('attach "with attachments")
                 ('encrypted ", is encrypted")
                 ('signed ", is signed")
                 ('unread "unread"))) flags " "))

(defun speechd-mu4e-speak-msg-header ()
  (let* ((mail-list (mu4e-message-field-at-point :mailing-list))
         (from-1 (first (mu4e-message-field-at-point :from)))
         (from (concat (if (listp from-1)
                            (first from-1)
                         from-1)
                       (if mail-list
                           (format " in list %s" mail-list)
                         "")))
         (subject (mu4e-message-field-at-point :subject))
         (date (format-time-string mu4e-headers-date-format
                                   (mu4e-message-field-at-point :date)))
         (flags (speechd-mu4e-msg-status
                 (mu4e-message-field-at-point :flags))))
    (speechd-out-text (format "%s from %s subject %s on %s"
                              flags from subject date)
                      :priority 'text)))

(speechd-speak-command-feedback mu4e-headers-next after
                                (speechd-mu4e-speak-msg-header))

(speechd-speak-command-feedback mu4e-headers-prev after
                                (speechd-mu4e-speak-msg-header))

(provide 'speechd-mu4e)
;;; speechd-mu4e.el ends here
