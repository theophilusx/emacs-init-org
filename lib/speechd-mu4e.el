;;; speechd-mu4e --- A simple extension of speechd-el for mu4e
;;; Commentary:
;;; This is a simple extension for speechd-el which enhances the
;;; speech output for mu4e

;;; Code:
(require 'mu4e)

(defun speechd-init-mu4e ()
  "Speech enable mu4e."
  (speechd-speak-command-feedback mu4e-headers-next after (speechd-speak-read-line))
  (speechd-speak-command-feedback mu4e-headers-prev after (speechd-speak-read-line)))

(provide 'speechd-mu4e)
;;; speechd-mu4e.el ends here
