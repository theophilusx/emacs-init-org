;;; speechd-extra --- Some extra utility functions for speechd-el
;;; Commentary:
;;; This package provides some useful utility functions for working
;;; with speechd-el
;;; Code:

(require 'speechd-speak)

(defun speechd-extra-speak-window ()
  "Read the window."
  (interactive)
  (speechd-speak-read-region (point) (point-max)))

(provide 'speechd-extra)
;;; speechd-extra.el ends here
