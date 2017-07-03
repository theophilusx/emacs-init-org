;;; emacspeak-mu4e.el --- Speech-enable mu4e  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable mu4e An Emacs Interface to mu mail 
;;; Keywords: Emacspeak,  Audio Desktop mu4e mail
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITN<SKELETON> FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; <SKELETON> == 

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'mu4e)
;;}}}
;;{{{ Map Faces:

;; (let ((print-length 0)
;;       (faces (emacspeak-wizards-enumerate-unmapped-faces "^<skeleton>"))
;;       (start (point)))
;;   (insert "\n\n(voice-setup-add-map \n'(\n")
;;   (cl-loop for f in faces do 
;;            (insert (format "(%s)\n" f)))
;;   (insert "\n)\n)")
;;   (goto-char start)
;;   (backward-sexp)
;;   (kill-sexp)
;;   (goto-char (search-forward "("))
;;   (indent-pp-sexp))

;;}}}
;;{{{ Interactive Commands:

;; (let ((print-length nil)
;;       (start (point))
;;       (commands (emacspeak-wizards-enumerate-uncovered-commands "^<skeleton>")))
;;   (insert "'(\n")
;;   (cl-loop for c in commands do (insert (format "%s\n" c)))
;;   (insert ")\n")
;;   (goto-char start)
;;   (backward-sexp)
;;   (kill-sexp)
;;   (goto-char (search-forward "("))
;;   (indent-pp-sexp))

;;; defadvice

(defun emacspeak-mu4e-msg-status (flags)
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

(defun emacspeak-mu4e-speak-msg-header ()
  (let* ((from-1 (first (mu4e-message-field-at-point :from)))
         (from (if (listp from-1)
                   (first from-1)
                 from-1))
        (subject (mu4e-message-field-at-point :subject))
        (date (format-time-string mu4e-headers-date-format
                                  (mu4e-message-field-at-point :date)))
        (flags (emacspeak-mu4e-msg-status
                (mu4e-message-field-at-point :flags))))
    (dtk-speak (format "%s from %s subject %s on %s"
                       flags from subject date))))

(defadvice mu4e-headers-next (around emacspeak pre act comp)
  (let ((docid (mu4e~headers-docid-at-point)))
    ad-do-it
    (when (eq docid (mu4e~headers-docid-at-point))
      (emacspeak-auditory-icon 'alert-user))
    (emacspeak-mu4e-speak-msg-header)))

(defadvice mu4e-headers-prev (around emacspeak pre act comp)
  (let ((docid (mu4e~headers-docid-at-point)))
    ad-do-it
    (when (eq docid (mu4e~headers-docid-at-point))
      (emacspeak-auditory-icon 'alert-user))
    (emacspeak-mu4e-speak-msg-header)))

(defadvice mu4e-view-scroll-up-or-next (after emacspeak pre act comp)
  "Speak the window after scrolling up"
  (save-excursion
    (emacspeak-auditory-icon 'scroll)
    (let* ((window (get-buffer-window (current-buffer)))
          (start (window-start window))
          (end (window-end window t)))
      (emacspeak-speak-region start end)))) 

(defun emacspeak-mu4e-speak-main-window ()
  "Speak the main mu4e window"
  ;; Just speak the buffer for now
  (dtk-speak "j jump to maildir. s search C compose and b bookmarks"))

(add-hook 'mu4e-main-mode-hook 'emacspeak-mu4e-speak-main-window t)

(defun emacspeak-mu4e-speak-message ()
  "Speak the current message"
  (save-excursion
    (re-search-forward "^[ \t\f]*$")
    (forward-line 1)
    (let ((start (point))
          (window (get-buffer-window (current-buffer))))
      (emacspeak-speak-region start (window-end window t)))))

(add-hook 'mu4e-view-mode-hook 'emacspeak-mu4e-speak-message t)


;;}}}
(provide 'emacspeak-mu4e)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
