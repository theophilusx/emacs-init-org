(defun ido-set-matches-1 (items &optional do-full)
  "Return list of matches in ITEMS."
  (let* ((case-fold-search  ido-case-fold)
	 (slash (and (not ido-enable-prefix) (ido-final-slash ido-text)))
	 (text (if slash (substring ido-text 0 -1) ido-text))
	 (rex0 (if ido-enable-regexp text (regexp-quote text)))
	 (rexq (concat rex0 (if slash ".*/" "")))
	 (re (if ido-enable-prefix (concat "\\`" rexq) rexq))
	 (full-re (and do-full
		       (not (and (eq ido-cur-item 'buffer)
				 ido-buffer-disable-smart-matches))
		       (not ido-enable-regexp)
		       (not (string-match "$\\'" rex0))
		       (concat "\\`" rex0 (if slash "/" "") "\\'")))
	 (suffix-re (and do-full slash
			 (not (and (eq ido-cur-item 'buffer)
				   ido-buffer-disable-smart-matches))
			 (not ido-enable-regexp)
			 (not (string-match "$\\'" rex0))
			 (concat rex0 "/\\'")))
	 (prefix-re (and full-re (not ido-enable-prefix)
			 (concat "\\`" rexq)))
	 (non-prefix-dot (or (not ido-enable-dot-prefix)
			     (not ido-process-ignore-lists)
			     ido-enable-prefix
			     (= (length ido-text) 0)))
	 full-matches suffix-matches prefix-matches matches)
    (setq ido-incomplete-regexp nil)
    (condition-case error
        (mapc
         (lambda (item)
           (let ((name (ido-name item)))
	     (if (and (or non-prefix-dot
                          (and (> (length name) 0)
                               (if (= (aref ido-text 0) ?.)
                                   (= (aref name 0) ?.)
                                 (/= (aref name 0) ?.))))
		      (string-match re name))
		 (cond
		  ((and (eq ido-cur-item 'buffer)
			(or (not (stringp ido-default-item))
			    (not (string= name ido-default-item)))
			(string= name (buffer-name ido-entry-buffer)))
		   (setq matches (cons item matches)))
		  ((and full-re (string-match full-re name))
		   (setq full-matches (cons item full-matches)))
		  ((and suffix-re (string-match suffix-re name))
		   (setq suffix-matches (cons item suffix-matches)))
		  ((and prefix-re (string-match prefix-re name))
		   (setq prefix-matches (cons item prefix-matches)))
		  (t (setq matches (cons item matches))))))
	   t)
         items)
      (invalid-regexp
       (setq ido-incomplete-regexp t
             ;; Consider the invalid regexp message internally as a
             ;; special-case single match, and handle appropriately
             ;; elsewhere.
             matches (cdr error))))
    (when prefix-matches
      (ido-trace "prefix match" prefix-matches)
      ;; Bug#2042.
      (setq matches (nconc prefix-matches matches)))
    (when suffix-matches
      (ido-trace "suffix match" (list text suffix-re suffix-matches))
      (setq matches (nconc suffix-matches matches)))
    (when full-matches
      (ido-trace "full match" (list text full-re full-matches))
      (setq matches (nconc full-matches matches)))
    (when (and (null matches)
	       ido-enable-flex-matching
	       (> (length ido-text) 1)
	       (not ido-enable-regexp))
      (setq re (concat (regexp-quote (string (aref ido-text 0)))
		       (mapconcat (lambda (c)
				    (concat "[^" (string c) "]*"
					    (regexp-quote (string c))))
				  (substring ido-text 1) "")))
      (if ido-enable-prefix
	  (setq re (concat "\\`" re)))
      (mapc
       (lambda (item)
	 (let ((name (ido-name item)))
	   (if (string-match re name)
	       (setq matches (cons item matches)))))
       items))
    (delete-consecutive-dups matches t)))

(provide 'ido-fix)
