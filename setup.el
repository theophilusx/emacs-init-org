;;; Simple elisp file to setup my emacs init files

;; Setup directory structure if it doesn't already exist
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
      (lib-dir (expand-file-name "lib" user-emacs-directory))
      (snippet-dir (expand-file-name "snippets" user-emacs-directory))
      (elpa-dir (expand-file-name "elpa" user-emacs-directory)))
       (if (not (file-directory-p lisp-dir))
           (make-directory-internal lisp-dir))
       (if (not (file-directory-p lib-dir))
           (make-directory-internal lib-dir))
       (if (not (file-directory-p snippet-dir))
           (make-directory-internal snippet-dir))
       (if (not (file-directory-p elpa-dir))
           (make-directory-internal elpa-dir)))

;; load init.org and tangle
(require 'ob-tangle)

(org-babel-load-file (expand-file-name "init.org" user-emacs-directory))
