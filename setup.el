;;; Simple elisp file to setup my emacs init files
(setq debug-on-error t)

(defun tangle-my-init (my-org-file)
  (message "Tangling init file from %s" my-org-file)
  ;; Setup directory structure if it doesn't already exist
  (message "Building directory structure")
  (let ((lib-dir (expand-file-name "lib" user-emacs-directory))
	(snippet-dir (expand-file-name "snippets" user-emacs-directory))
	(elpa-dir (expand-file-name "elpa" user-emacs-directory)))
    (if (not (file-directory-p lib-dir))
	(make-directory-internal lib-dir))
    (if (not (file-directory-p snippet-dir))
	(make-directory-internal snippet-dir))
    (if (not (file-directory-p elpa-dir))
	(make-directory-internal elpa-dir)))

  ;; load init.org and tangle
  (message "Get use-package package from elpa")
  (require 'package)

  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (add-to-list 'package-archives `("melpa" . "https://melpa.org/packages/"))
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)
  (require 'diminish)
  (require 'bind-key)
  (require 'ob-tangle)
  (org-babel-load-file (expand-file-name my-org-file user-emacs-directory)))


