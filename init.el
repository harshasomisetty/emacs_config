(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; make straight-use-package work with package.el stuff
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-recipe-repositories '(org-elpa melpa gnu-elpa-mirror el-get emacsmirror-mirror ))


(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
