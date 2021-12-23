;;; General
;;;; Bootstrapping, package downloading
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

;;;; metadata
  (global-auto-revert-mode 1)
  
  (require 'package)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  
  (defun set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment variable to match
  that used by the user's shell.
  
  This is particularly useful under Mac OS X and macOS, where GUI
  apps are not started from a shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string
                            "[ \t\n]*$" "" (shell-command-to-string
                                            "$SHELL --login -c 'echo $PATH'"
                                            ))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  
  (defun reload-config ()
    (interactive)
    (load-file "~/.emacs.d/init.el"))
  
  (use-package all-the-icons
    :config
    (setq all-the-icons-scale-factor 1.1))
  
  (use-package speed-type)
  
                                          ; directories
  (setq backup-directory-alist '(("." . "~/.emacs.d/backup/")) ; ignore files wtih ~
        user-init-file "~/.emacs.d/myinit.org"
        default-directory "~/org/"  
        auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
        )
  
  (setq backup-by-copying t    ; Don't delink hardlinks
        version-control t      ; Use version numbers on backup
        delete-old-versions t  ; Automatically delete excess backup
        kept-new-versions 20   ; how many of the newest versions to keep
        kept-old-versions 5    ; and how many of the old
        ring-bell-function 'ignore)
  
  (defun join (sep lst)
    (mapconcat 'identity lst sep))
  
  (set-exec-path-from-shell-PATH)  
  
(set-register ?i (cons 'file user-init-file))


;;;; org bootstrap
(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
