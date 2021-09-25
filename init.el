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

 '(custom-safe-themes
   '("cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" default))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "PT Mono" :height 140 :weight thin))))
 '(org-document-title ((t (:inherit default :weight normal :font "Cochin" :foreground "white" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight normal :font "Cochin" :foreground "white" :height 1.35))))
 '(org-level-2 ((t (:inherit default :weight normal :font "Cochin" :foreground "white" :height 1.27))))
 '(org-level-3 ((t (:inherit default :weight normal :font "Cochin" :foreground "white" :height 1.17))))
 '(org-level-4 ((t (:inherit default :weight normal :font "Cochin" :foreground "white" :height 1.05))))
 '(org-level-5 ((t (:inherit default :weight normal :font "Cochin" :foreground "white" :height 1.02))))
 '(org-level-6 ((t (:inherit default :weight normal :font "Cochin" :foreground "white" :height 1))))
 '(org-level-7 ((t (:inherit default :weight normal :font "Cochin" :foreground "white" :height 1))))
 '(org-level-8 ((t (:inherit default :weight normal :font "Cochin" :foreground "white" :height 1))))
 '(variable-pitch ((t (:family "Cochin" :height 165 :weight normal)))))
