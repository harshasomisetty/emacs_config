; auto reload files edited outside of emacs
(global-auto-revert-mode t)

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

(set-exec-path-from-shell-PATH)

;  backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")) ; ignore files wtih ~
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backup
  delete-old-versions t  ; Automatically delete excess backup
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
)

(use-package deft
  :config
  (setq deft-extensions '("org"))
  (setq deft-directory "~/org")
  (setq deft-recursive t)
  )
(global-set-key (kbd "C-x C-g") 'deft-find-file)

; window settings
(window-divider-mode)
(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode +1))

;; Move cursor to end of current line
 ;; Insert new line below current line
 ;; it will also indent newline
 (global-set-key (kbd "<C-return>") (lambda ()
                    (interactive)
                    (end-of-line)
                    (newline-and-indent)))

 ;; Move cursor to previous line
 ;; Go to end of the line
 ;; Insert new line below current line (So it actually insert new line above with indentation)
 ;; it will also indent newline
 (global-set-key (kbd "<C-S-return>") (lambda ()
                        (interactive)
                        (previous-line)
                        (end-of-line)
                        (newline-and-indent)
                        ))


; insert parens in pairs, highlights, etc

(use-package avy)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

; customized startup screen

 (setq inhibit-startup-screen t)
 ; (setq initial-frame-alist '((top . 0) (left . 1060) (width . 302) (height . 105)))
 (add-to-list 'default-frame-alist '(fullscreen . maximized))

;   (setq initial-buffer-choice "~/org/literature/doehw1.org")
   ; (split-window-right)
   ; (find-file "~/org/literature/DOE.org")
   ; (switch-to-buffer-other-window "DOE.org")
   ; (let ((org-agenda-window-setup)) (org-agenda nil "a"))

(use-package org)
(use-package org-contrib)
(defun org-clocking-buffer (&rest _))


(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (python . t)
   (C . t)
   (R . t)
   (java . t)
 ))

(setq org-babel-R-command "/Library/Frameworks/R.framework/Resources/R --slave --no-save")

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("C" "Resources" "python"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(org-reload)

(setq org-agenda-files '(
  "~/org/inbox.org"
  "~/org/gtd.org"
))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq calendar-week-start-day 0)

(with-eval-after-load 'org
  (bind-key "C-c a" #'org-agenda org-mode-map)
  (bind-key "C-c c" #'org-capture ))

(setq org-todo-keywords
    '((sequence "TODO(t)" "WAITING(w)" "NEXT(n)" "|" "DONE(d!)"))
)

(setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 1)
                           ("~/org/time.org" :level . 1)
))

(setq org-capture-templates
`(("t" "Todo [inbox]" entry
  (file+headline "~/org/inbox.org" "Inbox")
       "* TODO %i%?" :empty-lines 1))
)

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(require 'org-clock)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(add-to-list 'org-modules 'org-habit)
(require 'org-habit)
(setq org-habit-following-days 2)
(setq org-habit-preceding-days 7)

(use-package org-journal
  :bind (("C-c j" . org-journal-mode)
  )
  :custom
  (org-journal-dir "~/org/journal/")
  (org-journal-file-format "%Y%m%d")
  (org-journal-date-format "%e %b %Y (%A)")
  (org-journal-time-format "")
)

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates '(
     ("d" "default" plain
        "%?"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
        :unnarrowed t)
     ("c" "concept" plain
        "\n* ${title}\n**%?"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: %^{tags}\n")
        :unnarrowed t)
  ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
        )
  :bind-keymap
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)
)

(use-package magit
  :bind (("C-M-g" . magit-status)))

(use-package helm
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-split-window-inside-p t
    helm-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  )
