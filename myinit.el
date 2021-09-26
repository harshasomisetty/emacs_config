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

(require 'org-tempo)
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

(require 'subr-x)

(use-package org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

    (use-package avy)
    (global-set-key (kbd "M-g w") 'avy-goto-word-1)

    (use-package yasnippet
      :config
      (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
     (yas-global-mode 1)
    )


   (use-package company
    :ensure t
    :config
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)
    (global-company-mode t)
  )

(use-package ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-scope 'frame)

; reduce visual clutter
 (menu-bar-mode -1)
 (tool-bar-mode -1)
 (toggle-scroll-bar -1)
 (blink-cursor-mode -1)
 (show-paren-mode 1)
 (fset 'yes-or-no-p 'y-or-n-p)
 (global-display-line-numbers-mode)
 (setq display-line-numbers 'relative)
 (setq line-number-mode t)

 ; clean whitespaces
 ; (add-hook 'before-save-hook 'whitespace-cleanup)

 ; theme
 (load-theme 'doom-acario-dark t)

 ; padding
 (setq header-line-format " ")
; (setq left-margin-width 2)
 (setq right-margin-width 2)

; (use-package disable-mouse)
 ; (global-disable-mouse-mode)

(use-package spaceline-config
:straight (spaceline :host github :repo "TheBB/spaceline" :branch "master")
:config
(setq spaceline-workspace-numbers-unicode t)
(spaceline-toggle-major-mode-on)
(spaceline-toggle-column-on)
(spaceline-emacs-theme)
(spaceline-helm-mode 1))

; customized startup screen

(setq inhibit-startup-screen t)
(setq initial-frame-alist '((top . 0) (left . 1060) (width . 302) (height . 105)))
; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq initial-buffer-choice "~/org/school/os/hw1/sigHandler.c")
  ; (split-window-right)
  ; (find-file "~/org/literature/DOE.org")
  ; (switch-to-buffer-other-window "DOE.org")
  ; (let ((org-agenda-window-setup)) (org-agenda nil "a"))

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 1)
(setq tramp-verbose 6)
(put 'temporary-file-directory 'standard-value
     (list temporary-file-directory))

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
  (not (member lang '("C" "R" "python" "emacs-lisp"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(define-key c-mode-map (kbd "C-c m") #'compile)

    (defun execute-c-program ()
      (interactive)
      (save-buffer)
      (defvar foo)
      (setq foo (concat "./" (substring  (buffer-name) 0 (- (length (buffer-name)) 2)) ))
      (shell)
      (kill-new foo)
      (org-yank)
    )

  (define-key c-mode-map (kbd "C-c r") 'execute-c-program)

(use-package ess-site
  :straight ess
  :config
  ;; Execute screen options after initialize process
  (add-hook 'ess-post-run-hook 'ess-execute-screen-options)

  ;; Disable IDO so helm is used instead
  (setq ess-use-ido nil)

  ;; We don’t want R evaluation to hang the editor, hence
  (setq ess-eval-visibly 'nowait)

  ;; Unbind ess-insert-assign (defaut value is "_")
  (setq ess-smart-S-assign-key nil))
(setq inferior-R-program-name "/Library/Frameworks/R.framework/Resources/R")

(use-package ess-r-mode
  :straight ess
  :config
  ;; Hot key C-S-m for pipe operator in ESS
  (defun pipe_R_operator ()
    "R - %>% operator or 'then' pipe operator"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (just-one-space 1))

  ;; ESS syntax highlight
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:constants . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)
          (ess-R-fl-keyword:%op% . t)))

  (setq inferior-ess-r-font-lock-keywords
        '((ess-S-fl-keyword:prompt . t)
          (ess-R-fl-keyword:messages . t)
          (ess-R-fl-keyword:modifiers . nil)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . nil)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls . nil)
          (ess-fl-keyword:numbers . nil)
          (ess-fl-keyword:operators . nil)
          (ess-fl-keyword:delimiters . nil)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . nil)))

  :bind
  (:map ess-r-mode-map
   ("M--" . ess-insert-assign)
   ("C-S-m" . pipe_R_operator)
   :map
   inferior-ess-r-mode-map
   ("M--" . ess-insert-assign)
   ("C-S-m" . pipe_R_operator))
  )

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "python3"))

(use-package elpy
  :after python
  :init
  ;; Truncate long line in inferior mode
  (add-hook 'inferior-python-mode-hook (lambda () (setq truncate-lines t)))
  ;; Enable company
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'inferior-python-mode-hook 'company-mode)
  ;; Enable highlight indentation
  (add-hook 'highlight-indentation-mode-hook
            'highlight-indentation-current-column-mode)
  ;; Enable elpy
  (elpy-enable)
  :config
  ;; Do not enable elpy flymake for now
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

  ;; The old `elpy-use-ipython' is obseleted, see:
  ;; https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup
  ;; (setq python-shell-interpreter "ipython3"
  ;; python-shell-interpreter-args "-i --simple-prompt")

  (setq elpy-rpc-python-command "python3")

  ;; Completion backend
  (setq elpy-rpc-backend "rope")

  ;; Function: send block to elpy: bound to C-c C-c
  (defun forward-block (&optional n)
    (interactive "p")
    (let ((n (if (null n) 1 n)))
      (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" n)))

  (defun elpy-shell-send-current-block ()
    (interactive)
    (beginning-of-line)
    "Send current block to Python shell."
    (push-mark)
    (forward-block)
    (elpy-shell-send-region-or-buffer)
    (display-buffer (process-buffer (elpy-shell-get-or-create-process))
                    nil
                    'visible))

  ;; Font-lock
  (add-hook 'python-mode-hook
            '(lambda()
               (font-lock-add-keywords
                nil
                '(("\\<\\([_A-Za-z0-9]*\\)(" 1
                   font-lock-function-name-face) ; highlight function names
                  ))))

  :bind (:map python-mode-map
         ("C-c <RET>" . elpy-shell-send-region-or-buffer)
         ("C-c C-c" . elpy-send-current-block)))

(use-package pipenv
  :hook (python-mode . pipenv-mode))

(setq gdb-many-windows t
        gdb-use-separate-io-buffer t)
  
  (advice-add 'gdb-setup-windows :after
            (lambda () (set-window-dedicated-p (selected-window) t)))


    (defconst gud-window-register 123456)
 
(defun gud-quit ()
  (interactive)
  (gud-basic-call "quit"))
 
(add-hook 'gud-mode-hook
          (lambda ()
            (gud-tooltip-mode)
            (window-configuration-to-register gud-window-register)
            (local-set-key (kbd "C-q") 'gud-quit)))
 
(advice-add 'gud-sentinel :after
            (lambda (proc msg)
              (when (memq (process-status proc) '(signal exit))
                (jump-to-register gud-window-register)
                (bury-buffer))))

(use-package org)
(use-package org-contrib)
(defun org-clocking-buffer (&rest _))


(org-reload)

(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

   (use-package org-bullets)
 (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

   (setq org-hide-emphasis-markers t)

(setq org-startup-indented t
      org-ellipsis " ->" ;; folding symbol
      org-pretty-entities t
      org-hide-emphasis-markers t
      ;; show actually italicized text instead of /italicized text/
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

; ; table
(use-package valign)
(setq valign-fancy-bar t)
(add-hook 'org-mode-hook #'valign-mode)

(setq org-src-fontify-natively t)

 (let* ((variable-tuple
          (cond ((x-list-fonts "Cochin")         '(:font "Cochin" :foreground "white"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight normal)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple :height 1))))
   `(org-level-7 ((t (,@headline ,@variable-tuple :height 1))))
   `(org-level-6 ((t (,@headline ,@variable-tuple :height 1))))
   `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.02))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.05))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.17))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.27))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.35))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 1.50 :underline nil))))))

 (custom-theme-set-faces
     'user
     ; '(default ((t (:family "Cochin" :height 140 :weight normal :foreground "gray70"))))
     '(variable-pitch ((t (:family "Cochin" :height 165 :weight normal))))
     '(fixed-pitch ((t (:family "PT Mono" :height 140 :weight thin))))
 )


;line fill
(add-hook 'org-mode-hook 'visual-line-mode) ; make lines go to full screen
(add-hook 'org-mode-hook 'variable-pitch-mode) ; auto enable variable ptich for new buffers

(use-package org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
(setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))
(use-package tex
   :straight auctex
   :defer t
   :config
   (setq TeX-auto-save t)
   (setq TeX-parse-self t))
 (require 'texmathp)
(use-package cdlatex)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

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
      '((sequence "TODO(t)"  "NEXT(n)" "|" "DONE(d!)"))
  )

  (setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 1)
                             ("~/org/time.org" :level . 1)
  ))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-archive-location "~/.emacs.d/archive.org::")

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
  (setq org-journal-find-file 'find-file)
  )

(defun org-journal-find-location ()
;; Open today's journal, but specify a non-nil prefix argument in order to
;; inhibit inserting the heading; org-capture will insert the heading.
(org-journal-new-entry t)
(unless (eq org-journal-file-type 'daily)
  (org-narrow-to-subtree))
(goto-char (point-max)))

(defun org-journal-save-entry-and-exit()
  "Simple convenience function.
    Saves the buffer of the current day's entry and kills the window
    Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(add-hook 'org-journal-mode-hook
          (lambda ()
            (define-key org-journal-mode-map
              (kbd "C-x C-s") 'org-journal-save-entry-and-exit)))

(setq org-capture-templates
      `(
      ("t" "Todo [inbox]" entry (file+headline "~/org/inbox.org" "Inbox") "* TODO %i%?" :empty-lines 1)
      ("j" "Journal entry" plain (function org-journal-find-location) "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?" :jump-to-captured t :immediate-finish t)
      )
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

(use-package magit)


