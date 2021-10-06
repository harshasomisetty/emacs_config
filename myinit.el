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

(set-exec-path-from-shell-PATH)

;  backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")) ; ignore files wtih ~
  user-init-file "~/.emacs.d/myinit.org"
  default-directory "~/org/"
  shell-file-name "/bin/bash"
  auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
)

(setq backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backup
  delete-old-versions t  ; Automatically delete excess backup
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  ring-bell-function 'ignore)


(use-package deft
  :bind ("C-x C-g" . deft-find-file)
  :config
  (setq deft-extensions '("org")
        deft-directory "~/org"
        deft-recursive t
        deft-use-filename-as-title t))

(set-register ?i (cons 'file user-init-file))
(set-register ?l (cons 'file (concat default-directory "learning.org")))

(setq inhibit-startup-screen t
        initial-scratch-message "Hello Harsha! Enjoy the Grind")

  (defun display-startup-echo-area-message ()
    (message "Start Grind"))

  ; (setq initial-frame-alist '((top . 0) (left . 1060) (width . 302) (height . 105)))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
;  (setq initial-buffer-choice "~/org/literature/osnotes.org")
                                          ;  (split-window-right)
                                          ; (find-file "~/.emacs.d/myinit.org")
                                          ;(switch-to-buffer-other-window "myinit.org")
                                          ; (let ((org-agenda-window-setup)) (org-agenda nil "a"))

(use-package avy
  :bind ("C-;" . avy-goto-word-1))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame)
)

(use-package disable-mouse)
(global-disable-mouse-mode)

(require 'org-tempo)

;; Move cursor to end of line, new line and indent

(global-set-key (kbd "<C-return>") (lambda ()
                                     (interactive)
                                     (end-of-line)
                                     (newline-and-indent)))



;; Move cursor to previous line, new line, indent
(global-set-key (kbd "<C-S-return>") (lambda ()
                                       (interactive)
                                       (previous-line)
                                       (end-of-line)
                                       (newline-and-indent)
                                       ))



(require 'subr-x)
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

 ; theme
 (load-theme 'doom-acario-dark t)

 ; padding
 (setq header-line-format " ")
; (setq left-margin-width 2)
 (setq right-margin-width 2)


; window settings
(window-divider-mode)
(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode +1))

(setq org-src-window-setup 'current-window)
; (use-package disable-mouse)
 ; (global-disable-mouse-mode)

(use-package spaceline-config
  :straight (spaceline :host github :repo "TheBB/spaceline" :branch "master")
  :config
  (setq spaceline-workspace-numbers-unicode t)
  (spaceline-toggle-major-mode-on)
  (spaceline-toggle-column-on)
  (spaceline-emacs-theme))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
  (emacs-lisp . t)
  (python . t)
  (C . t)
  (R . t)
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
 (define-key c-mode-map (kbd "C-c g") #'gdb)

(use-package ess-site
  :straight ess
  :config
  ;; Execute screen options after initialize process
  (add-hook 'ess-post-run-hook 'ess-execute-screen-options)

  (setq ess-use-ido nil ; use helm
        ess-eval-visibly 'nowait ; don't hang with R
        ess-smart-S-assign-key nil ; unbind ess-insert-align
        ) ; use helm
  )


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

  (setq elpy-rpc-python-command "python3"
        elpy-rpc-backend "rope" ; completion backend
  )
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



(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 1)
(setq tramp-verbose 6)
(put 'temporary-file-directory 'standard-value
     (list temporary-file-directory))

(set-register ?s (cons 'file "/ssh:hs884@ilab1.cs.rutgers.edu:"))

(add-hook
   'c-mode-hook
   (lambda () (when (file-remote-p default-directory) (company-mode -1))))

(use-package helm
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  (:map helm-command-map
        ("<tab>" . helm-execute-persistent-action)
        ("C-i" . helm-execite-persistent-action)
        ("C-z" . helm-select-action))
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20
        helm-autoresize-mode 1))

(use-package magit)

(use-package smudge)

(use-package org)
(use-package org-contrib)
(defun org-clocking-buffer (&rest _))


(org-reload)

(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
(setq-default indent-tabs-mode nil)


(use-package org-bullets
  :hook ((org-mode) . org-bullets-mode))

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-startup-indented t
      org-ellipsis " ▼ " ;; folding symbol
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-src-fontify-natively t
      org-fontify-quote-and-verse-blocks t)

(use-package valign
  :config
   (setq valign-fancy-bar t)
  :hook ((org-mode) . valign-mode)
  )

(defun my/buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Cochin"
                                        :height 150
                                        :width normal))
  (buffer-face-mode))

(defun my/style-org ()
  ;; I have removed indentation to make the file look cleaner
  (my/buffer-face-mode-variable)
  (setq line-spacing 0.05)

  (variable-pitch-mode +1)
  (mapc
   (lambda (face) ;; Other fonts that require it are set to fixed-pitch.
     (set-face-attribute face nil :inherit 'fixed-pitch))
   (list 'org-block
         'org-table
         'org-verbatim
         'org-block-begin-line
         'org-block-end-line
         'org-meta-line
         'org-date
         'org-drawer
         'org-property-value
         'org-special-keyword
         'org-document-info-keyword))
  (mapc ;; This sets the fonts to a smaller size
   (lambda (face)
     (set-face-attribute face nil :height 0.8))
   (list 'org-document-info-keyword
         'org-block-begin-line
         'org-block-end-line
         'org-meta-line
         'org-drawer
         'org-property-value
         'minibuffer-prompt
         ))

  (set-face-attribute 'org-code nil
                      :inherit '(shadow fixed-pitch))
  (set-face-attribute 'default nil
                      :height 150
                      :foreground "gray70")
  (set-face-attribute 'variable-pitch nil
                      :family "Cochin"
                      :height 1.2)
  (set-face-attribute 'fixed-pitch nil
                      :height 1
                      :family "PT Mono")
  (set-face-attribute 'org-level-1 nil
                      :height 1.25
                      :foreground "#6C88C4")
  (set-face-attribute 'org-level-2 nil
                      :height 1.15
                      :foreground "#00B0BA")
  (set-face-attribute 'org-level-3 nil
                      :height 1.1
                      :foreground "#E7C582")
  (set-face-attribute 'org-level-4 nil
                      :height 1.05
                      :foreground "#FF828B")
  (set-face-attribute 'org-level-5 nil
                      :foreground "#C05780")
  (set-face-attribute 'org-date nil
                      :foreground "#ECBE7B"
                      :height 0.8)
  (set-face-attribute 'org-document-title nil
                      :foreground "DarkOrange3"
                      :height 1.3)
  (set-face-attribute 'org-ellipsis nil
                      :foreground "#3256A8" :underline nil)

  )

(add-hook 'org-mode-hook 'my/style-org)
(add-hook 'org-mode-hook 'visual-line-mode) ; make lines go to full screen
(add-hook 'org-mode-hook 'variable-pitch-mode) ; auto enable variable ptich for new buffers

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autosubmarkers t
        org-appear-autolinks t
        org-appear-autoentities t
        org-appear-delay .1
        org-appear-autokeywords t))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
(setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))

(use-package tex
   :straight auctex
   :defer t
   :config
   (setq TeX-auto-save t)
   (setq TeX-parse-self t))

(use-package cdlatex
  :requires texmathp
  :hook (org-mode . turn-on-org-cdlatex))

(use-package org-download
  :ensure t
  :hook (dired-mode . org-download-enable)
  :config
  ;; add support to dired
  (setq-default org-download-image-dir "~/Pictures/emacs-pics")
  )


(defun ros ()
  (interactive)
  (if buffer-file-name
      (progn
        (message "Waiting for region selection with mouse...")
        (let ((filename
               (concat "./"
                       (file-name-nondirectory buffer-file-name)
                       "_"
                       (format-time-string "%Y%m%d_%H%M%S")
                       ".png")))
          (if (executable-find "scrot")
              (call-process "scrot" nil nil nil "-s" filename)
            (call-process "screencapture" nil nil nil "-s" filename))
          (insert (concat "[[" filename "]]"))
          (org-display-inline-images t t)
          )
        (message "File created and linked...")
        )
    (message "You're in a not saved buffer! Save it first!")
    )
  )

(global-set-key (kbd "C-c r") #'ros)

(setq org-agenda-files '(
                         "~/org/inbox.org"
                         "~/org/gtd.org"
                         ))

(set-register ?g (cons 'file (concat default-directory "gtd.org")))

(setq org-agenda-start-with-log-mode t
      org-log-done 'time
      org-log-into-drawer t
      calendar-week-start-day 0)

(setq org-agenda-prefix-format
      '(
        (agenda . " %-12b %?-15t% s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")
        )
      )

(with-eval-after-load 'org
  (bind-key "C-c a" #'org-agenda global-map)
  (bind-key "C-c c" #'org-capture ))

(setq org-todo-keywords
      '((sequence "TODO(t)"  "NEXT(n)" "|" "DONE(d)" "FAILED(f)"))
      )

(setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 1)
                           ("~/org/time.org" :level . 1)
                           ))


(defun gtd_settings ()
  (interactive)
  (find-file "~/org/gtd.org")
  )
(global-set-key (kbd "C-c g") #'gtd_settings)
;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-archive-location "~/.emacs.d/archive.org::")

(require 'org-clock)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(add-to-list 'org-modules 'org-habit)
(require 'org-habit)
(setq org-habit-following-days 1
      org-habit-preceding-days 14
      org-habit-show-habits-only-for-today t
      org-habit-graph-column 35)


(defun org-habit-streak-count ()
  (goto-char (point-min))
  (while (not (eobp))
    ;;on habit line?
    (when (get-text-property (point) 'org-habit-p)
      (let ((streak 0)
            (counter (+ org-habit-graph-column (- org-habit-preceding-days org-habit-following-days)))
            )
        (move-to-column counter)
        ;;until end of line
        (while (= (char-after (point)) org-habit-completed-glyph)
          (setq streak (+ streak 1))
          (setq counter (- counter 1))
          (backward-char 1))
        (end-of-line)
        (insert (number-to-string streak))))
    (forward-line 1)))

(add-hook 'org-agenda-finalize-hook 'org-habit-streak-count)

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

  (defun insert-created-date (&rest ignore)
    (insert (format-time-string
             (concat
              "Goals\n"
              "** Accomplishments\n"
              "** Moments\n"
              ))))

  (defvar org-journal--date-location-scheduled-time nil)

(defun org-journal-date-location (&optional scheduled-time)
  (let ((scheduled-time (or scheduled-time (org-read-date nil nil nil "Date:"))))
    (setq org-journal--date-location-scheduled-time scheduled-time)
    (org-journal-new-entry t (org-time-string-to-time scheduled-time))
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max))))

  (add-hook 'org-journal-after-entry-create-hook
            #'insert-created-date)

(setq org-capture-templates
       `(
         ("t" "Todo [inbox]" entry (file+headline "~/org/inbox.org" "Inbox") "* TODO %i%?" :empty-lines 1)
         ("j" "Journal entry" plain (function org-journal-find-location) "*** %^{Moment}\n%?"
:jump-to-captured t :immediate-finish t)
         ("f" "Future Journal entry" plain (function org-journal-date-location)  "%?\nn" :jump-to-captured t)

         )
       )

(use-package org-roam
    :init
    (setq org-roam-v2-ack t) ; stops warning message
    :custom
    (org-roam-directory "~/org/roam/")
    (org-roam-completion-everywhere t)
    (org-roam-capture-templates '(
                                  ("d" "default" plain
                                   "\n\n* %?"
                                   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+filetags: %^{tags}\n#+title: ${title}\n")
                                   :unnarrowed t)
                                  ("t" "Term/Definition" plain
                                   "\n\n* Definition\n** %?\n* Understanding\n** \n* Prerequisites\n* References\n"
                                   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+filetags: %^{tags}\n#+title: ${title}\n")
                                   :unnarrowed t)
  
                                  ))
    :config
    (org-roam-setup)
    (org-roam-db-autosync-mode)
    :bind (("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n r" . org-roam-node-random)		    
           (:map org-mode-map
                 (("C-c n i" . org-roam-node-insert)
                  ("C-c n o" . org-id-get-create)
                  ("C-c n t" . org-roam-tag-add)
                  ("C-c n a" . org-roam-alias-add)
                  ("C-M-i" . completion-at-point)
                  ("C-c n l" . org-roam-buffer-toggle)
                  ("C-c n I" . org-roam-node-insert-immediate)))))
  (require 'org-roam)
  (cl-defmethod org-roam-node-directories ((node org-roam-node))
  (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (format "(%s)" (car (f-split dirs)))
    ""))

(cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
  (let* ((count (caar (org-roam-db-query
                       [:select (funcall count source)
                                :from links
                                :where (= dest $s1)
                                :and (= type "id")]
                       (org-roam-node-id node)))))
    (format "[%d]" count)))

(setq org-roam-node-display-template "${directories:10} ${tags:10} ${title:100} ${backlinkscount:6}")

(use-package org-roam-ui
:straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-noter
  :config
  (setq org-noter-default-notes-file-name '("notes.org")
        org-noter-notes-search-path '("~/org")
        org-noter-separate-notes-from-heading t))
