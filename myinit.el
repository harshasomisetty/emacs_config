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

(set-exec-path-from-shell-PATH)  

(set-register ?i (cons 'file user-init-file))
(set-register ?l (cons 'file (concat default-directory "learning.org")))

(menu-bar-mode -1)
 (tool-bar-mode -1)
 (toggle-scroll-bar -1)
 (blink-cursor-mode -1)
 (show-paren-mode 1)
 (fset 'yes-or-no-p 'y-or-n-p)
 (global-display-line-numbers-mode)
 (setq display-line-numbers 'relative
       line-number-mode t)

 ; padding
 (setq header-line-format " "
       left-margin-width 1
       right-margin-width 1)

; window settings
(window-divider-mode)
(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode +1))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-env-version t
        doom-modeline-enable-word-count t
        doom-modeline-buffer-encoding nil
         ))

(setq frame-resize-pixelwise t
      l (display-monitor-attributes-list)
      max-frame-width (nth 3 (nth 0 (nth 0 l)))
      max-frame-height (nth 4 (nth 0 (nth 0 l))))

(defun left-two-thirds ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) (* 2 (/ max-frame-width 3)) max-frame-height t))

(defun left-one-thirds ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) (* 1 (/ max-frame-width 3)) max-frame-height t))

(defun right-two-thirds ()
  (interactive)
  (set-frame-position (selected-frame) (- (/ max-frame-width 3) 10) 0)
  (set-frame-size (selected-frame) (* 2 (/ max-frame-width 3)) max-frame-height t))

(defun right-one-thirds ()
  (interactive)
  (set-frame-position (selected-frame) (* 2 (/ max-frame-width 3)) 0)
  (set-frame-size (selected-frame) (* 1 (/ max-frame-width 3)) max-frame-height t))

(defun center-third ()
  (interactive)
  (set-frame-position (selected-frame) (/ max-frame-width 3) 0)
  (set-frame-size (selected-frame) (* 1 (/ max-frame-width 3)) max-frame-height t))

(defun left-half ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) (* 1 (/ max-frame-width 2)) max-frame-height t))

(defun right-half ()
  (interactive)
  (set-frame-position (selected-frame) (/ max-frame-width 2) 0)
  (set-frame-size (selected-frame) (* 1 (/ max-frame-width 2)) max-frame-height t))

(defun full-screen ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) (* 1 (/ max-frame-width 1)) max-frame-height t))

(global-set-key (kbd "C-c w e") 'left-two-thirds)
(global-set-key (kbd "C-c w d") 'left-one-thirds)
(global-set-key (kbd "C-c w t") 'right-two-thirds)
(global-set-key (kbd "C-c w g") 'right-one-thirds)
(global-set-key (kbd "C-c w <left>") 'left-half)
(global-set-key (kbd "C-c w <right>") 'right-half)
(global-set-key (kbd "C-c w f") 'center-third)
(global-set-key (kbd "C-c w <return>") 'full-screen)

(use-package transpose-frame
  :bind ("C-x 4 4" . transpose-frame))

(setq inhibit-startup-screen t)

      (load-theme 'doom-horizon t)
      (defun scratch-setup ()
        (load "~/.emacs.d/.quotes.el")
        (setq initial-scratch-message
              (concat (nth (random (length quotes)) quotes)
                      "\n\n\n")))
      (scratch-setup)

      (defun files-startup-screen (file2 &rest files)
        "choose 2 files to display on startup, file2 goes on left, file1 goes on right"  

        (dotimes (n (length files))
          (setq index (- (- (length files) n) 1))
          (switch-to-buffer (find-file (nth index files)))
          (split-window-right))

        (switch-to-buffer (find-file file2 )))

      (defun agenda-startup-screen ()
        "Display the weekly org-agenda and all todos."
        (org-agenda nil "a")
        (delete-other-windows)
     ;   (split-window-right)
      ;  (switch-to-buffer-other-window "*scratch*")
        )


      (defun emacs-startup-screen ()


                                              ;    (files-startup-screen "~/org/literature/DOE.org" "~/.emacs.d/myinit.org")
        (files-startup-screen "~/code/twit_blog/downloadData.py")
  ;        (files-startup-screen "~/code/exercises/README.md")        
        ;(files-startup-screen "~/org/sem/OS/hw2/benchmarks/test.c"  "~/org/sem/OS/hw2/mypthread.c" "~/org/sem/OS/hw2/mypthread.h")
;       (agenda-startup-screen)
        (right-two-thirds)
        (balance-windows)
        )
      (add-hook 'emacs-startup-hook #'emacs-startup-screen)

(use-package avy
    :bind ("C-;" . avy-goto-word-1))

  (use-package ace-window
    :bind ("C-x o" . ace-window)
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (setq aw-scope 'frame)
    )

  (use-package disable-mouse
    :config
    (global-disable-mouse-mode))

  (use-package no-spam
    :disabled
    :config
    (no-spam-add-repeat-delay next-line 10
                              previous-line 10
                              forward-char 10
                              backward-char 10)
    (no-spam-mode))

  (defun my-split-vertical ()
    (interactive)
    (split-window-vertically)
    (other-window 1))

(defun my-split-horizontal ()
    (interactive)
    (split-window-horizontally)
    (other-window 1))

(global-set-key (kbd "C-x 2") 'my-split-vertical)
(global-set-key (kbd "C-x 3") 'my-split-horizontal)

(use-package dired
  :straight nil
  :ensure nil
  :config
  (setq insert-directory-program "gls" dired-use-ls-dired t
        dired-listing-switches "-agho --group-directories-first"))

(use-package dired-plus
  :disabled
  :straight
  (:host github :repo "emacsmirror/dired-plus" :branch "main" :files ("*.el"))
  :custom
  (diredp-toggle-find-file-reuse-dir t))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-quick-sort
  :config
  (dired-quick-sort-setup))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config (define-key dired-mode-map "." #'dired-hide-dotfiles-mode))

(use-package deft
  :demand t
  :bind
  ("C-x C-g" . deft-find-file)
  :config
  (setq deft-extensions '("org")
        deft-directory "~/org"
        deft-recursive t
        deft-use-filename-as-title t)
  (global-set-key (kbd "C-x C-g") 'deft-find-file)
  (defcustom deft-ignore-file-regexp
    (concat "\\(?:"
            "Fall19"
            "\\)")
    "Regular expression for files to be ignored."
    :type 'regexp
    :safe 'stringp
    :group 'deft))

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
  :disabled t
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2)
  :hook (after-init . global-company-mode))

(use-package ispell)

(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)


(add-hook 'after-init-hook #'global-flycheck-mode)
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_US")

(use-package pdf-tools
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :config
  (setq pdf-view-display-size 'fit-page)
  :hook ((pdf-view-mode . pdf-view-midnight-minor-mode)))
(pdf-tools-install)

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

(setq-default c-basic-offset 4)
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
(define-key c-mode-map (kbd "C-c C-/") 'uncomment-region)
   (use-package clang-format)

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
        ("C-c r" . R)
        :map
        inferior-ess-r-mode-map
        ("M--" . ess-insert-assign)
        ("C-S-m" . pipe_R_operator))
  )

(use-package elpy
    :init
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
    :bind (:map elpy-mode-map

                ("<M-left>" . nil)
                ("<M-right>" . nil)
                ("<M-S-left>" . elpy-nav-indent-shift-left)
                ("<M-S-right>" . elpy-nav-indent-shift-right)
                ("M-." . elpy-goto-definition)
                ("M-," . pop-tag-mark))
    :config
    (setq elpy-rpc-virtualenv-path 'current)
    (add-hook 'elpy-mode-hook (lambda ()
                          (add-hook 'before-save-hook
                                    'elpy-format-code nil t)))
    )

  (use-package python
    :mode ("\\.py" . python-mode)
    :config
    (setq python-indent-offset 4
          python-indent-guess-indent-offset nil
          python-shell-completion-native-enable nil)
    (elpy-enable))   

  (use-package pyenv-mode
    :disabled
    :init
    (add-to-list 'exec-path "~/.pyenv/shims")
    (setenv "WORKON_HOME" "~/.pyenv/versions/")
    :config
    (pyenv-mode)
    (setq pyenv-mode-map
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-m") 'pyenv-mode-set)
            (define-key map (kbd "C-c C-u") 'pyenv-mode-unset)
            map))
    :bind
    ("C-x p e" . pyenv-activate-current-project))

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (pyenv-mode-set pyenv-current-version)
          (message (concat "Setting virtualenv to " pyenv-current-version))))))

  (defvar pyenv-current-version nil nil)

(defun pyenv-init()
  "Initialize pyenv's current version to the global one."
  (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
    (message (concat "Setting pyenv version to " global-pyenv))
    (pyenv-mode-set global-pyenv)
    (setq pyenv-current-version global-pyenv)))

(add-hook 'after-init-hook 'pyenv-init)

(use-package js2-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    :hook
    (js2-mode . js2-imenu-extras-mode))

  (use-package js2-refactor
    :init
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    :config
    (js2r-add-keybindings-with-prefix "C-c C-r")


    ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
    ;; unbind it.
    (define-key js-mode-map (kbd "M-.") nil)

    (add-hook 'js2-mode-hook (lambda ()
       (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

  (use-package xref-js2)

  ; autoformatting
  (use-package prettier-js
    :init
    (add-hook 'js2-mode-hook 'prettier-js-mode))

;      (add-hook 'elpy-mode-hook (lambda ()
 ;                           (add-hook 'before-save-hook
  ;                                    'elpy-format-code nil t)))

(use-package tide
  :after (typescript-mode company flycheck)
  :config
  (setq company-tooltip-align-annotations t)
  :hook
  ((typescript-mode . tide-setup)
       (typescript-mode . tide-hl-identifier-mode)
       (before-save . tide-format-before-save)))

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package web-mode
  :mode
  (
   ".twig$"
   ".html?$"
   ".hbs$"
   ".vue$"
   ".blade.php$"
   )
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 2
   web-mode-script-padding 2
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-indentation t)

  ;; Let smartparens handle auto closing brackets, e.g. {{ }} or {% %}
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/web/%2Bhtml.el#L56
  (dolist (alist web-mode-engines-auto-pairs)
    (setcdr alist
            (cl-loop for pair in (cdr alist)
                     unless (string-match-p "^[a-z-]" (cdr pair))
                     collect (cons (car pair)
                                   (string-trim-right (cdr pair)
                                                      "\\(?:>\\|]\\|}\\)+\\'")))))
  )

;; (defun gf/web-mode-toggle-markup-offset ()
;;   "Switch between 2 and 4 spaces for markup indentation"
;;   (interactive)
;;   (if (eq web-mode-markup-indent-offset 2)
;;       (setq web-mode-markup-indent-offset 4)
;;     (setq web-mode-markup-indent-offset 2))
;;   (message (format "Set markup indendation to %s spaces" web-mode-markup-indent-offset))
;;   (web-mode))

(defun gf/toggle-php-web-mode ()
  "Switch between php-mode and web-mode for the current buffer."
  (interactive)
  (if (equal (symbol-name (buffer-local-value 'major-mode (current-buffer))) "web-mode")
      (php-mode)
    (web-mode)))

(defun gf/web-maybe-activate-lsp ()
  "Maybe activate language server protocol for the current buffer."
  (if (equal (gf/filename-extension (buffer-file-name)) "vue")
      (lsp-vue-mmm-enable)))

;; (when (not (version< emacs-version "25.1"))
;;   (use-package lsp-vue
;;     :after web-mode
;;     :config
;;         (add-hook 'web-mode-hook #'gf/web-maybe-activate-lsp)))

(provide 'setup-web-mode)

(use-package emmet-mode
:commands emmet-mode
:hook
(web-mode html-mode))

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(put 'temporary-file-directory 'standard-value
     (list temporary-file-directory))

(set-register ?s (cons 'file "/ssh:hs884@ilab1.cs.rutgers.edu:"))

(add-hook
   'c-mode-hook
   (lambda () (when (file-remote-p default-directory) (company-mode -1))))

(use-package term
    :config
    (setq explicit-shell-file-name "zsh"
          term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

  (use-package bash-completion
    :config
    (bash-completion-setup))

  (use-package shell-pop
    :init
    (setq shell-pop-universal-key "C-t"
          shell-pop-window-position "bottom"
;          shell-pop-shell-type "terminal"
          shell-pop-cleanup-buffer-at-process-exit t
          shell-pop-window-size 30)
    (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
    )

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
        helm-autoresize-mode 1)
  (bind-keys ("C-x C-f" . helm-find-files)))

(use-package projectile
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  (with-eval-after-load 'magit-mode
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t)))

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
      org-hide-leading-stars t
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-src-fontify-natively t
      org-fontify-quote-and-verse-blocks t)

(use-package valign
  :config
   (setq valign-fancy-bar t)
  :hook ((org-mode) . valign-mode))

(use-package org-visual-outline
  :disabled t
  :config
  (org-dynamic-bullets-mode)
  (org-visual-indent-mode))

(defun col-strip (col-str)
  (butlast (split-string (mapconcat (lambda (x) (concat "#" x " "))
                                    (split-string col-str "-")
                                    "") " ")))

(setq color-schemes (list
                     (col-strip "a21d1d-5497de-8e35b7-ffff5b-56cb7d-df5252-707efa") ; red blue purple study
                     (col-strip "2278bf-e15554-3bb273-507c6d-6e5775-598d91-7768ae") ; blue red green okay
                     ))
(setq pick-color 0)

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
     (set-face-attribute face nil :height 0.85))
   (list 'org-document-info-keyword
         'org-block-begin-line
         'org-block-end-line
         'org-meta-line
         'org-drawer
         'org-property-value
         'minibuffer-prompt
         'mode-line
         'mode-line-inactive
         ))
    (setq color-theme (nth pick-color color-schemes))
  (set-face-attribute 'org-code nil
                      :inherit '(shadow fixed-pitch)
                      :height .8)
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
                      :height 1.3
                      :foreground (nth 0 color-theme))
  (set-face-attribute 'org-level-2 nil
                      :height 1.2
                      :foreground (nth 1 color-theme))
  (set-face-attribute 'org-level-3 nil
                      :height 1.1
                      :foreground (nth 2 color-theme))
  (set-face-attribute 'org-level-4 nil
                      :height 1.05
                      :foreground (nth 3 color-theme))
  (set-face-attribute 'org-level-5 nil
                      :foreground (nth 4 color-theme))
  (set-face-attribute 'org-level-6 nil
                      :foreground (nth 5 color-theme))
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
  (setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))

  (use-package tex
    :straight auctex
    :defer t
    :config
    (setq TeX-auto-save t)
    (setq TeX-parse-self t))

  (use-package cdlatex
    :requires texmathp
    :config
;    (setq cdlatex-paired-parens "")
       )
(add-hook 'org-mode-hook #'turn-on-org-cdlatex)

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

(use-package org-agenda
  :straight nil :ensure nil
  :config
  (setq org-agenda-start-with-log-mode t
        org-log-done 'time
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-log-into-drawer t
        org-agenda-span 4
        org-agenda-start-day "+0d"
        org-archive-location "~/.emacs.d/archive.org::"
        org-agenda-files '(
                           "~/org/inbox.org"
                           "~/org/gtd.org"
                           "~/org/habits.org"
                           )
        org-agenda-prefix-format '(
                                        ;                                     (agenda . " %-12b %?-15t% s")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                        ;                                     (search . " %i %-12:c")
                                   )
        org-todo-keywords '((sequence "TODO(t)"  "NEXT(n)" "|" "DONE(d)" "FAILED(f)"))
        org-refile-targets '(("~/org/gtd.org" :maxlevel . 1)
                             ("~/org/time.org" :level . 1)
                             )
        org-capture-templates
        `(("t" "Todo [inbox]" entry (file+headline "~/org/inbox.org" "Inbox") "* TODO %i%?" :empty-lines 1))
        )

  (org-agenda-align-tags)
  )
(set-register ?g (cons 'file  "~/org/gtd.org"))
(use-package dash)
(use-package ht)
(use-package s)
(use-package ts)

(use-package org-super-agenda
  :config
  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          (:name "Today"  ; Optionally specify section name
                 :time-grid t  ; Items that appear on the time grid
                 :priority "A"
                 )
          (:order-multi ( (:name "DOE"
                                  :tag "DOE")
                           (:name "CStats"
                                  :tag "CStats")
                           (:name "MStats"
                                  :tag "MStats")
                           (:name "Networking"
                                  :tag "Networking")
                           (:name "OS"
                                  :tag "OS")))
          (:name "Habits"
                 :habit t
                 :tag "Habits")
          (:name "Projects"
                 :tag "Projects")
          )
        )
  (org-super-agenda-mode)
  )
(with-eval-after-load 'org
  (bind-key "C-c a" #'org-agenda global-map)
  (bind-key "C-c c" #'org-capture ))

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
        (move-to-column (+ org-habit-graph-column org-habit-preceding-days org-habit-following-days 1))
        (insert (number-to-string streak))))
    (forward-line 1)))

(add-hook 'org-agenda-finalize-hook 'org-habit-streak-count)

(setq org-clock-into-drawer t
      org-clock-idle-time 5
      org-time-stamp-rounding-minutes (quote (0 5))
      org-clock-history-length 23
      org-clock-persist t
      org-clock-in-resume t
      org-clock-persist-query-resume nil)

(use-package org-journal
  :bind (("C-c j" . org-journal-new-entry)  
         )
  :custom
  (org-journal-dir "~/org/journal/")
  (org-journal-file-format "%Y%m%d")
  (org-journal-date-format "%e %b %Y (%A)")
  (setq org-journal-date-prefix "")
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
  (insert (concat
           "\n* Gratitude\n"
             )))

(add-hook 'org-journal-after-header-create-hook
          #'insert-created-date)


(add-hook 'org-journal-after-entry-create-hook
          'beginning-of-line
          'kill-line
          'end-of-buffer)

(add-to-list 'org-capture-templates
             '("j" "Journal entry" plain (function org-journal-find-location)
               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
               :jump-to-captured t :immediate-finish t))

(use-package org-roam
      :init
      (setq org-roam-v2-ack t) ; stops warning message
      :demand t
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

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
  (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (format "(%s)" (string-join (f-split dirs) "/"))
    ""))

  (setq org-roam-node-display-template "${directories:10} ${title:100} ${tags:10} ${backlinkscount:6}")
  (set-register ?n (cons 'file "~/org/roam/roam_directory.org"))

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
  :bind ("C-c o" . org-noter)
  :config
  (setq org-noter-default-notes-file-name '("notes.org")
        org-noter-notes-search-path '("~/org")
        org-noter-notes-window-location "Horizontal"
        org-noter-separate-notes-from-heading t))

(defun my/no-op (&rest args))
(advice-add 'org-noter--set-notes-scroll :override 'my/no-op)

(defun grind()
  (interactive)
  (setq pick-color 1)
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? "))
                     " | sudo -S ~/bin/grind"))
  (load-theme 'doom-acario-dark  t)
  (my/style-org))

(global-set-key (kbd "C-c g") #'grind)

(defun ungrind()
  (interactive)
  (setq pick-color 0)
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? "))
                         " | sudo -S ~/bin/ungrind"))
  (load-theme 'doom-horizon t)
  (my/style-org))
  (global-set-key (kbd "C-c u") #'ungrind)
