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
(setq straight-recipe-repositories '(melpa gnu-elpa-mirror el-get emacsmirror-mirror ))

(require 'package)
(straight-use-package '(org :host github :repo "yantar92/org" :branch "feature/org-fold-universal-core"
			    :files (:defaults "contrib/lisp/*.el")))

(setq mac-option-modifier 'meta
      mac-pass-command-to-system nil
      mac-command-modifier 'super)

;;;; metadata

(global-auto-revert-mode 1)


(require 'package)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 1.1))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")) ; ignore files wtih ~
      user-init-file "~/.emacs.d/init.el"
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

(set-register ?i (cons 'file user-init-file))


;;;;; outline mode

(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'outshine-mode)

;; (defun init-startup-hook ()
;;   (when (string= (file-name-nondirectory (buffer-file-name)) "init.el")
;;     (lambda() (outline-hide-sublevels 6))
;;     )
;; )

(add-hook 'outline-minor-mode-hook '(lambda() (outline-hide-sublevels 6)))
;; (add-to-list 'auto-mode-alist '("\\init.el\\'" . outline-minor-mode))
;; (add-to-list 'auto-mode-alist '("\\init.el\\'" . outshine-mode))



                                        ; code from [[http://www.modernemacs.com/post/outline-ivy/][here]]


(use-package outshine
  :init
  (defvar outline-minor-mode-prefix "\C-c"))

(use-package dash-functional)

                                        ; this package doesn't work, gotta figure this out eventually
(use-package pretty-outlines
  :straight
  (:host github :repo "harshasomisetty/pretty-outlines" :branch "main" :files ("*.el"))
  :hook ((outline-mode       . pretty-outlines-set-display-table)
         (outline-minor-mode . pretty-outlines-set-display-table)
         (emacs-lisp-mode . pretty-outlines-add-bullets)
         (python-mode     . pretty-outlines-add-bullets))
  :config
  (setq pretty-outlines-ellipsis " ▼ " ))

(defun -add-font-lock-kwds (FONT-LOCK-ALIST)
  (font-lock-add-keywords
   nil (--map (-let (((rgx uni-point) it))
                `(,rgx (0 (progn
                            (compose-region (match-beginning 1) (match-end 1)
                                            ,(concat "\t" (list uni-point)))
                            nil))))
              FONT-LOCK-ALIST)))

(defmacro add-font-locks (FONT-LOCK-HOOKS-ALIST)
  `(--each ,FONT-LOCK-HOOKS-ALIST
     (-let (((font-locks . mode-hooks) it))
       (--each mode-hooks
         (add-hook it (-partial '-add-font-lock-kwds
                                (symbol-value font-locks)))))))


(defconst emacs-outlines-font-lock-alist
  ;; Outlines
  '(("\\(^;;;\\) "          ?■)
    ("\\(^;;;;\\) "         ?○)
    ("\\(^;;;;;\\) "        ?✸)
    ("\\(^;;;;;;\\) "        ?✦)
    ("\\(^;;;;;;;\\) "        ?✧)
    ("\\(^;;;;;;;;\\) "       ?✿)))

(defconst lisp-outlines-font-lock-alist
  ;; Outlines
  '(("\\(^;; \\*\\) "          ?■)
    ("\\(^;; \\*\\*\\) "       ?○)
    ("\\(^;; \\*\\*\\*\\) "    ?✸)
    ("\\(^;; \\*\\*\\*\\*\\) " ?✿)))



(add-font-locks
 '((emacs-outlines-font-lock-alist emacs-lisp-mode-hook)
   (lisp-outlines-font-lock-alist clojure-mode-hook hy-mode-hook)))


;;;; Display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq display-line-numbers-type 'relative)



                                        ;padding
(setq header-line-format " "
      left-margin-width 1
      right-margin-width 1
      split-height-threshold 80
      split-window-threshold 160)

                                        ; window settings
(window-divider-mode)
(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode +1))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-env-version t
        doom-modeline-enable-word-count t
        doom-modeline-buffer-encoding nil
        )
  (display-time-mode))

(use-package anzu
  :config
  (global-anzu-mode +1))


;;;;; Window Resizing
(setq frame-resize-pixelwise t
      l (display-monitor-attributes-list)
      max-frame-width (nth 3 (nth 0 (nth 0 l)))
      max-frame-height (nth 4 (nth 0 (nth 0 l))))

(defun left-two-thirds ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) (- (* 2 (/ max-frame-width 3)) 20) max-frame-height t))

(defun left-one-thirds ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) (- (* 1 (/ max-frame-width 3)) 20) max-frame-height t))

(defun right-two-thirds ()
  (interactive)
  (set-frame-size (selected-frame) (- (* 2 (/ max-frame-width 3)) 17) max-frame-height t)
  (set-frame-position (selected-frame) (- (- (/ max-frame-width 3) 10) -10) 0))


(defun right-one-thirds ()
  (interactive)
  (set-frame-position (selected-frame) (* 2 (/ max-frame-width 3)) 0)
  (set-frame-size (selected-frame) (* 1 (/ max-frame-width 3)) max-frame-height t))

(defun center-third ()
  (interactive)
  (set-frame-position (selected-frame) (/ max-frame-width 3) 0)
  (set-frame-size (selected-frame) (- (* 1 (/ max-frame-width 3)) 20)  max-frame-height t))

(defun left-half ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) (- (* 1 (/ max-frame-width 2)) 20) max-frame-height t))

(defun right-half ()
  (interactive)
  (set-frame-position (selected-frame) (- (/ max-frame-width 2) 0) 0)
  (set-frame-size (selected-frame) (- (* 1 (/ max-frame-width 2)) 20) max-frame-height t))

(defun full-screen ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) (- (* 1 (/ max-frame-width 1)) 20) max-frame-height t))

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

;;;;; Startup

(setq inhibit-startup-screen t)

(load "~/.emacs.d/config/quotes.el")



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
  (split-window-right)
  (switch-to-buffer-other-window "*scratch*")
  )

(setq screens_file "~/.emacs.d/config/screens_file.el")
;; (write-region "(setq screens_list \'\(\))" nil screens_file)
;; (write-region "(setq screens_list '\(\"\"))" nil screens_file)
(load-file screens_file)

(use-package dashboard
      :ensure t
      :config
      (dashboard-setup-startup-hook)
      (setq dashboard-footer-messages quotes
            dashboard-items '((recents  . 5)
                              (projects . 5)
                              (agenda . 5)
                              )))

(defun emacs-startup-screen ()
  "startup screen config"
  (if (and (eq (boundp 'screens_list) t) (> (length screens_list) 0))
      (progn (apply 'files-startup-screen screens_list) )
     nil
    )

  (balance-windows)
  (if (> (length screens_list) 0)
      (kill-buffer "*dashboard*")))



(add-hook 'emacs-startup-hook #'emacs-startup-screen)
(add-hook 'emacs-startup-hook #'full-screen)
(defun save-screen-var ()
  (interactive)
  (with-temp-buffer
    (setq s2 (let (value) (dolist (elt screens_list value)
                            (setq value (cons (prin1-to-string elt) value)))))
    (insert (concat "(setq screens_list '(" (join " " s2) "))"))
    (write-region (point-min) (point-max) screens_file))
  (save-buffer))

(defun set-screens-list ()
  "Inserts path as string "
  (interactive)
  (push (read-file-name "Pick F to Open: " ) screens_list)
  (save-screen-var))

(defun reset-screens-list ()
  (interactive)
  (setq screens_list '())
  (save-screen-var))



;;;; Navigation
(global-unset-key (kbd "M-g M-g"))

(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("M-g M-g" . avy-goto-line)))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?h ?z ?x ?c ?v))
  (setq aw-scope 'frame)
  )

;; (use-package disable-mouse
;;   )
;; (
;; disable-mouse-mode 1)

(setq sentence-end-double-space nil)

(use-package no-spam
  :disabled
  :config
  (setq no-spam-default-repeat-delay 10)
  (no-spam-add-repeat-delay (next-line
                             previous-line
                             forward-char
                             backward-char))
  (no-spam-mode))

;;;;; Dired

(use-package dired
  :straight nil
  :ensure nil
  :config
  (setq insert-directory-program "gls" dired-use-ls-dired t
        dired-listing-switches "-agho --group-directories-first"))

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config (define-key dired-mode-map "." #'dired-hide-dotfiles-mode))

;;;;; Deft

(use-package deft
  :demand t
  :bind
  ("C-x C-g" . deft-find-file)
  :config
  (setq deft-extensions '("org" "pdf" "txt")
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

;;;;; Which key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;;;;; Helm
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

;;;;; Perspective
(use-package perspective)

;;;; Editing
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

(use-package autopair
  :config
  (autopair-global-mode))

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top)
  )


(defun delete-line-no-kill ()
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
  )
(global-set-key (kbd "C-S-k") 'delete-line-no-kill)

(require 'subr-x)
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/config/snippets"))
  (yas-global-mode 1)
  )

(setq create-lockfiles nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)




;;;;; Company
(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-idle-delay 0.2
        ;; min prefix of 2 chars
        company-minimum-prefix-length 2
        company-require-match nil)
  :bind
  ("C-c C-c" . company-complete))

(use-package company-quickhelp          ; Show help in tooltip
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (company-quickhelp-mode)))

;;;;; Spelling
;;[[https://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html][ispell code from here]]
(use-package ispell
  :config
  (setq ispell-program-name "hunspell"
        ispell-local-dictionary "en_US"))


(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(use-package helm-flyspell
  :config )

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)


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

(global-set-key (kbd "C-'") #'endless/ispell-word-then-abbrev)

(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") nil))
                                        ;[[https://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html][ispell code from here]]
;;;; Development
(use-package inheritenv)
(use-package language-id)
(use-package emacs-format-all-the-code
  :straight
  (:host github :repo "lassik/emacs-format-all-the-code" :branch "master" :files ("*.el"))
  :hook (prog-mode . format-all-mode))

(use-package flycheck :ensure)


;;;;; Babel


(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (python . t)
   (latex . t)
   (C . t)
   (R . t)
   (gnuplot . t)
   ))


(setq org-confirm-babel-evaluate nil)


(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)



;;;;; Languages
;;;;;; C
(use-package clang-format)
;;;;;; Python

(use-package python
  :mode ("\\.py" . python-mode)
  :config
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil
        python-shell-completion-native-enable nil))

(use-package pyenv-mode
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode))

(defun dd/py-workon-project-venv ()
  "Call pyenv-workon with the current projectile project name.
This will return the full path of the associated virtual
environment found in $WORKON_HOME, or nil if the environment does
not exist."
  (let ((pname (projectile-project-name)))
    (pyvenv-workon pname)
    (if (file-directory-p pyvenv-virtual-env)
        pyvenv-virtual-env
      (pyvenv-deactivate))))

(defun dd/py-auto-lsp ()
  "Turn on lsp mode in a Python project with some automated logic.
Try to automatically determine which pyenv virtual environment to
activate based on the project name, using
`dd/py-workon-project-venv'. If successful, call `lsp'. If we
cannot determine the virtualenv automatically, first call the
interactive `pyvenv-workon' function before `lsp'"
  (interactive)
  (let ((pvenv (dd/py-workon-project-venv)))
    (if pvenv
        (lsp)
      (progn
        (call-interactively #'pyvenv-workon)
        (lsp)))))

(bind-key (kbd "C-c C-a") #'dd/py-auto-lsp python-mode-map)

;;;;;; Rust
                                        ; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))
;;;;;; Javascript
(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  (setq-default js2-basic-offset 2)
  :hook
  (js2-mode . js2-imenu-extras-mode))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2))

(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode)))

                                        ; autoformatting
(use-package prettier-js
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  :config
  (setq prettier-js-args '(
                           "--bracket-spacing" "false"
                           )))

;;;;;; ESS and R
(use-package ess-site
  :straight ess
  :config
  (add-hook 'ess-post-run-hook 'ess-execute-screen-options)
  (setq ess-use-ido nil ; use helm
        ess-eval-visibly 'nowait ; don't hang with R
        ess-smart-S-assign-key nil ; unbind ess-insert-align
        ))

;; (setq org-babel-R-command "/Library/Frameworks/R.framework/Resources/R --slave --no-save")
;; (setq inferior-R-program-name "/Library/Frameworks/R.framework/Resources/R")

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


;;;;;; Webmode
(use-package web-mode
  :mode
  (
   ".twig$"
   ".html?$"
   ".css$"
   ".hbs$"
   ".vue$"
   ".blade.php$"
   )
  :hook ((web-mode . company-mode))
  :config
  (setq
   indent-tabs-mode nil
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 2
   web-mode-script-padding 2
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-indentation t))

;;;;; LSP Mode
(use-package helm-lsp
  :ensure t
  :after (lsp-mode)
  :commands (helm-lsp-workspace-symbol)
  :init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-auto-guess-root t
        lsp-log-io nil
        lsp-enable-indentation t
        lsp-enable-imenu t
        lsp-prefer-flymake nil
        lsp-ui-sideline-enable nil
        lsp-pylsp-plugins-flake8-enabled t
        lsp-pylsp-plugins-autopep8-enabled t
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-eldoc-render-all t
        lsp-idle-delay 1
        lsp-rust-analyzer-server-display-inlay-hints t)

  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :hook (lsp-mode . lsp-enable-which-key-integration))


(setq lsp-restart 'ignore)

(defun dotfiles--lsp-deferred-if-supported ()
  "Run `lsp-deferred' if it's a supported mode."
  (unless (derived-mode-p 'emacs-lisp-mode)
    (lsp-deferred)))

(add-hook 'prog-mode-hook #'dotfiles--lsp-deferred-if-supported)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-peek-always-show t
        lsp-ui-sideline-show-hover t
        lsp-ui-doc-enable nil))

;;;;;; Tailwind
(use-package lsp-tailwindcss
  :straight
  (:host github :repo "merrickluo/lsp-tailwindcss" :branch "master" :files ("*.el"))
  :config
  (setq lsp-tailwindcss-add-on-mode t))
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;;;;; Modes

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package json-mode)

(use-package toml-mode :ensure)

(defun testfn ()
  (interactive)
  (csv-align-mode t)
  (toggle-truncate-lines 1)
  (csv-header-line t)
  )

(use-package csv-mode
  :mode (".tsv" ".csv" ".tabular" ".vcf")
  :custom
  (csv-comment-start "##")
  :hook
  (csv-mode . (lambda ()
                (run-at-time 0 nil 'testfn))))


(use-package emacs-dotenv-mode
  :disabled
  :straight
  (:host github :repo "preetpalS/emacs-dotenv-mode" :branch "master" :files ("*.el"))
  :config
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

;;;;; Tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(put 'temporary-file-directory 'standard-value
     (list temporary-file-directory))

(set-register ?s (cons 'file "/ssh:hs884@kill.cs.rutgers.edu:"))



(add-hook
 'c-mode-hook
 (lambda () (when (file-remote-p default-directory) (company-mode -1))))

;;;;; shell
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

;;;;; Projectile
(use-package helm-projectile)
(use-package projectile
  :config
  (projectile-global-mode)

  (setq projectile-completion-system 'helm
        projectile-indexing-method 'alien
        projectile-sort-order 'recently-active
        projectile-enable-caching t
        projectile-switch-project-action 'helm-projectile
        projectile-find-file 'helm-projectile-find-file
        )


  (setq projectile-project-search-path '("~/org/" "~/code/"))
  (helm-projectile-on)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  ("s-p" . projectile-command-map))
(setq projectile-globally-ignored-directories '(".idea"
                                                ".vscode"
                                                ".ensime_cache"
                                                ".eunit"
                                                ".git"
                                                ".hg"
                                                ".fslckout"
                                                "_FOSSIL_"
                                                ".bzr"
                                                "_darcs"
                                                ".tox"
                                                ".svn"
                                                "node_modules"
                                                ".stack-work"
                                                ".ccls-cache"
                                                ".cache"
                                                ".clangd"))


(use-package helm-ag)

;;;;; Magit
(use-package magit
  :config
  (with-eval-after-load 'magit-mode
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t))
  :bind
  ("C-x g" . magit-status))

;;; Org-mode
;;;; Init

(use-package gnuplot)
(global-set-key "\M-\C-g" 'org-plot/gnuplot)
;[[http://gewhere.github.io/gnuplot-orgmode][source for org plotting]]
;;;; Formatting
;;;;; Looks
; insp from [[https://hugocisneros.com/org-config/][here]]
;;;;;; Gen
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


;;;;;; Colors
(defun col-strip (col-str)
  (butlast (split-string (mapconcat (lambda (x) (concat "#" x " "))
                                    (split-string col-str "-")
                                    "") " ")))

(setq color-schemes (list
                     (col-strip "a21d1d-5497de-8e35b7-ffff5b-56cb7d-df5252-707efa") ; red blue purple study
                     (col-strip "2278bf-e15554-3bb273-507c6d-6e5775-598d91-7768ae") ; blue red green okay
                     (col-strip "619ff0-3d8ced-2078ec-1569d8-1661c9-1452b6-1246a6-0d2e88") ; blue spectrum
                     ))
(setq pick-color 0)


;;;;;; Fonts
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
  (setq line-spacing 0.01)

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
     (set-face-attribute face nil :height 0.85
                         :foreground "orange"))
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
  (mapc ;; This sets the fonts to a smaller size
   (lambda (face)
     (set-face-attribute face nil :height 0.85
                         :foreground "green"))
   (list 'org-todo
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
  (set-face-attribute 'outline-1 nil
                      :height 1.3
                      :foreground (nth 0 color-theme))
  (set-face-attribute 'outline-2 nil
                      :height 1.2
                      :foreground (nth 1 color-theme))
  (set-face-attribute 'outline-3 nil
                      :height 1.1
                      :foreground (nth 2 color-theme))
  (set-face-attribute 'outline-4 nil
                      :height 1.05
                      :foreground (nth 3 color-theme))
  (set-face-attribute 'outline-5 nil
                      :foreground (nth 4 color-theme))
  (set-face-attribute 'outline-6 nil
                      :foreground (nth 5 color-theme))
  )

(add-hook 'org-mode-hook 'my/style-org)
(add-hook 'org-mode-hook 'visual-line-mode) ; make lines go to full screen
(add-hook 'org-mode-hook 'variable-pitch-mode) ; auto enable variable ptich for new buffers

;;;;; Latex
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
(setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex" "tex~" "aux" "idx" "log" "out" "toc" "nav" "bcf" "run" "run.xml" "xml" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))

(use-package tex
  :straight auctex
  :defer t
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)

  (add-hook 'LaTeX-mode-hook 'add-my-latex-environments)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  (defun add-my-latex-environments ()
    (LaTeX-add-environments
     '("align*" LaTeX-env-label)
     '("align" LaTeX-env-label)
     '("tikzpicture" LaTeX-env-label)
     '("equation*" LaTeX-env-label)))
  )



;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
;; (
(setq org-latex-pdf-process (list "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))


(add-to-list 'org-latex-packages-alist '("" "tikz"  t))
(add-to-list 'org-latex-packages-alist '("" "sgame"  t))

(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

(setq org-preview-latex-default-process 'imagemagick)
(setq org-latex-create-formula-image-program 'imagemagick)

(use-package cdlatex
  ;; :requires texmathp
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
                                        ;    (setq cdlatex-paired-parens "")
  (setq cdlatex-command-alist
        '(("tkz" "Insert axiom env"   "" cdlatex-environment ("tikzpicture") t nil)
          ("thr" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)
          ("gam" "Insert game env" "" cdlatex-environment ("game") t nil))))

;;;;; Images

(use-package org-download
  :ensure t
  :hook (dired-mode . org-download-enable)
  :bind (("C-c s s" . org-download-screenshot)
         ("C-c s c" . org-download-clipboard))
  :config

  (setq-default org-download-image-dir "~/Pictures/emacs-pics")
  (setq org-download-screenshot-method "screencapture -i %s"))

;;;; Life
;;;;; Agenda
(use-package org-agenda
  :straight nil :ensure nil
  :config
  (setq org-agenda-start-with-log-mode t
        org-log-done 'time
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-log-into-drawer t
        org-agenda-window-setup 'current-window
        org-agenda-span 4
        org-agenda-start-day "+0d"
        org-archive-location "~/.emacs.d/archive.org::"
        org-agenda-files '(
                           "~/org/inbox.org"
                           "~/org/gtd.org"
                           "~/org/tickler.org"
                           )
        org-agenda-prefix-format '(
                                        ;                                     (agenda . " %-12b %?-15t% s")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                        ;                                     (search . " %i %-12:c")
                                   )
        org-todo-keywords '((sequence "TODO(t)"  "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "FAILED(f)"))
        org-refile-targets '(("~/org/gtd.org" :maxlevel . 1)
                             ("~/org/time.org" :level . 1)
                             )
        org-capture-templates
        `(("t" "Todo" entry (file "~/org/inbox.org") "* TODO %i%?" :empty-lines 1)))

  (org-agenda-align-tags))

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
(unbind-key "C-'" org-mode-map)


;;;;; Habits

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

;;;;; Clocking
(setq org-clock-into-drawer t
      org-clock-idle-time 5
      org-time-stamp-rounding-minutes (quote (0 5))
      org-clock-history-length 23
      org-clock-persist t
      org-clock-in-resume t
      org-clock-persist-query-resume nil)


                                        ;[[org-clock-persist-query-resume nil][good ref]], [[http://doc.norang.ca/org-mode.html#Clocking][link]]
;;;;; Journal
(use-package org-journal
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-file-format "%Y%m%d"
        org-journal-date-format "%A, %e %b %Y"
        org-journal-date-prefix ""
        org-journal-find-file 'find-file)
  )

(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
   (pcase org-journal-file-type
     (`daily "#+TITLE: "))))

(setq org-journal-file-header 'org-journal-file-header-func)

(defun org-journal-save-entry-and-exit()
  (interactive)
  (save-buffer)
  (kill-buffer))

(add-hook 'org-journal-mode-hook
          (lambda ()
            (define-key org-journal-mode-map
              (kbd "C-x C-s") 'org-journal-save-entry-and-exit)))

(defun insert-created-date (&rest ignore)
  (insert (concat
           "\n* Gratitude"
           "\n* Moments"
           "\n* Accomplishments"
           )))

(add-hook 'org-journal-after-header-create-hook
          #'insert-created-date)

(global-set-key ;; run this at the beginning of day to generate journal
 (kbd "C-c J")
 (lambda ()
   (interactive)
   (let ((current-prefix-arg 4)) ;; emulate C-u
     (call-interactively 'org-journal-new-entry)
     (org-journal-save-entry-and-exit)
     (setq entry-path (org-journal--get-entry-path))
     )))

(setq entry-path (org-journal--get-entry-path))
(set-register ?j (cons 'file entry-path))

(add-to-list 'org-capture-templates
             '("g" "Gratitude entry" entry (file+headline entry-path "Gratitude")
               "** %^{Title}\n%i%?"))

(add-to-list 'org-capture-templates
             '("G" "Goal entry" entry (file+headline entry-path "Goals")
               "** %^{Title}\n%i%?"))

(add-to-list 'org-capture-templates
             '("j" "Journal entry" entry (file+headline entry-path "Moments")
               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"))

;;;; Publishing

(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox
  :config
  (setq org-hugo-base-dir "~/code/website/roam-notes"
        org-hugo-section "notes"))
(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun zeeros/fix-doc-path (path)
  (replace-regexp-in-string ".*\/" "" (replace-in-string "Dropbox/org/roam/" ""  (replace-in-string "../" "" path))))

(advice-add 'org-export-resolve-id-link :filter-return #'zeeros/fix-doc-path)

(require 'find-lisp)
(setq org-id-extra-files (find-lisp-find-files "~/org/roam/" "\.org$"))


;;;; Ledger Finances
(use-package ledger-mode)
;;; Literature
                                        ;current workflow is org roam with directories for main ideas, subject facts, books, pdfs, podcasts
                                        ;tweets and reddit posts etc will be directly files into ideas, subjects, main ideas, with a reference to the sorce
                                        ;currently I have roam capture templates that capture facts and papers (going to eventually incorporate books an podcasts etc)
                                        ;so workflow itself is reading through papers, use capture template and org noter to take notes and write a fina summary
;;bibtex to cite
;;;; Roam
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
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                 :unnarrowed t)
                                ("f" "Fact" plain
                                 "\n\n* %?"
                                 :if-new (file+head "facts/%<%Y%m%d%H%M%S>-${slug}.org" "#+filetags: %^{tags}\n#+title: ${title}\n")
                                 :unnarrowed t)
                                ("b" "Book" plain
                                 "\n\n* %?"
                                 :if-new (file+head "books/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                 :unnarrowed t)
                                ("P" "Podcast" plain
                                 "\n\n* %?"
                                 :if-new (file+head "podcasts/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                 :unnarrowed t)
                                ))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
  :config
  (require 'org-roam-dailies)
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  (setq org-roam-dailies-directory "~/org/roam/daily/")
  (setq org-roam-mode-section-functions
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              ;; #'org-roam-unlinked-references-section
              ))
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
                ("C-c n I" . org-roam-node-insert-immediate))
               :map org-roam-dailies-map(
                                         ("Y" . org-roam-dailies-capture-yesterday)
                                         ("T" . org-roam-dailies-capture-tomorrow))))

  :bind-keymap
  ("C-c n d" . org-roam-dailies-map))



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

(use-package org-randomnote
  :ensure t)

;;;;;; Org roam ui
(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;;;;; Noter
(use-package org-noter
  :bind ("C-c o" . org-noter)
  :config
  (setq org-noter-default-notes-file-name '("notes.org")
        org-noter-notes-search-path '("~/org")
        org-noter-notes-window-location "Vertical"
        org-noter-separate-notes-from-heading t))

(defun my/no-op (&rest args))
(advice-add 'org-noter--set-notes-scroll :override 'my/no-op)

;;;;; Bibtex
(use-package helm-bibtex
  :config
  ;; In the lines below I point helm-bibtex to my default library file.
  (setq bibtex-completion-bibliography '("~/org/refs.bib")
        bibtex-completion-library-path '("~/org/papers")
        bibtex-completion-notes-path "~/org/roam/papers"
        bibtex-completion-pdf-field "File"
        )
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "open" nil 0 nil fpath)))
  ;; (add-hook 'bibtex-mode-hook 'flyspell-mode)
  :hook (Tex . (lambda () (define-key Tex-mode-map "\C-ch" 'helm-bibtex))))

(defun insert-line()
  (dotimes (_ 2)
    (forward-sexp))
  (forward-char)
  (open-line 1))
(defun bibtex-add-new-lines()
  (interactive)
  (move-beginning-of-line nil)
  (while (re-search-forward " = " nil t)
    (replace-match "="))
  (move-beginning-of-line nil)
  (re-search-forward ",")
  (condition-case nil
      (while (< 3 4)
        (insert-line))
    (scan-error nil))
  )
(add-hook 'bibtex-clean-entry-hook 'bibtex-add-new-lines)
;; Set up org-ref stuff
(use-package org-ref
  ;; :disabled
  :requires helm-bibtex
  :config
  ;; Again, we can set the default library
  (setq org-ref-insert-cite-function 'org-ref-cite-insert-helm
        org-ref-insert-label-function 'org-ref-insert-label-link
        org-ref-insert-ref-function 'org-ref-insert-ref-link
        )
  (global-set-key (kbd "C-c r c") 'org-ref-cite-insert-helm)
  (global-set-key (kbd "C-c r r") 'org-ref-insert-ref-link)
  (global-set-key (kbd "C-c r l") 'org-ref-insert-label-link))

(use-package org-roam-bibtex

  :after org-roam
  :config
  (require 'org-ref)
  )
(setq orb-preformat-keywords
      '("citekey" "title" "url" "author-or-editor" "keywords" "file")
      orb-process-file-keyword t
      orb-attached-file-extensions '("pdf"))

(add-to-list 'org-roam-capture-templates
             '("p" "paper" plain
               (file "~/.emacs.d/config/capture_templates/biblio.org")
               :target
               (file "papers/${citekey}.org")))



(org-roam-bibtex-mode)

;;;;; Epub
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 1.0))
  ;; (add-hook 'nov-mode-hook 'my-nov-font-setup)
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  (setq nov-text-width 80)
  )



;;;;; PDF Tool
(defun my/save-buffer-no-args ()
  "Save buffer ignoring arguments"
  (save-buffer))

(use-package pdf-tools
  :ensure t
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :hook ((pdf-view-mode . pdf-view-midnight-minor-mode)
         (pdf-view-mode . (lambda () (pdf-view-fit-height-to-window)))
         (pdf-view-mode . (lambda () (cua-mode 0))))
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"
        pdf-view-display-size 'fit-height
        pdf-annot-activate-created-annotations t))
(pdf-tools-install)


(defun pdf-view--rotate (&optional counterclockwise-p page-p)
  "Rotate PDF 90 degrees.  Requires pdftk to work.\n"
  ;; error out when pdftk is not installed
  (if (null (executable-find "pdftk"))
      (error "Rotation requires pdftk")
    ;; only rotate in pdf-view-mode
    (when (eq major-mode 'pdf-view-mode)
      (let* ((rotate (if counterclockwise-p "left" "right"))
             (file   (format "\"%s\"" (pdf-view-buffer-file-name)))
             (page   (pdf-view-current-page))
             (pages  (cond ((not page-p)                        ; whole doc?
                            (format "1-end%s" rotate))
                           ((= page 1)                          ; first page?
                            (format "%d%s %d-end"
                                    page rotate (1+ page)))
                           ((= page (pdf-info-number-of-pages)) ; last page?
                            (format "1-%d %d%s"
                                    (1- page) page rotate))
                           (t                                   ; interior page?
                            (format "1-%d %d%s %d-end"
                                    (1- page) page rotate (1+ page))))))
        ;; empty string if it worked
        (if (string= "" (shell-command-to-string
                         (format (concat "pdftk %s cat %s "
                                         "output %s.NEW "
                                         "&& mv %s.NEW %s")
                                 file pages file file file)))
            (pdf-view-revert-buffer nil t)
          (error "Rotation error!"))))))

(defun pdf-view-rotate-clockwise (&optional arg)
  "Rotate PDF page 90 degrees clockwise.  With prefix ARG, rotate
  entire document."
  (interactive "P")
  (pdf-view--rotate nil (not arg)))

(defun pdf-view-rotate-counterclockwise (&optional arg)
  "Rotate PDF page 90 degrees counterclockwise.  With prefix ARG,
  rotate entire document."
  (interactive "P")
  (pdf-view--rotate :counterclockwise (not arg)))

(define-key pdf-view-mode-map (kbd "R") 'pdf-view-rotate-clockwise)
(use-package pdfgrep
  :config
  (pdfgrep-mode))


;;;; Other
;;;;; Grind mode

(defun grind-theme()
  (interactive)
  (setq pick-color 2)
  (load-theme 'doom-acario-dark  t)
  )
(defun grind()
  (interactive)

  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? "))
                         " | sudo -S ~/bin/grind"))
  (with-temp-buffer
    (insert (concat "(setq grind-on t)"))
    (write-region (point-min) (point-max) grind-file ))
  (grind-theme)
  (my/style-org))

(global-set-key (kbd "C-c g") #'grind)

(defun ungrind-theme()
  (setq pick-color 0)
  ;; (load-theme 'doom-horizon t)
  )

(defun ungrind()
  (interactive)


  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? "))
                         " | sudo -S ~/bin/ungrind"))

  (with-temp-buffer
    (insert (concat "(setq grind-on nil)"))
    (write-region (point-min) (point-max) grind-file ))

  (ungrind-theme)
  (my/style-org))
(global-set-key (kbd "C-c u") #'ungrind)

(setq grind-file "~/.emacs.d/config/grind.el")

(load-file grind-file)
(if (eq grind-on t)
    (grind-theme)
  (ungrind-theme))

;;;;; Statistics
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
