;; Searching for help: 
;; C-h r i search-term 


;; User Info
(setq user-full-name "Nicky van Foreest")
(setq user-mail-address "vanforeest@gmail.com")


(require 'package)
(setq package-check-signature 'nil)  ;; don't ask for signature files. I also don't know how to do this btw.

(package-initialize)
(setq package-archives (append package-archives
			 '(("melpa" . "http://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/"))))
(package-initialize)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; Enable use-package
(eval-when-compile
  (require 'use-package))

(use-package emacs
  :config
  (setq ;; ring-bell-function 'ignore       ; minimise distraction
        frame-resize-pixelwise t
        default-directory "~/"   ;; todo, perhaps I want "."
        inhibit-startup-message t
        kill-whole-line 'always  
        load-prefer-newer t ;;   Don't use the compiled code if its the older package.
        create-lockfiles nil ;; no lockfiles
        auto-save-default nil ;; Do not autosave.
        indent-tabs-mode nil ;;  use spaces for indentation in stead of hard tabs
        tab-width 4 ;; 
        sentence-end-double-space 'nil ;; no double space at end of sentence
        auto-fill-mode -1  ;; don't insert returns in long lines
        frame-title-format (list (format "%s %%S: %%j " (system-name))
              '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
              )
        )

  (fset 'yes-or-no-p 'y-or-n-p) ;; Change all yes/no questions to y/n type
  (set-face-attribute 'default nil :height 95) ;; make font slightly smaller
  (set-language-environment "UTF-8") ; Allow for French accents
  (global-hl-line-mode t)  ;; highlight the line with point
  (defun accents ()
    (interactive)
    (activate-input-method "latin-1-alt-postfix")
    )
  ;; (defun current-lang () ;; I don't know whether I need this to be able to type French characters. 
  ;;   (interactive)
  ;;   (eval-expression current-language-environment)
  ;;   )
  :hook (before-save whitespace-cleanup)
  )

(use-package material-theme
  :ensure t
  )

(use-package better-defaults
  :ensure t
  :config
  (column-number-mode t)
  (global-linum-mode t) ;; enable line numbers globally
  :bind ("M-z" . 'zap-to-char) ;; Weird, but this does not work without the '
  )

(use-package diminish ;; suppress minor modes, but I don't seem to see the effect of it.
  :ensure t
  )

(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  ;; Delete intermediate buffers when navigating through dired.
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))

(use-package ack ;; practical searching
  :ensure t
  )

(use-package dimmer  ;; This dimms the buffer(s) that don't have point. 
  :ensure t
  :config (dimmer-mode t)
  (setq dimmer-fraction 0.2)
  )

(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1)
  )


(use-package ediff
  ; side by side differences rather than in two buffers under neath each other.
  :ensure nil
  :config (setq ediff-split-window-function 'split-window-horizontally)
  )


(use-package elec-pair  ;; make matching pair of e.g. brackets
  :ensure nil
  :hook (prog-mode . electric-pair-mode)
  )

(use-package files 
  :ensure nil
  :config
  (setq confirm-kill-processes nil;  "Just kill, do not aske for confirmation
        make-backup-files nil ;; My copies are on dropbox and github
        )
  ) 

(use-package deft  ;; very practical note taking package
  :ensure t
  :defer
  :bind ("C-c d" . deft)
  :config
  (setq deft-extensions '("txt"))
  (setq deft-directory "~/org/deft")
  (setq deft-auto-save-interval 0)
  )

;; ensure:
;;; pip install jedi flake8 importmagic autopep8 yapf
(use-package elpy
  :ensure t
  :after python
  :config
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (elpy-enable)
  )

(use-package blacken
  :ensure t
  :demand t
  :after python
  :config
  (setq blacken-skip-string-normalization t
        blacken-line-length 90
        )
  :hook(python-mode . blacken-mode)   ;; autoformat with black on save
  )


