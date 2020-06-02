;; Searching for help: 
;; C-h r i search-term 
;; reload file
;; M-x eval-buffer



;; User Info
(setq user-full-name "Nicky van Foreest")
(setq user-mail-address "vanforeest@gmail.com")


(require 'package)
(setq package-check-signature 'nil)  ;; don't ask for signature files. I also don't know how to do this btw.

(package-initialize)
(setq package-archives (append package-archives
			 '( ("melpa-stable" . "https://stable.melpa.org/packages/")
			    ("melpa" . "http://melpa.org/packages/")
			    ("gnu" . "http://elpa.gnu.org/packages/")
			    ("elpy" . "http://jorgenschaefer.github.io/packages/")))
      )
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
  (setq ;; ring-bell-function 'ignore       ; no sound
        visible-bell t  ;; flash if command makes no sense, like when C-g has nothing to do
        frame-resize-pixelwise t
        default-directory "~/"   ;; don't set it to . because that seems to lead to a cycle
        inhibit-startup-message t
	inhibit-startup-echo-area-message t
        kill-whole-line 'always  
        load-prefer-newer t ;;   Don't use the compiled code if it's the older package.
        create-lockfiles nil ;; no lockfiles
        auto-save-default nil ;; Do not autosave.
        indent-tabs-mode nil ;;  use spaces for indentation in stead of hard tabs
        tab-width 4 ;; 
        sentence-end-double-space 'nil ;; no double space at end of sentence
        auto-fill-mode -1  ;; don't insert returns in long lines
        frame-title-format (list (format "%s %%S: %%j " (system-name))
              '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
              )
	require-final-newline t
	apropos-do-all t
        mouse-yank-at-point t;; middle-mouse-click pastes at mouse location
        )

  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (global-linum-mode t)               ;; enable line numbers globally
  (global-hl-line-mode t)             ;; highlight the line with point
  (column-number-mode t)
  (fset 'yes-or-no-p 'y-or-n-p)       ;; Change all yes/no questions to y/n type
  (save-place-mode 1)                 ;; save last visited place of buffer
  (set-face-attribute 'default nil :height 95) ;; make font slightly smaller

  (set-language-environment "UTF-8") ; Allow for French accents
  (defun accents ()
    (interactive)
    (activate-input-method "latin-1-alt-postfix")
    )
  ;; (defun current-lang () ;; I don't know whether I need this to be able to type French characters. 
  ;;   (interactive)
  ;;   (eval-expression current-language-environment)
  ;;   )
  :bind (
         ("M-/" . hippie-expand)  ;; useful, at least in python mode to autocomplete filenames in open("..")
         ("C-X C-b" . ibuffer) 
         ("M-o" . other-window)  ;; quicker than C-x o
         )
  :hook (before-save whitespace-cleanup)
  )

(use-package uniquify
  ;; use <dir-name> behind file name to distinguish files
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  )

(use-package misc
  ;;   "Kill up to, but not including ARGth occurrence of CHAR."
  :bind (("M-z" . zap-up-to-char))
  )


(use-package dired
  :config
  (setq delete-by-moving-to-trash t)
  ;; Delete intermediate buffers when navigating through dired.
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))

(use-package ido
  :config
  (setq ido-everywhere t
	ido-enable-flex-matching t ;; show any name that has the typed characters
	ido-use-virtual-buffers t  ;; list of past visited files
	ido-create-new-buffer 'always  ;; do not ask to create new buffer when C-x b
	confirm-nonexistent-file-or-buffer nil  ;; also do not ask to confirm in case of C-x b
	ido-default-buffer-method 'selected-window
	ido-file-extensions-order '(".tex" ".py")
	completion-ignored-extensions '(".o" ".pdf" "~" ".bin" ".ilg" ".idx" ".ind" ".log"
                                      ".obj" ".map" ".a" ".so" ".ptxcode" ".toc" ".rel" ".out"
                                      ".mod" ".aux" ".out" ".pyg")
	ido-ignore-extensions t  ;; ignore files with the above extensions
	ido-ignore-directories '("auto" "_minted*" "__pycache__" ".git") ;; this works with C-x d, but not with C-x C-f
	ido-ignore-files '("auto" "_minted*" "__pycache__") ;; this works with C-x C-f
	)
  (ido-mode t)
  )


(use-package material-theme
  :ensure t
  )

(use-package diminish ;; suppress minor modes, but I don't seem to see the effect of it.
  :ensure t
  )

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
  :config (setq ediff-split-window-function 'split-window-horizontally
		ediff-window-setup-function 'ediff-setup-windows-plain ;; otherwise ediff opens another window
		)
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

;; a convenient interface to your recently and most frequently used commands.
(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize)
)

;; Company is a text completion framework for Emacs. The name stands for "complete anything". 
(use-package company
  :ensure t
  :defer t
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              )
  :config
  (setq company-idle-delay 0.0)
  (global-company-mode t)
  )


(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )

(use-package flyspell
  :ensure t
  :defer t
  :init  (flyspell-mode 1)
  :config
  ;;   (setq 
  (setq ispell-program-name "aspell"
	ispell-list-command "--list" ;; this is necessary when using aspell instead of ispell
	;; ispell-dictionary   "english" ; Default dictionary to use
	)
  :hook((prog-mode . flyspell-mode)
        (text-mode . flyspell-mode)
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

;; ensure to have run pip install jedi flake8 importmagic autopep8 yapf
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


(use-package tex-site ;; If I don't use latex here, the add-to-list below does not work
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :init
  (setq reftex-plug-into-AUCTeX t )
  :bind (("M-q" . ales/fill-paragraph))  ;; start every sentence on a new line
  :config
  (setq-default TeX-master nil ); by each new fie AUCTEX will ask for a master fie.
  (setq-default auto-fill-function nil) ;; 
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil  ;dont ask to save if you want to compile with C-c C-c
        TeX-PDF-mode t      ; use pdflatex
        TeX-file-extensions '("tex" "sty")
        TeX-ispell-extend-skip-list t
	reftex-isearch-minor-mode t ; search whole document, not just the current file
        )
  (defun ales/fill-paragraph (&optional P)
    "When called with prefix argument call `fill-paragraph'. Otherwise split the current paragraph into one sentence per line."
    (interactive "P")
    (if (not P)
        (save-excursion
          (let ((fill-column 12345678)) ;; relies on dynamic binding
            (fill-paragraph) ;; this will not work correctly if the paragraph is
            ;; longer than 12345678 characters (in which case the
            ;; file must be at least 12MB long. This is unlikely.)
            (let ((end (save-excursion
                         (forward-paragraph 1)
                         (backward-sentence)
                         (point-marker))))  ;; remember where to stop
              (beginning-of-line)
              (while (progn (forward-sentence)
                            (<= (point) (marker-position end)))
                (just-one-space) ;; leaves only one space, point is after it
                (delete-char -1) ;; delete the space
                (newline)        ;; and insert a newline
                (LaTeX-indent-line) ;; I only use this in combination with late, so this makes sense
                ))))
      ;; otherwise do ordinary fill paragraph
      (fill-paragraph P))
    )
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (visual-line-mode) ; hiermee kan een zin over meerdere regels lopen, zonder dat ie wordt opgehakt.
              (turn-on-reftex); load reftex
              (LaTeX-math-mode) ; enable math-mode right away in  math environment; `a expands right away to \alpha
	      (electric-indent-local-mode -1)
              (LaTeX-add-environments
               '("axiom" LaTeX-env-label)
               '("corollary" LaTeX-env-label)
               '("lemma" LaTeX-env-label)
               '("proposition" LaTeX-env-label)
               '("theorem" LaTeX-env-label)
               '("exercise" LaTeX-env-label)
               '("example" LaTeX-env-label)
               '("remark" LaTeX-env-label)
               ;; '("question" "point")
               ;;'("questionText" "point")
               '("slide" "title")
               '("wideslide" "title")
               )
              )
            )
  (add-to-list 'LaTeX-verbatim-environments "exercise" "solution")
  (add-to-list 'LaTeX-indent-environment-list
	       '("exercise" current-indentation)
               '("solution" current-indentation)
	       )
  )

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        reftex-plug-into-AUCTeX t
        reftex-cite-prompt-optional-args t; Prompt for empty optional arguments in cite
        )
  (setq reftex-label-alist
        '(("axiom"   ?a "ax:"  "~\\ref{%s}" nil ("axiom"   "ax.") -2)
          ("corollary" ?c "cor:" "~\\ref{%s}" nil   ("corollary" "co.") -3)
          ("exercise" ?x "ex:" "~\\ref{%s}" nil   ("exercise" "ex.") -4)
          ("extra" ?x "ex:" "~\\ref{%s}" nil   ("exercise" "ex.") -4)
          ("lemma" ?l "lem:" "~\\ref{%s}" nil   ("lemma" "le.") -5)
          ("proposition" ?p "prop:" "~\\ref{%s}" nil   ("proposition" "pr.") -6)
          ("theorem" ?h "thr:" "~\\ref{%s}" nil   ("theorem" "th.") -7)
          ("example" ?p "exa:" "~\\ref{%s}" nil   ("example" "exa.") -8)
          ("remark" ?r "rem:" "~\\ref{%s}" nil   ("remark" "rem.") -9)
          ("definition" ?d "def:" "~\\ref{%s}" nil   ("definition" "def.") -10)
          )
        )
  (setq reftex-external-file-finders
        '(("tex" . "kpsewhich -format=.tex %f")
          ("bib" . "kpsewhich -format=.bbl %f")
          )
        )
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
)


(use-package csv-mode
    :ensure t
    :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
    :config (setq csv-separators '("," ";" "|" " "))
    )

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  )


