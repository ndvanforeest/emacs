;;
;; The site
;;https://github.com/ianpan870102/yay-evil-emacs/blob/master/config.org
;; is super handig.

;;; Code:

(setq package-check-signature 'nil) ;; anders gaat ie om een gpg signature vragen, heel irritant

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("ctrlf" . "https://github.com/raxod502/ctrlf/"))


(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
   (package-refresh-contents)
   (package-install 'use-package))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


(use-package emacs
  :preface
  (defvar ian/indent-width 4) ; change this value to your preferred width
  :config
  (setq-default indent-tabs-mode nil)

  ;; (setq-default auto-fill-function nil) 
  ;; (setq-default auto-fill-function 'do-auto-fill) ;; Ga verder met
  ;; commentaar op de volgende regel.
  ; (setq fill-column 150)
  (setq inhibit-startup-message t
        kill-whole-line 'always
        sentence-end-double-space 'nil
        backup-directory-alist `(("." . "~/.saves"))
        auto-fill-mode t ;
        )
  (setq frame-title-format ; zet window titel
        (list (format "%s %%S: %%j " (system-name))
              '(buffer-file-name "%f" (dired-directory dired-directory "%b")))
        )
  (global-linum-mode t) ;; enable line numbers globally
  (column-number-mode t)
  ;; (menu-bar-mode -1)
  ;;  (tool-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  ;(global-hl-line-mode t)
  (set-face-attribute 'default nil :height 90)
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (fset 'yes-or-no-p 'y-or-n-p) ;; change all prompts to y or n
  (set-language-environment "UTF-8") ; franse leestekens
  (save-place-mode 1)
  (defun accents ()
    (interactive)
    (activate-input-method "latin-1-alt-postfix")
    )
  (defun current-lang ()
    (interactive)
    (eval-expression current-language-environment)
    )
  :hook (before-save whitespace-cleanup)
  )



;;   (global-set-key (kbd "M-/") 'hippie-expand)
;;   (global-set-key (kbd "C-x C-b") 'ibuffer)
;;   (global-set-key (kbd "M-z") 'zap-up-to-char)


;;   (setq save-interprogram-paste-before-kill t
;;         apropos-do-all t
;;         mouse-yank-at-point t
;;         require-final-newline t
;;         visible-bell t
;;         load-prefer-newer t
;;         ediff-window-setup-function 'ediff-setup-windows-plain)

;;   (unless backup-directory-alist
;;     (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
;;                                                    "backups"))))))

1
;; lbus ving ctrl-space af om het keyboard te veranderen. Ik heb
;; ibus-setup gerund, en de keybinding veranderd in alt-space.


;; packages

;; Ik laadde better defaults eerst, maar dit bleek de functionaliteit
;; van C-s en M-C-s (zoeken dus), om te draaien. Daar ben ik niet van
;; gediend, dus ik laad better defaults niet mer. 

(use-package better-defaults
  :ensure t
  )

;; (use-package ctrlf
;;   :ensure t
;;   :hook
;;   (after-init . ctrlf-mode)
;;   )

(use-package dired
;; Delete intermediate buffers when navigating through dired.
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))

;; deal met lange regels in bijv python mode
;; Het lijkt nu even niet te werken, en ik wil geen tijd besteden aan het uitzoeken.
;; (use-package multi-line
;;   :ensure t
;;   :bind ("C-;" . multi-line)
;;   )

;; syntax high lighting
(use-package highlight-numbers
  :ensure t
  :hook ( (text-mode . highlight-numbers-mode)
          (prog-mode . highlight-numbers-mode)
          )
  ;; :config
  ;; (add-hook 'latex-mode-hook 'highlight-numbers-mode)
  )

(use-package highlight-operators
  :ensure t
  :hook ((prog-mode . highlight-operators-mode)
         (text-mode . highlight-operators-mode)
         )
  )

(use-package highlight-escape-sequences
  :ensure t
  :hook (prog-mode . hes-mode)
  )

(use-package hl-line+
  ;; Voorkomt dat de line waarin je op dat moment tikt steeds
  ;; gehighligth wordt. Nu gebeurt dat pas na een paar second, 2 hier.
  :config
  (toggle-hl-line-when-idle 1)
  (hl-line-when-idle-interval 2)
  )

(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0
        blink-matching-paren nil
        ; show-paren-style 'expression   ; hierdoor kan je vaak de expressie niet meer lezen
        )
  ; (show-paren-mode 1) ;; dit lijk je te moeten zetten, maar volgens
  ;; mij is paren mode altijd al aan.
  )


(use-package ediff
  ; side by side differences rather than in two buffers under neath each other.
  :ensure nil
  :config (setq ediff-split-window-function 'split-window-horizontally)
  )


;;; files.el --- file input and output commands for Emacs  
(use-package files 
  :ensure nil
  :config
  (setq confirm-kill-processes nil;  "How to ask for confirmation when leaving Emacs. If nil, the default, don't ask at all. 
        make-backup-files nil)) 

(use-package ack
  :ensure t
  )

(use-package dimmer  ;; Dit is handig als je meerdere buffers open
  ;; hebt. Het buffer met de punt is helder, en de rest is wat gedimd.
  :ensure t
  :config (dimmer-mode t)
  (setq dimmer-fraction 0.5)
  )

(use-package material-theme
;; (use-package nyx-theme ;; wel erg fel
; (use-package zenburn-theme ;; veel te wazig met mijn kleine letters
; (use-package solarized-theme ;; te wit
; (use-package lush-theme
; (use-package spacemeacs-theme
  :ensure t
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

(use-package ivy
  :ensure t
  :defer 0.1
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  )

;; Ivy, a generic completion mechanism for Emacs.
;; Counsel, a collection of Ivy-enhanced versions of common Emacs commands.

(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode)
  )

;; (use-package swiper
;;   :ensure t
;;   :after ivy
;;   :bind (
;;          ("C-s" . swiper)
;;          ("C-r" . swiper)
;;          ;; ("C-s" . swiper-isearch)           ; op isearch zit geen autocompletion, en dat is onhandig.
;;          )
;;   )

(use-package deft
  :ensure t
  :defer
  :bind ("C-c d" . deft)
  :config
  (setq deft-extensions '("txt"))
  (setq deft-directory "~/org/deft")
  (setq deft-auto-save-interval 0)
  )


(use-package flycheck
  :ensure t
  :defer t
  ;; :init (global-flycheck-mode)
  ;;:config
  ;; (setq flycheck-global-modes '(not LaTeX-mode latex-mode) )
  ;;(add-hook 'python-mode-hook 'flycheck-mode)
  :hook(python-mode . flycheck-mode)
  :after python
  )


;; pyvenv is nodig voor elpy of zo. Weet niet waarom. In ieder geval,
;; als ik het niet laad, dan leest emacs geen file van de command prompt.
(use-package pyvenv
  :ensure t
  )

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"
        elpy-eldoc-show-current-function nil
        ;; py-set-fill-column-p t
        )
  )


(use-package blacken
  :ensure t
  :demand t
  ;; autoformat with black on save
  :hook(python-mode . blacken-mode)
  :after python
  :config
  (setq blacken-skip-string-normalization t
        blacken-line-length 90
        )
  )


;; (setq hs-minor-mode-map
;;       (let ((map (make-sparse-keymap)))
;;         ;; These bindings roughly imitate those used by Outline mode.
;;         (define-key map (kbd "C-c h C-h") 'hs-hide-block)
;;         (define-key map (kbd "C-c h C-s") 'hs-show-block)
;;         (define-key map (kbd "C-c h M-h") 'hs-hide-all)
;;         (define-key map (kbd "C-c h M-s") 'hs-show-all)
;;         (define-key map (kbd "C-c h C-l") 'hs-hide-level)
;;         (define-key map (kbd "C-c h C-c") 'hs-toggle-hiding)
;;         (define-key map [(shift mouse-2)] 'hs-mouse-toggle-hiding)
;;         map))


(use-package flyspell
  :ensure t
  :defer t
  :init  (flyspell-mode 1)
  :config
  ;;(setq ispell-dictionary "german")
  ;;   (setq ispell-dictionary   "english") ; Default dictionary to use
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "--list") ;; If you use flyspell with
  ;; aspell instead of ispell you have to add this.
  :hook((prog-mode . flyspell-mode)
        (text-mode . flyspell-mode)
        )
  )

(use-package latex ; tex-site; latex
  :defer t
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :bind (("M-q" . ales/fill-paragraph))
  :config
  (setq-default TeX-master nil ); by each new fie AUCTEX will ask for a master fie.
  (setq-default auto-fill-function nil) ;; dit moet een default zijn,
  ;; geen idee waarom.
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil              ;dont ask to save if you want to compile with C-c C-c
        TeX-PDF-mode t
        ; TeX-show-compilation nil ; todo: weet niet wat dit doet
        TeX-file-extensions '("tex" "sty")
        TeX-ispell-extend-skip-list t
        ; tex-run-command "pdflatex"
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
              ; (company-mode) ; ik geloof niet dat dit heel handig is.
              (visual-line-mode) ; hiermee kan een zin over meerdere regels lopen, zonder dat ie wordt opgehakt.
              ; (smartparens-mode) ; als ik dit opneem, wordt latex-mode niet automatisch opgeladen
              (turn-on-reftex); dit lijkt nodig om reftex te laden
              ; (flycheck-line-mode) ; todo, welk van de twee
              ; (flyspell-line-mode)
              (LaTeX-math-mode) ; hiermee staat math-mode meteen aan in math omgeving, handig voor `a -> \alpha
              (reftex-isearch-minor-mode t) ; todo, wat doet dit?
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
  (add-to-list 'LaTeX-indent-environment-list
               '("exercise" current-indentation)
               '("solution" current-indentation)
               ;; '("hint" current-indentation)
               )
  )


(use-package reftex
  :ensure t
  :defer t
  ; :commands turn-on-reftex
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


(use-package ivy-bibtex
  :ensure t
  :bind ("C-c b b" . ivy-bibtex)
  :config
  (setq bibtex-completion-bibliography
        '("~/texmf/bibtex/bib/biblio_nicky.bib" "~/texmf/bibtex/bib/foreest.bib")
        )
  (setq ivy-bibtex-default-action 'bibtex-completion-insert-citation)
  )
