(require 'package)
; (setq package-enable-at-startup nil)

(setq package-achives '(("melpa" . "https://melpa.org/packages/")
                        ("org" . "https://orgmode.org/elpa/")
                        ("elpy" . "http://jorgenschaefer.github.io/packages/")
                        '("htmlize" . "https://github.com/hniksic/emacs-htmlize")
                        ("elpa" . "https://elpa.gnu.org/packages")))

;; htlmlize is  needed by nikola to render code nicely

; (add-to-list 'package-archives '
;; (add-to-list 'package-archives '("org" . "https://stable.melpa.org/elpa/") t)


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;(require 'use-package-ensure)
(require 'use-package)
(setq use-package-always-ensure t)
; (use-package-report)
(setq use-package-verbose t)

; i don't know whether I want this.
; (setq use-package-always-defer t)


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))


; (use-package no-littering)
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
