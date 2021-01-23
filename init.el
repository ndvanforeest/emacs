(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
   '("melpa" . "https://melpa.org/packages/")
   '("elpy" . "http://jorgenschaefer.github.io/packages/"))


(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives '("org" . "https://stable.melpa.org/elpa/") t)

;; This is needed by nikola to render code nicely
(add-to-list 'package-archives
   '("htmlize" . "https://github.com/hniksic/emacs-htmlize"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)



(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))


