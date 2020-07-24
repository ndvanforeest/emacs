(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
   '("melpa" . "https://melpa.org/packages/")
   '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))


