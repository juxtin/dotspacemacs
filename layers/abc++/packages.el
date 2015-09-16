;;; packages.el --- abc++ Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq abc++-packages
    '(
      cc-mode
      ))

;; List of packages to exclude.
(setq abc++-excluded-packages '())

;; For each package, define a function abc++/init-<package-name>
;;
(defun abc++/init-cc-mode ()
  "Initialize abc++"
  (use-package cc-mode
    :defer t
    :init
    (progn
      (load (expand-file-name "~/.emacs.d/private/abc++/config.el"))
      (defun abc++-hook ()
        (c-add-style "Google" google-c-style t)
        (c-set-style "Google")
        (c-toggle-hungry-state 1))
      (add-hook 'c++-mode-hook 'abc++-hook))))
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
