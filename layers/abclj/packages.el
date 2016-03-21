;;; packages.el --- abclj (always be clojing) Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 Justin Holguin
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Justin Holguin <justin.h.holguin@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar abclj-packages
  '(
    ;; package abcljs go here
    clj-refactor
    clojure-mode
    slamhound
    expand-region
    paredit
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init functions

(defun abclj/post-init-clj-refactor ()
  "Initialize clj-refactor"
  (require 'clj-refactor)
  (add-hook 'always-be-clojing-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-r"))))

(defun abclj/init-slamhound ()
  nil)

(defun abclj/init-expand-region ()
  (use-package expand-region
    :config
    (progn
      (global-set-key (kbd "C-=") 'er/expand-region))))

(defun abclj/post-init-paredit ()
  (enable-paredit-mode)
  (defun disable-paredit-backslash ()
    (local-set-key [remap paredit-backslash]
                   (lambda ()
                     (interactive)
                     (insert "\\"))))
  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other functions

(defun current-datestamp ()
  ;; 2014 01 14 13 17 03
  ;;   %Y %m %d %H %M %S
  (let ((datestamp-format "%Y%m%d%H%M%S"))
    (format-time-string datestamp-format (current-time))))

(defun create-migration (name directory)
  (interactive "sMigration name: \nDWhere?")
  (let* ((base (concat (current-datestamp) "-" name))
         (up (concat base ".up.sql"))
         (down (concat base ".down.sql")))
    (shell-command (concat "touch " up))
    (shell-command (concat "touch " down))
    (message "Created %s and %s" up down)
    (find-file up)))

(defun helm-clojure-headlines ()
  "Display headlines for the current Clojure file."
  (interactive)
  (setq helm-current-buffer (current-buffer)) ;; Fixes bug where the current buffer sometimes isn't used
  (jit-lock-fontify-now) ;; https://groups.google.com/forum/#!topic/emacs-helm/YwqsyRRHjY4
  (helm :sources (helm-build-in-buffer-source "Clojure Headlines"
                   :data (with-helm-current-buffer
                           (goto-char (point-min))
                           (cl-loop while (re-search-forward "^(\\|testing\\|^;.*[a-zA-Z]+" nil t)
                                    for line = (buffer-substring (point-at-bol) (point-at-eol))
                                    for pos = (line-number-at-pos)
                                    collect (propertize line 'helm-realvalue pos)))
                   :get-line 'buffer-substring
                   :action (lambda (c) (helm-goto-line c)))
        :buffer "helm-clojure-headlines"))

(defun remove-spyscope-traces (start end)
  "Remove all #spy/... calls from the region/buffer.
  Cribbed from http://wikemacs.org/wiki/Emacs_Lisp_Cookbook#Scripted_Use"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char 1)
    (let ((case-fold-search nil))
      (while (search-forward-regexp "#spy/[pdt] " nil t)
        (replace-match ""
                       t nil)))))

(defun register-align-let! ()
  (fset 'align-let
        [?v ?i ?\[ ?\M-x ?a ?l ?i ?g ?n ?- ?r ?e ?g ?e ?x ?p return ?\( return ?= ?a ?p]))

(defun abclj--other-ns (ns)
  "If `ns' ends in `-test', strip that off. Otherwise, add `-test'."
  (if (string-match-p "-test$" ns)
      (string-remove-suffix "-test" ns)
    (concat ns "-test")))

(defun abclj-jump-to-test-ns-or-back (&optional arg)
  "If the point is in `project.namespace', jump to `project.namespace-test'. If
the point is in `project.namespace-test', jump to `project.namespace'."
  (interactive "P")
  (let ((target-ns (abclj--other-ns (cider-current-ns))))
    (cider-find-ns arg target-ns)))

(defvar abclj-excluded-packages '()
  "List of packages to exclude.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode always-be-clojing-mode
  "Additional customizations for Clojure by Justin Holguin."
  :lighter " ABClj"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c h") 'helm-clojure-headlines)
            (define-key map (kbd "C-c C-d") 'ac-cider-popup-doc)
            (define-key map (kbd "C-x 3") (lambda (&args) (interactive "P") (split-window-horizontally) (helm-projectile)))
            (define-key map (kbd "C-x 2") (lambda (&args) (interactive "P") (split-window-vertically) (helm-projectile)))
            map)
  ;(evil-leader/set-leader "<SPC>")

  (evil-leader/set-key-for-mode 'clojure-mode "h"   #'helm-clojure-headlines)
  (evil-leader/set-key-for-mode 'clojure-mode "m s p" (lambda (&args) (interactive "P") (insert "#spy/p ")))
  (evil-leader/set-key-for-mode 'clojure-mode "m s d" (lambda (&args) (interactive "P") (insert "#spy/d ^{:marker \"\"} ") (backward-char 3)))
  (evil-leader/set-key-for-mode 'clojure-mode "m s t" (lambda (&args) (interactive "P") (insert "#spy/t ")))

  ;; (local-unset-key "M-.")
  ;; (local-unset-key "M-,")
  ;; (fill-keymap evil-normal-state-local-map
  ;;              "M-." 'cider-jump-to-var
  ;;              "M-," 'cider-jump-back
  ;;              "C-c h" 'helm-clojure-headlines)
  ;; (define-key evil-normal-state-local-map (kbd "M-.") 'cider-jump-to-var)
  (define-key evil-insert-state-local-map (kbd "RET") 'paredit-newline)

  ;; other customizations
  ;;
  ;; break -> and ->> so Dan will be happy
  (define-clojure-indent
    (try 1)
    (try+ 1)
    (->  1)
    (->> 1)
    (GET 1)
    (PUT 1)
    (POST 1)
    (DELETE 1)
    (prop/for-all 1)
    (tc/quick-check 1))

  (put-clojure-indent 'prop/for-all 1)
  (put-clojure-indent 'm/match 1)
  (put-clojure-indent 'match 1)
  (put-clojure-indent 'tc/quick-check 1)

  ;; Use the :repl profile. If you want to add/remove Leiningen
  ;; profiles to/from CIDER, do it in the string below (in the concat expression)
  ;; NOTE: if you don't have a :repl profile defined, Leiningen will
  ;; emit a warning when you call cider-jack-in. As far as I can tell,
  ;; this won't prevent CIDER from working
  (setq cider-lein-parameters
        (if (string-match-p "with-profile" cider-lein-parameters)
            cider-lein-parameters
          (concat "with-profile +repl " cider-lein-parameters))))



;; cribbed from http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(add-hook 'always-be-clojing-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|BREAK\\|BLOCK\\|TODO\\|BUG\\)" 1 font-lock-warning-face t)))))

(add-hook 'always-be-clojing-mode-hook
          #'enable-paredit-mode)


(add-hook 'clojure-mode-hook 'always-be-clojing-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)

(provide 'always-be-clojing-mode)
;; For each package, define a function abclj/init-<package-abclj>
;;
;; (defun abclj/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
