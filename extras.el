(eval-when-compile (require 'cl))

(defun nice-cider-repl ()
  (interactive)
  (split-window-below-and-focus)
  (window-resize (get-buffer-window) -15 nil)
  (windmove-up)
  (cider-switch-to-repl-buffer 't)
  (cider-switch-to-last-clojure-buffer))

(defun configure-racket ()
  (setq racket-racket-program "/Applications/Racket v6.2.1/bin/racket")
  (setq racket-raco-program "/Applications/Racket v6.2.1/bin/raco"))

(defun use-ido ()
  (spacemacs/set-leader-keys "ff" 'ido-find-file)
  (global-set-key (kbd "C-x C-f") 'ido-find-file))

(defun configure-c++ ()
  (set-variable 'ycmd-server-command '("python2.7" "/Users/justin/src/ycmd/ycmd"))
  (set-variable 'ycmd-extra-conf-whitelist '("~/src/*")))

;; (defun configure-org-babel ()
;;   (setq org-src-fontify-natively t)
;;   (setq org-src-tab-acts-natively t)
;;   (setq org-confirm-babel-evaluate nil)
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '(
;;      (emacs-lisp . t)
;;      (clojure . t)
;;      (python . t)
;;      (haskell . t)
;;      (ruby . t)
;;      (C . t)
;;      )))

(defun configure-clojure ()
  ;; (setq clojure-enable-fancify-symbols t) ;; adding this directly to layer instantiation
  ;; (put 'prop/for-all 'clojure-backtracking-indent '(4 (2)))
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-connected-hook #'cider-refresh)
  (add-hook 'cider-connected-hook #'nice-cider-repl))

;; M-u to toggle transparency
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(90 85))))

(defun activate-linum ()
  (setq linum-relative-current-symbol "") ;; just show the current (absolute) line number
  (linum-relative-toggle)
  (global-linum-mode))

(defun extend-monokai ()
    ;; linum-relative doesn't provide a nice way to customize this.
  (set-face-attribute 'fringe nil :background "grey11")
  (set-face-attribute 'linum nil :background "black")
  (add-to-list 'default-frame-alist '(background-color . "black"))
  (defface linum-relative-current-face
    '((t :inherit linum :foreground "DarkOrange2" :background "grey11" :weight bold))
    "Face for displaying current line."
    :group 'linum-relative))

(defun refine-scrolling ()
  ;; Disable overeager scrolling
  (setq mouse-wheel-follow-mouse 't)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq redisplay-dont-pause t
        scroll-margin 1
        scroll-step 1
        scroll-conservatively 10000
        scroll-preserve-screen-position 1))

(defun sensible-splits ()
  (defadvice split-window (after move-point-to-new-window activate)
    "Moves the point to the newly created window after splitting."
    (other-window 1)))

(defun configure-elm ()
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-elm)))
