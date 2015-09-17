;;; packages.el --- evil-monkey Layer packages File for Spacemacs
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

;; Redefining EVIL to be more Paredit-friendly
;; THIS IS GROSS AND WRONG. I know just enough elisp to know that this should be a minor mode, but not enough to make it one.


(defvar evil-monkey-packages
  '( )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun evil-monkey--init-evil () ;; hack to be able to call this function directly for initialization purposes
  (load "~/.spacemacs.d/layers/evil-monkey/cofi-util")
  (require 'cofi-util)
  (evil-define-operator evil-delete (beg end type register yank-handler)
    "Altered to respect paredit! -JHH
   Delete text from BEG to END with TYPE.
   Save in REGISTER or in the kill-ring with YANK-HANDLER."
    (interactive "<R><x><y>")
    (unless register
      (let ((text (filter-buffer-substring beg end)))
        (unless (string-match-p "\n" text)
          ;; set the small delete register
          (evil-set-register ?- text))))
    (let ((evil-was-yanked-without-register nil))
      (evil-yank beg end type register yank-handler))
    (cond
     ((eq type 'block)
      (evil-apply-on-block #'paredit-delete-region beg end nil))
     ((and (eq type 'line)
           (= end (point-max))
           (or (= beg end)
               (/= (char-before end) ?\n))
           (/= beg (point-min))
           (=  (char-before beg) ?\n))
      (paredit-delete-region (1- beg) end))
     (t
      (paredit-delete-region beg end)))
    ;; place cursor on beginning of line
    (when (and (evil-called-interactively-p)
               (eq type 'line))
      (evil-first-non-blank)))

  (fill-keymap evil-normal-state-map
               "Y" (kbd "y$")
               "+" 'evil-numbers/inc-at-pt
               "-" 'evil-numbers/dec-at-pt
               "go" 'goto-char
               "C-t" 'transpose-chars
               "C-:" 'eval-expression
               "C-u" 'evil-scroll-up
               "D" 'paredit-kill
               "C" (lambda (arg) (interactive "P") (paredit-kill arg) (evil-insert arg))
               "I" (lambda (arg) (interactive "P") (move-beginning-of-line arg) (evil-insert arg)))
  (fill-keymap evil-motion-state-map
               "y" 'evil-yank
               "Y" (kbd "y$")
               "_" 'evil-first-non-blank
               "C-e" 'end-of-line
               "C-S-d" 'evil-scroll-up
               "C-S-f" 'evil-scroll-page-up
               "C-y" nil)

  ;; Return some Emacs conveniences to insert mode
  (define-key evil-insert-state-map (kbd "C-k") 'paredit-kill)
  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-scroll-line-up)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-scroll-line-down)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)

  ;; I don't know why this is necessary:
  (define-key evil-insert-state-map (kbd "C-w") 'evil-delete-backward-word)

  ;; move between windows like a civilized fucking human being
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)

  ;; remap k to gk and j to gj
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "gk") 'evil-previous-line)
  (define-key evil-normal-state-map (kbd "gj") 'evil-next-line)

  (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "gk") 'evil-previous-line)
  (define-key evil-motion-state-map (kbd "gj") 'evil-next-line)

  ;; remap SPC and RET so that they won't be active in evil mode (since they just duplicate j and l)
  (defun my-move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another, deleting from the old location. "
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil))
  (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
  (my-move-key evil-motion-state-map evil-normal-state-map " ")

  ;; Show violet box if in Emacs mode
  (setq evil-emacs-state-cursor  '("violet" box))
  (setq evil-normal-state-cursor '("gray" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("gray" bar))
  (setq evil-motion-state-cursor '("gray" box)))

(evil-monkey--init-evil)

(defvar evil-monkey-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function evil-monkey/init-<package-evil-monkey>
;;
;; (defun evil-monkey/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
