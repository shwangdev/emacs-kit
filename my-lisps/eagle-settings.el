;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-04-10 22:40:48 Saturday by ahei>

(require 'util)
(require 'eagle)

(setq eagle-twitter-username "aheiii"
      eagle-show-user-images t
      eagle-save-twitter-password t)

(defun eagle-settings ()
  (make-local-variable 'hl-line-face)
  (setq hl-line-face 'hl-line-nonunderline-face)
  (setq hl-line-overlay nil)
  (color-theme-adjust-hl-line-face))

(add-hook 'eagle-mode-hook 'eagle-settings)

(apply-define-key
 eagle-mode-map
 `(("'"       switch-to-other-buffer)))

(apply-define-key
 global-map
 `(("C-x M-t" eagle-home-tc)
   ("C-x M-e" eagle-switch-to-home)))

(provide 'eagle-settings)
