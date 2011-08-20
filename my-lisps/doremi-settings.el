;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-04-10 14:20:21 Saturday by ahei>

(defun doremi-settings ()
  "settings of `doremi'."
  (setq doremi-up-keys   '(?\M-p ?p ?k))
  (setq doremi-down-keys '(?\M-n ?n ?j ?\ ))

  (setq doremi-RGB-increment-factor 256))

(eval-after-load "icicles"
  `(doremi-settings))

(provide 'doremi-settings)
