;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-04-10 20:14:22 Saturday by ahei>

(require 'full-ack)

(defun full-ack-settings ()
  "settings for `full-ack'."
  (setq ack-context 0)
  (setq ack-executable (executable-find "ack-grep"))
  (setq ack-heading nil)
  (setq ack-prompt-for-directory t)

  (defun ack-arguments-from-options (regexp)
    (let ((arguments (list "--color"
                           (ack-option "smart-case" (eq ack-ignore-case 'smart))
                           (ack-option "heading" ack-heading)
                           (ack-option "env" ack-use-environment))))
      (unless ack-ignore-case
        (push "-i" arguments))
      (unless regexp
        (push "--literal" arguments))
      (if (> ack-context 0)
          (push (format "--context=%d" ack-context) arguments))
      arguments))

  (eal-define-keys
   'ack-mode-map
   `(("j"   next-line)
     ("k"   previous-line)
     ("h"   backward-char)
     ("l"   forward-char)
     ("u"   View-scroll-half-page-backward)
     ("SPC" View-scroll-page-forward)
     ("o"   other-window)
     ("g"   beginning-of-buffer)
     ("G"   end-of-buffer)
     (">"   end-of-buffer)
     ("<"   beginning-of-buffer)
     ("1"   delete-other-windows)
     ("'"   switch-to-other-buffer)
     ("Q"   kill-this-buffer))))

(eval-after-load "full-ack"
  `(full-ack-settings))

(provide 'full-ack-settings)
