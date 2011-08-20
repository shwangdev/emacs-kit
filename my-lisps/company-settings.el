;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-04-10 22:35:52 Saturday by ahei>

(require 'company)

(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(setq company-begin-commands '(self-insert-command))
(define-key company-mode-map (kbd "M-RET") 'company-expand-top)

(am-add-hooks
 `(c-mode-common-hook lisp-mode-hook emacs-lisp-mode-hook
                      java-mode-hook lisp-interaction-mode-hook sh-mode-hook
                      ,(if (not is-before-emacs-21) 'awk-mode-hook)
                      ruby-mode-hook)
 'company-mode)

(provide 'company-settings)
