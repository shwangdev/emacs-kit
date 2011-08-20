;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-04-10 22:40:05 Saturday by ahei>

(setq todochiku-command
      (case system-type 
        (windows-nt (concat my-emacs-lisps-path "snarl_command.exe"))
        (darwin "/usr/local/bin/growlnotify")
        (t "/usr/bin/notify-send")))

(require 'todochiku)

(let ((non-exist (not (file-exists-p todochiku-command))))
  (setq todochiku-tooltip-too (and non-exist window-system))
  (setq todochiku-message-too (and (or non-exist (not window-system)) (not todochiku-tooltip-too))))

(setq todochiku-icons-directory (concat my-emacs-lisps-path "todochiku/icons"))

(setq todochiku-timeout 10)

(defun todochiku-get-arguments (title message icon)
  "Gets todochiku arguments.
This would be better done through a customization probably."
  (case system-type
    ('windows-nt (list "/M" title message icon "/T" (int-to-string todochiku-timeout)))
    ('darwin (list title "-m" message "--image" icon ))
    (t (list "-i" icon "-t" (int-to-string (* 1000 todochiku-timeout)) title message))))

(provide 'todochiku-settings)
