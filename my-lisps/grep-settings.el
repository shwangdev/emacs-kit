;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-09-05 16:23:23 Sunday by taoshanwen>

(unless is-before-emacs-21
  (eal-define-keys-commonly
   global-map
   `(("C-x F"   find-grep-in-current-dir)
     ("C-x f"   find-grep-in-dir)
     ("C-x M-f" find-grep-current-word-in-current-dir)
     ("C-x M-F" find-grep-current-word)))
  
  (eal-define-keys
   'grep-mode-map
   `(("q"     bury-buffer)
     ("Q"     kill-this-buffer)
     ("1"     delete-other-windows)
     ("<"     beginning-of-buffer)
     (">"     end-of-buffer)
     ("'"     switch-to-other-buffer)
     ("u"     scroll-down)
     ("S-SPC" View-scroll-half-page-backward)
     ("SPC"   View-scroll-half-page-forward)
     ("/"     describe-symbol-at-point)
     ("t"     sb-toggle-keep-buffer)
     ("N"     select-buffer-forward)
     ("P"     select-buffer-backward)
     ("L"     count-brf-lines))))

(autoload 'grep-apply-setting "grep"
  "Set SYMBOL to VALUE, and update `grep-host-defaults-alist'.
SYMBOL should be one of `grep-command', `grep-template',
`grep-use-null-device', `grep-find-command',
`grep-find-template', `grep-find-use-xargs', or
`grep-highlight-matches'.")

;;;###autoload
(defun find-grep-in-dir (dir)
  "Run `find-grep' in directory DIR."
  (interactive (list (read-directory-name "Directory to find in: " default-directory "" t)))
  (let ((prompt (concat "find " dir " -type f ! -path \"*/.svn*\" ! -path \"*~\" -print0 | xargs -0 -e grep -nH -e ")))
    (set-grep-command prompt)
    (call-interactively 'find-grep)))

;;;###autoload
(defun find-grep-in-current-dir (dir)
  "Run `find-grep' in current directory."
  (find-grep-in-dir default-directory))

;;;###autoload
(defun find-grep-current-word (dir &optional is-prompt)
  "Run `grep' to find current word in directory DIR."
  (interactive
   (list
    (read-directory-name "Directory to grep in: " default-directory "" t)
    current-prefix-arg))
  (set-grep-command   (concat "find " dir " -type f ! -path \"*/.svn*\" ! -path \"*~\" -print0 | xargs -0 -e grep -nH -e "))
  (let* ((word (current-word)) command-args)
    (if (not word)
        (message "No word under cursor.")
      (setq command-args
            (if grep-find-command
                (concat grep-find-command word)
              (concat grep-command word " " dir "/*")))
      (if is-prompt
          (grep (read-shell-command "Run grep (like this): " command-args 'grep-history))
        (grep command-args)))))

;;;###autoload
(defun find-grep-current-word-in-current-dir (&optional is-prompt)
  "Run `grep' to find current word in directory DIR."
  (interactive "P")
  (find-grep-current-word default-directory is-prompt))

(defvar grep-find-prompt
  "find . -type f ! -path \"*/.svn*\" ! -path \"*~\" -print0 | xargs -0 -e grep -nH -e "
  "*Default prompt of `grep-find'.")

;;;###autoload
(defun set-grep-command (command)
  "Set `grep-command'."
  (if is-after-emacs-23
      (grep-apply-setting 'grep-find-command command)
    (setq grep-find-command command)))

;;;###autoload
(defun set-default-grep-command ()
  (set-grep-command grep-find-prompt))

;;;###autoload
(defun grep-settings ()
  "settings for `grep'."
  (set-default-grep-command)

  (defvar grep-ignore-case nil "When run `grep' ignore case or not.")

  (when grep-ignore-case
    (if is-after-emacs-23
        (grep-apply-setting 'grep-command "grep -inH -e ")
      (setq grep-command "grep -inH -e "))))

(eval-after-load "grep"
  `(grep-settings))

(provide 'grep-settings)
