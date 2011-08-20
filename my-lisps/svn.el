;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-11-20 23:18:19 Saturday by taoshanwen>

(defun svn-dired-jump ()
  "Jump to a dired buffer, containing the file at point."
  (interactive)
  (let* ((line-info (svn-status-get-line-information))
         (file-full-path
          (if line-info
              (setq file-full-path (svn-status-line-info->full-path line-info))))
         (default-directory
           (if line-info
               (file-name-as-directory
                (expand-file-name (svn-status-line-info->directory-containing-line-info line-info t)))
             default-directory)))
    (dired-jump)
    (if line-info
        (dired-goto-file file-full-path))))

(defun svn-delete-files ()
  "Delete selected file(s).
See `svn-status-marked-files' for what counts as selected."
  (interactive)
  (let* ((marked-files (svn-status-marked-files))
         (fnames (mapcar 'svn-status-line-info->full-path marked-files))
         (num-of-files (length marked-files)))
    (when (yes-or-no-p
           (if (= 1 num-of-files)
               (format "Remove %s? " (svn-status-line-info->filename (car marked-files)))
             (format "Remove %d files? " num-of-files)))
      (message "removing: %S" (svn-status-marked-file-names))
      (mapc 'delete-file fnames)
      (call-interactively 'svn-status-update))))

(defun svn-cleanup (files)
  "Run `svn cleanup' on files FILES."
  (interactive "P")
  (if files
      (progn
        (message "svn-status-cleanup %S" files)
        (svn-run-svn t t 'cleanup (append (list "cleanup") files)))
    (message "No valid file selected - No status cleanup possible")))

(defun svn-cleanup-current-dir ()
  "Run `svn cleanup' on curent directory."
  (interactive)
  (svn-cleanup `(,default-directory)))

(defun svn-create-arg-file (file-name prefix files postfix)
  (with-temp-file file-name
    (insert prefix)
    (let ((temp-files files))
      (while temp-files
        (insert (car temp-files))
        (insert "\n")
        (setq temp-files (cdr temp-files)))
      (insert postfix))))

(defun svn-scroll-half-page-backward ()
  "Scroll backward a \"half page\" lines in `svn-status-mode'.
See also `svn-scroll-half-page-forward'."
  (interactive)
  (call-interactively 'View-scroll-half-page-backward)
  (goto-char (+ (svn-point-at-bol) svn-status-default-column)))

(defun svn-scroll-half-page-forward ()
  "Scroll forward a \"half page\" lines in `svn-status-mode'.
See also `svn-scroll-half-page-backward'."
  (interactive)
  (call-interactively 'View-scroll-half-page-forward)
  (goto-char (+ (svn-point-at-bol) svn-status-default-column)))

(defun svn-scroll-up ()
  "Scroll up in `svn-status-mode'.
See also `svn-scroll-down'."
  (interactive)
  (call-interactively 'scroll-up)
  (goto-char (+ (svn-point-at-bol) svn-status-default-column)))

(defun svn-scroll-down ()
  "Scroll down in `svn-status-mode'.
See also `svn-scroll-down'."
  (interactive)
  (call-interactively 'scroll-down)
  (goto-char (+ (svn-point-at-bol) svn-status-default-column)))

(provide 'svn)
