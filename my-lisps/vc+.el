;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-11-21 14:41:49 Sunday by taoshanwen>

(require 'vc)

(defvar vc-rename-disp-current-file nil "Display current file name when vc rename file.")

;;;###autoload
(defun vc-rename-current-file (new)
  "VC rename current file to NEW."
  (interactive
   (list (read-file-name
          (format "VC rename %s to: " (if vc-rename-disp-current-file (buffer-file-name) "current file")))))
  (vc-rename-file (buffer-file-name) new))

;;;###autoload
(defun vc-rename ()
  "VC rename."
  (interactive)
  (if (buffer-file-name)
      (call-interactively 'vc-rename-current-file)
    (call-interactively 'vc-rename-file)))

;;;###autoload
(defun vc-delete ()
  "VC delete."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (vc-delete-file file)
      (call-interactively 'vc-delete-file))))

(defvar vc-ediff-restore-window t "*After `ediff' in vc, restore window or not.")
(defvar vc-ediff-old-buffer nil "Old buffer when run `ediff' in vc.")
(defvar vc-ediff-buffers-to-kill nil "Buffers to kill when run `ediff' in vc.")

;;;###autoload
(defun vc-ediff-startup-hook ()
  (add-hook 'ediff-after-quit-hook-internal 'vc-ediff-restore-window))

;;;###autoload
(defun vc-ediff-restore-window ()
  (switch-to-buffer vc-ediff-old-buffer)
  (dolist (buffer vc-ediff-buffers-to-kill)
    (kill-buffer buffer))
  (delete-other-windows))

;;;###autoload
(defun vc-version-ediff (file rev1 rev2)
  "Run `ediff' between revision REV1 and revision REV2 of the file FILE in the repository history."
  (interactive "fFile to ediff: \nsRevision1: \nsRevision2: ")
  (when (string= rev1 "") (setq rev1 nil))
  (when (string= rev2 "") (setq rev2 nil))
  (let* ((backend (car (vc-deduce-fileset t))))
    (unless rev1
      (cond
       ;; if the file is not up-to-date, use working revision as older revision
       ((not (vc-up-to-date-p file))
        (setq rev1 (vc-working-revision file)))
       ;; if the file is not locked, use last and previous revisions as defaults
       (t
        (setq rev1 (vc-call-backend backend 'previous-revision file
                                    (vc-working-revision file)))
        (when (string= rev1 "") (setq rev1 nil))))))
  (when (and (not rev1) rev2)
    (error "Not a valid revision range."))
  (let* ((backend (car (vc-deduce-fileset t)))
         (buffer1 (vc-find-revision file rev1))
         (buffer2 (if rev2 (vc-find-revision file rev2) (current-buffer)))
         (hook (if vc-ediff-restore-window '(vc-ediff-startup-hook) nil)))
    (setq vc-ediff-old-buffer (current-buffer))
    (if rev2
        (setq vc-ediff-buffers-to-kill (list buffer1 buffer2))
      (setq vc-ediff-buffers-to-kill (list buffer1)))
    (ediff-buffers buffer1 buffer2 hook)))

;;;###autoload
(defun vc-ediff (rev1 rev2)
  "Run `ediff' between revision REV1 and revision REV2 on current file."
  (interactive
   (let* ((vc-fileset (vc-deduce-fileset t)) ;FIXME: why t?  --Stef
          (files (cadr vc-fileset))
          (backend (car vc-fileset))
          (first (car files))
          (completion-table
           (vc-call-backend backend 'revision-completion-table files))
          (rev1-default nil)
          (rev2-default nil))
     (cond
      ;; if the file is not up-to-date, use working revision as older revision
      ((not (vc-up-to-date-p first))
       (setq rev1-default (vc-working-revision first)))
      ;; if the file is not locked, use last and previous revisions as defaults
      (t
       (setq rev1-default (vc-call-backend backend 'previous-revision first
                                           (vc-working-revision first)))
       (when (string= rev1-default "") (setq rev1-default nil))
       (setq rev2-default (vc-working-revision first))))
     ;; construct argument list
     (let* ((rev1-prompt (if rev1-default
                             (concat "Older revision (default " rev1-default "): ")
                           "Older revision: "))
            (rev2-prompt (concat "Newer revision (default "
                                 (or rev2-default "workfile") "): "))
            (rev1 (if completion-table
                      (completing-read rev1-prompt completion-table
                                       nil nil nil nil rev1-default)
                    (read-string rev1-prompt nil nil rev1-default)))
            (rev2 (if completion-table
                      (completing-read rev2-prompt completion-table
                                       nil nil nil nil rev2-default)
                    (read-string rev2-prompt nil nil rev2-default))))
       (when (string= rev1 "") (setq rev1 nil))
       (when (string= rev2 "") (setq rev2 nil))
       (list rev1 rev2))))
  (vc-version-ediff buffer-file-name rev1 rev2))

;;;###autoload
(defun vc-ediff-with-prev-rev ()
  "Run `ediff' between workfile and previous revision."
  (interactive)
  (if (vc-up-to-date-p buffer-file-name)
      (message "Current file is up-to-date.")
    (vc-ediff nil nil)))

;;;###autoload
(defun vc-revert-update-modeline ()
  "Call `vc-revert', and then `force-mode-line-update'."
  (interactive)
  (call-interactively 'vc-revert)
  (svn-status-update-modeline))

;;;###autoload
(defun vc-checkout-working-revision ()
  "Checkout working revision."
  (interactive)
  (if (vc-up-to-date-p buffer-file-name)
      (message "Current file is up-to-date.")
    (let* ((vc-fileset (vc-deduce-fileset t)) ;FIXME: why t?  --Stef
           (files (cadr vc-fileset))
           (backend (car vc-fileset))
           (first (car files))
           (rev (vc-working-revision first)))
      (switch-to-buffer (vc-find-revision buffer-file-name rev)))))

;;;###autoload
(defun vc-update-and-revert-buffer ()
  (interactive)
  (vc-update)
  (revert-buffer-no-confirm))

(provide 'vc+)
