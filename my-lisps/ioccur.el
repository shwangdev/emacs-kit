;; -*- Emacs-Lisp -*-

;; Time-stamp: <09/18/2009 23:29:05 ÐÇÆÚÎå by ahei>

(require 'anything)

(defun anything-delayed-occur-candidates ()
  (setq anything-occur-buf (get-buffer-create "*Anything Occur*"))
  (with-current-buffer anything-occur-buf
    (erase-buffer)
    (let ((count (occur-engine anything-pattern
                               (list anything-c-source-occur-current-buffer)
                               anything-occur-buf
                               list-matching-lines-default-context-lines
                               (if (and case-fold-search search-upper-case)
                                   (isearch-no-upper-case-p anything-pattern t)
                                 case-fold-search)
                               list-matching-lines-buffer-name-face
                               nil list-matching-lines-face
                               (not (eq occur-excluded-properties t)))))
      (when (> count 0)
        (let ((lines (split-string (buffer-string) "\n" t)))
          (cdr lines))))))

(defvar anything-c-source-delayed-occur
  '((name . "Occur")
    (init . (lambda ()
              (setq anything-c-source-occur-current-buffer
                    (current-buffer))))
    (candidates . anything-delayed-occur-candidates)
    (action . (("Goto line" .
                (lambda (candidate)
                  (goto-line (string-to-number candidate)
                             anything-c-source-occur-current-buffer)))))
    (requires-pattern . 1)
    (volatile)
    (delayed)))

(defun ioccur ()
  (interactive)
  (anything 'anything-c-source-delayed-occur nil nil nil nil "*Anything Occur*"))

(provide 'ioccur)
