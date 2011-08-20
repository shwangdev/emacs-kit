(defun line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
Counting starts at (point-min), so the value refers
to the contents of the accessible portion of the buffer."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun looking-back (regexp &optional limit greedy)
  "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying a minimum
starting position, to avoid checking matches that would start
before LIMIT.

If GREEDY is non-nil, extend the match backwards as far as possible,
stopping when a single additional previous character cannot be part
of a match for REGEXP."
  (let ((start (point))
        (pos
         (save-excursion
           (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
                (point)))))
    (if (and greedy pos)
        (save-restriction
          (narrow-to-region (point-min) start)
          (while (and (> pos (point-min))
                      (save-excursion
                        (goto-char pos)
                        (backward-char 1)
                        (looking-at (concat "\\(?:"  regexp "\\)\\'"))))
            (setq pos (1- pos)))
          (save-excursion
            (goto-char pos)
            (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
    (not (null pos))))

(provide 'for-emacs-21)
