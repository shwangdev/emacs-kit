;;; w3m-lnis.el --- Color Theme by ahei

;; Copyright (C) 2009 ahei

;; Author: ahei <ahei0802@126.com>
;; Keywords: color theme ahei

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; w3m-lnis stand for ISearch Link Numbers in w3m.

;;; Installation:
;;
;; Copy w3m-lnis.el to your load-path and add following statement
;; to your .emacs:
;;
;; (require 'w3m-lnis)
;;

;;; History:
;;
;; 2009-11-24
;;      * initial version 1.0.

;;; Code:

(require 'w3m-lnum)

(defgroup w3m-lnis nil
  "Group for `w3m-lnis-mode'."
  :prefix "w3m-lnis-")

(defcustom w3m-lnis-mode-line-format (propertize "LNIS" 'face 'w3m-lnis-mode-line-face)
  "Mode line format of function `w3m-lnis-mode'."
  :group 'w3m-lnis)

(defface w3m-lnis-mode-line-face
  '((((type tty pc)) :foreground "yellow" :background "magenta")
    (t (:foreground "white" :background "darkmagenta")))
  "Face used highlight `w3m-lnis-mode-line-format'.")

(defface w3m-lnis-link-numbering-face
  '((((class color) (background light)) (:foreground "gray60"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Face used to highlight link numbers."
  :group 'w3m-lnis)

(defvar w3m-lnis-link-numbers nil "Link numbers list.")

;; must do this
(put 'w3m-lnis-mode-line-format 'risky-local-variable t)

(setq minor-mode-alist
      (append
       `((w3m-lnis-mode " ") (w3m-lnis-mode ,w3m-lnis-mode-line-format))
       (delq (assq 'w3m-lnis-mode minor-mode-alist) minor-mode-alist)))

;;;###autoload
(define-minor-mode w3m-lnis-mode
  "Toggel `w3m-lnis-mode'.

  \\{w3m-lnis-mode-map}
Entry to this mode calls the value of `w3m-lnis-mode-hook'
if that value is non-nil.  \\<w3m-lnis-mode-map>"
  :group 'w3m-lnis
)

(defun w3m-lnis-link-numbering (&rest args)
  "Make overlays that display link numbers."
  (when w3m-lnis-mode
    (save-excursion
      (goto-char (point-min))
      (let ((i 0) overlay num)
        (catch 'already-numbered
          (while (w3m-goto-next-anchor)
            (add-to-list 'w3m-lnis-link-numbers (1+ i))
            (when (get-char-property (point) 'w3m-lnis-link-numbering-overlay)
              (throw 'already-numbered nil))
            (setq overlay (make-overlay (point) (1+ (point)))
                  num (format "[%d]" (incf i)))
            (w3m-static-if (featurep 'xemacs)
                (progn
                  (overlay-put overlay 'before-string num)
                  (set-glyph-face (extent-begin-glyph overlay) 'w3m-lnis-link-numbering-face))
              (w3m-add-face-property 0 (length num) 'w3m-lnis-link-numbering-face num)
              (overlay-put overlay 'before-string num)
              (overlay-put overlay 'evaporate t))
            (overlay-put overlay 'w3m-lnis-link-numbering-overlay i)))))))

(defun w3m-lnis-isearch-forward (&optional regexp-p no-recursive-edit)
  "In Dired, run `isearch-forward' but match only at file names."
  (interactive)
  (let ((isearch-search-fun-function 'w3m-lnis-isearch-search-fun-function))
    (isearch-forward regexp-p no-recursive-edit)))

(defun w3m-lnis-isearch-search-fun-function ()
  "Return the isearch function used to Isearch link numbers."
  'w3m-lnis-search-forward)

(defun w3m-lnis-search-forward (string &optional bound noerror count)
  "Search link numbers forward."
  ;; (save-excursion
  ;; ;; (w3m-goto-next-anchor)
  ;; (point)
  ;; )
  ;; (goto-char (point-min))
  ;; (string-match "3\\(4\\)" "01234567890123456789")
  ;; (let ((inhibit-changing-match-data t))
  ;; (string-match "3" "1111111111111111111111111111111111111111111111111111113"))
  ;; (set-match-data '(15 18))
  (let ((point (+ 50 (point))))
  ;; (let ((point (if bound (+ 5 bound) (point))))
  (set-match-data (list point (+ 3 point)))
  ;; (set-match-data '(15 18 28 47))
  ;; (set-match-data '(15 18 20 25 38))
  ;; (setq last-data )
  ;; (replace-match-data nil nil '(15 18 20 25 38))
  ;; (replace-match-data nil nil '(15 18))
  ;; (set-match-data '(30 80 110 220 ))
  ;; (beginning-of-buffer)
  ;; (forward-word)
  ;; (point)
  (goto-char (+ 3 point)))
  ;; (+ 3 point))
  ;; 18
  ;; (message "%s" count)
  ;; (message "%d" count)
  ;; (sleep-for 3)
  ;; (+ 18 count)
  ;; (if count (+ 18 count) 18)
  ;; (search-forward string bound noerror count))
)

(provide 'w3m-lnis)

;;; w3m-lnis.el ends here
