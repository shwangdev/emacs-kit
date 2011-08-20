;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-05-21 18:05:46 Friday by ahei>

(defun facemenup-palette-face-at-point (&optional is-bg)
  "Open palette and set face at point to face which to be changed."
  (interactive "P")
  (let (face (hl-line hl-line-mode))
    (if hl-line
        (hl-line-mode -1))
    (setq face (eyedrop-face-at-point))
    (if hl-line
        (hl-line-mode 1))
    (if face
        (facemenup-palette-face face is-bg)
      (call-interactively 'facemenup-palette-face))))

(defun facemenup-palette-face (face &optional is-bg)
  "Open palette and set face FACE to face which to be changed."
  (interactive
   (list
    (read-face-name
     (concat "Face whose " (if current-prefix-arg "background" "foreground") " to change"))
    current-prefix-arg))
  (setq facemenup-palette-change-face-bg is-bg)
  (let ((color
         (if is-bg
             (facemenup-face-bg face)
           (facemenup-face-fg face))))
    (setq facemenup-last-face-bg (facemenup-face-bg face))
    (setq facemenup-last-face-fg (facemenup-face-fg face))
    (setq facemenup-last-face-color color)
    (setq facemenup-last-face-changed face)
    (when facemenup-palette-update-while-editing-flag
      (palette-set-face-changed-to-foreground is-bg))
    (condition-case nil
        (palette color)
      (quit (set-face-foreground face color)))))

(provide 'palette-misc)
