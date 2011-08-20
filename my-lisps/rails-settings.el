;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-11-26 13:41:34 Friday by taoshanwen>

(require 'rails)
(require 'two-mode-mode)
(require 'rhtml-mode)

(eal-define-keys
 `(rhtml-mode-map)
 `(("TAB" complete-or-indent-for-ruby)))

(autoload 'rhtml-minor-mode "rhtml-minor-mode"
  "Minor mode for .rhtml files"
  t)
(defun rhtml-modes ()
  (two-mode-mode)
  (rhtml-minor-mode))
(setq auto-mode-alist (cons '("\\.rhtml$" . rhtml-modes) auto-mode-alist))

(eal-define-keys
 'compilation-mode-map
 `(("u" 'View-scroll-page-backward)))

(setq auto-mode-alist (cons '("\\.jsp$" . java-mode) auto-mode-alist))

(autoload 'css-mode "css-mode"
  "Major mode for editing CSS style sheets.
\\{cssm-mode-map}"
  t)
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))

(provide 'rails-settings)
