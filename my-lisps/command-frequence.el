;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-04-08 18:30:48 Thursday by ahei>

;;; Commentary:

;; 修改自command-frequency.el, 记得当时这个扩展是从水木下载下来的

(require 'desktop)
(require 'display-line-number)

(defvar cf-frequence-threshrold 1 "*显示命令报告的时候命令频率大于等于该值的时候才显示")
(defvar cf-stat-self-insert-command nil "*Non-nil means also statistic `self-insert-command'")
(defvar cf-buffer-name "*command frequence*" "the name of buffer command frequence")

(defvar cf-command-history nil "命令频率列表")

(defun cf-add-command ()
  (when (and last-command
             (or cf-stat-self-insert-command (not (equal last-command 'self-insert-command))))
    (let ((cmd (assoc last-command cf-command-history)))
      (if cmd
          (setcdr cmd (1+ (cdr cmd)))
        (add-to-list 'cf-command-history (cons last-command 1))))))

(defun command-frequence ()
  (interactive)
  (with-current-buffer (get-buffer-create cf-buffer-name)
    (linum-mode t)
    (View-quit)
    (erase-buffer)
    (let ((cmds (copy-sequence cf-command-history)) (all 0))
      (dolist (c cmds)
        (setq all (+ all (cdr c))))
      (insert (format "命令总次数: %d" all))
      (unless cf-stat-self-insert-command
        (insert "(不包括命令`self-insert-command')"))
      (insert "\n\n")
      (insert (format "%-5s %-5s %-30s %s\n" "次数" "频率" "命令" "按键"))
      (dolist (c (sort cmds (lambda (c1 c2) (> (cdr c1) (cdr c2)))))
        (unless (< (cdr c) cf-frequence-threshrold)
          (insert (format "%-5d %.3f %-30S %s\n" (cdr c) (/ (cdr c) (float all)) (car c)
                          (mapconcat 'key-description (where-is-internal (car c)) ", ")))))
      (goto-char (point-min))
      (setq major-mode 'emacs-lisp-mode)
      (setq mode-name "Emacs-Lisp")
      (use-local-map emacs-lisp-mode-map)
      (view-mode t)
      (switch-to-buffer (current-buffer)))))

(defun cf-clear-command-history ()
  "清除命令历史"
  (interactive)
  (setq cf-command-history nil))

(add-hook 'post-command-hook 'cf-add-command)
(add-to-list 'desktop-globals-to-save 'cf-command-history)

(provide 'command-frequence)
