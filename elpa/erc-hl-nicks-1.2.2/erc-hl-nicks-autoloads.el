;;; erc-hl-nicks-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (erc-hl-nicks) "erc-hl-nicks" "erc-hl-nicks.el"
;;;;;;  (20360 14676))
;;; Generated autoloads from erc-hl-nicks.el

(autoload 'erc-hl-nicks "erc-hl-nicks" "\
Retrieves a list of usernames from the server and highlights them

\(fn)" nil nil)

(eval-after-load 'erc '(add-to-list 'erc-modules 'hl-nicks t))

(when (and (boundp 'erc-modules) (not (member 'hl-nicks 'erc-modules))) (add-to-list 'erc-modules 'hl-nicks))

;;;***

;;;### (autoloads nil nil ("erc-hl-nicks-pkg.el") (20360 14676 532504))

;;;***

(provide 'erc-hl-nicks-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; erc-hl-nicks-autoloads.el ends here
