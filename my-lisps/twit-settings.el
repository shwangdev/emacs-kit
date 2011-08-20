;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-04-10 22:40:43 Saturday by ahei>

(require 'twit)

(customize-set-variable 'twit-user "ahei0802")
(setq twit-show-user-images t)
(add-hook 'twit-new-tweet-hook 'twit-todochiku)

(apply-define-key
 twit-status-mode-map
 `(("C-k"     twit-remove-friend)
   ("a"       twit-add-friend)
   ("A"       twit-analyse-user)
   ("S"       twit-search)
   ("G"       twit-analyse-graph-user)
   ("g"       eagle-show-recent-tweets-async)
   ("n"       twit-next-tweet)
   ("p"       twit-previous-tweet)
   
   ("d"       twit-direct)
   ("N"       twit-post)
   ("t"       twit-post-to)
   ("C-c n"   twit-post-url)
   ("r"       twit-post-loud-reply)
   ("R"       twit-post-reply)
   ("c"       twit-post-retweet)

   ("C-c C-a" eagle-show-at-tweets)
   ("'"       switch-to-other-buffer)
   ("SPC"     scroll-up)
   ("u"       View-scroll-page-backward)
   ("e"       move-end-of-line)
   ("f"       twit-show-followers)
   ("F"       twit-show-friends)
   ("h"       backward-char)
   ("l"       forward-char)
   ("j"       next-line)
   ("k"       previous-line)
   ("J"       roll-down)
   ("K"       roll-up)
   ("o"       other-window)
   ("O"       twit-open-link)

   ("1"       delete-other-windows)
   ("q"       bury-buffer)
   ("Q"       kill-this-buffer)
   ("<"       beginning-of-buffer)
   (">"       end-of-buffer)
   ("G"       end-of-buffer)))

(provide 'twit-settings)
