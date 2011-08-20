;;; eagle.el --- Twitter client

(defvar eagle-version-number "1.0")

;; Copyright (C) 2009 ahei

;; Time-stamp: <2010-03-19 22:13:40 Friday by ahei>
;; Author: ahei <ahei0802@126.com>
;; Keywords: twitter eagle

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
;; This library is enhacement of twit.el.
;; 1.
;; If you in some country which "forbit" great people to use twitter, twit.el
;; can not help you, but eagle can let you set twitter api address which your
;; network can reach.
;; 2.
;; With twit, you must write your twitter password in el file, that is very unsecure,
;; unless you must type your twitter password many times when twit connect twitter server.
;; but eagle will prompt you to input your twitter password when you have not set it. and,
;; after you input it, eagle use it to build authentication, but not save it which twit do.
;;
;; see article http://ahei.yo2.cn/twitter.htm

;;; Installation:
;;
;; Copy eagle.el to your load-path and add to your .emacs:
;;
;; (require 'eagle)

;; then use eagle-show-recent-tweets to show recent tweets.

;;; History:
;;
;; 2010-2-20
;;      * Before kill buffer `eagle-status-buffer-name' just confirm it.
;;
;; 2010-2-19
;;      * Add command eagle-start-update-status to update status.
;;      * Add command eagle-reply to reply tweet.
;;      * Add function `eagle-switch-to-home'.
;;
;; 2010-2-7
;;      * Add command eagle-at-tweets.
;;
;; 2010-2-5
;;      * Add function eagle-first-tweet, eagle-last-tweet, command eagle-this-tweet.
;;
;; 2010-2-4
;;      * Add eagle-last-fetched-tweet-id to fix bug of eagle-last-fetched-tweets.
;;      * Add function eagle-home-update-internal, command eagle-home-update to update home timeline.
;;      * Delete unused function eagle-get-attrib-value, because there have function twitter4e-get-attrib-value.
;;      * Fix bug of function eagle-compile-format-string and eagle-write-tweet of getting face property.
;;      * Add function eagle-next-tweet-internal to provoide better way to move forward or backward to
;;        next or previous tweet.
;; 2009-12-6
;;      * initial version 1.0.

;;; Code:

(eval-when-compile (require 'cl))
(require 'twitter4e)
(require 'image-file)

(defgroup eagle nil
  "Enhacement for twit."
  :prefix "eagle-")

(defcustom eagle-twitter-username nil
  "Your twitter username."
  :group 'eagle
  :type 'string)

(defcustom eagle-twitter-password nil
  "Your twitter password."
  :group 'eagle
  :type 'string)

(defcustom eagle-save-twitter-password nil
  "Save twitter password or not."
  :group 'eagle
  :type 'boolean)

(defcustom eagle-http-port 80
  "Http port."
  :type 'integer
  :group 'eagle)

(defcustom eagle-https-port 443
  "Https port."
  :type 'integer
  :group 'eagle)

(defcustom eagle-home-buffer-name "*Twitter home*"
  "Home buffer name."
  :type 'string
  :group 'eagle)

(defcustom eagle-at-tweets-buffer-name-format "*Twitter @%s*"
  "Format of buffer name @username."
  :type 'string
  :group 'eagle)

(defcustom eagle-status-buffer-name "*Twitter status*"
  "Name of buffer used to edit status."
  :type 'string
  :group 'eagle)

(defcustom eagle-cur-time-format "%Y-%m-%d %T"
  "Time format."
  :type 'string
  :group 'eagle)

(defcustom eagle-unknown-screenname "unknown"
  "Unknow screen name."
  :type 'string
  :group 'eagle)

(defcustom eagle-max-display-tweets-count 50
  "Max tweets count to display in one page."
  :type 'integer
  :group 'eagle)

(defcustom eagle-switch-home-when-new-tweets nil
  "Switch to home when new tweets arrived."
  :type 'boolean
  :group 'eagle)

(defface eagle-message-face
  `((((type tty pc)) :foreground "yellow")
    (t :foreground "#FFFFFEA78873"))
  "The font face to use for a twitter message."
  :group 'eagle)

(defface eagle-twitter-username-face
  '((((type tty pc)) :foreground "magenta")
    (t :foreground "#F1C280F1FFFF"))
  "The face to use for twitter username."
  :group 'eagle)

(defface eagle-screenname-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Orchid"))
    (((class color) (min-colors 88) (background dark)) (:foreground "cornflower blue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Orchid"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:weight bold)))
  "The face to use for twitter username."
  :group 'eagle)

(defface eagle-info-face
  `((((type tty pc)) :height 0.8 :bold t :foreground "blue")
    (t :foreground "#D58EE0FAFFFF"))
  "Normal info face."
  :group 'eagle)

(defface eagle-small-info-face
  `((((type tty pc)) :height 0.8 :bold t :foreground "blue")
    (t :height 0.8 :foreground "#D58EE0FAFFFF"))
  "Small info face."
  :group 'eagle)

(defface eagle-time-face
  '((((type tty pc)) :height 0.8 :bold t :foreground "blue")
    (t :height 0.8 :foreground "#AA7CF7B2FFFF"))
  "The face to use for a twitter source."
  :group 'eagle)

(defface eagle-location-face
  '((((type tty pc)) :height 0.8 :bold t :foreground "blue")
    (t :height 0.8 :foreground "#FFFFDC99D00E"))
  "The face to use for a twitter source."
  :group 'eagle)

(defface eagle-source-face
  '((((type tty pc)) :height 0.8 :bold t :foreground "blue")
    (t :height 0.8 :foreground "#6777FFFF6CA5"))
  "The face to use for a twitter source."
  :group 'eagle)

(defface eagle-tc-mode-line-face
  '((((type tty pc)) :foreground "yellow" :background "magenta")
    (t (:foreground "#FFFFBCA80000" :background "#00005E177C05")))
  "Face used highlight `eagle-tc-mode-line-format'.")

(defface eagle-title-face
  `((((background light))
     :background "PowderBlue" :bold t
     :box (:line-width 2 :color "PowderBlue" :style 0))
    (((background dark))
     :background "#D58EFFFFFC18" :foreground "blue")
    (t :underline "white"))
  "Title Area of the recent tweets buffer."
  :group 'eagle)

(defface eagle-hash-at-face
    '((((class color) (background light))
       (:foreground "GoldenRod3"))
      (((class color) (background dark))
       (:foreground "GoldenRod"))
      (t (:underline "white")))
  "Face to show @msgs in"
  :group 'eagle)

(defface eagle-too-long-face
    '((((supports :strike-through t)) :strike-through t )
      (t :inherit 'font-lock-warning-face))

  "Face for highlighting a twit that's too long to post"
  :group 'eagle)

(defface eagle-url-face
  '((default
      :weight bold))
  "Face for showing hyperlinks"
  :group 'eagle)

(defface eagle-status-overlong-face
  '((t (:foreground "red")))
  "face used for characters in overly long Twitter statuses."
  :group 'eagle)

(defface eagle-status-normal-len-face
  '((t (:foreground "black")))
  "face used for characters in normal length Twitter statuses."
  :group 'eagle)

(defcustom eagle-tc-mode-line-format
  (propertize "TCRT" 'face 'eagle-tc-mode-line-face)
  "Mode line format of function `eagle-tc-mode'."
  :group 'eagle)

(defcustom eagle-time-format 'eagle-format-time-for-display
  "Function or string describing the format to display time stamps in.
If the value is a string it should be a format string with %
characters and it will be passed to format-time-string.

If the value is a function it will be called with a single
argument which will be a two element list containing the high and
low part of the number of seconds since epoch. The value can be
converted to broken down time using decode-time.

Otherwise the variable can be nil in which case the time string
from Twitter will be displayed directly."
  :type '(choice (const :tag "No translation" nil)
                 string
                 function)
  :group 'eagle)

(defcustom eagle-status-format
  (concat 
   "%p" 
   (propertize "%u" 'face 'eagle-twitter-username-face)
   (propertize "("  'face 'eagle-info-face)
   (propertize "%n" 'face 'eagle-screenname-face)
   (propertize ")"  'face 'eagle-info-face)
   "\n"
   (propertize "%m" 'face 'eagle-message-face)
   "\n"
   (propertize "%t"   'face 'eagle-time-face)
   (propertize " @"   'face 'eagle-small-info-face)
   (propertize " %l"  'face 'eagle-location-face)
   (propertize " via" 'face 'eagle-small-info-face)
   (propertize " %s"  'face 'eagle-source-face)
   "\n\n")
  "Format string describing how to display twitter statuses
It should be a string containing '%' characters followed by one
of the following commands:

%n - the full name of the person posting the tweet
%u - the screen name of the person posting the tweet
%t - the time the tweet was created. This gets formatted
     according to twitter-time-format
%r - a reply button
%m - the tweet's text
%M - the tweet's text but filled with fill-region
%s - the name of the program used to send the tweet

%i - the numeric id of the tweet
%T - whether the tweet was truncated

%U - the screen name of the person who the tweet was a reply to
%R - the status id of the tweet that this is a reply to
%S - the user id of the tweet that this is a reply to

%I - the id of the user posting the tweet
%l - the location of the user posting the tweet
%d - a description of the user posting the tweet
%A - a URL to the image for the person posting the tweet
%L - a URL to the home page for the person posting the tweet
%p - photo of the person posting the tweet
%F - the number of followers of the person posting the tweet
%P - whether posts from this user are protected

%% - a literal percent character

Any other text is copied directly into the buffer. Text
properties are preserved and the properties of the % markers will
be applied to the resulting string.

The marker can optionally be given a padding value after the %
symbol. If the value is negative, the padding will be added to
the right otherwise it will be added to the left."
  :type 'string
  :group 'eagle)

(defcustom eagle-time-format 'eagle-format-time-for-display
  "Function or string describing the format to display time stamps in.
If the value is a string it should be a format string with %
characters and it will be passed to format-time-string.

If the value is a function it will be called with a single
argument which will be a two element list containing the high and
low part of the number of seconds since epoch. The value can be
converted to broken down time using decode-time.

Otherwise the variable can be nil in which case the time string
from Twitter will be displayed directly."
  :type '(choice (const :tag "No translation" nil)
                 string
                 function)
  :group 'eagle)

(defcustom eagle-recent-tweets-header-format "Recent tweets (Page %s) [%s]"
  "Format of recent tweets header."
  :type 'string
  :group 'eagle)

(defcustom eagle-recent-tweets-no-page-header-format "Recent tweets [%s]"
  "Format of recent tweets header."
  :type 'string
  :group 'eagle)

(defcustom eagle-follow-idle-interval 90
  "How long in time to wait before checking for new tweets.
Right now it will check every 90 seconds, Which will generate a maximum
of 40 requests, leaving you another 30 per hour to play with.

The variable name is a bit of a misnomer, because it is not actually
based on idle time (anymore)."
  :type 'integer
  :group 'eagle)

(defcustom eagle-filter-diarrhea 0
  "After x number of tweets from the same user, ignore tweets by them.

0 means let all tweets through.

Some twitter users have the annoying habit of going on twitter bombs,
where they output 3 or more tweets.  Generally, I find that these
diarrhea sessions are very content free, and worth ignoring.

A good number here is 3."
  :type 'integer
  :group 'eagle)

(defcustom eagle-filter-tweets-regex ""
  "Filter all tweets with this regex.

This is useful if you do not want to see a particular style of tweet.
For isntance, if hash-tagging pisses you off, you could set this to \"#\" and
no hash-tagging messages would get to you."
  :type 'regexp
  :group 'eagle)

(defcustom eagle-filter-at-tweets nil
   "Filter any tweets that have @user of a user you don't know.

If enabled, every tweet will be scanned for any @msgs.  If they contain one,
and the username is not a user that you are following (or you) then it will be
ignored."
   :type 'boolean
   :group 'eagle)

(defcustom eagle-debug nil
  "Whether or not to run twit.el in debug mode."
  :group 'eagle
  :type 'boolean)

(defcustom eagle-new-tweets-hook
  (if (featurep 'todochiku)
      'eagle-todochiku-last-fetched-tweets
    nil)
  "Functions to execute when there is new tweets.
If you have Todochiku, you will be notified when new tweets appears."
  :type 'hook
  :group 'eagle)

(defcustom eagle-max-todochiku-tweets-count 5
  "Max count of todochiku tweets."
  :type 'integer
  :group 'eagle)

(defcustom eagle-cursor-point-when-next-tweet 'eagle-tweet-text
  "Which thing cursor point when call `eagle-next-tweet' or `eagle-previous-tweet'."
  :type 'symbol
  :group 'eagle)

(defcustom eagle-twitter-source
  "<a href=\"http://www.emacswiki.org/cgi-bin/emacs/twitter4e.el\" target=\"_blank\">twitter4e</a>"
  "Source when use eagle to update status."
  :type 'string
  :group 'eagle)

(defcustom eagle-status-remaining-length-format "Status remaining length: %s"
  "Remaining lenght of status."
  :type 'string
  :group 'eagle)

(defcustom eagle-replace-enter nil
  "Replace enter which space when update status or not."
  :type 'boolean
  :group 'eagle)

(defconst eagle-status-commands
  '((?i . id)
    (?R . in_reply_to_status_id)
    (?S . in_reply_to_user_id)
    (?U . in_reply_to_screen_name)
    (?T . truncated))
  "Alist mapping format commands to XML nodes in the status element.")

(defconst eagle-user-commands
  '((?n . name)
    (?u . screen_name)
    (?m . text)
    (?t . time)
    (?s . source)
    (?I . id)
    (?l . location)
    (?d . description)
    (?A . profile_image_url)
    (?L . url)
    (?F . followers_count)
    (?P . protected))
  "Alist mapping format commands to XML nodes in the user element.")

(defconst eagle-month-map
  '(("Jan" . 1)
    ("Feb" . 2)
    ("Mar" . 3)
    ("Apr" . 4)
    ("May" . 5)
    ("Jun" . 6)
    ("Jul" . 7)
    ("Aug" . 8)
    ("Sep" . 9)
    ("Oct" . 10)
    ("Nov" . 11)
    ("Dec" . 12))
  "Assoc list mapping month abbreviations to month numbers")

(defconst eagle-hash-at-regex "\\([#@][a-zA-Z0-9_.]+\\)"
  "Regular expression form for matching hashtags (#) and directions (@).")

(defconst eagle-url-regex "\\(http://[a-zA-Z0-9.]+\.[a-zA-Z0-9%#;~/.=+&$,?@-]+\\)"
   "Regular expression for urls.")

(defconst eagle-emacs-lisp-regex "\\([a-zA-Z0-9-.]+\\)\\.el"
  "Regex for Emacs Lisp files.")

(defconst eagle-maximum-tweet-length 140
  "Maximum length of a tweet.")

(defconst eagle-text-logo "eagle" "Text log of eagle.")

(defvar eagle-status-compiled-format nil
  "Compiled format of `eagle-status-format'.")

(defvar eagle-current-displaying-tweets nil "Current tweets displaying in buffer.")
(defvar eagle-last-fetched-tweets       nil "Last fetched tweets.")
(defvar eagle-last-fetched-tweet-id     nil "Last fetched tweet id.")
(defvar eagle-fetched-tweets-displayed  nil "Fetched tweets is dispalyed or not.")

(defvar eagle-mode-map (make-sparse-keymap) "Keymap for `eagle-mode'.")
(let ((map eagle-mode-map))
  (define-key map (kbd "C-k")     'eagle-remove-friend)
  (define-key map (kbd "a")       'eagle-add-friend)
  (define-key map (kbd "A")       'eagle-analyse-user)
  (define-key map (kbd "S")       'eagle-search)
  (define-key map (kbd "G")       'eagle-analyse-graph-user)
  (define-key map (kbd "g")       'eagle-home-update)
  (define-key map (kbd "n")       'eagle-next-tweet)
  (define-key map (kbd "p")       'eagle-previous-tweet)
  
  (define-key map (kbd "d")       'eagle-direct)
  (define-key map (kbd "N")       'eagle-post)
  (define-key map (kbd "t")       'eagle-post-to)
  (define-key map (kbd "C-c n")   'eagle-post-url)
  (define-key map (kbd "r")       'eagle-post-loud-reply)
  (define-key map (kbd "R")       'eagle-post-reply)
  (define-key map (kbd "c")       'eagle-post-retweet)

  (define-key map (kbd "C-c C-a") 'eagle-at-tweets)
  (define-key map (kbd "SPC")     'scroll-up)
  (define-key map (kbd "u")       'View-scroll-page-backward)
  (define-key map (kbd "e")       'move-end-of-line)
  (define-key map (kbd "f")       'eagle-show-followers)
  (define-key map (kbd "F")       'eagle-show-friends)
  (define-key map (kbd "h")       'backward-char)
  (define-key map (kbd "l")       'forward-char)
  (define-key map (kbd "j")       'next-line)
  (define-key map (kbd "k")       'previous-line)
  (define-key map (kbd "J")       'roll-down)
  (define-key map (kbd "K")       'roll-up)
  (define-key map (kbd "o")       'other-window)
  (define-key map (kbd "O")       'eagle-open-link)

  (define-key map (kbd "1")       'delete-other-windows)
  (define-key map (kbd "q")       'bury-buffer)
  (define-key map (kbd "Q")       'kill-this-buffer)
  (define-key map (kbd "<")       'eagle-first-tweet)
  (define-key map (kbd ">")       'eagle-last-tweet)
  (define-key map (kbd "G")       'end-of-buffer))

(defvar eagle-status-edit-mode-map (make-sparse-keymap) "Keymap for `eagle-status-edit-mode'.")
(let ((map eagle-status-edit-mode-map))
  (set-keymap-parent map text-mode-map)
  (define-key map (kbd "C-c C-c") 'eagle-update-status)
  (define-key map (kbd "C-c C-k") 'eagle-kill-status-buffer))

(defvar eagle-is-recent-tweets-first-page nil
  "Current is display first page of recent tweets or not.")

(defvar eagle-user-image-list 'nil "List containing all user images.")

(defvar eagle-timer nil
  "Timer object that handles polling the recent tweets.")

(defvar eagle-created-auth nil "Created auth or not.")

(defvar eagle-frame-configuration nil
  "Frame configuration from immediately before `eagle-update-status'
 is called")

(defvar eagle-tweet-overlong-overlay nil
  "Overlay used to highlight overlong status messages.")

;; must do this
(put 'eagle-tc-mode-line-format 'risky-local-variable t)

(setq minor-mode-alist
      (append
       `((eagle-tc-mode " ")
         (eagle-tc-mode ,eagle-tc-mode-line-format))
       (delq (assq 'eagle-tc-mode minor-mode-alist) minor-mode-alist)))

(defun nthcar (n list)
  "Return first N elements of LIST.
If LIST length below N, return entire list.
If LIST is nil, return nil."
  (reverse (nthcdr (- (length list) n) (reverse list))))

(defun eagle-write-title (title &rest args)
  "Helper function to write out a title bar for a eagle buffer.

TITLE is the title to display, and it is formatted with ARGS."
  (setq header-line-format
        (propertize (apply 'format title args) 'face 'eagle-title-face)))

(defmacro eagle-with-buffer (buffer-name &rest forms)
  "Create a buffer with name BUFFER-NAME, and execute FORMS.

The value returned is the current buffer."
  `(with-current-buffer (get-buffer-create ,buffer-name)
     ,@forms
     ,buffer-name))

(defun eagle-home-internal (&optional page sync tc)
  "Open home with page PAGE.

SYNC is non-nil means open home synchronously.
TC is non-nil means run `eagle-tc-mode'."
  (eagle-set-username-password)
  (setq eagle-is-recent-tweets-first-page (= page 1))
  (unless tc
    (setq eagle-current-displaying-tweets nil))
  (if sync
      (eagle-show-page (twitter4e-get-home-timeline page) page tc eagle-home-buffer-name t)
    (if tc
        (eagle-show-page nil page tc eagle-home-buffer-name t)
      (twitter4e-get-home-timeline page 'eagle-show-page-callback (list page tc eagle-home-buffer-name t)))))

(defun eagle-show-page-callback (status url xml cbargs)
  "Callback function, called by `eagle-home-internal'."
  (when (not status)
    (let ((page (nth 0 cbargs))
          (tc (nth 1 cbargs))
          (buffer-name (nth 2 cbargs))
          (is-record (nth 3 cbargs)))
      (eagle-show-page xml page tc buffer-name is-record))))

(defun eagle-show-page (xml page tc &optional buffer-name is-record)
  "Show page.

If IS-RECORD is non-nil, record `eagle-current-displaying-tweets'."
  (if tc
      (let ((displayed eagle-fetched-tweets-displayed))
        (eagle-with-buffer
         eagle-home-buffer-name
         (eagle-write-fetched-tweets))
        (eagle-home-update t (not displayed)))
    (switch-to-buffer
     (eagle-with-buffer
      buffer-name
      (eagle-execute-in-eagle-mode
       (eagle-clean-tweets-buffer)
       (eagle-write-tweets-page-use-xml xml page is-record))
      (eagle-first-tweet)
      (when is-record
        (setq eagle-current-displaying-tweets eagle-last-fetched-tweets)))))
  (when tc
    (eagle-with-buffer buffer-name (eagle-tc-mode 1))))

(defun eagle-clean-tweets-buffer ()
  "Clean tweets buffer."
  (remove-overlays (point-min) (point-max))
  (delete-region (point-min) (point-max)))

(defun eagle-home-sync (&optional page)
  "Open home with page PAGE synchronously."
  (interactive "p")
  (eagle-home-internal page t))

(defun eagle-home-sync-tc (&optional page)
  "Open home with page PAGE synchronously and run `eagle-tc-mode'."
  (interactive "p")
  (eagle-home-internal page t t))

(defun eagle-home (&optional page)
  "Open home with page PAGE asynchronously."
  (interactive "p")
  (eagle-home-internal page))

(defun eagle-home-tc (&optional page)
  "Open home with page PAGE asynchronously and run `eagle-tc-mode'."
  (interactive "p")
  (eagle-home-internal page nil t))

(defun eagle-home-update (&optional switch to-first)
  "Update home tweets.

If SWITCH is non-nil, switch to buffer whose name is `eagle-home-buffer-name'."
  (interactive)
  (eagle-home-update-internal nil switch to-first))

(defun eagle-tc-timer-function ()
  "Timer function for timing check home timeline, called via a timer."
  (eagle-home-update-internal t))

(defun eagle-home-update-internal (&optional notify switch to-first)
  "Internal update home function.

If NOTIFY is non-nil, notify new tweets.
If SWITCH is non-nil, switch to buffer whose name is `eagle-home-buffer-name'."
  (let ((since-id eagle-last-fetched-tweet-id))
    (if since-id
        (twitter4e-get-home-timeline-since since-id 'eagle-tc-home-callback (list nil notify switch to-first))
      (twitter4e-get-home-timeline 1 'eagle-tc-home-callback (list 1 notify switch to-first)))))

(defun eagle-tc-home-callback (status url xml &optional cbargs)
  "Callback function, called by `eagle-tc-timer-function'.

STATUS, URL and XML are all set by `eagle-tc-timer-function'."
  (if status
      (message "Have error: %s" status)
    (let ((page (nth 0 cbargs))
          (notify (nth 1 cbargs))
          (switch (nth 2 cbargs))
          (to-first (nth 3 cbargs)))
      (save-window-excursion
        (save-excursion
          (eagle-with-buffer
           eagle-home-buffer-name
           (eagle-write-tweets-page-use-xml xml page t))
          (setq eagle-current-displaying-tweets
                (append eagle-last-fetched-tweets eagle-current-displaying-tweets))
          (when (and notify eagle-last-fetched-tweets)
            (run-hook-with-args 'eagle-new-tweets-hook eagle-last-fetched-tweets))))
      (if to-first
          (eagle-with-buffer
           eagle-home-buffer-name
           (eagle-first-tweet)))
      (if switch (switch-to-buffer eagle-home-buffer-name)))))

(defun eagle-todochiku-last-fetched-tweets (tweets)
  "Helper function for use by the todochiku package."
  (let ((size (length tweets)))
    (dolist (tweet (nthcar eagle-max-todochiku-tweets-count tweets))
      (todochiku-message
       eagle-text-logo
       (format "From %s:\n%s" (twitter4e-get-user-name tweet) (twitter4e-get-tweet-text tweet))
       (todochiku-icon 'social)))
    (if (> size eagle-max-todochiku-tweets-count)
        (todochiku-message eagle-text-logo (format "Received %d updates" size) (todochiku-icon 'social)))))

(defun eagle-write-tweets-page-use-xml (xml &optional page is-record)
  "Write tweets XML.

This function return buffer `eagle-home-buffer-name',
if page PAGE is nil, it default to 1."
  (eagle-write-tweets-use-tweets-data 'eagle-write-tweets-use-xml xml page is-record))

(defun eagle-write-tweets-page (status-nodes &optional page is-record)
  "Write tweets STATUS-NODES.

If page PAGE is nil, it default to 1."
  (eagle-write-tweets-use-tweets-data 'eagle-write-tweets status-nodes page is-record))

(defun eagle-write-tweets-use-tweets-data (write-tweets-fun tweets-data &optional page is-record)
  "Write tweets TWEETS-DATA use function WRITE-TWEETS-FUN.

This function return buffer `eagle-home-buffer-name',
if page PAGE is nil, it default to 1."
  (eagle-execute-in-eagle-mode
   (if page
       (eagle-write-title
        eagle-recent-tweets-header-format
        page
        (format-time-string eagle-cur-time-format))
     (eagle-write-title
      eagle-recent-tweets-no-page-header-format
      (format-time-string eagle-cur-time-format)))
   (funcall write-tweets-fun tweets-data is-record)
   (setq eagle-fetched-tweets-displayed t)))

(defmacro eagle-execute-in-eagle-mode (&rest forms)
  "execute forms in `eagle-mode'."
  `(progn
     (eagle-enter-eagle-mode)
     (buffer-disable-undo)
     (toggle-read-only -1)
     (goto-char (point-min))
     ,@forms
     (set-buffer-modified-p nil)
     (toggle-read-only 1)))

(define-derived-mode eagle-mode text-mode
  "eagle"
  "Major mode for twitter.

  \\{eagle-mode-map}")

(defun eagle-enter-eagle-mode ()
  "Enter `eagle-mode'."
  (unless (eq major-mode 'eagle-mode)
     (eagle-mode)))

(defun eagle-write-fetched-tweets ()
  "Write fetched tweets."
  (when (and (not eagle-fetched-tweets-displayed) eagle-current-displaying-tweets)
    (eagle-write-tweets-page eagle-current-displaying-tweets nil t)))

(define-minor-mode eagle-tc-mode
  "Toggle timing check recent tweets."
  :group 'eagle
  :global t
  (if eagle-tc-mode
      (progn
        ;; if auth is not created, just creat it
        (unless eagle-created-auth
          (if (not (and eagle-twitter-username eagle-twitter-password))
              ;; if can not create auth, trun off `eagle-tc-mode'
              (if eagle-tc-mode
                  (eagle-tc-mode -1))
            (eagle-set-auth eagle-twitter-username eagle-twitter-password)))
        (eagle-write-fetched-tweets)
        (unless eagle-timer
          (setq eagle-timer
                (run-with-timer
                 eagle-follow-idle-interval
                 eagle-follow-idle-interval
                 'eagle-tc-timer-function))))
    (when eagle-timer
      (cancel-timer eagle-timer)
      (setq eagle-timer nil))))

(defun eagle-tc-mode-on ()
  "Trun on `eagle-tc-mode'."
  (interactive)
  (eagle-tc-mode 1))

(defun eagle-tc-mode-off ()
  "Trun off `eagle-tc-mode'."
  (interactive)
  (eagle-tc-mode -1))

(defun eagle-tc-mode-off-right ()
  "Trun off `eagle-tc-mode'
only when `major-mode' is `eagle-mode'."
  (if (and (eq major-mode 'eagle-mode) (string= (buffer-name) eagle-home-buffer-name))
      (eagle-tc-mode -1)))

(defun eagle-at-tweets-internal (&optional page sync)
  "Show @username tweets page PAGE.

SYNC is non-nil means show tweets synchronously."
  (eagle-set-username-password)
  (let ((buffer-name (format eagle-at-tweets-buffer-name-format eagle-twitter-username)))
    (if sync
        (eagle-show-page (twitter4e-get-mentions page) page nil buffer-name)
      (twitter4e-get-mentions page 'eagle-show-page-callback (list page nil buffer-name)))))

(defun eagle-at-tweets (&optional page)
  "Show @username tweets page PAGE.

SYNC is non-nil means show tweets asynchronously."
  (interactive)
  (eagle-at-tweets-internal page nil))

(defun eagle-at-tweets-sync (&optional page)
  "Show @username tweets page PAGE synchronously."
  (interactive)
  (eagle-at-tweets-internal page t))

(defun eagle-compile-format-string (format-string)
  "Converts FORMAT-STRING into a list that is easier to scan.
See twitter-status-format for a description of the format. The
returned list contains elements that are one of the following:

- A string. This should be inserted directly into the buffer.

- A four element list like (RIGHT-PAD WIDTH COMMAND
  PROPERTIES). RIGHT-PAD is t if the - flag was specified or nil
  otherwise. WIDTH is the amount to pad the string to or nil if
  no padding was specified. COMMAND is an integer representing
  the character code for the command. PROPERTIES is a list of
  text properties that should be applied to the resulting
  string."
  (let (parts last-point)
    (with-temp-buffer
      (insert format-string)
      (goto-char (point-min))
      (setq last-point (point))
      (while (re-search-forward "%\\(-?\\)\\([0-9]*\\)\\([a-zA-Z%]\\)" nil t)
        ;; Push the preceeding string (if any) to copy directly into
        ;; the buffer
        (when (> (match-beginning 0) last-point)
          (push (buffer-substring last-point (match-beginning 0)) parts))
        ;; Make the three element list describing the command
        (push (list (> (match-end 1) (match-beginning 1)) ; is - flag given?
                    (if (> (match-end 2) (match-beginning 2)) ; is width given?
                        (string-to-number (match-string 2)) ; extract the width
                      nil) ; otherwise set to nil
                    ;; copy the single character for the command number directly
                    (char-after (match-beginning 3))
                    ;; extract all of the properties so they can be
                    ;; copied into the final string
                    (second (memq 'face (text-properties-at (match-beginning 0)))))
              parts)
        ;; Move last point to the end of the last match
        (setq last-point (match-end 0)))
      ;; Add any trailing text
      (when (< last-point (point-max))
        (push (buffer-substring last-point (point-max)) parts)))
    (nreverse parts)))

(defun eagle-get-node-text (node)
  "Return the text of XML node NODE.
All of the text elements are concatenated together and returned
as a single string."
  (let (text-parts)
    (dolist (part (xml-node-children node))
      (when (stringp part)
        (push (xml-substitute-special part) text-parts)))
    (apply 'concat (nreverse text-parts))))

(defun eagle-time-to-time (time)
  "Convert TIME to a number of seconds since some epoch."
  (let ((case-fold-search t))
    (unless (string-match
             (concat "\\`[a-z]\\{3\\} "
                     "\\([a-z]\\{3\\}\\) "
                     "\\([0-9]\\{1,2\\}\\) "
                     "\\([0-9]\\{2\\}\\):"
                     "\\([0-9]\\{2\\}\\):"
                     "\\([0-9]\\{2\\}\\) "
                     "\\([+-][0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\) "
                     "\\([0-9]\\{4\\}\\)\\'") time)
      (error "Invalid time string: %s" time))
    (encode-time (string-to-number (match-string 5 time))
                 (string-to-number (match-string 4 time))
                 (string-to-number (match-string 3 time))
                 (string-to-number (match-string 2 time))
                 (cdr (assoc (match-string 1 time) eagle-month-map))
                 (string-to-number (match-string 8 time))
                 (concat (match-string 6 time) ":" (match-string 7 time)))))

(defun eagle-format-time-for-display (time)
  "Convert TIME to a friendly human readable string.
TIME should be a high/low pair as returned by encode-time."
  ;; This is based on a similar function from Tweet
  (let* ((now (current-time))
         (age (subtract-time now time))
         (age-days (- (time-to-days now) (time-to-days time))))
    (if (or (< (car age) 0)
            (>= (car age) 16) ; more than about 12 days
            (>= age-days 7))
        (format-time-string "%x at %H:%M" time)
      (let* ((age-seconds (logior (lsh (car age) 16) (cadr age)))
             (age-minutes (/ age-seconds 60))
             (age-hours (/ age-minutes 60)))
        (cond ((< age-seconds 60)
               "Less than a minute ago")
              ((<= age-minutes 1)
               "About a minute ago")
              ((< age-minutes 60)
               (format "About %d minutes ago" age-minutes))
              ((<= age-hours 1)
               "About an hour ago")
              ((< age-minutes 360)
               (format "About %d hours ago" age-hours))
              ((<= age-days 0)
               (format-time-string "Today at %H:%M" time))
              ((<= age-days 1)
               (format-time-string "Yesterday at %H:%M" time))
              (t
               (format-time-string "Last %A at %H:%M" time)))))))

(defun eagle-reply (pos)
  "Sets up a status edit buffer to reply to the message at POS.
eagle-reply-status-id is set to the id of the status
corresponding to the status so that it will be marked as a
reply. The status' screen name is initially entered into the
buffer.

When called interactively POS is set to point."
  (interactive "d")
  (let ((screenname (get-char-property pos 'eagle-user-screenname))
        (tweet-id (get-char-property pos 'eagle-tweet-id)))
    (when (null screenname)
      (error "Missing screen name in status"))
    (when (null tweet-id)
      (error "Missing status id"))
    (setq eagle-reply-tweet-id tweet-id)
    (eagle-status-edit)
    (if (string= (buffer-string) "")
        (insert "@" screenname " "))))

(defun eagle-start-update-status ()
  "Open buffer for writing status to update."
  (interactive)
  (setq eagle-reply-tweet-id nil)
  (eagle-status-edit))

(defun eagle-status-edit ()
  "Edit your twitter status in a new buffer.
A new buffer is popped up in a special edit mode. Press
\\[eagle-status-post] when you are finished editing to send the
message."
  (interactive)
  (setq eagle-frame-configuration (current-frame-configuration))
  (pop-to-buffer eagle-status-buffer-name)
  (eagle-status-edit-mode))

(define-derived-mode eagle-status-edit-mode text-mode "Twitter Status Edit"
  "Major mode for updating your Twitter status."
  ;; Schedule to update the character count after altering the buffer
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions 'eagle-status-edit-after-change)
  ;; Add the remaining character count to the mode line
  ;; messages
  (make-local-variable 'eagle-tweet-overlong-overlay)
  (make-local-variable 'kill-buffer-query-functions)
  (setq kill-buffer-query-functions '(eagle-confirm-kill-status-edit))
  ;; Update the mode line immediatly
  (eagle-status-edit-update-length))

(defun eagle-confirm-kill-status-edit ()
  "Ask kill current buffer or not."
  (if (or (not (buffer-modified-p)) (yes-or-no-p "Kill this buffer?"))
      t))

(defun eagle-status-edit-after-change (begin end old-size)
  "After change function of edit status."
  (eagle-status-edit-update-length))

(defun eagle-status-edit-update-length ()
  "Updates the character count in Twitter status buffers.
This should be run after the text in the buffer is changed. Any
characters after the maximum status update length are
hightlighted in the face `eagle-status-overlong-face' and the
character count on the mode line is updated."
  ;; Update the remaining characters in the mode line
  (let* ((remaining (- eagle-maximum-tweet-length (buffer-size)))
         (face (if (>= remaining 0) 'eagle-status-normal-len-face 'eagle-status-overlong-face))
         (remaining-str (number-to-string remaining)))
    (setq header-line-format
          (concat
           (format eagle-status-remaining-length-format (propertize remaining-str 'face face))
           (if eagle-reply-tweet-id
               (concat ", reply tweet id " eagle-reply-tweet-id)))))
  ;; Highlight the characters in the buffer that are over the limit
  (if (> (buffer-size) eagle-maximum-tweet-length)
      (let ((start (+ (point-min) eagle-maximum-tweet-length)))
        (if (null eagle-tweet-overlong-overlay)
            (overlay-put
             (setq eagle-tweet-overlong-overlay (make-overlay start (point-max)))
             'face 'eagle-status-overlong-face)
          (move-overlay eagle-tweet-overlong-overlay
                        start (point-max))))
    ;; Buffer is not too long so just hide the overlay
    (when eagle-tweet-overlong-overlay
      (delete-overlay eagle-tweet-overlong-overlay))))

(defun eagle-get-status ()
  "Get the contents of the current buffer as a string.
All groups of spaces in the string are replaced with a single
space if `eagle-replace-enter' is non-nil."
  (let ((other-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties other-buffer)
      (goto-char (point-min))
      (if eagle-replace-enter
          (while (re-search-forward "[\n\t ]+" nil t)
            (replace-match " " t t)))
      (buffer-substring (point-min) (point-max)))))

(defun eagle-update-status (&optional sync)
  (interactive "P")
  "Update status."
  (let ((len (buffer-size)))
    (when (or (<= len eagle-maximum-tweet-length)
              (y-or-n-p (format "The message is %i characters long. Are you sure? " len)))
      (let ((msg (if eagle-reply-tweet-id "Send reply" "Update status")))
        (message "%s ..." msg)
        (eagle-update-status-internal (eagle-get-status) msg eagle-reply-tweet-id sync)))))

(defun eagle-update-status-internal (status msg &optional reply-id sync)
  "Internal function of update status."
  (if sync
      (eagle-update-status-core
       (twitter4e-update-status status reply-id nil nil eagle-twitter-source) msg eagle-home-buffer-name)
    (twitter4e-update-status
     status reply-id 'eagle-update-status-callback (list msg eagle-home-buffer-name) eagle-twitter-source)))

(defun eagle-update-status-core (xml msg &optional buffer-name)
  "Core function of update status."
  (eagle-with-buffer
   buffer-name
   (eagle-execute-in-eagle-mode
    (eagle-write-tweets-use-xml xml nil)))
  (with-current-buffer eagle-status-buffer-name
    (let ((kill-buffer-query-functions nil))
      (eagle-kill-status-buffer)))
  (message "%s success." msg))

(defun eagle-update-status-callback (status url xml cbargs)
  "Callback function, called by `eagle-update-status-internal'."
  (let ((msg (nth 0 cbargs))
        (buffer-name (nth 1 cbargs)))
    (if status
        (message "%s error: %s" msg status)
      (eagle-update-status-core xml msg buffer-name))))

(defun eagle-kill-status-buffer ()
  "Kill the `eagle-status-buffer-name' buffer and restore the previous
frame configuration."
  (interactive)
  (when (get-buffer eagle-status-buffer-name)
    (kill-buffer eagle-status-buffer-name))
  (set-frame-configuration eagle-frame-configuration))

(defun eagle-clean-temp-data ()
  "Clean temp data."
  (interactive)
  (setq eagle-last-fetched-tweets nil
        eagle-last-fetched-tweet-id nil
        eagle-current-displaying-tweets nil
        eagle-fetched-tweets-displayed nil))

(defun eagle-reply-button-pressed (button)
  "Calls eagle-reply for the position where BUTTON is."
  (eagle-reply (overlay-start button)))

(defun eagle-insert-image (status-node)
  "Insert user's image to buffer."
  (let* ((screenname (eagle-get-screenname status-node))
         (user-img (eagle-get-user-image (twitter4e-get-user-image-url status-node) screenname)))
    (when user-img
      (insert-image user-img))))

(defun eagle-insert-tweet-time (status-node)
  "Insert tweet time to buffer."
  (let ((val (twitter4e-get-tweet-created-at status-node)))
    (when val
      (cond
       ((stringp eagle-time-format)
        (insert (format-time-string eagle-time-format (eagle-time-to-time val))))
       ((functionp eagle-time-format)
        (insert (funcall eagle-time-format (eagle-time-to-time val))))
       ((null eagle-time-format)
        (insert val))
       (t (error "Invalid value for `eagle-time-format'"))))))

(defun eagle-insert-source (status-node)
  "Insert tweet's source to buffer."
  (let ((link-source (twitter4e-get-tweet-source status-node))
        source)
    (when link-source
      (with-temp-buffer
        (insert link-source)
        (setq source (eagle-get-node-text (car (xml-parse-region (point-min) (point-max))))))
      (when (or (null source) (string= source "")) (setq source link-source))
      (insert source))))

(defun eagle-insert-tweet-attribute (status-node command)
  "Insert tweet's attribute to buffer."
  (let ((user-node (twitter4e-get-user-node status-node))
        val elem)
    (cond ((setq elem (assoc command eagle-user-commands))
           (setq val (twitter4e-get-attrib-value user-node (cdr elem))))
          ((setq elem (assoc command eagle-status-commands))
           (setq val (twitter4e-get-attrib-value status-node (cdr elem)))))
    (when val
      (insert val))))

(defun eagle-fill-string (str)
  "Run `fill-region' on string STR."
  (with-temp-buffer
    (fill-region (prog1 (point) (insert str)) (point))
    (buffer-substring (point-min) (point-max))))

(defun eagle-insert-tweet-text (status-node command)
  "Insert tweet's message to buffer."
  (let ((val (twitter4e-get-tweet-text status-node)))
    (when (and val (= command ?M))
      (setq val (eagle-fill-string val)))
    (insert (eagle-keymap-and-fontify-message val))))

(defun eagle-insert-status-part-for-command (status-node command)
  "Extract the string for COMMAND from STATUS-NODE and insert.
The command should be integer representing one of the characters
supported by eagle-status-format."
  (case command
    (?t (eagle-insert-tweet-time status-node))
    ;; ((= command ?r)
    ;;  (insert-button "reply"
    ;;                 'action 'eagle-reply-button-pressed))
    (?p (eagle-insert-image status-node))
    ((?m ?M) (eagle-insert-tweet-text status-node command))
    (?s (eagle-insert-source status-node))
    (?% (insert ?%))
    (t (eagle-insert-tweet-attribute status-node command))))

(defun eagle-get-screenname (status-node)
  "Get user screen name by status node STATUS-NODE."
  (or (twitter4e-get-user-screenname status-node) eagle-unknown-screenname))

(defun eagle-write-tweet (status-node &optional filter-tweets times-through)
  "Insert a tweet STATUS-NODE into the current buffer.
TWEET should be an xml parsed node, which could be a message or a status node.
FILTER-TWEETS is an optional boolean to disregard filtering.
TIMES-THROUGH is an integer representing the number of times a tweet has been
  displayed, for zebra-tabling."
  (let ((start (point)))
    (dolist (element eagle-status-compiled-format)
      (let ((part-start (point))
            property command)
        (if (stringp element)
            (progn
              (setq property (second (memq 'face (text-properties-at 0 element))))
              (insert (substring-no-properties element)))
          (let ((right-pad (nth 0 element))
                (padding (nth 1 element)))
            (setq command (nth 2 element))
            (setq property (nth 3 element))
            (eagle-insert-status-part-for-command status-node command)
            (when (and padding
                       (< (- (point) part-start) padding))
              (setq padding (make-string
                             (+ padding (- part-start (point))) ?\ ))
              (if right-pad
                  (insert padding)
                (let ((part-end (point)))
                  (goto-char part-start)
                  (insert padding)
                  (goto-char (+ part-end (length padding))))))))
        (let ((overlay (make-overlay part-start (point) nil t))
              (key (cdr (or (assoc command eagle-user-commands) (assoc command eagle-status-commands)))))
          (overlay-put overlay 'face property)
          (if key
              (overlay-put overlay 'key key)))))
    (let* ((overlay (make-overlay start (point) nil t))
           (tweet-id (twitter4e-get-tweet-id status-node))
           (user-id (twitter4e-get-user-id status-node))
           (screenname (eagle-get-screenname status-node)))
      (overlay-put overlay 'eagle-tweet-id tweet-id)
      (overlay-put overlay 'eagle-user-id user-id)
      (overlay-put overlay 'eagle-user-screenname screenname))))

(defun eagle-write-tweets-use-xml (xml-data is-record)
  "Function that writes tweets to the buffer.

XML-DATA is the sxml (with http header).
If IS-RECORD is non-nil, record `eagle-last-fetched-tweets' and
`eagle-last-fetched-tweet-id'."
  (setq eagle-last-fetched-tweets nil)
  (if (not xml-data)
      (message "No xml data.")
    (if (twitter4e-xml-header-error-p xml-data)
        (eagle-display-error xml-data)
	  (let ((status-nodes (twitter4e-get-status-nodes xml-data)))
        (if status-nodes
            (eagle-write-tweets status-nodes is-record))))))

(defun eagle-write-tweets (status-nodes is-record)
  "Function that writes tweets STATUS-NODES to the buffer.

If IS-RECORD is non-nil, record `eagle-last-fetched-tweets' and
`eagle-last-fetched-tweet-id'."
  (let* ((times-through 1)
         (cur-size (length status-nodes))
         (displaying-size (if is-record (length eagle-current-displaying-tweets) 0))
         (all-size (+ displaying-size cur-size))
         (fetched-tweets (nthcar eagle-max-display-tweets-count status-nodes)))
    (when is-record
      (setq eagle-last-fetched-tweets fetched-tweets)
      (setq eagle-last-fetched-tweet-id (twitter4e-get-tweet-id (car eagle-last-fetched-tweets)))
      (if (> all-size eagle-max-display-tweets-count)
          (eagle-delete-last-n-tweets-displaying (- all-size eagle-max-display-tweets-count))))
    (dolist (status-node fetched-tweets)
      (let ((message (twitter4e-get-tweet-text status-node))
            (author (twitter4e-get-user-name status-node)))
        (when (not (eagle-filter-tweet message author))
          (eagle-write-tweet status-node nil times-through)
          (setq times-through (+ 1 times-through)))))))
    
(defun eagle-delete-last-n-tweets-displaying (n)
  "Delete last N tweets displaying in buffer."
  (dolist (overlay
           (nthcar n
                   (sort
                    (delq nil
                          (mapcar
                           (lambda (overlay)
                             (if (overlay-get overlay 'eagle-tweet-id) overlay nil))
                           (overlays-in (point-min) (point-max))))
                    (lambda (elem1 elem2)
                      (> (overlay-start elem1) (overlay-start elem2))))))
    (delete-region (overlay-start overlay) (overlay-end overlay))
    (delete-overlay overlay))
  (setq eagle-current-displaying-tweets (butlast eagle-current-displaying-tweets n)))

(defun eagle-filter-tweet (message author)
  "Return t if the user wants MESSAGE filtered, nil if not."
  (when (> eagle-filter-diarrhea 0)
        (if (string-equal author (car eagle-last-author))
            (setcdr eagle-last-author (+ 1 (cdr eagle-last-author)))
            (setq eagle-last-author (cons author 1))))
  (not (and  (or (string-equal "" eagle-filter-tweets-regex)
                 (null eagle-filter-tweets-regex)
                 (not (string-match eagle-filter-tweets-regex message)))
             (or (not eagle-filter-at-tweets)
                 (not (string-match "@" message))
                 (string-match eagle-filter-at-tweets-retweet-regex message)
                 (and eagle-filter-at-tweets
                      (eagle-at-message-was-from-friend message)))
             (or (= 0 eagle-filter-diarrhea)
                 (>= eagle-filter-diarrhea
                     (cdr eagle-last-author))))))

(defun eagle-set-username-password (&optional force)
  "Check set twitter's password.
 If have not set, just prompt user to input password, and set.

If FORCE is non-nil, force create auth."
  (interactive "P")
  (if force
      (setq eagle-twitter-username nil
            eagle-twitter-password nil
            eagle-created-auth nil))
  (unless eagle-created-auth
    (let ((pass eagle-twitter-password))
      (unless eagle-twitter-username
        (call-interactively 'eagle-set-username))
      (unless pass
        (setq pass (call-interactively 'eagle-set-password)))
      (eagle-set-auth eagle-twitter-username pass))))

(defun eagle-set-username (username)
  "Set twitter username."
  (interactive "MInput your twitter username: ")
  (setq eagle-twitter-username username))

(defun eagle-set-password (pass)
  "Set twitter's password."
  (interactive
   (list
    (read-passwd
     (format "Input your twitter password of account %s: " eagle-twitter-username))))
  (if eagle-save-twitter-password
      (setq eagle-twitter-password pass))
  pass)

(defun eagle-set-auth (user pass)
  "Set the http url authentication string from USER and PASS."
  (interactive
   (list
    (read-string "Input your twitter account: ")
    (read-passwd
     (format "Input your twitter password of account %s: " eagle-twitter-username))))
  (twitter4e-set-auth user pass)
  (setq eagle-created-auth t))

(defun eagle-set-api-protocol (protocol)
  "Set twitter api protocol."
  (interactive "MProtocol to use: ")
  (twitter4e-set-api-protocol protocol)
  (eagle-set-api-domain twitter4e-api-domain))

(defun eagle-set-status-format (&optional format)
  "set `eagle-status-format'."
  (interactive)
  (if format (setq eagle-status-format format))
  (setq eagle-status-compiled-format (eagle-compile-format-string eagle-status-format)))

(defun eagle-display-error (xml)
  "Given an XML fragment that contain an error, display it to the user."
  (let ((header (twitter4e-parse-header (car xml))))
    (when (twitter4e-header-error-p header)
       (twitter4e-insert-with-overlay-attributes
         "(_x___}<"
         '((face "twitter4e-fail-whale-face")))
       (twitter4e-insert-with-overlay-attributes
          (concat "         HTTP ERROR!  "
                  "(" (cadr header) ") "
                  (caddr header) "\n\n"
                  "  "(twitter4e-get-header-error header) "\n\n"
                  "  The response from twitter was: "
                  (format "%s" (twitter4e-xml-first-childs-value (cadr xml) 'error))
                  "\n\n")
          '((face "twitter4e-error-face"))))))

(defun eagle-get-header-error (header)
   "Given a parsed HEADER from `twitter4e-parse-header', return human readable error."
   (if (null header)
       "Null header, probably an error with twit.el."
       (case (string-to-number (cadr header))
         ((200) "Everything is A OK!")
         ((304) "Nothing Changed.")
         ((400) "Bad Request. (probably rate limited)")
         ((401) "Not Authorized.  You need to log in, or your login/password is incorrect.\nIf this is the first time you have tried to use a twitter command\nenter your password and try again.")
         ((403) "You are FORBIDDEN")
         ((404) "Not Found.  404'ed!")
         ((406) "Something is bropken with twit.el's search!")
         ((500) "Something is horribly broken with twit.el, or even Twitter!")
         ((502) "Twitter is down. FAIL WHALE!")
         ((503) "Rate limited on search."))))

(defun eagle-keymap-and-fontify-message (message)
  "Scan through MESSAGE, and fontify and keymap all #foo and @foo."
  (let ((original-txt (substring message 0))) ;; Just to be sure we're using a copy
    (when (string-match eagle-hash-at-regex message) ;; usernames
      (setq message (replace-regexp-in-string
                     eagle-hash-at-regex
                     (lambda (str)
                       (let ((type (substring str 0 1))
                             (thing (substring str 1)))
                         (setq str (propertize str
                                               'face 'eagle-hash-at-face
                                               'pointer 'hand))
                         (when (string-equal "@" type)
                           (setq str (propertize str 'eagle-user thing)))
                         (propertize str 'eagle-search (concat type thing))))
                     message)))

    (when (string-match eagle-url-regex message) ;; URLs
      (setq message (replace-regexp-in-string
                     eagle-url-regex
                     (lambda (str)
                       (let ((map (make-sparse-keymap)))
                         (define-key map [enter] 'eagle-visit-link)
                         (define-key map [(control) (enter)] 'eagle-visit-link)
                         (define-key map [mouse-1] 'eagle-visit-link)
                         (define-key map [mouse 2] 'eagle-visit-link)
                         (define-key map [mouse-3] 'eagle-visit-link)
                         (propertize str
                                     'face 'eagle-url-face
                                     'pointer 'hand
                                     'eagle-url str
                                     'keymap map)))
                     message)))
    (when (string-match eagle-emacs-lisp-regex message) ;; .el's
      (setq message (replace-regexp-in-string
                     eagle-emacs-lisp-regex
                     (lambda (str)
                       (propertize str
                                   'face 'eagle-url-face
                                   'elisp str))
                     message)))

    ;; message content (plaintext)
    (propertize message 'eagle-tweet-text original-txt)))

(defun eagle-get-user-image (url user-id)
  "Retrieve the user image from the list, or from the URL.
USER-ID must be provided."
  (let ((img (assoc url eagle-user-image-list)))
    (if (and img (not (bufferp (cdr img))))
        (cdr (assoc url eagle-user-image-list))
      (if (file-exists-p (concat eagle-user-image-dir
                                 "/" user-id "-"
                                 (file-name-nondirectory url)))
          (let ((img (ignore-errors
                       (create-image
                        (concat eagle-user-image-dir "/" user-id "-" (file-name-nondirectory url))))))
            (add-to-list 'eagle-user-image-list (cons url img))
            img)
        (let* ((url-request-method "GET")
               (url-show-status nil)
               (url-buffer (url-retrieve url 'eagle-write-user-image
                                         (list url user-id))))
          (if url-buffer
              (progn
                (add-to-list 'eagle-user-image-list (cons url url-buffer))
                (if eagle-debug (message "Added image. List is %s" eagle-user-image-list)))
            (eagle-alert (format "Warning, couldn't load %s " url)))
          nil)))))

(defun eagle-write-user-image (status url user-id)
  "Called by `eagle-get-user-image', to write the image to disk.

STATUS, URL and USER-ID are all set by `url-retrieve'."
  (let ((image-file-name
         (concat eagle-user-image-dir
                 "/" user-id "-"
                 (file-name-nondirectory url))))
    (when (not (file-directory-p eagle-user-image-dir))
      (make-directory eagle-user-image-dir))
    (setq buffer-file-coding-system 'no-conversion)
    (setq buffer-file-name image-file-name)
    (goto-char (point-min))
    (let ((end (search-forward "\C-j\C-j" nil t)))
      (if end
          (delete-region (point-min) end)
        (message "Can not found C-j C-j")))
    (save-buffer)
    (delete (cons url (current-buffer)) eagle-user-image-list)
    (kill-buffer (current-buffer))
    (add-to-list 'eagle-user-image-list (cons url (create-image image-file-name)))))

(defun eagle-alert (msg &optional title)
  "Send some kind of alert MSG to the user, with the title TITLE.

If todochiku is available, use that.  Instead, just message the user."
  (when (null title) (setq title twitter4e-alert-default-title))
  (when (featurep 'todochiku)
    (todochiku-message title msg (todochiku-icon 'social)))
  (message "%s: %s" title msg))

(defun eagle-set-api-domain (domain)
  "Set twitter api domain."
  (interactive "MTwitter api domain to use: ")
  (twitter4e-set-api-domain domain)
  (setq eagle-created-auth nil))

(defun eagle-get-tweet-overlay (pos)
  "Get tweet overlay at postion POS."
  (cdr (get-char-property-and-overlay pos 'eagle-tweet-id)))

(defun eagle-get-tweet-text-start (tweet-overlay)
  "Get tweet text start position."
  (eagle-get-thing-start 'eagle-tweet-text tweet-overlay))

(defun eagle-get-thing-start (thing tweet-overlay)
  "Get THING start position by tweet overlay TWEET-OVERLAY."
  (next-single-char-property-change
   (overlay-start tweet-overlay) thing nil (overlay-end tweet-overlay)))

(defun eagle-tweet-start (overlay)
  "Get start position of tweet by overlay OVERLAY."
  (let ((start (eagle-get-thing-start eagle-cursor-point-when-next-tweet overlay)))
    (if (/= start (overlay-end overlay)) start (overlay-start overlay))))

(defun eagle-next-tweet (&optional arg)
  "Move forward to the next tweet.

With argument ARG, move to the ARGth next tweet."
  (interactive "p")
  (eagle-next-tweet-internal t arg))

(defun eagle-previous-tweet (&optional arg)
  "Move backward to the previous tweet.

With argument ARG, move to the ARGth previous tweet."
  (interactive "p")
  (eagle-next-tweet-internal nil arg))

(defun eagle-next-tweet-internal (next &optional arg)
  "Move forward to the next tweet or backward to the previous tweet.

If NEXT is non-nil, move forward, othere move backward.
With argument ARG, move to the ARGth next/previous tweet."
  (mapc
   (lambda (n)
     (let ((this-overlay (eagle-get-tweet-overlay (point))))
       (when (or this-overlay (not next))
         (let* (dst next-overlay)
           (setq dst
                 (if next
                     (overlay-end this-overlay)
                   (1- (if this-overlay (overlay-start this-overlay) (point)))))
           (if (> dst 0)
               (setq next-overlay (eagle-get-tweet-overlay dst)))
           (when next-overlay
             (goto-char (eagle-tweet-start next-overlay)))))))
   (number-sequence 1 (or arg 1))))

(defun eagle-this-tweet ()
  "Goto start position of current tweet."
  (interactive)
  (let ((this-overlay (eagle-get-tweet-overlay (point))))
    (if this-overlay
        (goto-char (eagle-tweet-start this-overlay)))))

(defun eagle-first-tweet ()
  "Goto first tweet."
  (interactive)
  (goto-char (point-min))
  (eagle-this-tweet))

(defun eagle-last-tweet ()
  "Goto last tweet."
  (interactive)
  (goto-char (point-max))
  (eagle-previous-tweet))

;;* interactive direct
;;;###autoload
(defun eagle-direct (user msg)
  "Send USER a direct message MSG.

If you are currently positioned over a tweet, then it will fill in the author
of that tweet as the default recipient.

This will attempt to do a completing read based on the people you
are following, if you have images turned on."
  (interactive
   (list (eagle-read-friend "Direct Message To: " t)
         (eagle-query-for-post "Message: " "")))
  (if (> (length msg) eagle-maximum-tweet-length)
      (error eagle-too-long-msg)
      (eagle-direct-message user msg)))

;;* interactive direct multi-account
(defun eagle-direct-with-account (account)
  "Send a user a direct tweet with `eagle-direct' under a different ACCOUNT."
  (interactive (list (eagle-read-account)))
  (with-eagle-account account
    (eagle-direct (eagle-read-friend "Direct Message To: " t)
                 (read-string "Message: "))))

;;* post interactive tweet
;;;###autoload
(defun eagle-post (prefix)
  "Send a post to twitter.com.
Prompt the first time for password and username \(unless
`eagle-user' and/or `eagle-pass' is set\) and for the text of the
post; thereafter just for post text.  Posts must be <= `eagle-maximum-tweet-length' chars
long.

A PREFIX argument will prompt you for your post in reply to a
specific author that the cursor is nearest to.  This behavior may
become deprecated."
  (interactive "P")
  (let* ((reply-to (when prefix
                     (eagle-get-text-property 'eagle-user)))
         (post (eagle-query-for-post
                (if reply-to
                    (concat "Reply to " reply-to)
                    "Post")
                (when reply-to
                  (concat "@" reply-to " ")))))
    (if (> (length post) eagle-maximum-tweet-length)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post))))

;;* post interactive tweet multi-account
(defun eagle-post-with-account (account post)
  "Like `eagle-post' but under a different ACCOUNT POST a tweet."
  (interactive (list (eagle-read-account)
                     (eagle-query-for-post "Post: " "")))
  (with-eagle-account account
    (if (> (length post) eagle-maximum-tweet-length)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post))))

;;* post interactive keymap
(defun eagle-post-to ()
  "Posts to a particular user.  Mostly used by keymaps."
  (interactive)
  (let ((post (eagle-query-for-post "Reply To: " (concat "@" (eagle-get-text-property 'eagle-user) " "))))
    (if (> (length post) eagle-maximum-tweet-length)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post))))

;;* post reply interactive
(defun eagle-post-reply ()
  "Reply to a status on twitter.com."
  (interactive)
  (let* ((reply-to (eagle-get-text-property 'eagle-user))
         (parent-id (eagle-get-text-property 'eagle-id))
         (post (eagle-query-for-post (concat "Reply to " reply-to)
                                    (concat "@" reply-to " "))))
    (if (> (length post) 140)
        (error eagle-too-long-msg)
        (eagle-post-status twitter4e-update-url post parent-id))))

;;* post "loud" reply interactive
(defun eagle-post-loud-reply ()
  "Reply to a status on twitter.com.

When you write @foo, only those followers of yours who are also
following @foo will see the reply. To get around this, if you
want all of your followers to see the reply anyways begin the
tweet with \".@\" or some other filler character."
  (interactive)
  (let* ((reply-to (eagle-get-text-property 'eagle-user))
         (parent-id (eagle-get-text-property 'eagle-id))
         (post (eagle-query-for-post (concat "Loud reply to " reply-to)
                                    (concat ".@" reply-to " "))))
    (if (> (length post) 140)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post parent-id))))

;;* post retweet
;;  Begins a post with "RT @foo: waka waka waka || "
(defun eagle-post-retweet ()
  "Retweet someone else's post."
  (interactive)
  (let* ((reply-to (eagle-get-text-property 'eagle-user))
         (parent-id (eagle-get-text-property 'eagle-id))
         (retweet-text (eagle-get-text-property 'eagle-tweet-text))
         (post (eagle-query-for-post
                (concat "Retweeting " reply-to)
                (concat "RT @" reply-to ": " retweet-text " || "))))
    (if (> (length post) 140)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post parent-id))))

(defun eagle-get-text-property (propname)
  "Return a property named PROPNAME or nil if not available.

This is the reverse of `get-char-property', it checks text properties first."
  (or (get-text-property (point) propname)
      (get-char-property (point) propname)))

(defun eagle-query-for-post (prompt-heading initial-input)
  "Query for a Twitter.com post text in the minibuffer.

PROMPT-HEADING is the prompt, and has \" (140 char max): \" appended to it.
INITIAL-INPUT is what it is."
  (let ((minibuffer-setup-hook
         (cons #'eagle--query-for-post-minibuffer-setup minibuffer-setup-hook)))
    (read-string (concat prompt-heading
                         (format " (  0/%3d characters): "
                                 eagle-maximum-tweet-length))
                 initial-input)))

;;* post url
;;  Prompts for a URL, then compresses it and starts a tweet with the shortened URL in the body
(defun eagle-post-url ()
  "Compress a URL, then start posting a tweet with the result."
  (interactive)
  (let* ((url (compress-url (read-string "URL: ")))
         (post (eagle-query-for-post "Post" url)))
    (if (> (length post) eagle-maximum-tweet-length)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post))))

(defun eagle--query-for-post-minibuffer-setup ()
  "Prepare the minibuffer for a twit entry.
Limit main field length to `eagle-maximum-tweet-length' characters"

  (eagle--query-for-post-update)
  (local-set-key [remap exit-minibuffer]
  #'eagle--query-for-post-exit-minibuffer)
  (add-hook 'after-change-functions
  #'eagle--query-for-post-update t t))

(defun eagle--query-for-post-exit-minibuffer ()
  (interactive)
  (let* ((field-begin (field-beginning (point-max)))
         (field-end (field-end (point-max)))
         (field-length (- field-end field-begin)))

    (if (<= field-length eagle-maximum-tweet-length)
        (exit-minibuffer)
        (beep)
        (eagle--query-for-post-update nil nil nil t)
        (sit-for 1)
        (eagle--query-for-post-update nil nil nil))))

(defun eagle--query-for-post-update (&optional beg end length invert)
  (let* ((field-begin (field-beginning (point-max)))
         (field-end (field-end (point-max)))
         (field-length (- field-end field-begin))
         overlay)

    ;; remove old overlays
    (mapcar #'(lambda (overlay)
                (when (overlay-get overlay 'eagle--query-for-post)
                  (delete-overlay overlay)))
            (overlays-in (point-min) (point-max)))

    ;; if necessary, add a new one
    (when (> field-length eagle-maximum-tweet-length)
      (setq overlay (make-overlay (+ field-begin eagle-maximum-tweet-length) field-end))
      (overlay-put overlay 'face 'eagle-too-long-face)
      (overlay-put overlay 'eagle--query-for-post t))

    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "(\\(  0\\)/[0-9]+ characters)" nil t)
        (setq overlay (make-overlay (match-beginning 1)
                                    (match-end 1)))

        (overlay-put overlay 'eagle--query-for-post t)
        (overlay-put overlay 'display (format "%3d" field-length))

        (let ((face
               `(:foreground
                 ,(if (<= field-length eagle-maximum-tweet-length) "green" "red"))))

          (when invert
            (setq face (append '(:inverse-video t) face)))

          (overlay-put overlay 'face face))))))

(defun eagle-kill-buffer-hook ()
  (when (eq major-mode 'eagle-mode)
    (setq eagle-fetched-tweets-displayed nil)))

(defun eagle-switch-to-home ()
  "Switch to buffer `eagle-home-buffer-name'.

If it is not exist, call `eagle-home-tc' first."
  (interactive)
  (let ((buffer eagle-home-buffer-name))
    (if (get-buffer buffer)
        (switch-to-buffer eagle-home-buffer-name)
      (call-interactively 'eagle-home-tc))))

(setq eagle-user-image-dir "~/.twitter/images")
(make-directory eagle-user-image-dir t)

(eagle-set-status-format)

(add-hook 'kill-buffer-hook 'eagle-tc-mode-off-right)
(add-hook 'kill-buffer-hook 'eagle-kill-buffer-hook)

(when (featurep 'desktop)
  (dolist (var '(eagle-twitter-username eagle-twitter-password eagle-save-twitter-password
                                        eagle-current-displaying-tweets eagle-last-fetched-tweets))
    (add-to-list 'desktop-globals-to-save var)))

(provide 'eagle)

;;; eagle.el ends here
