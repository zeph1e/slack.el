;;; slack.el

;; Copyright (C) 2015 Yunsik Jang

;; Author: Yunsik Jang <doomsday@kldp.org>
;; Created: 23 Oct 2015

;; Keywords: applications
;; Homepage: http://github.com/zeph1e/slack.el
;; License: WTFPL version 2, grab your copy here: http://www.wtfpl.net

;; This file is not part of GNU Emacs.

(defgroup slack nil
  "An Emacs slack client"
  :prefix "slack-"
  :group 'applications
  :link '(url-link "https://github.com/zeph1e/slack.el"))

(require 'slack-utils)

(defun slack-read-args ()
  "Prompt the user for where to connect and auto info for it."
  (let* ((auth-list (slack-auth-list-all))
         (url-list (let (r) (dolist (a auth-list)
                              (and (plist-get a :url)
                                   (push (plist-get a :url) r)) r)))
         url token user-input)
    (setq user-input
          (slack-utils-completing-read "Team Domain: https://"
                                       url-list
                                       nil
                                       'confirm
                                       nil
                                       'slack-url-history))
    ))

;;;###autoload
(defun slack (url token user)
  (interactive (slack-read-args))
  (and (stringp url) (stringp token) (stringp user)
       (slack-open url token user)))

(provide 'slack)
