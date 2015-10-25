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

(require 'slack-auth)
(require 'slack-utils)

(defconst slack-domain-regexp "\\`\\([A-Za-z0-9-_]+\\)\\(\\.[Ss][Ll][Aa][Cc][Kk]\\.[Cc][Oo][Mm]\\)?\\'"
  "Regular expression for slack domain input.")

(defconst slack-token-regexp "\\`xoxp-[0-9]+-[0-9]+-[0-9]+-[[:xdigit:]]+\\'"
  "Regular expression for slack token input.")

(defun slack-open (domain token)
  "Open RTM connection to given team domain.")

(defun slack-read-args ()
  "Prompt the user for where to connect and auto info for it."
  (let* ((auth-list (slack-auth-list-all))
         (domain-list (let (d) (dolist (a auth-list)
                                 (and (plist-get a :domain)
                                      (push (plist-get a :domain) d))) d))
         domain token new)
    (setq domain
          (let ((user-input (slack-utils-completing-read "Team Domain: "
                                                         domain-list
                                                         nil
                                                         'confirm
                                                         nil
                                                         'slack-url-history)))
            (or (and (stringp user-input)
                     (string-match slack-domain-regexp user-input)
                     (downcase (replace-match "\\1" nil nil user-input)))
                (error "Domain, %S is not in valid domain format." user-input))))

    (setq token (or (plist-get (slack-auth-read-auth domain) :token)
                    (read-from-minibuffer (format "Token for %S: " domain))))

    (or (and (stringp token) (string-match slack-token-regexp token))
        (error "Token, %S is not in valid token format." token))
    (slack-auth-write-auth domain token)
    (values domain token)))

;;;###autoload
(defun slack (domain token)
  (interactive (slack-read-args))
  (and (stringp domain) (stringp token)
       (slack-open domain token)))

(provide 'slack)
