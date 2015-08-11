;;; slack-auth.el

;; Copyright (C) 2015 Yunsik Jang

;; Author: Yunsik Jang <doomsday@kldp.org>
;; Created: 7 Aug 2015

;; Keywords: applications
;; Homepage: http://github.com/zeph1e/slack.el
;; License: WTFPL version 2, grab your copy here: http://www.wtfpl.net

;; This file is not part of GNU Emacs.

(defconst slack-auth-file "~/.emacs.d/.slack-auth"
  "The default filename where slack auth info be saved.")

(defvar slack-auth-list nil
  "The variable where slack auth info be loaded.")

(defun slack-auth-read-from-file (&optional auth-file)
  "Read auth info from file."
  (let* ((filename (if (stringp auth-file) auth-file slack-auth-file))
         (contents (when (file-exists-p filename)
                     (with-temp-buffer (insert-file-contents filename)(buffer-string))))
         (auth-str-list (if (stringp contents) (split-string contents "\n")))
         auth-list)
    (dolist (auth-str auth-str-list)
      (let (auth
            (item-list (split-string auth-str "\t" t)))
        (dolist (item item-list)
          (if (keywordp (intern item))
              (push (intern item) auth)
            (push item auth)))
        (if auth (push (reverse auth) auth-list))))
    (reverse auth-list)))

(defun slack-auth-write-into-file (&optional auth-list auth-file)
  "Write auth info into file."
  (let ((filename (if (stringp auth-file) auth-file slack-auth-file)))
    (with-temp-buffer
      (dolist (auth (if auth-list auth-list slack-auth-list))
        (while auth
          (let ((item (pop auth)))
            (insert (concat (if (symbolp item) (symbol-name item) item)  "\t"))))
        (insert "\n"))
      (write-file filename)
      (set-file-modes filename #o600))))

(defun slack-auth-read-auth (site)
  "Read saved token for given site."
  (setq slack-auth-list (slack-auth-read-from-file))
  (let (found-auth url-matched-auth auth (index 0) (len (length slack-auth-list)))
    (while (and (null found-auth) (< index len))
      (setq auth (nth index slack-auth-list))
      (if (listp auth)
          (cond ((string= (plist-get auth ':site) site)
                 (setq found-auth auth))
                ((string-match (concat site ".slack.com") (plist-get auth ':url))
                 (setq url-matched-auth auth))))
      (setq index (1+ index)))
    (if found-auth found-auth url-matched-auth)))

(defun slack-auth-write-auth (site token &optional url team user)
  "Save token for given site."
  (when (or (null url) (null team) (null user))
    (let ((result (slack-auth-verify-token token)))
      (setq url (plist-get result ':url)
            team (plist-get result ':team)
            user (plist-get result ':user))
      (if (or (null url) (null team) (null user)) ; check again
          (error (format "Failed to get names of team and user from %S" site))))
  (let ((existing-auth (slack-auth-read-auth site)))
    (if existing-auth
        (setq slack-auth-list (delete existing-auth slack-auth-list))))
  (let ((new-auth (list :site site :token token :url url :team team :user user)))
    (push new-auth slack-auth-list)
    (slack-auth-write-into-file)
    new-auth)))

(defun slack-auth-verify-token (token)
  "Verify auth token."
  (let ((result (slack-rpc-auth-test token)))
    (unless (eq (cdr (assq 'ok result)) t)
      (error (format "Invalid auth token : %S" token)))
    (list :token token
          :url (cdr (assq 'url result))
          :team (cdr (assq 'team result))
          :user (cdr (assq 'user result)))))

(defun slack-auth-list-all ()
  "List all auth info."
  (setq slack-auth-list (slack-auth-read-from-file))
  slack-auth-list)


(provide 'slack-auth)
