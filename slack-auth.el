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
            (item-list (split-string auth-str)))
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
            (insert (concat (if (symbolp item) (symbol-name item) item)  " "))))
        (insert "\n"))
      (write-file filename)
      (set-file-modes filename #o600))))

(defun slack-auth-read-token (site)
  "Read saved token for given site."
  (setq slack-auth-list (slack-auth-read-from-file))
  (let (token auth (index 0) (len (length slack-auth-list)))
    (while (and (null token) (< index len))
      (setq auth (nth index slack-auth-list))
      (if (listp auth)
          (if (string= (plist-get auth ':site) site)
              (setq token (plist-get auth ':token))))
      (setq index (1+ index)))
    token))

(defun slack-auth-write-token (site token)
  "Save token for given site.")
