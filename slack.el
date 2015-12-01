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
(require 'slack-rtm)
(require 'slack-utils)

(defvar slack-domain nil
  "The domain name of team.")
(make-variable-buffer-local 'slack-domain)

(defvar slack-token nil
  "The auth token of the team where this session is connected to.")
(make-variable-buffer-local 'slack-token)

(defvar slack-channel nil
  "The name of channel in team where this buffer is attached to.")
(make-variable-buffer-local 'slack-channel)

(defvar slack-user nil
  "The name of current user.")
(make-variable-buffer-local 'slack-user)

(defvar slack-websocket nil
  "The websocke for realtime messaging.")
(make-variable-buffer-local 'slack-websocket)

(defvar slack-self nil
  "The data about current user.")
(make-variable-buffer-local 'slack-self)

(defvar slack-team nil
  "The data about the team where the session is connected to.")
(make-variable-buffer-local 'slack-team)

(defvar slack-users nil
  "The hash table about the members in this team.")
(make-variable-buffer-local 'slack-users)

(defvar slack-channels nil
  "The hash table about the channels which are opened and visible to current user.")
(make-variable-buffer-local 'slack-channels)

(defvar slack-groups nil
  "The hash table about the private groups.")
(make-variable-buffer-local 'slack-groups)

(defvar slack-mpims nil
  "The hash table about multiparty IM.")
(make-variable-buffer-local 'slack-mpims)

(defvar slack-ims nil
  "The hash table about direct message channel.")
(make-variable-buffer-local 'slack-ims)

(defvar slack-bots nil
  "The hash table about bots.")
(make-variable-buffer-local 'slack-bots)


(defun slack-make-id-table (array)
  "Make hash table for array items, like users, channels, ..."
  (let ((table (make-hash-table :test 'equal))
        (len (length array))
        (index 0))
    (while (< index len)
      (let* ((object (aref array index))
             (id (cdr (assq 'id object))))
        (unless (stringp id)
          (error (format "No id field is specified in: %s" object)))
        (puthash id object table))
      (setq index(1+ index)))
    table))

(defun slack-buffer-p (&optional buffer)
  "Return non-nil if given BUFFER or current-buffer is slack buffer."
  (with-current-buffer (or buffer (current-buffer))
    (eq major-mode 'slack-mode)))

(defun slack-open-p ()
  "Return non-nil if rtm session is opened."
  (and (websocket-p slack-websocket)
       (websocket-openp slack-websocket)))

(defun slack-generate-new-buffer-name (domain user channel)
  "Create buffer name based on given domain, user, channel names."
  (let* ((candidates (or (and channel
                              (list channel
                                    (concat channel "<" user "@" domain ">")))
                         (list domain
                               (concat user "@" domain))))
         buffer-name)
    (dolist (candidate candidates)
      (if (and (not buffer-name)
               (get-buffer candidate)
               (with-current-buffer (get-buffer candidate)
                 (and (slack-buffer-p)
                      (not (slack-open-p)))))
          (set buffer-name candidate)))
    (or buffer-name (generate-new-buffer-name (car candidates)))))

(defun slack-get-buffer-create (domain user &optional channel)
  "Create a new buffer based on given arguments."
  (get-buffer-create (slack-generate-new-buffer-name domain user channel)))

(defconst slack-event-handlers
  '((open . slack-event-open)
    (close . slack-event-close)
    (hello . slack-event-hello)
    (message . slack-event-message))
  "Slack RTM event handlers.")

(defun slack-open (&optional domain token user channel websocket)
  "Open RTM connection to given team domain."
  (let ((buffer (slack-get-buffer-create domain user channel))
        (old-buffer (current-buffer)))
    (with-current-buffer buffer
      (slack-mode)
      (setq slack-domain domain
            slack-token token
            slack-channel channel
            slack-user user
            slack-websocket websocket)
      (goto-char (point-max))
      (forward-line 0)
      (when (not (slack-open-p))
        (let ((response (slack-rpc-rtm-start slack-token)))
          (setq slack-self (cdr (assq 'self response))
                slack-team (cdr (assq 'team response))
                slack-users (slack-make-id-table (cdr (assq 'users response)))
                slack-channels (slack-make-id-table (cdr (assq 'channels response)))
                slack-groups (slack-make-id-table (cdr (assq 'groups response)))
                slack-mpims (slack-make-id-table (cdr (assq 'mpims response)))
                slack-ims (slack-make-id-table (cdr (assq 'ims response)))
                slack-bots (slack-make-id-table (cdr (assq 'bots response)))
                slack-websocket (slack-rtm-open (cdr (assq 'url response)) slack-event-handlers))))

      (if (active-minibuffer-window)
          (display-buffer buffer)
        (display-buffer buffer)))))

(defun slack-close (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (slack-open-p)
      (error "Already disconnected"))
    (and (yes-or-no-p (message "Really close connection from %S? " (cdr (assq 'name slack-team))))
         (slack-rtm-close slack-websocket))))

(defconst slack-domain-regexp "\\`\\([A-Za-z0-9-_]+\\)\\(\\.[Ss][Ll][Aa][Cc][Kk]\\.[Cc][Oo][Mm]\\)?\\'"
  "Regular expression for slack domain input.")

(defconst slack-token-regexp "\\`xoxp-[0-9]+-[0-9]+-[0-9]+-[[:xdigit:]]+\\'"
  "Regular expression for slack token input.")

(defun slack-read-args ()
  "Prompt the user for where to connect and auto info for it."
  (let* ((auth-list (slack-auth-list-all))
         (domain-list (let (d) (dolist (a auth-list)
                                 (and (plist-get a :domain)
                                      (push (plist-get a :domain) d))) d))
         (domain (let ((user-input (slack-utils-completing-read "Team Domain: "
                                                                domain-list
                                                                nil
                                                                'confirm
                                                                nil
                                                                'slack-url-history)))
                   (or (and (stringp user-input)
                            (string-match slack-domain-regexp user-input)
                            (downcase (replace-match "\\1" nil nil user-input)))
                       (error "Domain, %S is not in valid domain format." user-input))))

         (token (or (plist-get (slack-auth-read-auth domain) :token)
                    (read-from-minibuffer (format "Token for %S: " domain))))
         (auth (or (and (stringp token) (string-match slack-token-regexp token)
                        (slack-auth-write-auth domain token))
                   (error "Token, %S is not in valid token format." token))))
    (values domain token (plist-get auth :user))))

(defcustom slack-prompt "SLACK>"
  "Prompt of slack")

(defun slack-prompt ()
  (let ((prompt (if (functionp slack-prompt)
                    (funcall slack-prompt)
                  slack-prompt)))
    (if (> (length prompt) 0)
        (concat prompt " ")
      prompt)))

(defvar slack-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    syntax-table)
  "Syntax table used while in Slack mode.")

(defvar slack-mode-abbrev-table nil
  "Abbrev table used whil in slack mode.")

(defvar slack-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left-margin> <mouse-1>") 'slack-event-sidebar)
    map)
  "Slack keybindings.")

(defun slack-mode ()
  "Major mode for Emacs slack client.

Keybindings:
\\{slack-mode-keymap}
"
  (kill-all-local-variables)
  (use-local-map slack-mode-keymap)
  (setq mode-name "#Slack"
        major-mode 'slack-mode
        local-abbrev-table slack-mode-abbrev-table
        line-move-ignore-invisible t
        linum-mode nil)
  (set-syntax-table slack-mode-syntax-table)
  (when (boundp 'next-line-add-newlines)
    (set (make-local-variable 'next-line-add-newlines) nil))
  (set (make-local-variable 'paragraph-separate)
       (concat "\C-l\\|\\(^" (regexp-quote (slack-prompt)) "\\)"))
  (set (make-local-variable 'paragraph-start)
       (concat "\\(" (regexp-quote (slack-prompt)) "\\)"))

  (run-hooks 'slack-mode-hook))


;;;###autoload
(defun slack (domain token user)
  (interactive (slack-read-args))
  (and (stringp domain) (stringp token)
       (slack-open domain token user)))

(provide 'slack)
