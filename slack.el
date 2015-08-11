;;; slack.el -- An Emacs Slack client

;; Copyright (C) 2015 Yunsik Jang

;; Author: Yunsik Jang <doomsday@kldp.org>
;; Created: 16 Jun 2015

;; Keywords: applications
;; Homepage: http://github.com/zeph1e/slack.el
;; License: WTFPL version 2, grab your copy here: http://www.wtfpl.net

;; This file is not part of GNU Emacs.

;; Many idea was from ERC(http://savannah.gnu.org/projects/erc/).

(defgroup slack nil
"An Emacs slack client.
Slack is a community service presented by http://slack.com
"
 :prefix "slack-"
 :group 'applications
 :link '(url-link "https://github.com/zeph1e/slack.el"))

(require 'slack-rpc)
(require 'slack-rtm)
(require 'slack-auth)
(require 'slack-utils)

(defcustom slack-mode-hook nil
  "Hook run after `slack-mode' setup is finished."
  :group 'slack-hooks
  :type 'hook)

;; http://emacswiki.org/emacs/ModeTutorial
(defvar slack-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    syntax-table)
  "Syntax table used while in Slack mode.")

(defvar slack-mode-abbrev-table nil
  "Abbrev table used whil in slack mode.")

(defvar slack-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'slack-open)
    (define-key map (kbd "C-c c") 'slack-close)
    (define-key map (kbd "C-m") 'slack-send-current-line)
    map)
  "Slack keybindings.")

(defvar slack-session nil
  "The session state for realtime messaging.")
(make-variable-buffer-local 'slack-session)

(defvar slack-websocket nil
  "The websocket for realtime messaging.")
(make-variable-buffer-local 'slack-websocket)

(defvar slack-site nil
  "The site name of team, hostname in team site url.")
(make-variable-buffer-local 'slack-site)

(defvar slack-token nil
  "The auth token of the team where this session is connected to.")
(make-variable-buffer-local 'slack-token)

(defvar slack-team nil
  "The team info, in alist, where this session is connected to.")
(make-variable-buffer-local 'slack-team)

(defvar slack-user nil
  "The user info, in alist, where this session is connected to.")
(make-variable-buffer-local 'slack-user)

(defvar slack-ims nil
  "The direct message channels, in list of alist, where this session is connected to.")
(make-variable-buffer-local 'slack-ims)

(defvar slack-users nil
  "The users in the team, in list of alist, where this session is connected to.")
(make-variable-buffer-local 'slack-users)

(defvar slack-bots nil
  "The bots in the team, in list of alist, where this session is connected to.")
(make-variable-buffer-local 'slack-bots)

(defvar slack-insert-marker nil
  "The place where insertion of new text.")
(make-variable-buffer-local 'slack-insert-marker)

(defvar slack-input-marker nil
  "The marker where input should be inserted.")
(make-variable-buffer-local 'slack-input-marker)

(defconst slack-session-handlers
  '((open . (lambda () (message "Opened")))
    (close . (lambda () (message "Closed")
               (setq slack-websocket nil)))
    (hello . (lambda (payload) (message "Hello")))
    (message . (lambda (payload)
                 (let* ((uid (cdr (assq 'user payload)))
                        (chid (cdr (assq 'channel payload)))
                        (text (cdr (assq 'text payload)))
                        (ts (cdr (assq 'ts payload)))
                        (tid (cdr (assq 'team payload)))
                        (user (cdr (assq 'name (gethash uid slack-users))))
                        (channel (cdr (assq 'name (gethash chid slack-channels))))
                        (team (cdr (assq 'name slack-team))))
                   (save-excursion
                     (switch-to-buffer (format "%s/%s" team channel))
                     (goto-char (point-max))
                     (insert (concat "<" user "> " text "\n"))))))
    ))

(defcustom slack-prompt "SLACK>"
  "Prompt of Slack.")

(defun slack-prompt ()
  (let ((prompt (if (functionp slack-prompt)
                    (funcall slack-prompt)
                  slack-prompt)))
    (if (> (length prompt) 0)
        (concat prompt " ")
      prompt)))

(defun slack-beginning-of-input-line ()
  (or (and (boundp 'slack-insert-marker)
           (markerp slack-insert-marker))
      (error "slack-insert-markser has no value."))
  (marker-position slack-insert-marker))

(defun slack-end-of-input-line ()
  (point-max))

(defun slack-user-input ()
  (buffer-substring-no-properties
   slack-input-marker
   (slack-end-of-input-line)))

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

(defun slack-generate-new-buffer-name (site user team channel)
  "Create a newbuffer name based on the arguments."
  (let (candidates buffer-name)
    (if channel
        (setq candidates (list (concat channel "/" site)
                               (concat channel "/" site "(" team ")")))
      (setq candidates (list (concat user "@" site)
                             (concat user "@" site "(" team ")"))))
    candidates))
;; TODO: find short one from candidates
;;    (dolist (candidate candidates)


(defun slack-get-buffer-create (site user team &optional channel)
  "Create a new buffer based on the arguments."
  (get-buffer-create (slack-generate-new-buffer-name site user team channel)))

(defun slack-open (site token team user connect &optional channel websocket)
  "Connect to SITE, the mnemonic of TEAM with TOKEN AS USER.

If CONNECT is non-nil, start rtm session. Otherwise assume 
already connected and just create a separate buffer for the new
target CHANNEL.

Returns the buffer for the given SITE or CHANNEL.")

  ;; (if (websocket-p slack-websocket)
  ;;     (error "Already connected"))
  ;; (let ((team-site team) team-token)
  ;;   (unless (stringp team-site)
  ;;     (setq team-site (read-string "Team: ")))
  ;;   ;; TODO: get saved token like:
  ;;   ;; (setq team-token (slack-auth-read-token team-site))
  ;;   (unless (stringp team-token)
  ;;     (setq team-token (read-string "Auth token: "))
  ;;     (message "Testing if token is valid...")
  ;;     (unless (eq (cdr (assq 'ok (slack-rpc-auth-test nil team-token))) t)
  ;;       (error (format "Invalid auth token: %S" team-token)))
  ;;     ;; TODO: now it's valid token save it into somewhere like:
  ;;     ;; (slack-auth-write-token team-site team-token)
  ;;     (message "Token is valid!"))
  ;;   (setq slack-site team-site
  ;;         slack-token team-token))
  ;; (let ((response (slack-rpc-rtm-start slack-token)))
  ;;   (setq slack-user (cdr (assq 'self response))
  ;;         slack-team (cdr (assq 'team response))
  ;;         slack-channels (slack--make-id-table (cdr (assq 'channels response)))
  ;;         slack-ims (slack--make-id-table (cdr (assq 'ims response)))
  ;;         slack-users (slack--make-id-table (cdr (assq 'users response)))
  ;;         slack-bots (slack--make-id-table (cdr (assq 'bots response)))
  ;;         slack-websocket (slack-rtm-open (cdr (assq 'url response)) slack-session-handlers))))

(defun slack-close ()
  (interactive)
  (unless (websocket-p slack-websocket)
      (error "Already disconnected"))
  (when (yes-or-no-p "Really close? ")
    (slack-rtm-close slack-websocket)))

(defun slack-mode ()
"Major mode for Emacs Slack client.

Keybindings:
\\{slack-mode-keymap}
"
  (kill-all-local-variables)
  (use-local-map slack-mode-keymap)
  (setq mode-name "#Slack"
	major-mode 'slack-mode
	local-abbrev-table slack-mode-abbrev-table)
  (set-syntax-table slack-mode-syntax-table)
  (when (boundp 'next-line-add-newlines)
    (set (make-local-variable 'next-line-add-newlines) nil))
  (setq line-move-ignore-invisible t)
  (set (make-local-variable 'paragraph-separate)
       (concat "\C-l\\|\\(^" (regexp-quote (slack-prompt)) "\\)"))
  (set (make-local-variable 'paragraph-start)
       (concat "\\(" (regexp-quote (slack-prompt)) "\\)"))

  (run-hooks 'slack-mode-hook))

(defun slack-team-list-mode ()
  (setq buffer-read-only t
        mode-name "#Slack Team List"
        major-mode 'slack-team-list-mode))

(defvar slack-site-history-list nil
  "Slack team site interactive selection history list.")

(defconst slack-read-args-description
  '((site . ("Team site" "`Team site' is a name of team site where you connect to.
This may be a mnemonic of a site or an actual host name of team site url.

For an example, you can connect to your team on https://myteam.slack.com
by giving either \"myteam\" or \"team1\", if you, previously, saved the team as \"team1\"."))
    (token . ("Token" "`Token' is an authentication token which identifies a single user.
You may find tokens for your team sites on [https://api.slack.com/web#authentication]")))
  "Help descriptions for slack-read-args arguments.")

(defun slack-describe-read-args (arg)
  "Describe the args in slack-read-args for given ARG."
  (interactive "sDescribe input type: ")
  (let* ((describe-func
          (function
           (lambda (s)
             (if (assq (intern arg) slack-read-args-description)
                 (princ
                  (format "%s\n\n%s\n\n"
                          (car (cdr (assq (intern arg) slack-read-args-description)))
                          (cadr (cdr (assq (intern arg) slack-read-args-description))))))))))
    (help-setup-xref (list 'slack-describe-read-args arg) (interactive-p))
    (save-excursion
      (with-help-window (help-buffer)
        (mapcar describe-func '("*"))
        (with-current-buffer (help-buffer)
          (goto-char (point-min))
          (while
              (ignore-errors
                (let* ((endpt
                        (search-forward-regexp "\\[[Hh][Tt][Tt][Pp]\\([Ss]?\\)://[a-zA-Z0-9.#-_?=&]+\\]"))
                       (startpt (progn (backward-sexp) (point)))
                       (linkstr (buffer-substring startpt endpt))
                       (urlstr (substring linkstr 1 (1- (length linkstr))))
                       (inhibit-read-only t))
                  (when (stringp linkstr)
                    (delete-region startpt endpt)
                    (insert-button linkstr
                                   'action (lambda (x) (browse-url (button-get x 'url)))
                                   'url urlstr))))))))))
(defun slack-read-args ()
  "Prompt the user for where to connect and auth to connect to it."
  (let (site auth user-input)
    (setq user-input (read-from-minibuffer
                      "Team site (? for help): "
                      nil nil nil 'slack-site-history-list))
    (while (string-match "^\\s-+\\|\\s-+$" user-input)
      (setq user-input (replace-match "" t t user-input)))
    (cond ((string= "?" user-input) (slack-describe-read-args "site"))
          ((< 0 (length user-input)) (ignore-errors
                                       (setq site user-input
                                             auth (slack-auth-read-auth user-input))))
          (t (error "Nothing entered.")))
    (if site
        (unless auth
          (setq user-input (read-from-minibuffer (format "Token for %S (? for help): " site)))
          (while (string-match "^\\s-+\\|\\s-+$" user-input)
            (setq user-input (replace-match "" t t user-input)))
          (setq auth (cond ((string= "?" user-input) (slack-describe-read-args "token") nil)
                           ((< 0 (length user-input))
                            (if (y-or-n-p (format "Save this site, %S?" site))
                                (slack-auth-write-auth site user-input)
                              (append (list :site site) (slack-auth-verify-token user-input))))))))
      (values (plist-get auth ':site)
              (plist-get auth ':token)
              (plist-get auth ':team)
              (plist-get auth ':user))))

;;;###autoload
(defun slack (site token team user)
  (interactive (slack-read-args))
  (when (and (stringp site) (stringp token) (stringp team) (stringp user))
    (message "%s/%s -- %s@%s" site token user team)
    (slack-open site token team user t)))

;;;###autoload
(defun slack-list-teams ()
  (interactive)
  (let ((listbuf (get-buffer-create "#Slack"))
        (inhibit-read-only t))
    (with-current-buffer listbuf
      (pop-to-buffer listbuf)
      (delete-region (point-min)(point-max))
      (let ((auth-list (slack-auth-list-all)))
        ;; write team list in format
        )
      (slack-team-list-mode))))

(provide 'slack)
