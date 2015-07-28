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
    (define-key map (kbd "C-c o") 'slack-open-session)
    (define-key map (kbd "C-c c") 'slack-close-session)
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
  '((open . (lambda () (message "opened")))
    (close . (lambda () (message "closed")
               (setq slack-websocket nil)))))

(defcustom slack-prompt "SLACK>"
  "Prompt of Slack.")

(define-error 'slack-error "General Slack error" 'error)
(define-error 'slack-already-connected-error "Already connected" 'slack-error)
(define-error 'slack-invalid-auth-token-error "Invalid auth token" 'slack-error)

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

(defun slack-open-session (&optional team)
  (interactive)
  (if (websocket-p slack-websocket)
      (signal 'slack-already-connected-error '()))
  (let ((team-site team) team-token)
    (unless (stringp team-site)
      (setq team-site (read-string "Team: ")))
    ;; TODO: get saved token like:
    ;; (setq team-token (slack-auth-read-token team-site))
    (unless (stringp team-token)
      (setq team-token (read-string "Auth token: "))
      (message "Testing if token is valid...")
      (unless (eq (cdr (assq 'ok (slack-rpc-auth-test nil team-token))) t)
        (signal 'slack-invalid-auth-token-error (list team-token)))
      ;; TODO: now it's valid token save it into somewhere like:
      ;; (slack-auth-write-token team-site team-token)
      (message "Token is valid!"))
    (setq slack-site team-site
          slack-token team-token))
  (let ((response (slack-rpc-rtm-start nil slack-token)))
    (setq slack-user (cdr (assq 'self response))
          slack-team (cdr (assq 'team response))
          slack-channels (cdr (assq 'channels response))
          slack-ims (cdr (assq 'ims response))
          slack-users (cdr (assq 'users response))
          slack-bots (cdr (assq 'bots response))
          slack-websocket (slack-rtm-open (cdr (assq 'url response)) slack-session-handlers))))

(defun slack-close-session ()
  (interactive)
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

;;;###autoload
(defun slack ()
  (interactive)
  (set-buffer(get-buffer-create "slack"))
  (delete-region (point-min)(point-max))
  (switch-to-buffer-other-window (current-buffer))
  (slack-mode)

  (goto-char (point-max))
  (forward-line 0)


  )

(provide 'slack)
