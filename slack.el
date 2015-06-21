;;; slack.el -- An Emacs Slack client

;; Copyright (C) 2015 Yunsik Jang

;; Author: Yunsik Jang <doomsday@kldp.org>
;; Created: 16 Jun 2015

;; Keywords: applications
;; Homepage: http://github.com/zeph1e/slack.el
;; License: WTFPL version 2, grab your copy here: http://www.wtfpl.net

;; This file is not part of GNU Emacs.

(defgroup slack nil
"An Emacs slack client.
Slack is a community service presented by http://slack.com
"
 :prefix "slack-"
 :group 'applications
 :link '(url-link "https://github.com/zeph1e/slack.el"))

(require 'slack-rpc)
(require 'slack-rtm)

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
    ;; (define-key map (kbd "C-m") 'slack-send-message)
    map)
  "Slack keybindings.")

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
  (run-hooks 'slack-mode-hook))

;;;###autoload
(defun slack ()
  (interactive)
  (set-buffer(get-buffer-create "slack"))
  (delete-region (point-min)(point-max))
  (switch-to-buffer-other-window (current-buffer))
  (slack-mode))

(provide 'slack)
