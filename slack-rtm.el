;;; slack-rtm.el -- Realtime Messaging API

;; Copyright (C) 2015 Yunsik Jang

;; Author: Yunsik Jang <doomsday@kldp.org>
;; Created: 16 Jun 2015

;; Keywords: applications
;; Homepage: http://github.com/zeph1e/slack.el
;; License: WTFPL version 2, grab your copy here: http://www.wtfpl.net

;; This file is not part of GNU Emacs.

(require 'json)
(require 'slack-compat)

(require 'websocket)

(defun slack-rtm-open (ws-address handlers-plist)
  "Open Realtime messaging connection.

WS-ADDRESS is an websocket address to connect.
HANDLERS-PLIST is a plist of event handlers."
  (let ((ws (websocket-open (ws-address
                   :on-open (lambda (websocket))
                   :on-message (lambda (websocket frame))
                   :on-close (lambda (websocket))
                   :on-error (lambda (websocket cause error))))))
    )

)

(defun slack-rtm-send-message (channel message))

(defun slack-rtm-send-typing (channel))

(defun slack-rtm-onevent (event))

(defun slack-rtm-close ())



(provide 'slack-rtm)
