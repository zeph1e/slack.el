;;; slack-rtm.el -- Realtime Messaging API

;; Copyright (C) 2015 Yunsik Jang

;; Author: Yunsik Jang <doomsday@kldp.org>
;; Created: 16 Jun 2015

;; Keywords: applications
;; Homepage: http://github.com/zeph1e/slack.el
;; License: WTFPL version 2, grab your copy here: http://www.wtfpl.net

;; This file is not part of GNU Emacs.

(require 'json)
(require 'slack-utils)

(require 'websocket)

(define-error 'slack-rtm-error "Slack RTM error" 'error) ; general rtm error
(define-error 'slack-rtm-emacs-version "upgrade emacs to 24" 'slack-rtm-error)
(define-error 'slack-rtm-timeout-error "websocket timeout" 'slack-rtm-error)
(define-error 'slack-rtm-invalid-payload "invalid payload received" 'slack-rtm-error)
(define-error 'slack-rtm-invalid-context "invalid websocket context" 'slack-rtm-error)

(defvar slack-rtm-last-typing-sent '()
  "An alist which contains the last typing message sent and the channel where the message sent.")

(defun slack-rtm-open (ws-url handlers-alist)
  "Open Realtime messaging websocket and returns a websocket identifier.
WS-URL is an websocket URL to connect, returned from Slack Web API
`rtm.start'. As the Slack API guide says, you should call this
function in 30 seconds after `rtm.start' call. Otherwise, Slack would
refuse your connection.

... HANDLERS PART WILL BE UPDATED LATER
"
  (let* ((websocket
          (websocket-open
           ws-url
           :on-open (lambda (ws)
                      (let* ((ctx (websocket-client-data ws))
                             ;(req-timer (plist-get ctx ':req-timer))
                             (handler-table (plist-get ctx ':handlers)))
                        (cond ((not (hash-table-p handler-table))
                               (signal 'slack-rtm-invalid-context (list handler-table))))
                        (let ((handler (gethash "open" handler-table)))
                          (if (functionp handler) (funcall handler nil)))))

           :on-message (lambda (ws frame)
                         (let* ((ctx (websocket-client-data ws))
                                (handler-table (plist-get ctx ':handlers))
                                (payload (json-read-from-string (websocket-frame-payload frame)))
                                (type (cdr (assq 'type payload))))
                           (cond ((not (stringp type)) (signal 'slack-rtm-invalid-payload (list type)))
                                 ((not (hash-table-p handler-table))
                                  (signal 'slack-rtm-invalid-context (list handler-table))))
                           (let ((handler (gethash type handler-table)))
                             (if (functionp handler) (funcall handler payload)))))

           :on-close (lambda (ws)
                       (let* ((ctx (websocket-client-data ws))
                              (handler-table (plist-get ctx ':handlers)))
                         (let ((handler (gethash "close" handler-table)))
                           (if (functionp handler) (funcall handler nil)))))

           :on-error (lambda (ws type err)
                       (websocket-default-error-handler ws type err))))

         (handler-table (make-hash-table :test 'equal)))

    ;; build handler-table
    (dolist (pair handlers-alist nil)
      (let* ((event (slack-http--stringify (car pair)))
             (handler (cdr pair)))
        (puthash event (if (functionp handler) handler) handler-table)))

    ;; insert hander table into context
    (let ((ctx (list)))
      (setq ctx (plist-put ctx ':handlers handler-table))
      (setf (websocket-client-data websocket) ctx))
    websocket))

(defun slack-rtm-close (websocket)
  (websocket-close websocket))

(defun slack-rtm-send-message (websocket channel message)
  (websocket-send-text websocket
                       (json-encode
                        (list ':id (slack-utils-id)
                              ':type "message"
                              ':channel channel
                              ':text (slack-rtm--escape-string message)))))

(defun slack-rtm-send-typing (websocket channel)
  (websocket-send-text websocket
                       (json-encode
                        (list ':id (slack-utils-id)
                              ':type "typing"
                              ':channel channel))))


(defun slack-rtm--escape-string (text))


(provide 'slack-rtm)
