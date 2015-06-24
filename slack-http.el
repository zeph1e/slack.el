;;; slack-http.el

;; Copyright (C) 2015 Yunsik Jang

;; Author: Yunsik Jang <doomsday@kldp.org>
;; Created: 16 Jun 2015

;; Keywords: applications
;; Homepage: http://github.com/zeph1e/slack.el
;; License: WTFPL version 2, grab your copy here: http://www.wtfpl.net

;; This file is not part of GNU Emacs.

(defgroup slack-http nil
"Functions for HTTP/HTTPS request & response."
 :prefix "slack-http-"
 :group 'apis)

(require 'url)
(require 'json)
(require 'slack-compat)

(defconst slack-http-endpoint-url "https://slack.com/api/"
  "Slack API end point URL.")

(define-error 'slack-http-error "Slack HTTP error" 'error) ; general
(define-error 'slack-http-not-auth-error "Not authorized"  'slack-http-error) ;401
(define-error 'slack-http-invalid-api-error "Invalid slack api" 'slack-http-error) ; 404
(define-error 'slack-http-temporarily-unavailable-error "Temporarily unavailable" 'slack-http-error) ;408, 5XX
(define-error 'slack-http-changed-error "Slack api changed" 'slack-http-error) ; 3XX

(defun slack-http--extract-body (buffer)
"Extract body from buffer."
  (set-buffer buffer)
  (goto-char (point-min))
  (unless (eq (point-min) (point-max))
    (if (search-forward-regexp "^$" nil t) ; empty line
	(buffer-substring (1+ (point))(point-max)))))

(defun slack-http--callback (status callback method context)
"Callback bridge for `slack-http-post'."

  (if (> (length status) 0)
    (let ((type (car status))
	  (http-status url-http-response-status))
      (cond ((eq type ':redirect)
	     (signal 'slack-http-changed-error (list context method)))
	    ((eq type ':error)
	     (signal (car (cdr status)) (list context (cdr (cdr status)))))
	    ((eq http-status 404)
	     (signal 'slack-http-invalid-api-error (list context method)))
	    ((eq http-status 401)
	     (signal 'slack-http-not-auth-error (list context method)))
	    ((or (eq http-status 408) (>= http-tatus 500))
	     (signal 'slack-http-temporarily-unavailable-error (list context method)))
	    ((not (eq http-status 200)) (signal 'slack-http-error (list context method))))))

  (funcall callback context
	   (json-read-from-string (slack-http--extract-body (current-buffer)))))

(defun slack-http--stringify (s)
  "Stringify keyword or symbol."
  (cond ((keywordp s) (substring (symbol-name s) 1))
        ((symbolp s) (symbol-name s))
        ((stringp s) s)
        ((floatp s) (format "%f" s))
        ((integerp s) (format "%d" s))
        (t (signal 'wrong-type-argument (list s)))))

(defun slack-http--form-string (list)
  "Encode CGI form string from alist/plist."
  (cond ((json-alist-p list) (slack-http--form-string-from-alist list))
        ((json-plist-p list) (slack-http--form-string-from-plist list))
        ((null list) "")
        (t (signal 'wrong-type-argument (list list)))))

(defun slack-http--form-string-from-alist (list)
  "Encode CGI form string from alist."
  (mapconcat (lambda (arg)
	       (if arg
		   (let ((key (car arg))
			 (value (cdr arg)))
		     (if value
			 (concat (url-hexify-string (if (symbolp key) (symbol-name key) key)) "="
				 (url-hexify-string (if (symbolp value) (symbol-name value) value)))))))
	     list "&"))

(defun slack-http--form-string-from-plist (plist)
  "Encode CGI form string from plist."
  (let (result)
    (while plist
      (let* ((key (slack-http--stringify (car plist)))
             (value (slack-http--stringify (cadr plist))))
        (setq plist (cddr plist))
        (setq result (concat result (if result "&") key "=" value))))
    result))


(defun slack-http-call-method (method form-list &optional callback context)
"Receive a content from given URL over HTTP/HTTPS.

METHOD   : Slack API method to call.
FORM-LIST: list of HTTP form request key value pairs (in plist or alist).
CALLBACK : if non-nil, the response will be received synchronously
           and will return a list : (http-status content).
           Otherwise, the response will be delivered by calling callback.
CONTEXT  : Context for callback
"
  (let ((encoded-url
	 (url-encode-url (concat slack-http-endpoint-url
				 (cond ((symbolp method) (symbol-name method))
				       ((stringp method) method)
				       (t (signal 'wrong-type-argument (list method)))))))
	(url-request-extra-headers '(("Content-type" . "application/x-www-form-urlencoded")))
	(url-request-data (slack-http--form-string form-list)))
    (let ((url-request-method (if (> (+ (length encoded-url)(length url-request-data)) 510) "POST" "GET")))

      (if (string= url-request-method "GET")
      	  (progn
      	    (setq encoded-url (concat encoded-url "?" url-request-data))
      	    (setq url-request-data nil)
      	    (setq url-request-extra-headers nil)))

      (message "%s" url-request-data)
      (if (functionp callback)
	  (with-current-buffer
	      (url-retrieve encoded-url 'slack-http--callback (list callback method context) nil nil)
	    (make-local-variable 'url-http-response-status)
	    (let (process)
	      (ignore-errors
		(setq process (get-buffer-process (current-buffer))))
	      (if (processp process)
		  (progn
		    (unless (process-live-p process)
		      (process-kill-without-query process)
		      (delete-process process)
		      (signal 'slack-http-error (list process)))))
	    context))

	(with-current-buffer (url-retrieve-synchronously encoded-url)
	    (let (process)
	      (ignore-errors
		(setq process (get-buffer-process (current-buffer))))
	      (make-local-variable 'url-http-response-status)
	      (if (processp process)
		  (progn
		    (unless (process-live-p process)
		      (process-kill-without-query process))
		    (delete-process process)
		    (signal 'slack-http-error (list process))))
	      (let ((body (slack-http--extract-body (current-buffer))))
		(if body (json-read-from-string body)))))))))

(provide 'slack-http)
