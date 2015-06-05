;;; slack-http.el

(defgroup slack-http nil
"Functions for HTTP/HTTPS request & response."
 :prefix "slack-http-"
 :group 'apis)

(require 'url)
(require 'json)
(require 'slack-compat)

(defvar slack-http-endpoint-url "https://slack.com/api/")

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
  ;; TODO: fix me to handle redirect!!
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

  (funcall callback context (json-encode (slack-http--extract-body (current-buffer)))))


(defun slack-http--form-string (list)
"Convert list into CGI form string.

LIST : alist or plist
"
  (unless (listp list)
    (signal 'wrong-type-argument (list list)))
  (mapconcat (lambda (arg)
	       (concat (url-hexify-string (car arg)) "="
		       (url-hexify-string (cdr arg))))
	     list "&"))

(defun slack-http-call-method (method list &optional callback context)
"Receive a content from given URL over HTTP/HTTPS with GET method.

METHOD   : Slack API method to call.
LIST     : alist or plist which contains key-value pair
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
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded")))
	(url-request-data (slack-http--form-string (list))))
    (let ((url-request-method (if (> (+ (length encoded-url)(length url-request-data)) 510)
			       "POST" "GET")))
      (if (functionp callback)
	  (with-current-buffer
	      (url-retrieve encoded-url 'slack-http--callback (list callback method context) nil nil)
	    (make-local-variable 'url-http-response-status)
	    (let (process)
	      (ignore-errors
		(setq process (get-buffer-process (current-buffer))))
	      (if (processp process)
		(unless (process-live-p process)
		    (process-kill-without-query process)
		    (delete-process process)
		    (signal 'slack-http-error (list process))))
	    context))

	(with-current-buffer (url-retrieve-synchronously encoded-url)
	    (let (process)
	      (ignore-errors
		(setq process (get-buffer-process (current-buffer))))
	      (if (processp process)
		(unless (process-live-p process)
		    (process-kill-without-query process)
		    (delete-process process)
		    (signal 'slack-http-error (list process))))
	      (make-local-variable 'url-http-response-status)
	      (let ((json-object-type 'plist)
		    (body (slack-http--extract-body (current-buffer))))
		(if body (json-read-from-string body)))))))))

(provide 'slack-http)
