;;; slack-http.el

(defgroup slack-http nil
"Functions for HTTP/HTTPS request & response."
 :prefix "slack-http-"
 :group 'apis)

(require 'url)

(defun slack-http-extract-body (buffer)
"Extract body from buffer."
  (set-buffer buffer)
  (goto-char (point-min))
  (if (search-forward-regexp "^$" nil t) ; empty line
      (buffer-substring (1+ (point))(point-max))))


(defun slack-http-post-callback (status callback)
"Callback bridge for `slack-http-post'."
  (let ((body (slack-http-extract-body (current-buffer)))
	(http-status url-http-response-status))
    (funcall callback http-status body)))


(defun slack-http-post (url arg-alist &optional callback)
"Receive a content from given URL over HTTP/HTTPS.

URL : Url where to request
ARG-ALIST : An associate list which contains key-value pairs.
CALLBACK : If non nil, response will be sent asynchronously by calling
           function 'CALLBACK' = (lambda (http-status body)).
           Otherwise, `slack-http-post' will work synchronously and
           will return (values http-status body).
"
  (let ((encoded-url (url-encode-url url))
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "="
			      (url-hexify-string (cdr arg))))
		    arg-alist "&")))
    (if (functionp callback)
      (with-current-buffer
	  (url-retrieve encoded-url 'slack-http-post-callback (list callback) nil nil)
	  (make-local-variable 'url-http-response-status)
	  (current-buffer))
      (with-current-buffer
	  (url-retrieve-synchronously encoded-url)
	(make-local-variable 'url-http-response-status)
	(let (http-status body)
	  (setq http-status url-http-response-status
		body (slack-http-extract-body (current-buffer)))
	  (values http-status body))))))

(provide 'slack-http)
