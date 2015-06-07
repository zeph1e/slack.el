;;; slack-unittest.el -- Unit test for slack

(require 'slack)

(defvar slack-unittest-testcase-alist nil)

(defvar slack-unittest-auth-token nil)

;;FIXME: Is there any other way to improve adding testcase using macro than this?
(defun add-testcase (tc-alist)
  (setq slack-unittest-testcase-alist
	(append slack-unittest-testcase-alist tc-alist)))

(defun make-sync-process ()
    (start-process "sync" nil nil))

(defun wait-sync-process (process)
  (condition-case e
      (if (not (processp process))
	  (signal 'wrong-type-argument (list process))
	(accept-process-output process)
	(process-get process 'value))
    (error
     (destroy-sync-process process)
     (signal e nil))))

(defun notify-sync-process (process &optional value)
  (process-put process 'value value)
  (process-send-string process "\n"))

(defun destroy-sync-process (process)
  (ignore-errors
    (kill-process process)
    (delete-process process)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; self sanity test

;; -expect-true
(defun slack-unittest--unittest--expect-true-with-t ()
  (slack-unittest--expect-true (lambda () t)))
(add-testcase '((true . slack-unittest--unittest--expect-true-with-t)))

(defun slack-unittest--unittest--expect-true-with-nil ()
  (slack-unittest--expect-true (lambda () nil)))
(add-testcase '((false . slack-unittest--unittest--expect-true-with-nil)))

(defun slack-unittest--unittest--expect-true-with-error ()
  (slack-unittest--expect-true (lambda () (error))))
(add-testcase '((false . slack-unittest--unittest--expect-true-with-error)))

;; -expect-false
(defun slack-unittest--unittest--expect-false-with-t ()
  (slack-unittest--expect-false (lambda () t)))
(add-testcase '((false . slack-unittest--unittest--expect-false-with-t)))

(defun slack-unittest--unittest--expect-false-with-nil ()
  (slack-unittest--expect-false (lambda () nil)))
(add-testcase '((true . slack-unittest--unittest--expect-false-with-nil)))

(defun slack-unittest--unittest--expect-false-with-error ()
  (slack-unittest--expect-false (lambda () (error))))
(add-testcase '((false . slack-unittest--unittest--expect-false-with-error)))

;; -expect-error
(defun slack-unittest--unittest--expect-error-with-t ()
  (slack-unittest--expect-error (lambda () t)))
(add-testcase '((false . slack-unittest--unittest--expect-error-with-t)))

(defun slack-unittest--unittest--expect-error-with-nil ()
  (slack-unittest--expect-error (lambda () nil)))
(add-testcase '((false . slack-unittest--unittest--expect-error-with-nil)))

(defun slack-unittest--unittest--expect-error-with-error ()
  (slack-unittest--expect-error (lambda () (error))))
(add-testcase '((true . slack-unittest--unittest--expect-error-with-error)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slack-http unittests
(defun slack-unittest--form-string-empty ()
  (eq (length (slack-http--form-string nil)) 0))
(add-testcase '((true . slack-unittest--form-string-empty)))

(defun slack-unittest--form-string-foo-bar ()
  (string= (slack-http--form-string '((foo . "bar"))) "foo=bar"))
(add-testcase '((true . slack-unittest--form-string-foo-bar)))

(defun slack-unittest--http--call-method-sync-invalid-port ()
  (let ((slack-http-endpoint-url "http://localhost:0/"))
    (slack-http-call-method nil nil)))
(add-testcase '((error . slack-unittest--http--call-method-sync-invalid-port)))

(defun slack-unittest--http--call-method-sync-refused-port ()
  (let ((slack-http-endpoint-url "http://localhost:1/"))
    (slack-http-call-method nil nil)))
(add-testcase '((error . slack-unittest--http--call-method-sync-refused-port)))

(defun slack-unittest--http--call-method-sync-invalid-protocol ()
  (let ((slack-http-endpoint-url "foo://slack.com/"))
    (slack-http-call-method nil nil)))
(add-testcase '((error . slack-unittest--http--call-method-sync-invalid-protocol)))

(defun slack-unittest--http--call-method-sync-redirect ()
  (let ((slack-http-endpoint-url "http://slack.com/"))
    (slack-http-call-method api nil)))
(add-testcase '((error . slack-unittest--http--call-method-sync-redirect)))

(defun slack-unittest--http--call-method-sync-emtpy ()
  (cdr (assq 'ok (slack-http-call-method 'api.test nil))))
(add-testcase '((true . slack-unittest--http--call-method-sync-emtpy)))

(defun slack-unittest--http--call-method-empty ()
  (lexical-let ((process (make-sync-process)))
    (slack-http-call-method 'api.test nil
			    (lambda (process object)
			      (notify-sync-process process object)) process)
    (plist-get (prog1 (wait-sync-process process) (destroy-sync-process process)) ':ok)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slack-rpc unittests
(defun slack-unittest--rpc--new-request-id-unique ()
  (let (prev current (count 0))
    (dotimes (i 50 nil)
      (setq current (slack-rpc-new-request-id))
      (if (eq prev current) (signal 'error (list prev current))
	(setq prev current)
	(setq count (1+ count))))
    (eq count 50)))
(add-testcase '((true . slack-unittest--rpc--new-request-id-unique)))

;; api.test
(defun slack-unittest--rpc--api-test-ok ()
  (lexical-let ((process (make-sync-process)))
    (slack-rpc-api-test (lambda (process object)
			  (notify-sync-process process object)))
    (cdr (assq 'ok (prog1 (wait-sync-process process) (destroy-sync-process process))))))
(add-testcase '((true . slack-unittest--rpc--api-test-ok)))

(defun slack-unittest--rpc--api-test-error ()
  (lexical-let ((process (make-sync-process)))
    (slack-rpc-api-test (lambda (process object)
			  (notify-sync-process process object)) '((error . "my_error") nil))
    (let ((object (prog1 (wait-sync-process process) (destroy-sync-process process))))
      (and (eq (cdr (assq 'ok object)) ':json-false)
	   (string= (cdr (assq 'error object)) "my_error")
	   (let ((args (cdr (assq 'args object))))
	     (string= (cdr (assq 'error args)) "my_error"))))))
(add-testcase '((true . slack-unittest--rpc--api-test-error)))

;; auth.test
(defun slack-unittest--rpc--auth-test-empty-token ()
  (lexical-let ((process (make-sync-process)))
    (slack-rpc-auth-test (lambda (process object)
			   (notify-sync-process process object)) nil)
    (let ((object (prog1 (wait-sync-process process) (destroy-sync-process process))))
      (and (eq (cdr (assq 'ok object)) ':json-false)
	   (string= (cdr (assq 'error object)) "not_authed")))))
(add-testcase '((true . slack-unittest--rpc--auth-test-empty-token)))

(defun slack-unittest--rpc--auth-test-invalid-token ()
  (lexical-let ((process (make-sync-process)))
    (slack-rpc-auth-test (lambda (process object)
			   (notify-sync-process process object)) "my-invalid-token")
    (let ((object (prog1 (wait-sync-process process) (destroy-sync-process process))))
      (and (eq (cdr (assq 'ok object)) ':json-false)
	   (string= (cdr (assq 'error object)) "invalid_auth")))))
(add-testcase '((true . slack-unittest--rpc--auth-test-invalid-token)))

(defun slack-unittest--rpc--auth-test-valid-token ()
  (lexical-let ((process (make-sync-process)))
    (slack-rpc-auth-test (lambda (process object)
			   (notify-sync-process process object)) slack-unittest-auth-token)
    (let ((object (prog1 (wait-sync-process process) (destroy-sync-process process))))
      (and (cdr (assq 'ok object))
	   (stringp (cdr (assq 'url object)))
	   (stringp (cdr (assq 'team object)))
	   (stringp (cdr (assq 'user object)))
	   (stringp (cdr (assq 'team_id object)))
	   (stringp (cdr (assq 'user_id object)))))))
(add-testcase '((true . slack-unittest--rpc--auth-test-valid-token)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test functions
(defun slack-unittest--expect-true (function)
  (condition-case nil
      (funcall function) (error nil)))

(defun slack-unittest--expect-false (function)
  (condition-case nil
      (not (funcall function)) (error nil)))

(defun slack-unittest--expect-error (function)
  (condition-case nil
      (progn (funcall function) nil) (error t)))


;;;###autoload
(defun slack-unittest-run-test (&optional verbose)
  (interactive)
  (when (interactive-p)
      (setq verbose (y-or-n-p "Verbose test output? "))
      (if (eq slack-unittest-auth-token nil)
	  (setq slack-unittest-auth-token (read-string "Enter auth token: "))))

  (cond ((or (eq slack-unittest-testcase-alist nil)
	     (eq (length slack-unittest-testcase-alist) 0))
	 (error "slack-unittest: Nothing to run"))
        (t (set-buffer(get-buffer-create "slack-unittest"))
	    (setq inhibit-read-only t)
	    (delete-region (point-min)(point-max))
	    (switch-to-buffer-other-window (current-buffer))
	    (setq inhibit-read-only nil)
	    (sit-for 1)
	    (let ((tc-total (length slack-unittest-testcase-alist))
		  (tc-run 0) (tc-pass 0) (tc-fail 0))
	      (dolist (item slack-unittest-testcase-alist nil)
		(let ((expect (car item)) (tc (cdr item)))
		  (setq tc-run (1+ tc-run))
		  (let ((result (cond ((eq expect 'true) (slack-unittest--expect-true tc)) ; expect t
			    ((eq expect 'false) (slack-unittest--expect-false tc)) ; expect nil
			    ((eq expect 'error) (slack-unittest--expect-error tc)) ; expect error
			    (t nil))))
		    ;; delete previous statistics
		    (goto-char (point-max))
		    (beginning-of-line)
		    (if (string-match-p "^unit test: \\([0-9]+\\)/\\([0-9]+\\)\\(.+\\)"
					(buffer-substring (point)(point-max)))
			(delete-region (point)(point-max)))
		    (if result
		      (progn (setq tc-pass (1+ tc-pass)) ; success
			     (if verbose
				(insert (concat (propertize (format "%d: %s pass" tc-run (symbol-name tc))
							    'font-lock-face '(:foreground "green")
							    'read-only t)
						"\n"))))
		      (setq tc-fail (1+ tc-fail)) ; failure
		      (insert (concat (propertize (format "%d: %s fail" tc-run (symbol-name tc))
						  'font-lock-face '(:foreground "red")
						  'read-only t)
				      "\n")))
		  (goto-char (1+ (point-max)))
		  (insert (format "unit test: %d/%d (Pass: %d, Fail %d)"
					      tc-run tc-total tc-pass tc-fail)))
		  (sit-for 0.1))) ; give little time to update buffer
	      (message "slack-unittest: done. %d / %d passed (%d%%)"
		       tc-pass tc-total (/ (* tc-pass 100) tc-total))
	      (eq tc-fail 0)))))


(provide 'slack-unittest)
