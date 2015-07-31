;;; slack-unittest.el -- Unit test for slack

(require 'slack)
(require 'slack-utils)

(defvar slack-unittest-testcase-alist nil)

(defvar slack-unittest-auth-token nil)

(defmacro deftestcase (func expected &rest body)
  "Define a testcase & add it to testcase list.

FUNC is a function name for the testcase.
EXPECTED is an expected test result which is one of true, false, or error.
When the testcase is called, BODY will be evaluated.

example:
  (deftestcase my-tc-function 'true
    (string= (slack-http--form-string-from-plist '(:foo \"bar\")) \"foo=bar\"))
"
  `(setq slack-unittest-testcase-alist
         (append slack-unittest-testcase-alist
                  (list (cons ,expected
                         (defun ,func () (progn ,@body)))))))

;; to cope with eval-buffer
(setq slack-unittest-testcase-alist nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; self sanity test

;; -expect-true
(deftestcase slack-unittest--unittest--expect-true-with-t 'true
  (slack-unittest--expect-true (lambda () t)))

(deftestcase slack-unittest--unittest--expect-true-with-nil 'false
  (slack-unittest--expect-true (lambda () nil)))

(deftestcase slack-unittest--unittest--expect-true-with-error 'false
  (slack-unittest--expect-true (lambda () (error))))

;; -expect-false
(deftestcase slack-unittest--unittest--expect-false-with-t 'false
  (slack-unittest--expect-false (lambda () t)))

(deftestcase slack-unittest--unittest--expect-false-with-nil 'true
  (slack-unittest--expect-false (lambda () nil)))

(deftestcase slack-unittest--unittest--expect-false-with-error 'false
  (slack-unittest--expect-false (lambda () (error))))

;; -expect-error
(deftestcase slack-unittest--unittest--expect-error-with-t 'false
  (slack-unittest--expect-error (lambda () t)))

(deftestcase slack-unittest--unittest--expect-error-with-nil 'false
  (slack-unittest--expect-error (lambda () nil)))

(deftestcase slack-unittest--unittest--expect-error-with-error 'true
  (slack-unittest--expect-error (lambda () (error))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slack-http unittests
(deftestcase slack-unittest--form-string-foo-bar-alist 'true
  (string= (slack-http--form-string-from-alist '((foo . "bar"))) "foo=bar"))

(deftestcase slack-unittest--form-string-foo-bar-plist 'true
  (string= (slack-http--form-string-from-plist '(:foo "bar")) "foo=bar"))

(deftestcase slack-unittest--form-string-empty 'true
  (eq (length (slack-http--form-string nil)) 0))

(deftestcase slack-unittest--form-string-nested-list-in-plist 'error
  (slack-http--form-string-from-plist '(:foo "bar" :bar (:abc "def"))))

(deftestcase slack-unittest--http--call-method-sync-invalid-port 'error
  (let ((slack-http-endpoint-url "http://localhost:0/"))
    (slack-http-call-method nil nil)))

(deftestcase slack-unittest--http--call-method-sync-refused-port 'error
  (let ((slack-http-endpoint-url "http://localhost:1/"))
    (slack-http-call-method nil nil)))

(deftestcase slack-unittest--http--call-method-sync-invalid-protocol 'error
  (let ((slack-http-endpoint-url "foo://slack.com/"))
    (slack-http-call-method nil nil)))

(deftestcase slack-unittest--http--call-method-sync-redirect 'error
  (let ((slack-http-endpoint-url "http://slack.com/"))
    (slack-http-call-method api nil)))

(deftestcase slack-unittest--http--call-method-sync-emtpy 'true
  (cdr (assq 'ok (slack-http-call-method 'api.test nil))))

(deftestcase slack-unittest--http--call-method-empty 'true
  (lexical-let ((process (make-sync-process)))
    (slack-http-call-method 'api.test nil
			    (lambda (process object)
			      (notify-sync-process process object)) process)
    (cdr (assq 'ok (prog1 (wait-sync-process process) (destroy-sync-process process))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slack-rpc unittests
(deftestcase slack-unittest--rpc--new-request-id-unique 'true
  (let (prev current (count 0))
    (dotimes (i 50 nil)
      (setq current (slack-utils-id))
      (if (eq prev current) (signal 'error (list prev current))
	(setq prev current)
	(setq count (1+ count))))
    (eq count 50)))

;; api.test
(deftestcase slack-unittest--rpc--api-test-ok 'true
  (lexical-let ((process (make-sync-process)))
    (slack-rpc-api-test (lambda (process object)
			  (notify-sync-process process object)))
    (cdr (assq 'ok (prog1 (wait-sync-process process) (destroy-sync-process process))))))

(deftestcase slack-unittest--rpc--api-test-error 'true
  (lexical-let ((process (make-sync-process)))
    (slack-rpc-api-test (lambda (process object)
			  (notify-sync-process process object)) :error  "my_error")
    (let ((object (prog1 (wait-sync-process process) (destroy-sync-process process))))
      (and (eq (cdr (assq 'ok object)) ':json-false)
	   (string= (cdr (assq 'error object)) "my_error")
	   (let ((args (cdr (assq 'args object))))
	     (string= (cdr (assq 'error args)) "my_error"))))))

;; auth.test
(deftestcase slack-unittest--rpc--auth-test-empty-token 'true
  (lexical-let ((process (make-sync-process)))
    (slack-rpc-auth-test nil (lambda (process object)
			   (notify-sync-process process object)))
    (let ((object (prog1 (wait-sync-process process) (destroy-sync-process process))))
      (and (eq (cdr (assq 'ok object)) ':json-false)
	   (string= (cdr (assq 'error object)) "not_authed")))))

(deftestcase slack-unittest--rpc--auth-test-invalid-token 'true
  (lexical-let ((process (make-sync-process)))
    (slack-rpc-auth-test "my-invalid-token" (lambda (process object)
			   (notify-sync-process process object)))
    (let ((object (prog1 (wait-sync-process process) (destroy-sync-process process))))
      (and (eq (cdr (assq 'ok object)) ':json-false)
	   (string= (cdr (assq 'error object)) "invalid_auth")))))

(deftestcase slack-unittest--rpc--auth-test-valid-token 'true
  (lexical-let ((process (make-sync-process)))
    (slack-rpc-auth-test slack-unittest-auth-token (lambda (process object)
			   (notify-sync-process process object)))
    (let ((object (prog1 (wait-sync-process process) (destroy-sync-process process))))
      (and (cdr (assq 'ok object))
	   (stringp (cdr (assq 'url object)))
	   (stringp (cdr (assq 'team object)))
	   (stringp (cdr (assq 'user object)))
	   (stringp (cdr (assq 'team_id object)))
	   (stringp (cdr (assq 'user_id object)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rtm



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
