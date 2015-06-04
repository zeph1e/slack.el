;;; slack-unittest.el -- Unit test for slack

(require 'slack)

(defvar slack-unittest-testcase-alist nil)

(defun add-testcase (tc-alist)
  (setq slack-unittest-testcase-alist
	(append slack-unittest-testcase-alist tc-alist)))

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
(defun slack-unittest--http--post-invalid-protocol ()
  (slack-http-post "foo://slack.com" nil))
(add-testcase '((error . slack-unittest--http--post-invalid-protocol)))

(defun slack-unittest--http--post-sync-emtpy ()
  (eq (car (slack-http-post "https://slack.com/" nil)) 200))
(add-testcase '((true . slack-unittest--http--post-sync-emtpy)))

(defun slack-unittest--http--post-sync-404 ()
  (>= (car (slack-http-post "https://slack.com/THERES_NO_SUCH_PLACE" nil)) 400))
(add-testcase '((true . slack-unittest--http--post-sync-404)))
(add-testcase '((error . slack-unittest--http--post-sync-404)))


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
  (cond ((or (eq slack-unittest-testcase-alist nil)
	     (eq (length slack-unittest-testcase-alist) 0))
	 (error "slack-unittest: Nothing to run"))
        (t (set-buffer(get-buffer-create "slack-unittest"))
	    (setq inhibit-read-only t)
	    (delete-region (point-min)(point-max))
	    (switch-to-buffer-other-window (current-buffer))
;	    (setq inhibit-read-only nil)
	    (sit-for 1)
	    (let ((tc-total (length slack-unittest-testcase-alist))
		  (tc-run 0) (tc-success 0) (tc-failure 0))
	      (dolist (item slack-unittest-testcase-alist nil)
		(let ((expect (car item)) (tc (cdr item)))
		  (setq tc-run (1+ tc-run))
		  (goto-char (point-max))
		  (beginning-of-line)
		  (if (string-match "^Unittest: \\([0-9]+\\)/\\([0-9]+\\)\\(.+\\)"
		  		    (buffer-substring (point)(point-max)))
		      (delete-region (point)(point-max)))
		  (if (cond ((eq expect 'true) (slack-unittest--expect-true tc)) ; expect t
			    ((eq expect 'false) (slack-unittest--expect-false tc)) ; expect nil
			    ((eq expect 'error) (slack-unittest--expect-error tc)) ; expect error
			    (t nil))
		      (progn (setq tc-success (1+ tc-success)) ; success
			     (if verbose
				 (insert (propertize (format "%d: %s success\n" tc-run (symbol-name tc))
						     ;'font-lock-face '(:foreground "green")
						     'read-only t))))
		    (setq tc-failure (1+ tc-failure)) ; failure
		    (insert (propertize (format "%d: %s failed\n" tc-run (symbol-name tc))
					'font-lock-face '(:foreground "red") 'read-only t))
		  (goto-char (1+ (point-max)))
		  (insert (propertize (format "Unittest: %d/%d (Success: %d, Failure %d)"
					      tc-run tc-total tc-success tc-failure)
				      'read-only t))
		  (sit-for 1)))) ; give little time to update buffer
	      (eq tc-failure 0)))))












(provide 'slack-unittest)
