;;; slack-utils.el

;; generate sequencial id
(defvar slack-utils-id-counter 0)
(defun slack-utils-id ()
  (setq slack-utils-id-counter (1+ slack-utils-id-counter)))


;; define-error was introduced from 24.3
;; following code was from:
;;   http://emacs.stackexchange.com/questions/3905/define-error-for-older-emacs
(unless (fboundp 'define-error)
  (defun define-error (name message &optional parent)
    "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
    (unless parent (setq parent 'error))
    (let ((conditions
           (if (consp parent)
               (apply #'nconc
                      (mapcar (lambda (parent)
                                (cons parent
                                      (or (get parent 'error-conditions)
                                          (error "Unknown signal `%s'" parent))))
                              parent))
             (cons parent (get parent 'error-conditions)))))
      (put name 'error-conditions
           (delete-dups (copy-sequence (cons name conditions))))
      (when message (put name 'error-message message)))))

;; To convert async to sync
;; Original idea of following was from:
;;   http://nullprogram.com/blog/2013/01/14/
(defun make-sync-process ()
    (start-process "sync" nil nil))

(defun wait-sync-process (process)
  (condition-case e
      (if (not (processp process))
          (signal 'wrong-type-argument (list process))
      (accept-process-output process) ; wait for 10 sec
      (process-get process 'value))
    (error
     (destroy-sync-process process)
     (signal (car e) (cdr e)))))

(defun notify-sync-process (process &optional value)
  (process-put process 'value value)
  (process-send-string process "\n"))

(defun destroy-sync-process (process)
  (ignore-errors
    (kill-process process)
    (delete-process process)))

(provide 'slack-utils)
