(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successfull.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION. if NO-REFRESH is non-nil, the available package
lists will not be re-download in order to locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s` : `%S`" package err)
     nil)))

(provide 'init-elpa)
