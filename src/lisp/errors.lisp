(in-package :scrooge)


(setf *default-handler* (lambda ()
                          (setf (return-code*) +http-not-found+)))

(setf *handle-http-errors-p* t)

(setf *http-error-handler*
      (lambda (return-code)
        (cond
          ((= return-code +http-not-found+) (not-found))
          ((= return-code +http-bad-request+) (bad-request))
          ((= return-code +http-internal-server-error+) (internal-server-error))
          (t nil))))

(defun bad-request ()
  (with-document ()
    (:head
     (:title "Bad request"))
    (:body
     (:h1 "Bad Request")
     (:div :id "content"
           (:p "An bad request has been received.")
           (:p "You are supposed to see this page because of illegal manipulation of request parameters.")))))

(defun internal-server-error ()
  (with-document ()
    (:head
     (:title "Internal server error"))
    (:body
     (:h1 "Internal Server Error")
     (:div :id "content"
           (:p "An internal server error has occured.")))))

(defun not-found ()
  (with-document ()
    (:head
     (:title "Not found"))
    (:body
     (:h1 "Not Found")
     (:div :id "content"
           (:p "The page you are trying to access does not exist.")))))