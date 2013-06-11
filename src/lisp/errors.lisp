(in-package :scrooge)


(when (debug-p *scrooge*)
  (setf *catch-errors-p* nil))


(defmethod acceptor-status-message ((acceptor scrooge-acceptor) http-status-code
                                    &rest properties &key &allow-other-keys)
  (declare (ignore properties))
  (cond
    ((< http-status-code 300) (call-next-method))
    ((= http-status-code +http-not-found+) (not-found))
    ((= http-status-code +http-bad-request+) (bad-request))
    ((= http-status-code +http-internal-server-error+) (internal-server-error))
    (t (call-next-method))))

(defun bad-request ()
  (with-html-output-to-string (s)
    (:head
     (:title "Scrooge - Bad Request"))
    (:body
     (:h1 "Bad Request")
     (:div :id "content"
           (:p "An bad request has been received.")
           (:p "You are supposed to see this page because of illegal manipulation of request parameters.")))))

(defun internal-server-error ()
  (with-html-output-to-string (s)
    (:head
     (:title "Scrooge - Internal server error"))
    (:body
     (:h1 "Internal Server Error")
     (:div :id "content"
           (:p "An internal server error has occured.")))))

(defun not-found ()
  (with-html-output-to-string (s)
    (:head
     (:title "Not found"))
    (:body
     (:h1 "Scrooge - Page Not Found")
     (:div :id "content"
           (:p "The page you are trying to access does not exist.")))))
