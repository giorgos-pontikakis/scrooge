(in-package :scrooge)


;;; --- Actions --------------------

(define-dynamic-page verify-login (username scrooge-password) ("verify-login" :request-type :post) 
  (with-db ()
    (let ((user (select-dao-unique 'webuser (:= 'username username))))
      (if (or (null user)
	      (string/= scrooge-password (password user)))
	  (redirect (login))
	  (progn
	    (start-session)
	    (setf (session-value 'user) user)
	    (redirect (home)))))))


;;; --- Pages --------------------

(define-dynamic-page login () ("login")
  (with-page ()
    (:head
     :title "Σκρουτζ: Login")
    (:body
     (with-form (verify-login)
       (:p (label 'username "Username:"))
       (:p (textbox 'username))
       (:p (label 'scrooge-password "Password:"))
       (:p (textbox 'scrooge-password :passwordp t))
       (:p (submit "Login"))))))

(define-dynamic-page unauthorized () ("unauthorized")
  (with-page ()
    (:head
     :title "Σκρουτζ: Πρόβλημα εξουσιοδότησης")
    (:body
     (:p "Δεν έχετε εξουσιοδότηση για να δείτε αυτή τη σελίδα"))))

