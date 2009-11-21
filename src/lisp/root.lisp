(in-package :scrooge)

(define-dynamic-page debug-page (variable value) ("debug")
  (with-page ()
    (:body
     (:p "Variable " variable " = " value))))

;;; --- Home --------------------

(define-dynamic-page home () ("")
  (with-auth "root"
    (with-page ()
      (:head
       (:title "Σκρουτζ: Αρχική")
       (css "reset.css" "scrooge.css"))
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Αρχική"))
       (:div :id "body"
	     (:p "Home content not yet available"))))))


;;; --- Autocomplete --------------------

(define-dynamic-page autocomplete (table q) ("autocomplete" :content-type "text/plain")
  (with-db
    (let ((results (cond ((string-equal table "debit-accounts")
			  (query (:select 'id 'title
					  :from 'account
					  :where (:= 'debit-p t))))
			 ((string-equal table "credit-accounts")
			  (query (:select 'id 'title
					  :from 'account
					  :where (:= 'debit-p nil))))
			 (t
			  (query (:select 'id 'title :from (intern table)))))))
      (if results
	  (with-html-output (*standard-output* nil :indent nil :prologue nil) 
	    (iter (for (key val) in results)
		  (when (search (string-upcase-gr q) (string-upcase-gr val))
		    (fmt "~A|~A~&" val key))))
	  (with-html-output (*standard-output* nil :indent nil :prologue nil)
	    "|")))))
