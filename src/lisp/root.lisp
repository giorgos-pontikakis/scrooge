(in-package :scrooge)

(define-dynamic-page debug-page (variable value) ("debug")
  (with-page ()
    (:body
     (:p "Variable " variable " = " value))))

;;; --- Home --------------------

(define-dynamic-page home () ("")
  (with-page ()
    (:head
     (:title "Αρχική")
     (css "reset.css" "main.css"))
    (:body
     (:div :id "header"
	   (logo)
	   (primary-navbar 'home))
     (:div :id "body"
	   (:p "Home content not yet available")))))


;;; --- Autocomplete --------------------

(define-dynamic-page autocomplete (table q) ("autocomplete" :content-type "text/plain")
  (with-db
    (with-parameter-rebinding #'val
     (let ((results (query (:select 'id 'title :from (intern table)))))
       (if results
	   (with-html-output (*standard-output* nil :indent nil :prologue nil) 
	     (iter (for (key val) in results)
		   (when (search (string-upcase-gr q) (string-upcase-gr val))
		     (fmt "~A|~A~&" val key))))
	   (with-html-output (*standard-output* nil :indent nil :prologue nil)
	     "|"))))))

;;; --- Generic error --------------------


;;; :TODO: This should be a static page

(define-dynamic-page notfound () ("notfound")
  (no-cache)
  (with-page ()
    (:head
     (:title "Άγνωστη σελίδα")
     (css "reset.css" "main.css"))
    (:body
     (:div :id "header"
	   (logo)
	   (primary-navbar 'companies))
     (:div :id "body"
	   (:div :id "content" :class "summary"
		 (:p "Η σελίδα που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
		 (:p "Επιστρέψτε στο κεντρικό μενού και προσπαθήστε ξανά."))))))

(define-dynamic-page error-page () ("error-page")
  (no-cache)
  (with-page ()
    (:head
     (:title "Generic error page")
     (css "reset.css" "main.css"))
    (:body
     (:div :id "header"
	   (logo))
     (:div :id "body"
	   (:div :id "content" :class "summary"
		 (:p "An error has occured.")
		 (:p "You are supposed to see this page because of illegal URL manipulation"))))))


(define-dynamic-page no-fsm-data () ("no-fsm-data")
  (no-cache)
  (with-page ()
    (:head
     (:title "No fsm data")
     (css "reset.css" "main.css"))
    (:body
     (:div :id "header"
	   (logo)
	   (primary-navbar 'companies))
     (:div :id "body"
	   (:div :id "content" :class "summary"
		 (:p "No fsm transitions data found."))))))



