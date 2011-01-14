(in-package :scrooge)



;;; --- Home --------------------

(define-dynamic-page home ("") ()
  (with-document ()
    (:head
     (:title "Αρχική")
     (global-headers))
    (:body
     (:div :id "container" :class "container_12"
           (header 'home)
           (:div :class "clear")
           (:div :id "body"
                 (:p "Home content not yet available"))))))



;;; --- Autocomplete --------------------

(define-dynamic-page autocomplete ("autocomplete" :content-type "text/plain")
    ((table symbol)
     (column symbol)
     (term string))
  (with-db ()
    (let ((results (query (:select (val column) :distinct
                                   :from (val table)
                                   :where (:ilike (val column)
                                                  (ilike (string-upcase-gr (val term)))))
                          :column)))
      (if results
          (with-html-output (*standard-output* nil :indent nil :prologue nil)
            (write-json (coerce results 'vector) #|(map 'vector #'identity results)|#))
          (with-html-output (*standard-output* nil :indent nil :prologue nil)
            "[]")))))



;;; --- Error pages --------------------


;;; :TODO: This should be a static page

(define-dynamic-page notfound ("notfound") ()
  (no-cache)
  (with-document ()
    (:head
     (:title "Άγνωστη σελίδα")
     (error-headers))
    (:body
     (:div :id "header"
           (logo))
     (:div :id "body"
           (:div :id "content" :class "summary"
                 (:p "Η σελίδα που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
                 (:p "Επιστρέψτε στο κεντρικό μενού και προσπαθήστε ξανά."))))))

(define-dynamic-page error-page ("error-page") ()
  (no-cache)
  (with-document ()
    (:head
     (:title "Generic error page")
     (error-headers))
    (:body
     (:div :id "header"
           (logo))
     (:div :id "body"
           (:div :id "content" :class "summary"
                 (:p "An internal error has occured.")
                 (:p "You are supposed to see this page because of illegal URL manipulation"))))))
