(in-package :scrooge)


;;; --- login --------------------

(define-dynamic-page login ("login") (user)
  (with-document ()
    (:head
     (login-headers)
     (:title "Είσοδος"))
    (:body
     (:div :id "container" :class "container_12"
           (:div :class "window grid_6 prefix_3"
                 (:div :id "header"
                       (logo))
                 (when (and (suppliedp user) (not (null user)))
                   (htm (:div :id "login-error"
                              (:p :class "attention"
                                  "Λάθος όνομα χρήστη ή κωδικός πρόσβασης. Προσπαθείστε ξανά."))))
                 (:div :id "login-form" :class "data-form"
                       (with-form (verify-login)
                         (htm
                          (:p (label 'user "Όνομα χρήστη"))
                          (:p (textbox 'user :value (val user)))
                          (:p (label 'pass "Κωδικός πρόσβασης"))
                          (:p (textbox 'pass :passwordp t)))
                         (submit "Είσοδος"))))))))

(define-dynamic-page verify-login ("verify-login" :request-type :post)
    ((user string chk-user t)
     (pass string (chk-pass user pass) t))
  (if (and (validp user)
           (validp pass))
      (let ((login-time (get-universal-time)))
            (start-session)
            (setf (session-value 'user) (val user))
            (setf (session-value 'login-time) login-time)
            (see-other (home)))
      (see-other (login :user (raw user)))))

(define-dynamic-page logout ("logout") ()
  (remove-session *session*)
  (see-other (login)))



;;; --- home --------------------

(define-dynamic-page home ("") ()
  (with-auth ("configuration")
    (with-document ()
      (:head
       (:title "Αρχική")
       (global-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'home)
             (:div :class "clear")
             (:div :id "body"
                   (:p "Home content not yet available")))))))



;;; --- Autocomplete --------------------

(define-dynamic-page autocomplete ("autocomplete" :content-type "text/plain")
    ((table  symbol nil t)
     (column symbol nil t)
     (term   string nil t))
  (with-auth ("configuration")
    (if (every #'validp (parameters *page*))
        (with-db ()
          (let ((results (query (:select (val column) :distinct
                                         :from (val table)
                                         :where (:ilike (val column)
                                                        (ilike (val term))))
                                :column)))
            (if results
                (with-html-output (*standard-output* nil :indent nil :prologue nil)
                  (write-json (coerce results 'vector)))
                (with-html-output (*standard-output* nil :indent nil :prologue nil)
                  "[]"))))
        (see-other (error-page)))))



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

(define-dynamic-page access-denied ("access-denied") ()
  (no-cache)
  (with-document ()
    (:head
     (:title "Access denied")
     (error-headers))
    (:body
     (:div :id "header"
           (logo))
     (:div :id "body"
           (:div :id "content" :class "summary"
                 (:p "Δεν έχετε επαρκή δικαιώματα ώστε να προσπελάσετε αυτή τη σελίδα."))))))
