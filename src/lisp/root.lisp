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
                          (:p (input-text 'user :value (val user)))
                          (:p (label 'pass "Κωδικός πρόσβασης"))
                          (:p (input-text 'pass :password t)))
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
  ;; the when check is needed in case *session* has expired
  (when *session*
    (remove-session *session*))
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
             (:div :id "content"
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
