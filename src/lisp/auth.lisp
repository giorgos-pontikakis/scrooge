(in-package :scrooge)


(defun hash-password (password)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array password))))

(defun chk-user (username)
  (with-db ()
    (if (and (not (eql username :null))
             (get-dao 'usr username))
        nil
        'invalid-username)))

(defun chk-pass (username password)
  (with-db ()
    (let ((user-dao (get-dao 'usr username)))
      (if (and (not (eql password :null))
               (string= (password user-dao)
                        (hash-password password)))
          nil
          'invalid-password))))


;;; ------------------------------------------------------------
;;; Login and logout
;;; ------------------------------------------------------------

(defpage dynamic-page login ("login")
    (user origin)
  (with-document ()
    (:head
      (login-headers)
      (:title "Είσοδος"))
    (:body
      (:div :id "container" :class "container_12"
        (:div :class "grid_4 prefix_3"
          (:div :id "header"
            (logo)
            (:div :class "clear" ""))
          (when (and (suppliedp user) (not (null user)))
            (htm (:div :id "login-error"
                   (:div :class "attention"
                     "Λάθος όνομα χρήστη ή κωδικός πρόσβασης. Προσπαθείστε ξανά."))))
          (:div :id "login-form" :class "data-form"
            (with-form (verify-login :target (val origin))
              (htm
               (:div (label 'user "Όνομα χρήστη"))
               (:div (input-text 'user :value (val user)))
               (:div (label 'pass "Κωδικός πρόσβασης"))
               (:div (input-text 'pass :password t)))
              (submit "Είσοδος"))))))))

(defpage dynamic-page verify-login ("verify-login" :request-type :post)
    ((user   string chk-user             t)
     (pass   string (chk-pass user pass) t)
     (target string))
  (if (and (validp user)
           (validp pass))
      (let ((login-time (get-universal-time)))
        (start-session)
        (setf (session-value 'user) (val user))
        (setf (session-value 'login-time) login-time)
        (setf (session-max-time *session*) 3600)
        (see-other (or (val target) (home))))
      (see-other (login :user (raw user)))))

(defpage dynamic-page logout ("logout") ()
  ;; the when check is needed in case *session* has expired
  (when *session*
    (remove-session *session*))
  (see-other (login)))
