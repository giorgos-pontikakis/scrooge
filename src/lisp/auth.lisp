(in-package :scrooge)


(defun chk-user (username)
  (with-db ()
    (if (and (not (eql username :null))
             (get-dao 'usr username))
        nil
        'invalid-username)))

(defun chk-pass (username password)
  (if (with-db ()
        (let ((user-dao (get-dao 'usr username)))
          (and (not (eql password :null))
               (string= (password user-dao)
                        (md5sum-sequence->string password)))))
      nil
      'invalid-password))

(defmacro with-auth ((&rest groups) &body body)
  (with-gensyms (session-user user-dao)
    `(if (and *session*                 ; session is valid
              (let ((,session-user (session-value 'user)))
                (with-db ()
                  (let ((,user-dao (get-dao 'usr ,session-user)))
                                        ; session user exists and belongs to group
                    (and ,user-dao
                         (member (authgroup ,user-dao) ',groups :test #'string=))))))
         (progn
           ,@body)
         (see-other (access-denied)))))