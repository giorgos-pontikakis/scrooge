(in-package :scrooge)



(defun md5sum-sequence->string (str)
  (format nil "~(~{~2,'0X~}~)"
          (map 'list #'identity (md5:md5sum-sequence str))))

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
               (string= (userpass user-dao)
                        (md5sum-sequence->string password)))))
      nil
      'invalid-password))

(defmacro with-auth ((&rest groups) &body body)
  (with-gensyms (session-user user-dao authenticated-p)
    `(let ((,authenticated-p (and *session*  ;; session is valid
                                  (let ((,session-user (session-value 'user)))
                                    (with-db ()
                                      (let ((,user-dao (get-dao 'usr ,session-user)))
                                        ;; session user exists and belongs to group
                                        (and ,user-dao
                                             (member (authgroup ,user-dao) ',groups
                                                     :test #'string=))))))))
       (if ,authenticated-p
           (progn
             ,@body)
           (see-other (login))))))
