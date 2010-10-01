(in-package :scrooge)


(defmacro with-auth (role &body body)
  (with-gensyms (user)
    `(let ((,user (session-value 'user)))
       (cond ((null ,user)
              (redirect (login)))
             ((string-not-equal (webrole ,user) ,role)
              (redirect (unauthorized)))
             (t
              (progn
                ,@body))))))
