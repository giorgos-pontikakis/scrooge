(in-package :scrooge)



;;; ------------------------------------------------------------
;;; PARAMETER UTILITIES
;;; ------------------------------------------------------------

;;; Parameter utilites

(defun params->plist (params &optional (fn #'val))
  (mapcan (lambda (param)
            (list (parameter-key (attributes param))
                  (funcall fn param)))
          params))

(defun params->styles (&rest params)
  (mapcan (lambda (param)
            (list (parameter-key (attributes param))
                  (if (validp param)
                      ""
                      "attention")))
          params))

(defun collect-params (names &optional (parameters *parameters*))
  (remove-if-not (lambda (p)
                   (member (parameter-name (attributes p)) names))
                 parameters))



;;; ------------------------------------------------------------
;;; PAGE UTILITIES
;;; ------------------------------------------------------------

;;; Conditions

(define-condition data-entry-error ()
  ())

(define-condition bad-request-error ()
  ())

(define-condition authentication-error ()
  ())


;;; Page families

(defclass page-family-mixin ()
  ((system-parameter-names :reader system-parameter-names)
   (user-parameter-names   :reader user-parameter-names)
   (filter-parameter-names :reader filter-parameter-names)
   (allowed-groups         :reader allowed-groups)))

(flet ((collect-parameters (parameter-names parameters)
         (remove-if-not (lambda (p)
                          (member (parameter-name (attributes p))
                                  parameter-names))
                        parameters)))

  (defun system-parameters (&optional (page *page*) (parameters *parameters*))
    (collect-parameters (system-parameter-names page) parameters))

  (defun user-parameters (&optional (page *page*) (parameters *parameters*))
    (collect-parameters (user-parameter-names page) parameters))

  (defun filter-parameters (&optional (page *page*) (parameters *parameters*))
    (collect-parameters (filter-parameter-names page) parameters)))


;;; Authentication and page macros

(defun authenticated-p (groups)
  (and *session*  ;; session is valid
       (let ((session-user (session-value 'user)))
         (with-db ()
           (let ((user-dao (get-dao 'usr session-user)))
             ;; session user exists and belongs to group
             (and user-dao
                  (or (member "all" groups :test #'string=)
                      (member (authgroup user-dao) groups :test #'string=))))))))

(defmacro with-view-page (&body body)
  (with-gensyms (system-params)
    `(handler-case
         (progn
           (no-cache)
           (unless (authenticated-p (allowed-groups *page*))
             (error 'authentication-error))
           (when-let (,system-params (system-parameters))
             (unless (every #'validp ,system-params)
               (error 'bad-request-error)))
           (with-db ()
             ,@body))
       (bad-request-error ()
         (setf (return-code*) +http-bad-request+))
       (authentication-error ()
         (redirect (login :origin (conc (script-name*)
                                        (if (query-string*) "?" "")
                                        (query-string*)))
                   :code +http-temporary-redirect+
                   :add-session-id nil))
       (error (c)
         (if (debug-p *acceptor*)
             (signal c)
             (setf (return-code*) +http-internal-server-error+))))))

(defmacro with-controller-page (view-page-fn &body body)
  (with-gensyms (system-params user-params)
    `(handler-case
         (progn
           (no-cache)
           (unless (authenticated-p '("user" "admin"))
             (error 'authentication-error))
           (when-let (,system-params (system-parameters))
             (unless (every #'validp ,system-params)
               (error 'bad-request-error)))
           (when-let (,user-params (user-parameters))
             (unless (every #'validp ,user-params)
               (error 'data-entry-error)))
           (with-db ()
             ,@body))
       (data-entry-error ()
         (see-other ,(if view-page-fn
                         (let ((fn-call (ensure-list view-page-fn)))
                           `(apply #',(first fn-call) (append ',(rest fn-call)
                                                              (params->plist *parameters* #'raw))))
                         '(error "Data entry error"))))
       (bad-request-error ()
         (setf (return-code*) +http-bad-request+))
       (authentication-error ()
         (see-other (login :origin (conc (script-name*)
                                         (when-let (qs (query-string*))
                                           (conc "?" qs))))))
       (error (c)
         (if (debug-p *acceptor*)
             (signal c)
             (setf (return-code*) +http-internal-server-error+))))))

(defmacro with-xhr-page (auth-error-message &body body)
  (with-gensyms (system-params)
    `(handler-case
         (progn
           (no-cache)
           (unless (authenticated-p (allowed-groups *page*))
             (error 'authentication-error))
           (when-let (,system-params (system-parameters))
             (unless (every #'validp ,system-params)
               (error 'bad-request-error)))
           (with-db ()
             ,@body))
       (bad-request-error ()
         (setf (return-code*) +http-bad-request+))
       (authentication-error ()
         ,auth-error-message)
       (error (c)
         (if (debug-p *acceptor*)
             (signal c)
             (setf (return-code*) +http-internal-server-error+))))))



;;;----------------------------------------------------------------------
;;; SQL utilities
;;;----------------------------------------------------------------------

(defun ilike (filter)
  (if (or (null filter)
          (eq filter :null))
      "%"
      (concatenate 'string "%" filter "%")))

(defmacro define-existence-predicate (name table field)
  `(defun ,name (,field)
     (with-db ()
       (query (:select 1 :from ',table :where (:= ',field ,field)) :single))))

(defmacro define-uniqueness-predicate (name table unique-field id-field)
  `(defun ,name (,unique-field &optional ,id-field)
     (with-db ()
       (not (if ,id-field
                (query (:select 1
                                :from ',table
                                :where (:and (:not (:= ',id-field ,id-field))
                                             (:= ',unique-field ,unique-field)))
                       :single)
                (query (:select 1
                                :from ',table
                                :where (:= ',unique-field ,unique-field))
                       :single))))))

(defmacro define-existence-predicate* (name table field primary-key)
  `(defun ,name (,field &optional ,primary-key)
     (with-db ()
       (if ,primary-key
           (query (:select 1 :from ',table
                   :where (:and (:= ',field ,field)
                                (:not (:= ',primary-key ,primary-key))))
                  :single)
           (query (:select 1 :from ',table
                   :where (:= ',field ,field))
                  :single)))))



;;;----------------------------------------------------------------------
;;; Miscellaneous
;;;----------------------------------------------------------------------

(defun int-5digits-p (num)
  (and (integerp num)
       (> num 9999)
       (<= num 99999)))

(defun valid-tin-p (tin)
  "Check the taxation identification number (ΑΦΜ)"
  (flet ((char-parse-integer (d)
           (- (char-int d) (char-int #\0))))
    (let* ((len (length tin))
           (digits (map 'vector #'char-parse-integer
                        (nreverse (subseq tin 0 (1- len)))))
           (control-digit (char-parse-integer (elt tin (1- len)))))
      (let ((sum (iter (for d in-vector (subseq digits 0 (1- len)))
                       (for i from 1)
                       (reducing (* d (expt 2 i))
                                 by #'+))))
        (= (mod (mod sum 11)
                10)
           control-digit)))))

(defun today ()
  (universal-time-to-timestamp (get-universal-time)))

(defun parse-option-dao (option-dao)
  (flet ((parse-config-fn (lisp-type)
           (cond ((string= lisp-type "integer") #'parse-integer)
                 ((string= lisp-type "float")   #'parse-float)
                 ((string= lisp-type "date")    #'parse-date)
                 ((string= lisp-type "string")  #'identity)
                 (t (error "Unknown lisp-type in option table.")))))
    (let ((config-value (config-value option-dao)))
      (if (eql config-value :null)
          nil
          (funcall (parse-config-fn (lisp-type option-dao))
                   config-value)))))

(defun get-option (id)
  (with-db ()
    (parse-option-dao (get-dao 'option (etypecase id
                                         (symbol (string-downcase id))
                                         (string id))))))


(defun chk-amount (float)
  (if (positive-real-p float)
      nil
      :non-positive-amount))

(defun chk-amount* (float)
  "Same as chk-amount but allow null values"
  (if (or (eql float :null)
          (positive-real-p float)
          (zerop float))
      nil
      :non-positive-amount))
