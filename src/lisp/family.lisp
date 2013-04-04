(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; WIDGET FAMILIES
;;; ----------------------------------------------------------------------

(defclass family-mixin ()
  ((parameter-groups :reader parameter-groups :initarg :parameter-groups))
  (:default-initargs :parameter-groups '()))


;;; Utilities

(defun collect-params (group-name &optional (page *page*) (parameters *parameters*))
  (let ((parameter-names (getf (parameter-groups page) group-name)))
    (remove-if-not (lambda (p)
                     (member (parameter-name (attributes p))
                             parameter-names))
                   parameters)))

(defun sty (http-parameter)
  (if (validp http-parameter)
      ""
      "attention"))

(defun val-or-raw (http-parameter)
  (if (validp http-parameter)
      (val http-parameter)
      (raw http-parameter)))

(defun params->plist (fn params)
  (mapcan (lambda (param)
            (let ((value (funcall fn param)))
              (if (or value (requiredp (attributes param)))
                  (list (parameter-key (attributes param))
                        value)
                  nil)))
          params))

(defun params->values (group-name &key (fn #'val) (page *page*) (parameters *parameters*))
  (params->plist fn (collect-params group-name page parameters)))

(defun params->payload (&key (page *page*) (parameters *parameters*))
  (params->values :payload :fn #'val-or-raw :page page :parameters parameters))

(defun params->filter (&key (page *page*) (parameters *parameters*))
  (params->values :filter :fn #'val :page page :parameters parameters))

(defun params->styles (&key (page *page*) (parameters *parameters*))
  (params->values :payload :fn #'sty :page page :parameters parameters))


;;; family-params

(defgeneric family-params (page-identifier &rest group-names))

(defmethod family-params ((page dynamic-page) &rest group-names)
  (let* ((allowed-param-names (mapcar #'parameter-name (parameter-attributes page)))
         (group-param-names (mappend (lambda (group-name)
                                       (getf (parameter-groups page) group-name))
                                     group-names))
         (group-parameters (remove-if-not (lambda (param)
                                            (member (parameter-name (attributes param))
                                                    group-param-names))
                                          *parameters*)))
    (params->plist #'val
                   (remove-if-not (lambda (p)
                                    (member (parameter-name (attributes p))
                                            allowed-param-names))
                                  group-parameters))))

(defmethod family-params ((page-name symbol) &rest group-names)
  (apply #'family-params (find-page page-name) group-names))

(defmethod family-params ((page-name null) &rest group-names)
  (declare (ignore page-name group-names))
  (error "FAMILY-PARAMS: Page not found."))


;;; family-url-fn

(defgeneric family-url-fn (page-identifier &rest group-names))

(defmethod family-url-fn ((page regex-page) &rest group-names)
  (lambda (&rest args)
    (apply (page-name page)
           (append *registers*
                   (apply #'family-params page group-names)
                   args))))

(defmethod family-url-fn ((page dynamic-page) &rest group-names)
  (lambda (&rest args)
    (apply (page-name page)
           (append (apply #'family-params page group-names)
                   args))))

(defmethod family-url-fn ((page-name symbol) &rest group-names)
  (apply #'family-url-fn (find-page page-name) group-names))

(defmethod family-url-fn ((page-name null) &rest group-names)
  (declare (ignore page-name group-names))
  (error "FAMILY-URL-FN: Page not found."))


;;; family-url

(defgeneric family-url (page-identifier &rest group-names))

(defmethod family-url ((page page) &rest group-names)
  (funcall (apply #'family-url-fn page group-names)))

(defmethod family-url ((page-name symbol) &rest group-names)
  (apply #'family-url (find-page page-name) group-names))

(defmethod family-url ((page-name null) &rest group-names)
  (declare (ignore page-name group-names))
  nil)



;;; ------------------------------------------------------------
;;; AUTHENTICATION MIXIN
;;; ------------------------------------------------------------

(defclass authentication-mixin ()
  ((allowed-user-groups :reader allowed-user-groups :initarg :allowed-user-groups))
  (:default-initargs :allowed-user-groups '("user" "admin")))

(defclass auth-dynamic-page (dynamic-page authentication-mixin)
  ())

(defclass auth-regex-page (regex-page authentication-mixin)
  ())



;;; ------------------------------------------------------------
;;; PAGE MACROS
;;; ------------------------------------------------------------

;;; Conditions

(define-condition data-entry-error (error)
  ())

(define-condition bad-request-error (error)
  ())

(define-condition authentication-error (error)
  ())


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
           (unless (authenticated-p (allowed-user-groups *page*))
             (error 'authentication-error))
           (when-let (,system-params (collect-params :system))
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
         (if (debug-p *scrooge*)
             (signal c)
             (setf (return-code*) +http-internal-server-error+))))))

(defmacro with-controller-page (view-page-fn &body body)
  (with-gensyms (system-params payload-params)
    `(handler-case
         (progn
           (no-cache)
           (unless (authenticated-p '("user" "admin"))
             (error 'authentication-error))
           (when-let (,system-params (collect-params :system))
             (unless (every #'validp ,system-params)
               (error 'bad-request-error)))
           (when-let (,payload-params (collect-params :payload))
             (unless (every #'validp ,payload-params)
               (error 'data-entry-error)))
           (with-db ()
             ,@body))
       (data-entry-error ()
         (see-other ,(if view-page-fn
                         (let ((fn-call (ensure-list view-page-fn)))
                           `(apply #',(first fn-call) (list* ,@(rest fn-call)
                                                             (params->plist #'raw *parameters*))))
                         '(error "Data entry error"))))
       (bad-request-error ()
         (setf (return-code*) +http-bad-request+))
       (authentication-error ()
         (see-other (login :origin (conc (script-name*)
                                         (when-let (qs (query-string*))
                                           (conc "?" qs))))))
       (error (c)
         (if (debug-p *scrooge*)
             (signal c)
             (setf (return-code*) +http-internal-server-error+))))))

(defmacro with-xhr-page (auth-error-message &body body)
  (with-gensyms (system-params)
    `(handler-case
         (progn
           (no-cache)
           (unless (authenticated-p (allowed-user-groups *page*))
             (error 'authentication-error))
           (when-let (,system-params (collect-params :system))
             (unless (every #'validp ,system-params)
               (error 'bad-request-error)))
           (with-db ()
             ,@body))
       (bad-request-error ()
         (setf (return-code*) +http-bad-request+))
       (authentication-error ()
         ,auth-error-message)
       (error (c)
         (if (debug-p *scrooge*)
             (signal c)
             (setf (return-code*) +http-internal-server-error+))))))
