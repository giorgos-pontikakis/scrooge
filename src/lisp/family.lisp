(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; WIDGET FAMILIES
;;; ----------------------------------------------------------------------

(defclass family-mixin ()
  ((parameter-groups :reader   parameter-groups :initarg :parameter-groups)
   (action-url-fns   :reader   action-url-fns   :initarg :action-url-fns)
   (action-labels    :reader   action-labels    :initarg :action-labels)

   (op-groups        :accessor op-groups        :initarg :op-groups)
   )
  (:default-initargs :parameter-groups '()))

(defgeneric action-label (family-object op))

(defmethod action-label ((obj family-mixin) op)
  (or (getf (action-labels obj) op)
      (assoc-value *action-labels* op)))


(defgeneric action-url-fn (family-object op))

(defmethod action-url-fn ((obj family-mixin) op)
  (getf (action-url-fns obj) op))

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
            (list (parameter-key (attributes param))
                  (funcall fn param)))
          params))

(defun params->values (group-name &key (page *page*) (parameters *parameters*))
  (params->plist #'val-or-raw
                 (collect-params group-name page parameters)))

(defun params->styles (group-name &key (page *page*) (parameters *parameters*))
  (params->plist #'sty
                 (collect-params group-name page parameters)))

(defgeneric top-level-actions (family)
  (:documentation "top-level actions"))

;; (defgeneric zip-system-parameters (widget))

;; (defmethod zip-system-parameters ((widget family-mixin))
;;   (make-plist (mapcar #'make-keyword
;;                       (getf (parameter-groups widget) :system))
;;               (ensure-list (selected-key widget))))

(defun make-link (op &optional group-names parameter-values)
  (apply (getf (action-url-fns *page*) op)
         (make-family-args op group-names parameter-values)))

(defun make-family-args (op &optional group-names parameter-values)
  (append *registers*
          (mapcan (lambda (grp)
                    (or parameter-values (params->values grp)))
                  (or group-names (default-group-names op *page*)))))

(defun default-group-names (op page)
  (let ((op-groups (op-groups page)))
    (iter
      (for key in op-groups by #'cddr)
      (for val in (rest op-groups) by #'cddr)
      (when (or (and (keywordp key) (eql op key))
                (and (listp key) (member op key)))
        (return val)))))

(defun url-fn-aux ((url-fn-id symbol))
  (let ((allowed-params (remove-if-not (lambda (p)
                                         (requiredp (attributes p)))
                                       (parameter-attributes (find-page url-fn-id)))))
    (lambda (&rest args)
      (apply (symbol-function url-fn-id)
             (append *registers*
                     (remove-if-not (lambda (reqp)
                                      (member (parameter-name (attributes reqp))
                                              parameter-names))
                                    *parameters*)
                     args)))))

(defmethod url-fn ((op symbol))
  (url-fn-aux (getf (action-url-fns *page*) op)))



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
             (unless (every #'validp ,system-params)`
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
