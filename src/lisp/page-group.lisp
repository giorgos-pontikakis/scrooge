(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Auxiliary Functions
;;; ------------------------------------------------------------





;;; ------------------------------------------------------------
;;; Page groups
;;; ------------------------------------------------------------

(defparameter *page-groups* nil)

(defclass page-group ()
  ((name        :accessor name        :initarg  :name)
   (id-keys     :accessor id-keys     :initarg  :id-keys)
   (data-keys   :accessor data-keys   :initarg  :data-keys) 
   (filter-keys :accessor filter-keys :initarg  :filter-keys)
   (post-urls   :accessor post-urls   :initarg  :post-urls)
   (get-urls    :accessor get-urls    :initarg  :get-urls)))

(defun get-page-group (page-group-name)
  (find page-group-name *page-groups* :key #'name))

(defun define-page-group (page-group-name &rest args)
  (let ((obj (apply #'make-instance 'page-group :name page-group-name args)))
    (push obj *page-groups*)))



#|(defmethod initialize-instance :after ((obj table-inline-form)) 
  (let ((group (get-page-group (page-group-name obj))))
    ;; html-table
    (setf (renderer obj)
          #'(lambda (intent params)
              (let ((header (header obj))
                    (filters (filters params group))
                    (ids (ids params group))
                    (data (data params group))
                    (styles (styles params obj)))
                (with-html
                  (:table :id (concatenate 'string
                                           (string-downcase (symbol-name (name obj)))
                                           "-table")
                          :class (getf (table-styles obj) :table-style)
                          (:thead
                           (:tr (iter (for label in header) 
                                      (htm (:th (str label))))))
                          (:tbody
                           (when (eql intent :create)
                             (make-row intent id data styles))
                           (iter (for db-data in (funcall (db-data-table obj) filters))
                                 (funcall (html-row obj)
                                          intent
                                          :ids ids
                                          :data (if (activep ids db-data)
                                                    (unionf data db-data)
                                                    db-data)
                                          :styles styles))))))))
    ;; html-row
    (let ((post-urls (post-urls group))
          (get-urls (get-urls group)))
      (setf (html-row obj)
            #'(lambda (intent &key ids data styles) 
                (funcall (case intent
                           ((:view) (simple-row obj))
                           ((:create :update :delete) (form-row obj)))
                         intent
                         (getf get-urls :view)
                         (getf post-urls intent)
                         ids
                         data
                         styles))))
    ;; simple-row
    (setf (simple-row obj)
          #'(lambda (intent get-fn post-fn ids filters data styles)
              (declare (ignore post-fn))
              (let ((activep (activep ids data)))
                (with-html
                  (:tr :class (if activep "active" nil)
                       (selector-td activep (if activep
                                                (funcall get-fn)
                                                (apply get-fn ids filters)))
                       ;;(row-cells intent values styles)
                       (:td :class "button" "")
                       (:td :class "button" ""))))))
    ;; form-row
    (setf (form-row obj)
          #'(lambda (intent get-fn post-fn ids filters data styles) 
              (make-form (apply post-fn ids)
                         (html ()
                           (:tr :class (if (eql intent :delete) "attention" "active")
                                (selector-td t (funcall get-fn))
                                (funcall (row-cells obj) values)
                                (:td :class "button" (ok-button))
                                (:td :class "button" (cancel-button (funcall get-fn))))))))
    ;; row-cells
    (let ((data-keys (data-keys obj))
          (data-styles (data-styles obj))
          ())
      (setf (row-cells obj) 
            #'(lambda (values)
                (iter (for key in data-keys)
                      (let ((val (getf values key))
                            (sty (getf styles key))
                            (td-sty (getf data-styles key))
                            (widget (getf data-widgets key)))
                        (with-html
                          (:td :class td-sty
                               (render widget val sty)))
                        (ecase widget
                          (:str (with-html
                                  (:td :class td-sty
                                       (str (lisp-to-html val)))))
                          (:textbox (with-html
                                      (:td :class td-sty
                                           (textbox td-key :value val :style sty))))
                          (:dropdown )))))))))|#