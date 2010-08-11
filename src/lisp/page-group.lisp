(in-package :scrooge)

;;; ------------------------------------------------------------
;;; Auxiliary Functions
;;; ------------------------------------------------------------

(defun filters (params obj)
  (params->plist params #'name (filter-keys obj)))

(defun ids (params obj)
  (params->plist params #'name (id-keys obj)))

(defun data (params obj)
  (params->plist params #'name (data-keys obj)))

(defun header (obj)
  (concatenate 'string "" (data-header obj) "" ""))

(defun styles (params obj)
  (error "Need to be implemented."))

(defun activep (ids data)
  (let ((keys (plist-keys ids))
        (vals (plist-vals ids)))
    (and (notany #'null vals)
         (every #'equal ids (plist-subset data keys)))))



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
   (get-urls    :accessor get-urls    :initarg  :get-urls)
   (css         :accessor css         :initarg  :css)
   (html-table  :accessor html-table  :initform nil)
   (html-row    :accessor html-row    :initform nil)
   (db-data     :accessor db-data     :initform nil)))

(defun get-page-group (page-group-name)
  (find page-group-name *page-groups* :key #'name))

(defun define-page-group (page-group-name &rest args)
  (let ((obj (apply #'make-instance 'page-group :name name args)))
    (push obj *page-groups*)))



;;; ------------------------------------------------------------
;;; Widgets
;;; ------------------------------------------------------------

(defclass widget ()
  ((name :accessor name :initarg :name)))

(defun get-widget (widget-name)
  ) 

(defgeneric render (widget &key))

;;; ------------------------------------------------------------
;;; Cooperating widgets
;;; ------------------------------------------------------------

(defclass html-table-inline (widget)
  ((page-group-name :accessor page-group-name :initarg :page-group-name)
   (data-header     :accessor data-header     :initarg :data-header)
   (data-styles     :accessor data-styles     :initarg :data-styles)
   (data-widgets    :accessor data-widgets    :initarg :data-widgets) 
   (table-css       :accessor table-css       :initarg :table-css)
   (db-data         :accessor db-data         :initarg :dbdata)
   (renderer        :accessor renderer        :initarg :renderer)
   ;; auxiliary
   (html-row        :accessor html-row        :initarg :html-row)
   (simple-row      :accessor simple-row      :initarg :simple-row)
   (form-row        :accessor form-row        :initarg :form-row)
   (row-td          :accessor row-td          :initarg :row-td)))

(defmethod initialize-instance :after ((obj html-table-inline)) 
  (let ((group (get-page-group (page-group-name obj))))
    ;; html-table
    (setf (renderer obj)
          #'(lambda (intent params)
              (let ((header (header obj))
                    (filters (filters params group))
                    (ids (ids params group))
                    (data (data params group))
                    (styles (styles params obj)))
                (:table :id (concatenate 'string (string-downcase (symbol-name (name obj))) "-table")
                        :class (getf (table-css obj) :table-style)
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
                                                  (plist-union data db-data)
                                                  db-data)
                                        :styles styles)))))))
    ;; html-row
    (let ((post-urls (post-urls group))
          (get-urls (get-urls group)))
      (setf (html-row obj)
            #'(lambda (intent &key ids data styles) 
                (funcall (case intent
                           ((:view) (html-row-simple obj))
                           ((:create :update :delete) (html-row-form obj)))
                         intent
                         (getf get-urls :view)
                         (getf post-urls intent)
                         ids
                         data
                         styles))))
    ;; simple-row
    (setf (simple-row obj)
          #'(lambda (intent get-fn post-fn ids filters data styles)
              (declare (ignore get-fn))
              (let ((activep (activep ids data)))
                (with-html
                  (:tr :class (if activep "active" nil)
                       (selector-td activep (if activep
                                                (funcall get-fn)
                                                (apply get-fn ids filters)))
                       ;;(row-td intent values styles)
                       (:td :class "button" "")
                       (:td :class "button" ""))))))
    ;; form-row
    (setf (form-row obj)
          #'(lambda (intent get-fn post-fn ids filters data styles) 
              (make-form (apply post-fn ids)
                         (html ()
                           (:tr :class (if (eql intent :delete) "attention" "active")
                                (selector-td t (funcall get-fn))
                                (row-td)
                                (:td :class "button" (ok-button))
                                (:td :class "button" (cancel-button (funcall get-fn))))))))
    ;; row-td
    (let ((data-keys (data-keys obj))
          (data-styles (data-styles obj)))
      (setf (row-td obj) 
            #'(lambda (values)
                (iter (for key in data-keys)
                      (let ((val (getf values key))
                            (sty (getf styles key))
                            (td-sty (getf data-styles key))
                            (widget (if (member intent (:view :delete))
                                        :str
                                        (getf data-widgets key))))
                        (with-html
                          (:td :class td-sty
                               (funcall widget)))
                        (ecase widget
                          (:str (with-html
                                  (:td :class td-sty
                                       (str (lisp-to-html val)))))
                          (:textbox (with-html
                                      (:td :class td-sty
                                           (textbox td-key :value val :style sty))))
                          (:dropdown )))))))))

(defmethod render ((obj html-table-inline) &key intent params)
  (funcall (renderer obj) intent params))



(defun define-db-data (fn (obj html-table-inline))
  (setf (db-data obj) fn))


