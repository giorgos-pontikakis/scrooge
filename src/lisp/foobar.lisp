(in-package :scrooge)


;;; ------------------------------------------------------------
;;; Widgets superclass
;;; ------------------------------------------------------------

(defvar *widgets* nil)

(defclass widget ()
  ((name      :accessor name      :initarg :name)
   (db-getter :accessor db-getter :initarg :db-getter)
   (renderer  :accessor renderer  :initarg :renderer)))

(defun get-widget (widget-name)
  (find widget-name *widgets* :key #'name)) 

(defun define-widget (widget-name widget-class &rest args)
  (push (apply #'make-instance widget-class :name widget-name args) *widgets*))

(defgeneric render (widget &rest args))

(defmethod render ((obj widget) &rest args)
  (apply (renderer obj) args))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass td-class (widget)
  ((style :accessor style :initarg :style)
   (value :accessor value :initarg :value)))

(defmethod render ((obj cell))
  (with-html
      (:td :class (style obj)
           (render (value obj)))))

(defun td ())


(defclass textbox-class (widget)
  ((id       :accessor id       :initarg :id)
   (class    :accessor class    :initarg :class)
   (type     :accessor type     :initarg :type)
   (name     :accessor name     :initarg :name)
   (value    :accessor value    :initarg :value)
   (readonly :accessor readonly :initarg :readonly)
   (disabled :accessor disabled :initarg :disabled)))

(defmethod render ((obj value))
  (let ((computed-id (or id (string-downcase name)))) 
    (with-html
      (:input :id computed-id
              :class style
              :type (if passwordp "password" "text")
              :name (string-downcase name)
              :value (lisp-to-html (or value :null))
              :readonly readonlyp
              :disabled disabledp))))

(defclass text (widget)
  ((value    :accessor value    :initarg :value)))

(defmethod render ((obj text))
  (with-html
    (lisp-to-html text)))



(defclass dropdown (widget)
  ((name      :accessor name      :initarg :name)
   (value     :accessor value     :initarg :value)
   (style     :accessor style     :initarg :style)
   (readonlyp :accessor readonlyp :initarg :readonlyp)
   (disabledp :accessor disabledp :initarg :disabledp)
   (selected  :accessor selected  :initarg :selected)))

(defmethod render ((obj dropdown))
  (with-html
    (:select :id (string-downcase name)
	     :class style
	     :name (string-downcase name)
	     :disabled disabledp
	     (iter (for (label value) in label-value-alist) 
		   (htm (:option :value (lisp-to-html value)
				 :selected (equal value selected) 
				 :readonly readonlyp 
				 (str (lisp-to-html label))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass table-inline-form (widget)
  ((page-group-name :accessor page-group-name :initarg :page-group-name)
   ;; keys
   (id-keys     :accessor id-keys     :initarg  :id-keys)
   (data-keys   :accessor data-keys   :initarg  :data-keys) 
   (filter-keys :accessor filter-keys :initarg  :filter-keys)
   ;; urls
   (post-urls   :accessor post-urls   :initarg  :post-urls)
   (get-urls    :accessor get-urls    :initarg  :get-urls)
   ;; display
   (data-header     :accessor data-header     :initarg :data-header)
   (data-styles     :accessor data-styles     :initarg :data-styles)
   (data-widgets    :accessor data-widgets    :initarg :data-widgets) 
   (table-styles    :accessor table-styles    :initarg :table-styles) 
   ;; auxiliary functions
   (html-row        :accessor html-row        :initarg :html-row)
   (simple-row      :accessor simple-row      :initarg :simple-row)
   (form-row        :accessor form-row        :initarg :form-row)
   (row-cells       :accessor row-cells       :initarg :row-cells)))

(defmethod render ((obj table-inline-form) &key params intent)
  (let ((header (header obj))
        (filters (filters params obj))
        (ids (ids params obj)) 
        (data (data params obj)) 
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
                 (funcall (html-row obj)
                          intent
                          :ids ids
                          :data data
                          :styles styles))
               (iter (for db-data in (funcall (db-getter obj) filters)) 
                     (funcall (html-row obj)
                              intent
                              :ids ids
                              :data (if (activep ids db-data)
                                        (unionf data db-data)
                                        db-data)
                              :styles styles)))))))

(defun table-inline-form (params id-keys data-keys filter-keys
                          post-urls get-urls
                          data-header data-widgets style)
  (make-instance 'table-inline-form
                 :params params
                 :intent intent
                 :id-keys id-keys
                 :data-keys data-keys
                 :filter-keys filter-keys 
                 :post-urls post-urls
                 :get-urls get-urls
                 :data-header data-header
                 :data-widgets data-widgets
                 :style style))

(defun stran-table-maker (params)
  (lambda (params)
    (table-inline-form
     :data-keys '(:description :debit-acc :credit-acc :old-status :new-status) 
     :filter-keys '()
     :post-urls (list :create 'actions/stran/create
                      :update 'actions/stran/update
                      :delete 'actions/stran/delete
                      :chstat 'actions/stran/chstat) 
     :get-urls (list :view 'stran
                     :create 'stran/create
                     :update 'stran/update
                     :delete 'stran/delete
                     :chstat 'stran/chstat)
     :data-header '("Πίνακας" "Περιγραφή" "Αρχική Κατάσταση"
                    "Τελική Κατάσταση" "Λογ. Χρέωσης" "Λογ. Πίστωσης") 
     :cell-widgets (list (make-cell-textbox :description "data")
                         (make-cell-textbox :debit-acc "data")
                         (make-cell-textbox :credit-acc "data")
                         (make-cell-dropdown :old-status "data" (make-status-getter))
                         (make-cell-dropdown :new-status "data" (make-status-getter)))
     :table-styles '(:table-style "forms-in-row"))))







(make-instance 'table-inline-form
               :params 
               :id-keys '(:stran-id :tbl)
               :data-keys '(:description :debit-acc :credit-acc :old-status :new-status) 
               :filter-keys '()
               :post-urls (list :create 'actions/stran/create
                                :update 'actions/stran/update
                                :delete 'actions/stran/delete
                                :chstat 'actions/stran/chstat) 
               :get-urls (list :view 'stran
                               :create 'stran/create
                               :update 'stran/update
                               :delete 'stran/delete
                               :chstat 'stran/chstat)
               :data-header '("Πίνακας" "Περιγραφή" "Αρχική Κατάσταση"
                              "Τελική Κατάσταση" "Λογ. Χρέωσης" "Λογ. Πίστωσης") 
               :cell-widgets (list (make-cell-textbox :description "data")
                                   (make-cell-textbox :debit-acc "data")
                                   (make-cell-textbox :credit-acc "data")
                                   (make-cell-dropdown :old-status "data" (make-status-getter))
                                   (make-cell-dropdown :new-status "data" (make-status-getter)))
               :table-styles '(:table-style "forms-in-row"))

(defun stran-table-inline-form-defaults ())
(table-inline-form 'stran
                   :params 
                   :id-keys '(:stran-id :tbl)
                   :data-keys '(:description :debit-acc :credit-acc :old-status :new-status) 
                   :filter-keys '()
                   :post-urls (list :create 'actions/stran/create
                                    :update 'actions/stran/update
                                    :delete 'actions/stran/delete
                                    :chstat 'actions/stran/chstat) 
                   :get-urls (list :view 'stran
                                   :create 'stran/create
                                   :update 'stran/update
                                   :delete 'stran/delete
                                   :chstat 'stran/chstat)
                   :data-header '("Πίνακας" "Περιγραφή" "Αρχική Κατάσταση"
                                  "Τελική Κατάσταση" "Λογ. Χρέωσης" "Λογ. Πίστωσης") 
                   :cell-widgets (list (make-cell-textbox :description "data")
                                       (make-cell-textbox :debit-acc "data")
                                       (make-cell-textbox :credit-acc "data")
                                       (make-cell-dropdown :old-status "data" (make-status-getter))
                                       (make-cell-dropdown :new-status "data" (make-status-getter)))
                   :table-styles '(:table-style "forms-in-row"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; String cells

(defclass cell-str (cell)
  ())

(defun make-cell-str (name td-style)
  (make-instance 'cell-str
                 :name name
                 :td-style td-style))

(defmethod renderer ((widget cell-str))
  (with-slots (td-style) widget
    #'(lambda (datum style)
        (declare (ignore style))
        (with-html
          (:td :class td-style
               (lisp-to-html value))))))


;; Textbox cells

(defclass cell-textbox (cell)
  ())

(defun make-cell-textbox (name td-style)
  (make-instance 'cell-textbox
                 :name name
                 :td-style td-style))

(defmethod renderer ((widget cell-textbox))
  (with-slots (name td-style) widget
    #'(lambda (datum style) 
        (with-html
          (:td :class td-style
               (textbox name
                        :value datum
                        :style style))))))


;; Dropdown cells

(defclass cell-dropdown (cell)
  ())

(defun make-cell-dropdown (name td-style db-getter)
  (make-instance 'cell-dropdown
                 :name name
                 :td-style td-style
                 :db-getter db-getter))

(defmethod renderer ((widget cell-dropdown))
  (with-slots (name td-style db-getter) widget
    #'(lambda (datum style)
        (declare (ignore datum style))
        (with-html
          (:td :class td-style
               (dropdown name (funcall db-getter filters)
                         :selected name))))))
