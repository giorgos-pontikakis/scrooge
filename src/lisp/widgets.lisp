(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; Tables
(defmacro with-table2 ((&key caption header id style) (&rest columns) &body body)
  `(with-html
     (:table :id ,id :class ,style
	     ,(when caption
		    `(:caption (str (lisp-to-html ,caption)))) 
	     ,(when header
		    `(:thead (:tr (iter (for label in ,header)
					(htm (:td (str (lisp-to-html label))))))))
	     ,(if body
		  `(:tbody 
		    ,@(iter (for row in body) 
			    (collect `(:tr
				       ,@(iter (for datum in row)
					       (for col in (or columns (make-list (length row))))
					       (if (and (listp datum) (keywordp (first datum)))
						   (collect `(:td ,datum))
						   (collect `(:td :class ,col
								  (str (lisp-to-html ,datum))))))))))
		  `(:tbody (:tr (:td "No available data")))))))



;;; ------------------------------------------------------------
;;; Widgets superclass
;;; ------------------------------------------------------------

(defvar *widgets* (make-hash-table))

(defclass widget ()
  ((name :accessor name :initarg :name)
   (body :accessor body :initarg :body)))

;; (defun find-widget (widget-name)
;;   (gethash widget-name *widgets*)) 

;; (defun register-widget (widget-name widget-class &rest args)
;;   (let ((widget (apply #'make-instance widget-class :name widget-name args)))
;;     (setf (gethash widget-name *widgets*) widget)
;;     widget))

(defgeneric style (widget))

(defgeneric render (thing &key)
  (:documentation "A function that produces html from the widget object"))

(defmethod render ((fn function) &key)
  (funcall fn))

(defmethod render ((value t) &key) 
  (with-html
    (str (lisp-to-html value))))

(defmethod render ((list list) &key) 
  (with-html
    (mapc #'render list)))



;;; ------------------------------------------------------------
;;; Forms
;;; ------------------------------------------------------------
(defclass form (widget)
  ((submit-page  :accessor submit-page  :initarg :submit-page) 
   (hidden     :accessor hidden     :initarg :hidden)
   (body       :accessor body       :initarg :body)))

(defmethod render ((form form) &key)
  (let ((page (page (submit-page form))))
    (with-html
      (:form :method (request-type page)
             :action (url (base-url page))
             (iter (for key in (hidden form) by #'cddr)
                   (for val in (rest (hidden form)) by #'cddr)
                   (with-html
                     (:input :type "hidden"
                             :id (string-downcase key)
                             :class "display-none"
                             :name (string-downcase key)
                             :value (lisp-to-html val))))
             (render (body form))))))

(defun make-form (&key submit-page hidden body)
  (make-instance 'form
                 :submit-page submit-page
                 :hidden hidden 
                 :body body))



;;; ------------------------------------------------------------
;;; Tables 
;;; ------------------------------------------------------------

(defclass table (widget)
  ((header     :accessor header     :initarg :header)
   (styles     :accessor styles     :initarg :styles)))

(defun make-table (header styles)
  (make-instance 'table 
                 :header header
                 :styles styles))

(defgeneric read-db (table-crud &key filters)
  (:documentation
   "Read the database, returning a list of rows as property
   lists. Returned rows must be compatible with the accepted arguments
   of cells-constructor."))

;; (defgeneric cells-constructor (table)
;;   (:documentation
;;    "Return a function which accepts a row as a first argument followed
;;    by keyword arguments which correspond to the the plist of a
;;    database row."))

;; (defmethod cells-constructor ((tbl table))
;;   (lambda (&rest args)
;;     (make-list (length args) :initial-element #'identity)))



;;; ------------------------------------------------------------
;;; Rows
;;; ------------------------------------------------------------

(defclass row (widget)
  ((table    :accessor table    :initarg :table)
   (style    :accessor style    :initarg :style) 
   (data     :accessor data     :initarg :data))
  (:default-initargs :style ""))

(defun make-row (&key name table style data)
  (make-instance 'row
                 :name name
                 :table table
                 :style style 
                 :data data))

(defmethod render ((row row) &key)
  (let ((cells-list (funcall (cells (table row)) row)))
   (with-html
     (:tr :class (style row)
          (render cells-list)))))



;;; ------------------------------------------------------------
;;; Cells 
;;; ------------------------------------------------------------

(defclass cell (widget)
  ((row   :accessor row   :initarg :row)
   (style :accessor style :initarg :style)
   (value :accessor value :initarg :value)))

(defun make-cell (&key row value style)
  (make-instance 'cell
                 :row row
                 :value value
                 :style style))

(defmethod render ((cell cell) &key)
  (with-html
    (:td :class (style cell)
         (str (lisp-to-html (value cell))))))



;;; ------------------------------------------------------------
;;; Page interface 
;;; ------------------------------------------------------------

(defclass page-interface ()
  ((params      :accessor params      :initarg :params)
   (id-keys     :accessor id-keys     :initarg :id-keys)
   (data-keys   :accessor data-keys   :initarg :data-keys) 
   (filter-keys :accessor filter-keys :initarg :filter-keys)))

(defun params->plist (bag params) 
  (mapcan (lambda (key)
            (let ((par (find key params :key #'name)))
              (list key par)))
          bag))

(defmethod id ((obj page-interface))
  (plist-map-vals #'val*
                  (params->plist (id-keys obj) (params obj))))

(defmethod data ((obj page-interface))
  (plist-map-vals #'val*
                  (params->plist (data-keys obj) (params obj))))

(defmethod filters ((obj page-interface))
  (plist-map-vals #'val*
                  (params->plist (filter-keys obj) (params obj))))



;;; ------------------------------------------------------------
;;; Table-CRUD 
;;; ------------------------------------------------------------

(defclass table-crud (table page-interface)
  ((operation    :accessor operation    :initarg :operation)
   (main-page    :accessor main-page    :initarg :main-page) 
   (submit-pages :accessor submit-pages :initarg :submit-pages)
   (cells        :accessor cells        :initarg :cells)))


(defun make-table-crud (&key
                        header styles  
                        params id-keys data-keys filter-keys
                        operation get-pages submit-pages)
  (make-instance 'table-crud
                 ;; table
                 :header header
                 :styles styles  
                 ;; page-interface
                 :params params
                 :id-keys id-keys
                 :data-keys data-keys
                 :filter-keys filter-keys
                 ;; crud
                 :operation operation
                 :get-pages get-pages
                 :submit-pages submit-pages))

(defgeneric submit-page (table))

(defmethod submit-page ((tbl table-crud))
  (getf (submit-pages tbl) (operation tbl)))


(defmethod render ((table table-crud) &key)
  (let* ((db-table (read-db table))) 
    (with-html 
      (:table :id (name table)
              :class (getf (styles table) :table)
              (:thead
               (:tr (plist-do (lambda (key label)
                                (declare (ignore key))
                                (htm (:th (str label))))
                              (header table))))
              (:tbody 
               (case (operation table)
                 (:view 
                  (iter (for db-row in db-table) 
                        (render (make-row-crud :table table 
                                               :data db-row))))
                 (:create
                  (render (make-form :submit-page (submit-page table) 
                                     :body (make-row-crud :table table 
                                                          :data (append (id table) (data table))
                                                          :active-style "active"))) 
                  (iter (for db-row in db-table)
                        (render (make-instance 'row-crud
                                               :table table 
                                               :data db-row))))
                 ((:update :delete) 
                  (let ((rows
                         (iter (for db-row in db-table)
                               (let* ((activep (set-equal (plist-collect (id-keys table) db-row)
                                                          (id table)
                                                          :test #'equal))
                                      (active-style (if (eql (operation table) :delete)
                                                        "attention"
                                                        "active"))
                                      (data (if activep
                                                (plist-union (append (id table) (data table))
                                                             db-row)
                                                db-row)))
                                 (collect (make-row-crud :table table 
                                                         :active-style active-style
                                                         :data data)))))) 
                    (render (make-form :submit-page (submit-page table)
                                       :hidden (id table)
                                       :body rows))))))))))



;;; ------------------------------------------------------------
;;; Row-CRUD
;;; ------------------------------------------------------------

(defclass row-crud (row)
  ((active-style :accessor active-style :initarg :active-style))
  (:default-initargs :active-style ""))

(defun make-row-crud (&key name table style data active-style)
  (make-instance 'row-crud
                 :name name
                 :table table
                 :style style 
                 :data data
                 :active-style active-style))

(defgeneric id (table-or-row))

(defmethod id ((row row-crud))
  (plist-collect (id-keys (table row)) (data row)))


(defgeneric active-row-p (row-crud))

(defmethod active-row-p ((row row))
  (set-equal (id row)
             (id (table row))
             :test #'equal))


(defmethod render ((row row-crud) &key)
  (let ((cells-list (funcall (cells (table row)) row)))
    (with-html
      (:tr :class (if (active-row-p row)
                      (conc (style row) " " (active-style row))
                      (style row)) 
           (render cells-list)))))



;;; ------------------------------------------------------------
;;; Cells for Table-CRUD
;;; ------------------------------------------------------------

(defclass cell-crud (cell)
  ((operations :accessor operations :initarg :operations)))

(defgeneric enabledp (cell-crud))

(defmethod enabledp ((cell cell-crud)) 
  (and (member (operation (table (row cell))) (operations cell))
       (active-row-p (row cell))))


;;; Selector

(defclass cell-selector (cell-crud)
  ())

(defmethod render ((cell cell-selector) &key)
  (flet ((selector-href (row &key barep)
           (apply (main-page (table row))
                  (if barep
                      (filters (table row))
                      (append (id row) 
                              (filters (table row)))))))
    (with-html 
      (:td :class (style cell)
           (if (active-row-p (row cell))
               (htm (:a :href (selector-href (row cell) :barep t)
                        (htm (:img :src (url "img/bullet_red.png")))))
               (htm (:a :href (selector-href (row cell))
                        (htm (:img :src (url "img/bullet_blue.png"))))))))))

(defun make-cell-selector (&key row name style operations)
  (make-instance 'cell-selector
                 :name name 
                 :row row
                 :style style
                 :operations operations))


;;; Textbox

(defclass cell-textbox (cell-crud)
  ())

(defmethod render ((cell cell-textbox) &key)
  (labels ((param (cell)
             (find (name cell) (params (table (row cell))) :key #'name))
           (box-style ()
             (let ((p (param cell)))
               (if (or (null p) (validp p)) "" "attention"))))
    (if (enabledp cell) 
        (with-html
          (:td :class (style cell) 
               (textbox (name cell)
                        :value (value cell)
                        :style (box-style))))
        (call-next-method))))

(defun make-cell-textbox (&key row name value style operations)
  (make-instance 'cell-textbox
                 :name name 
                 :row row
                 :value value
                 :style style
                 :operations operations))


;;; Dropdown

(defclass cell-dropdown (cell-crud)
  ((pairs :accessor pairs :initarg :pairs)))

(defmethod render ((cell cell-dropdown) &key)
  (if (enabledp cell)
      (with-html
        (:td :class (style cell)
             (dropdown (name cell)
                       (pairs cell)
                       :selected (value cell))))
      (call-next-method)))

(defun make-cell-dropdown (&key row name pairs value style operations)
  (make-instance 'cell-dropdown
                 :row row
                 :name name
                 :style style
                 :value value
                 :pairs pairs
                 :operations operations))


;; Submit

(defclass cell-submit (cell-crud)
  ())

(defmethod render ((cell cell-submit) &key)
  (if (enabledp cell)
      (with-html
        (:td :class (style cell)
             (:button :type "submit"
                      (:img :src (url "img/tick.png")))))
      (with-html
        "")))

(defun make-cell-submit (&key row name style operations)
  (make-instance 'cell-submit
                 :row row
                 :name name
                 :style style
                 :operations operations))


;; Cancel

(defclass cell-cancel (cell-crud)
  ())

(defmethod render ((cell cell-cancel) &key)
  (flet ((cancel-href (row)
           (apply (main-page (table row))
                  (filters (table row)))))
    (if (enabledp cell)
        (with-html
          (:td :class (style cell)
               (:a :href (cancel-href (row cell))
                   (:img :src (url "img/cancel.png")))))
        (with-html
          ""))))

(defun make-cell-cancel (&key row name style operations)
  (make-instance 'cell-cancel
                 :row row
                 :name name
                 :style style
                 :operations operations))



;;; ------------------------------------------------------------
;;; Navigation bars
;;; ------------------------------------------------------------

(defmacro define-navbar (name (&rest arglist) (&key id div-style ul-style)
                         &body body) 
  (multiple-value-bind (items fns) (iter (for sexp in body)
                                         (destructuring-bind (sym href &rest forms) sexp
                                           (collect sym into items)
                                           (collect `(lambda (class)
                                                       (with-html
                                                         (:li (:a :class class
                                                                  :href ,href
                                                                  ,@forms)))) into fns)
                                           (finally (return (values items fns)))))
    `(defun ,name (active-item ,@arglist) 
       (with-html
         (:div :id ,id :class ,div-style
               (:ul :class ,ul-style 
                    (iter (for item in ',items)
                          (for fn in (list ,@fns))
                          (funcall fn (if (eql item active-item) "active" nil)))))))))

(defmacro define-menu (name (&rest args) (&key id div-style ul-style)
		       &body body)
  (with-gensyms (opt-list)
    (let ((options (iter (for (key fn-body) in body)
			 (collect key)
			 (collect `(lambda ,args
				     (declare (ignorable ,@args))
				     ,fn-body)))))
      `(defun ,name ,(append args `(&rest ,opt-list))
	 (let ((fns (list ,@options)))
	   (with-html
	     (:div :id ,id :class ,div-style
		   (:ul :class ,ul-style
			(iter (for key in ,opt-list)
			      (funcall (getf fns key) ,@args))))))))))


(defmacro define-errorbar (name (&key id div-style ul-style)
			   &body body)
  (let ((arglist (mapcar #'first body)))
    `(defun ,name ,arglist
       (unless (every #'validp (list ,@arglist))
	 (with-html
	   (:div :id ,id :class ,div-style
		 (:ul :class ,ul-style 
		      ,@(iter (for (arg msg) in body)
                              (collect `(unless (validp ,arg)
                                          (htm (:li ,msg))))))))))))

(defmacro filter (action id filter disabledp)
  `(with-form (,action :id ,id)
     (with-html
       (:p "Φίλτρο: " (textbox 'filter :value ,filter :disabledp ,disabledp) (ok-button)))))









