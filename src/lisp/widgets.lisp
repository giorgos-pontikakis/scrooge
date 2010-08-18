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

(defun find-widget (widget-name)
  (gethash widget-name *widgets*)) 

(defun register-widget (widget-name widget-class &rest args)
  (let ((widget (apply #'make-instance widget-class :name widget-name args)))
    (setf (gethash widget-name *widgets*) widget)
    widget))

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
;;; Table cells
;;; ------------------------------------------------------------

(defclass cell (widget)
  ((name             :accessor name             :initarg :name)
   (row              :accessor row              :initarg :row)
   (style            :accessor style            :initarg :style)
   (disabled-intents :accessor disabled-intents :initarg :disabled-intents)))

(defmethod render ((cell cell) &key)
  (with-html
    (:td :class (style cell)
         (str (lisp-to-html (value cell))))))

(defgeneric disabledp (widget))

(defmethod param ((cell cell))
  (find (name cell) (params (table (row cell))) :key #'name))

(defmethod disabledp ((cell cell)) 
  (or (member (intent (table (row cell))) (disabled-intents cell))
      (not (active-row-p (row cell)))))


;;; Selector

(defclass cell-selector (cell)
  ())

(defmethod render ((cell cell-selector) &key) 
  (with-html 
    (:td :class (style cell)
         (if (active-row-p (row cell))
             (htm (:a :href (get-url (row cell) :barep t)
                      (htm (:img :src (url "img/bullet_red.png")))))
             (htm (:a :href (get-url (row cell))
                      (htm (:img :src (url "img/bullet_blue.png")))))))))

(defun make-cell-selector (&key row name style disabled-intents)
  (make-instance 'cell-selector
                 :name name 
                 :row row
                 :style style
                 :disabled-intents disabled-intents))


;;; Textbox

(defclass cell-textbox (cell)
  ((value :accessor value :initarg :value)))

(defmethod render ((cell cell-textbox) &key)
  
  (if (disabledp cell)
      (call-next-method)
      (flet ((box-style ()
               (let ((p (param cell)))
                 (if (or (null p) (validp p)) "" "attention"))))
        (with-html
          (:td :class (style cell) 
               (textbox (name cell)
                        :value (value cell)
                        :style (box-style)))))))

(defun make-cell-textbox (&key row name value style disabled-intents)
  (make-instance 'cell-textbox
                 :name name 
                 :row row
                 :value value
                 :style style
                 :disabled-intents disabled-intents))


;;; Dropdown

(defclass cell-dropdown (cell)
  ((pairs :accessor pairs :initarg :pairs)
   (value :accessor value :initarg :value)))

(defmethod render ((cell cell-dropdown) &key)
  (if (disabledp cell)
      (call-next-method)
      (with-html
        (:td :class (style cell)
             (dropdown (name cell)
                       (pairs cell)
                       :selected (value cell))))))

(defun make-cell-dropdown (&key row name pairs value style disabled-intents)
  (make-instance 'cell-dropdown
                 :row row
                 :name name
                 :style style
                 :value value
                 :pairs pairs
                 :disabled-intents disabled-intents))


;; Submit

(defclass cell-submit (cell)
  ())

(defmethod render ((cell cell-submit) &key)
  (if (disabledp cell)
      (with-html
        "")
      (with-html
        (:td :class (style cell)
             (:button :type "submit"
                      (:img :src (url "img/tick.png")))))))

(defun make-cell-submit (&key row name style disabled-intents)
  (make-instance 'cell-submit
                 :row row
                 :name name
                 :style style
                 :disabled-intents disabled-intents))


;; Cancel

(defclass cell-cancel (cell)
  ())

(defmethod render ((cell cell-cancel) &key)
  (if (disabledp cell)
      (with-html
        "")
      (with-html
        (:td :class (style cell)
             (:a :href (get-url (row cell))
                 (:img :src (url "img/cancel.png")))))))

(defun make-cell-cancel (&key row name style disabled-intents)
  (make-instance 'cell-cancel
                 :row row
                 :name name
                 :style style
                 :disabled-intents disabled-intents))



;;; ------------------------------------------------------------
;;; Rows
;;; ------------------------------------------------------------

;;; Base row class

(defclass row (widget)
  ((table    :accessor table    :initarg :table)
   (style    :accessor style    :initarg :style) 
   (data     :accessor data     :initarg :data))
  (:default-initargs :style ""))

(defun make-row (&key name table style cols data)
  (make-instance 'row
                 :name name
                 :table table
                 :style style
                 :cols cols
                 :data data))

(defmethod render ((row row) &key)
  (let ((cells (apply (cells-constructor (table row))
                      row
                      (data row)))) 
    (with-html
      (:tr :class (style row)
           (render cells)))))


;;; Selectable row subclass

(defclass row-with-id (row)
  ((active-row-style :accessor active-row-style :initarg :active-row-style))
  (:default-initargs :active-row-style ""))

(defgeneric active-row-p (row-with-id))

(defmethod active-row-p ((row row))
  (set-equal (collectf (id-keys (table row)) (data row))
             (id-param-plist (table row))
             :test #'equal))

(defmethod render ((row row-with-id) &key)
  (let ((cells (apply (cells-constructor (table row))
                      row
                      (data row)))) 
    (with-html
      (:tr :class (if (active-row-p row)
                      (conc (style row) " " (active-row-style row))
                      (style row)) 
           (render cells)))))

(defgeneric get-url (obj &key barep))

(defmethod get-url ((row row-with-id) &key barep)
  (apply (get-url-fn (table row))
         (if barep
             (filter-param-plist (table row))
             (append (collectf (id-keys (table row)) (data row))
                     (filter-param-plist (table row))))))



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
   (styles     :accessor styles     :initarg :styles)
   (cells      :accessor cells      :initarg :cells)))

(defun make-table (col-keys header styles)
  (make-instance 'table 
                 :header header
                 :styles styles))



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

(defgeneric id-param-plist (obj))

(defmethod id-param-plist ((obj page-interface))
  (mapvalf #'val*
           (params->plist (id-keys obj) (params obj))))

(defmethod data-param-plist ((obj page-interface))
  (mapvalf #'val*
           (params->plist (data-keys obj) (params obj))))

(defmethod filter-param-plist ((obj page-interface))
  (mapvalf #'val*
           (params->plist (filter-keys obj) (params obj))))



;;; ------------------------------------------------------------
;;; Table CRUD 
;;; ------------------------------------------------------------

(defclass table-crud (table page-interface)
  ((intent      :accessor intent      :initarg :intent)
   (get-url-fn  :accessor get-url-fn  :initarg :get-url-fn)
   (post-url-fn :accessor post-url-fn :initarg :post-url-fn)))

(defmethod get-url ((tbl table-crud) &key barep)
  (apply (get-url-fn tbl)
         (if barep
             (filter-param-plist tbl)
             (append (id-param-plist tbl)
                     (filter-param-plist tbl)))))

(defun make-table-crud (&key
                        header styles  
                        params id-keys data-keys filter-keys
                        intent get-url-fn post-url-fn)
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
                 :intent intent
                 :get-url-fn get-url-fn
                 :post-url-fn post-url-fn))

(defmethod render ((table table-crud) &key)
  (let* ((db-table (read-db table))) 
    (with-html 
      (:table :id (name table)
              :class (getf (styles table) :table)
              (:thead
               (:tr (dof (lambda (key label)
                           (declare (ignore key))
                           (htm (:th (str label))))
                         (header table))))
              (:tbody 
               (case (intent table)
                 (:view 
                  (iter (for db-row in db-table) 
                        (render (make-instance 'row-with-id
                                               :table table 
                                               :data db-row))))
                 (:create
                  (render (make-form :submit-page (post-url-fn table) 
                                     :body (make-instance 'row-with-id
                                                          :table table 
                                                          :data (id-param-plist table) ;; hack?
                                                          :active-row-style "active"))) 
                  (iter (for db-row in db-table)
                        (render (make-instance 'row-with-id
                                               :table table 
                                               :data db-row))))
                 ((:update :delete)
                  (iter (for db-row in db-table)
                        (for row = (make-instance 'row-with-id
                                                  :table table 
                                                  :active-row-style (if (eql intent :delete)
                                                                        "attention" "active") 
                                                  :data (unionf db-row
                                                                (data-param-plist table))))
                        (render
                         (make-form :submit-page (post-url-fn table)
                                    :hidden (id-param-plist table)
                                    :body row))))))))))


(defgeneric read-db (table-crud &key filters)
  (:documentation
   "Read the database, returning a list of rows as property
   lists. Returned rows must be compatible with the keyword
   arguments of cells-constructor."))

(defgeneric cells-constructor (table)
  (:documentation
   "Return a function which accepts a row as a first argument followed
   by keyword arguments which correspond to the the plist of a
   database row."))


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









