(in-package :scrooge)


(defmacro with-ul ((&key id style) &body li-list)
  `(with-html
     (:ul :id ,id
	  :class ,style
	  ,@(iter (for option in li-list)
		  (collect `(:li (str (lisp-to-html ,option))))))))

;;; Links (de-activateable)
(defun link (label &key href style)
  (with-html
    (if href
	(htm (:a :class style :href href (str (lisp-to-html label))))
	(htm (:span :class style (str (lisp-to-html label)))))))


;;; Tables
(defmacro with-table ((&key caption header id style) &body body)
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
					       (if (and (listp datum) (keywordp (first datum)))
						   (collect `(:td ,datum))
						   (collect `(:td (str (lisp-to-html ,datum))))))))))
		  `(:tbody (:tr (:td "No available data")))))))

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


(defun table (table-data &key caption header id-fn td-fn href-fn active-id id style)
  (with-html 
    (:table :id id :class style
	    (when caption
	      (htm (:caption (str caption))))
	    (when header
	      (htm (:thead (:tr (iter (for label in header)
				      (htm (:th (str label))))))))
	    (if table-data
		(htm (:tbody
		      (iter (for row in table-data)
			    (for row-id = (if id-fn (funcall id-fn row) nil))
			    (for row-td = (if td-fn (funcall td-fn row) row))
			    (for href = (if href-fn (funcall href-fn row-id) nil))
			    (for pred = (and active-id (equal active-id row-id)))
			    (htm (:tr :class (if pred "active" nil)
				      (iter (for datum in row-td)
					    (if pred
						(htm (:td (:p (str (lisp-to-html datum)))))
						(htm (:td (:a :href href
							      (str (lisp-to-html datum))))))))))))
		(htm (:tbody (:tr (:td "No available data"))))))))


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



;;; ------------------------------------------------------------
;;; Widgets
;;; ------------------------------------------------------------

(defvar *widgets* nil)

(defclass widget ()
  ((name :accessor name :initarg :name)))

(defun get-widget (widget-name)
  (find widget-name *widgets* :key #'name)) 

(defun define-widget (widget-name widget-class &rest args)
  (push (apply #'make-instance widget-class :name widget-name args) *widgets*))

(defgeneric render (widget &rest args))

(defmethod render ((obj widget) &rest args)
  (apply (renderer obj) args))



;;; ------------------------------------------------------------
;;; Table with inline form
;;; ------------------------------------------------------------

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
   (db-getter       :accessor db-getter       :initarg :dbdata)
   (renderer        :accessor renderer        :initarg :renderer)
   ;; auxiliary functions
   (html-row        :accessor html-row        :initarg :html-row)
   (simple-row      :accessor simple-row      :initarg :simple-row)
   (form-row        :accessor form-row        :initarg :form-row)
   (row-cells       :accessor row-cells       :initarg :row-cells)))

(defmethod filters (params (obj table-inline-form))
  (objects->plist params #'name (filter-keys obj)))

(defmethod ids (params (obj table-inline-form))
  (objects->plist params #'name (id-keys obj)))

(defmethod data (params (obj table-inline-form))
  (objects->plist params #'name (data-keys obj)))

(defmethod header (obj)
  (cons "" (append (data-header obj) (list "" ""))))

(defmethod styles (params (obj table-inline-form))
  (mapf (lambda (param)
               (if (or (null param) (validp param))
                   nil
                   "attention"))
             (data params obj)))

(defun activep (ids params)
  (let ((keys (keysf ids))
        (vals (valuesf ids)))
    (and (notany #'null vals)
         (every #'equal ids (collectf keys params)))))

(defmethod renderer ((obj table-inline-form))
  #'(lambda (intent params) 
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
                                  :styles styles))))))))

(defmethod html-row ((obj table-inline-form))
  (with-slots (post-urls get-urls) obj
    #'(lambda (intent &key ids data styles)
        (case intent
          ((:view) (funcall (simple-row obj)
                            intent
                            (getf get-urls :view) 
                            ids
                            data
                            styles))
          ((:create :update :delete) (funcall (form-row obj)
                                              intent
                                              (getf get-urls :view)
                                              (getf post-urls intent)
                                              ids
                                              data
                                              styles))))))

(defmethod simple-row ((obj table-inline-form))
  #'(lambda (get-fn ids data styles)
      (let ((activep (activep ids data)))
        (with-html
          (:tr :class (if activep "active" nil)
               (selector-td activep (if activep
                                        (funcall get-fn)
                                        (apply get-fn ids)))
               (funcall (row-cells obj) data styles)
               (:td :class "button" "")
               (:td :class "button" ""))))))

(defmethod form-row ((obj table-inline-form))
  #'(lambda (intent get-fn post-fn ids data styles) 
      (make-form (apply post-fn
                        (mapf #'val
                              (collectf (keyparams (page post-fn)) ids))) 
                 (html () 
                   (:tr :class (if (eql intent :delete) "attention" "active")
                        (selector-td t (funcall get-fn))
                        (funcall (row-cells obj) data styles)
                        (:td :class "button" (ok-button))
                        (:td :class "button" (cancel-button (funcall get-fn))))))))

(defmethod row-cells ((obj table-inline-form))
  (with-slots (data-styles data-keys data-widgets) obj
    #'(lambda (data styles) 
        (iter (for key in data-keys)
              (let ((val (getf data key))
                    (sty (getf styles key)) 
                    (widget (find key data-widgets :key #'name)))
                (render widget (val* val) sty))))))

#|(defmethod define-db-getter ((obj table-inline-form) fn)
  (setf (db-getter obj) fn))|#


;;; ------------------------------------------------------------
;;; Table cells
;;; ------------------------------------------------------------
(defclass cell (widget)
  ((td-style :accessor td-style :initarg :td-style)))


;; String cells

(defclass cell-str (cell)
  ())

(defun make-cell-str (name td-style)
  (make-instance 'cell-str
                 :name name
                 :td-style td-style))

(defmethod renderer ((widget cell-str))
  (with-slots (td-style) widget
    #'(lambda (value style)
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
    #'(lambda (value style) 
        (with-html
          (:td :class td-style
               (textbox name
                        :value value
                        :style style))))))


;; Dropdown cells

(defclass cell-dropdown (cell) (
   (pairs :accessor pairs :initarg :pairs)))

(defun make-cell-dropdown (name td-style pairs)
  (make-instance 'cell-dropdown
                 :name name
                 :td-style td-style
                 :pairs pairs))

(defmethod renderer ((widget cell-dropdown))
  (with-slots (name td-style pairs) widget
    #'(lambda (value style)
        (declare (ignore style))
        (with-html
          (:td :class td-style
               (dropdown name pairs
                         :selected value))))))

