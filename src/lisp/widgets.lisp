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

;; (defgeneric style (widget))

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
  ((submit-page :accessor submit-page :initarg :submit-page) 
   (hidden      :accessor hidden      :initarg :hidden)
   (body        :accessor body        :initarg :body)))

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
;;; Page interface mixin
;;; ------------------------------------------------------------

(defclass page-interface-mixin ()
  ((params       :accessor params       :initarg :params)
   (id-keys      :accessor id-keys      :initarg :id-keys)
   (payload-keys :accessor payload-keys :initarg :payload-keys) 
   (filter-keys  :accessor filter-keys  :initarg :filter-keys)))

(defun params->plist (bag params) 
  (mapcan (lambda (key)
            (let ((par (find key params :key #'name)))
              (list key par)))
          bag))

(defgeneric id-params (obj))

(defmethod id-params ((obj page-interface-mixin))
  (plist-map-vals #'val*
                  (params->plist (id-keys obj) (params obj))))


(defgeneric payload-params (obj))

(defmethod payload-params ((obj page-interface-mixin))
  (plist-map-vals #'val*
                  (params->plist (payload-keys obj) (params obj))))


(defgeneric filter-params (obj))

(defmethod filter-params ((obj page-interface-mixin))
  (plist-map-vals #'val*
                  (params->plist (filter-keys obj) (params obj))))


;;; ------------------------------------------------------------
;;; CRUD mixin  
;;; ------------------------------------------------------------

(defclass crud-mixin ()
  ((operation    :accessor operation    :initarg :operation)
   (main-page    :accessor main-page    :initarg :main-page) 
   (submit-pages :accessor submit-pages :initarg :submit-pages)))


(defgeneric submit-page (obj))

(defmethod submit-page ((obj crud-mixin))
  (getf (submit-pages obj) (operation obj)))


;;; ------------------------------------------------------------
;;; Tables 
;;; ------------------------------------------------------------

;;; Non-CRUD

(defclass table (widget)
  ((header    :accessor header    :initarg :header)
   (styles    :accessor styles    :initarg :styles) 
   (data-fn   :accessor data-fn   :initarg :data-fn)
   (cells-fn  :accessor cells-fn  :initarg :cells-fn)))

(defclass table-normal (table)
  ((tbody-class :reader tbody-class :initform 'tbody-normal)
   (row-class   :reader row-class   :initform 'row-normal)))

(defmethod render ((table table-normal) &key)
  (with-html
    (:table :id (name table)
            :class (getf (styles table) :table)
            ;; header
            (:thead (plist-do (lambda (key label)
                                (declare (ignore key))
                                (htm (:th (str label))))
                              (header table)))
            ;; body
            (render (make-instance (tbody-class table)
                                   :table table
                                   :style (getf (styles table) :tbody)
                                   :data (funcall (data-fn table) (filter-params table)))))))

(defclass table-ul (table)
  ((tbody-class :reader tbody-class :initform 'tbody-ul)
   (row-class   :reader row-class   :initform 'row-ul)))

(defmethod render ((table table-ul) &key)
  (with-html
    (:div :id (name table)
          :class (getf (styles table) :table)
          ;; header
          (:ul (:li (plist-do (lambda (key label)
                                (declare (ignore key))
                                (htm (:th (str label))))
                              (header table))))
          ;; body
          (render (make-instance (tbody-class table)
                                 :table table
                                 :style (getf (styles table) :tbody)
                                 :data (funcall (data-fn table) (filter-params table)))))))


;;; CRUD

(defclass table-crud (table page-interface-mixin crud-mixin)
  ())

(defclass table-normal-crud (table-normal table-crud)
  ((tbody-class :reader tbody-class :initform 'tbody-normal-crud)
   (row-class   :reader row-class   :initform 'row-normal-crud)))

(defclass table-ul-crud (table-ul table-crud)
  ((tbody-class :reader tbody-class :initform 'tbody-ul-crud)
   (row-class   :reader row-class   :initform 'row-ul-crud)))



;;; ------------------------------------------------------------
;;; Table body
;;; ------------------------------------------------------------

;;; Non-CRUD

(defclass tbody ()
  ((table :accessor table :initarg :table)
   (data  :accessor data  :initarg :data) 
   (style :accessor style :initarg :style)))

(defclass tbody-normal (tbody)
  ())

(defmethod render ((tbody tbody-normal) &key)
  (with-html
    (:tbody :style (style tbody)
            (render-tbody tbody (row-class (table tbody))))))


(defclass tbody-ul (tbody)
  ())

(defmethod render ((tbody tbody-ul) &key)
  (with-html
    (:ul :style (style tbody)
         (render-tbody tbody (row-class (table tbody))))))


(defun render-tbody (tbody row-class)
  (let ((row-style (getf (styles (table tbody)) :row)))
    (iter (for data-row in (data tbody))
          (render (make-instance row-class
                                 :tbody tbody
                                 :data data-row
                                 :style row-style)))))

;;; CRUD

(defclass tbody-normal-crud (tbody-normal)
  ())


(defmethod render ((tbody tbody-normal-crud) &key)
  (with-html
    (:tbody :style (style tbody)
            (render-tbody-crud tbody (row-class (table tbody))))))

(defclass tbody-ul-crud (tbody-ul)
  ())

(defmethod render ((tbody tbody-ul-crud) &key)
  (with-html
    (:ul :style (style tbody)
         (render-tbody-crud tbody (row-class (table tbody))))))

(defun render-tbody-crud (tbody row-class) 
  (let ((active-row-style (getf (styles (table tbody)) :active-row))
        (inactive-row-style (getf (styles (table tbody)) :inactive-row))
        (attention-row-style (getf (styles (table tbody)) :attention-row))
        (table (table tbody))) 
    (case (operation table)
      (:view 
       (iter (for data-row in (data tbody)) 
             (render (make-instance row-class
                                    :table table
                                    :style (if (active-row-p table data-row)
                                               active-row-style
                                               inactive-row-style)
                                    :data data-row))))
      (:create
       (render (make-form :submit-page (submit-page table) 
                          :body (make-instance row-class
                                               :table table
                                               :style active-row-style
                                               :data (append (id-params table)
                                                             (payload-params table))))) 
       (iter (for data-row in (data tbody))
             (render (make-instance row-class
                                    :table table
                                    :style inactive-row-style
                                    :data data-row))))
      ((:update :delete) 
       (let ((rows
              (iter (for data-row in (data tbody))
                    (let* ((activep (active-row-p table data-row))
                           (row-style (if activep
                                          (if (eql (operation table) :delete)
                                              attention-row-style
                                              active-row-style)
                                          inactive-row-style))
                           (data (if activep
                                     (plist-union (append (id-params table)
                                                          (payload-params table))
                                                  data-row)
                                     data-row)))
                      (collect (make-instance row-class
                                              :table table 
                                              :style row-style
                                              :data data)))))) 
         (render (make-form :submit-page (submit-page table)
                            :hidden (id-params table)
                            :body rows)))))))



;;; ------------------------------------------------------------
;;; Rows
;;; ------------------------------------------------------------

;;; Non-CRUD

(defclass row (widget)
  ((table :accessor table :initarg :table)
   (style :accessor style :initarg :style) 
   (data  :accessor data  :initarg :data)))

(defclass row-normal (row)
  ())

(defmethod render ((row row-normal) &key)
  (let ((cells-list (funcall (cells-fn (table row)) row)))
    (with-html
      (:tr :class (style row)
           (render cells-list)))))

(defclass row-ul (row)
  ())

(defmethod render ((row row-ul) &key)
  (let ((cells-list (funcall (cells-fn (table row)) row)))
    (with-html
      (:li :class (style row)
           (render cells-list)))))

;;; CRUD

(defclass row-crud (row)
  ())

(defclass row-normal-crud (row-normal row-crud)
  ())

(defclass row-ul-crud (row-ul row-crud)
  ())


;;; Other row utilities

;;; Compare: id-data <--> id-params
;;; and payload-data <--> payload-params

(defgeneric id-data (row-crud))

(defmethod id-data ((row row-crud))
  (plist-collect (id-keys (table row)) (data row)))

(defgeneric payload-data (row-crud))

(defmethod payload-data ((row row-crud))
  (plist-collect (payload-keys (table row)) (data row)))



;;; ------------------------------------------------------------
;;; Cells -- Non-CRUD
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
;;; Cells -- CRUD
;;; ------------------------------------------------------------

(defclass cell-crud (cell)
  ((operations :accessor operations :initarg :operations)))

(defgeneric enabledp (cell-crud))

(defmethod enabledp ((cell cell-crud))
  (let* ((row (row cell))
         (table (table row))) 
    (and (member (operation table) (operations cell))
         (active-row-p (table row) (data row)))))


;;; Selector

(defclass cell-selector (cell-crud)
  ())

(defmethod render ((cell cell-selector) &key)
  (let* ((row (row cell))
         (table (table row)))
    (flet ((selector-href (&key barep)
             (apply (main-page table)
                    (if barep
                        (filter-params table)
                        (append (id-data row) 
                                (filter-params table))))))
      (with-html 
        (:td :class (style cell)
             (if (active-row-p (table row) (data row))
                 (htm (:a :href (selector-href :barep t)
                          (htm (:img :src (url "img/bullet_red.png")))))
                 (htm (:a :href (selector-href)
                          (htm (:img :src (url "img/bullet_blue.png")))))))))))

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
        (:td :class (style cell)
             ""))))

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
                  (filter-params (table row)))))
    (if (enabledp cell)
        (with-html
          (:td :class (style cell)
               (:a :href (cancel-href (row cell))
                   (:img :src (url "img/cancel.png")))))
        (with-html
          (:td :class (style cell)
               "")))))

(defun make-cell-cancel (&key row name style operations)
  (make-instance 'cell-cancel
                 :row row
                 :name name
                 :style style
                 :operations operations))


;;; ------------------------------------------------------------
;;; Utilities 
;;; ------------------------------------------------------------

(defun active-row-p (table data)
  (set-equal (plist-collect (id-keys table) data)
             (id-params table)
             :test #'equal))



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























