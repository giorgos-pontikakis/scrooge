(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

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


;;; Tables
;; (defmacro with-table2 ((&key caption header id style) (&rest columns) &body body)
;;   `(with-html
;;      (:table :id ,id :class ,style
;; 	     ,(when caption
;; 		    `(:caption (str (lisp-to-html ,caption)))) 
;; 	     ,(when header
;; 		    `(:thead (:tr (iter (for label in ,header)
;; 					(htm (:td (str (lisp-to-html label))))))))
;; 	     ,(if body
;; 		  `(:tbody 
;; 		    ,@(iter (for row in body) 
;; 			    (collect `(:tr
;; 				       ,@(iter (for datum in row)
;; 					       (for col in (or columns (make-list (length row))))
;; 					       (if (and (listp datum) (keywordp (first datum)))
;; 						   (collect `(:td ,datum))
;; 						   (collect `(:td :class ,col
;; 								  (str (lisp-to-html ,datum))))))))))
;; 		  `(:tbody (:tr (:td "No available data")))))))



;;; ------------------------------------------------------------
;;; Widgets superclass
;;; ------------------------------------------------------------

;; (defvar *widgets* (make-hash-table))

;; (defclass widget ()
;;   ((name :accessor name :initarg :name)))

;; (defun find-widget (widget-name)
;;   (gethash widget-name *widgets*)) 

;; (defun register-widget (widget-name widget-class &rest args)
;;   (let ((widget (apply #'make-instance widget-class :name widget-name args)))
;;     (setf (gethash widget-name *widgets*) widget)
;;     widget))

;; (defgeneric style (widget))

;; (defgeneric render (thing &key)
;;   (:documentation "A function that produces html from the widget object"))

;; (defmethod render ((fn function) &key) 
;;   (funcall fn))

;; (defmethod render ((value t) &key) 
;;   (with-html
;;     (str (lisp-to-html value))))

;; (defmethod render ((list list) &key) 
;;   (mapc #'render list))



;;; ------------------------------------------------------------
;;; Forms
;;; ------------------------------------------------------------

(defhtml form (submit-page hidden body)
  (let ((page (find-page submit-page)))
    (with-html
      (:form :method (request-type page)
             :action (base-url page)
             (iter (for key in hidden by #'cddr)
                   (for val in (rest hidden) by #'cddr)
                   (with-html
                     (:input :type "hidden"
                             :id (string-downcase key)
                             :class "display-none"
                             :name (string-downcase key)
                             :value (lisp->html val))))
             (funcall body)))))



;; ;;; ------------------------------------------------------------
;; ;;; Page interface mixin
;; ;;; ------------------------------------------------------------

;; (defclass page-interface-mixin ()
;;   ((params       :accessor params       :initarg :params)
;;    (id-keys      :accessor id-keys      :initarg :id-keys)
;;    (payload-keys :accessor payload-keys :initarg :payload-keys) 
;;    (filter-keys  :accessor filter-keys  :initarg :filter-keys)
;;    (aux-keys     :accessor aux-keys     :initarg :aux-keys)))

;; (defun params->plist (bag params) 
;;   (mapcan (lambda (key)
;;             (let ((par (find key params :key #'name)))
;;               (list key par)))
;;           bag))

;; (defgeneric id-params (obj))

;; (defmethod id-params ((obj page-interface-mixin))
;;   (plist-map-vals #'val*
;;                   (params->plist (id-keys obj) (params obj))))


;; (defgeneric payload-params (obj))

;; (defmethod payload-params ((obj page-interface-mixin))
;;   (plist-map-vals #'val*
;;                   (params->plist (payload-keys obj) (params obj))))


;; (defgeneric filter-params (obj))

;; (defmethod filter-params ((obj page-interface-mixin))
;;   (plist-map-vals #'val*
;;                   (params->plist (filter-keys obj) (params obj))))

;; (defgeneric aux-params (obj))

;; (defmethod aux-params ((obj page-interface-mixin))
;;   (plist-map-vals #'val*
;;                   (params->plist (aux-keys obj) (params obj))))


;; ;;; ------------------------------------------------------------
;; ;;; CRUD mixin  
;; ;;; ------------------------------------------------------------

;; (defclass crud-mixin ()
;;   ((operation    :accessor operation    :initarg :operation)
;;    (main-page    :accessor main-page    :initarg :main-page) 
;;    (submit-pages :accessor submit-pages :initarg :submit-pages)))


;; (defgeneric submit-page (obj))

;; (defmethod submit-page ((obj crud-mixin))
;;   (getf (submit-pages obj) (operation obj)))


;; ;;; ------------------------------------------------------------
;; ;;; Tables 
;; ;;; ------------------------------------------------------------

;; (defclass table (widget)
;;   ((header      :accessor header      :initarg :header)
;;    (styles      :accessor styles      :initarg :styles) 
;;    (data-fn     :accessor data-fn     :initarg :data-fn) 
;;    (cells-fn    :accessor cells-fn    :initarg :cells-fn)
;;    (tbody-class :accessor tbody-class :initform 'tbody)))

;; (defclass table-crud (table page-interface-mixin crud-mixin)
;;   (tbody-class :accessor tbody-class :initform 'tbody-crud))

;; (defmethod render ((table table) &key)
;;   (with-html
;;     (:table :id (name table)
;;             :class (getf (styles table) :table)
;;             ;; header
;;             (:thead 
;;              (:tr (plist-do (lambda (key label) 
;;                               (htm (:th :class (getf (getf (styles table) :header) key)
;;                                         (str label))))
;;                             (header table))))
;;             ;; body 
;;             (render (make-instance (tbody-class table)
;;                                    :table table
;;                                    :style (getf (styles table) :tbody)
;;                                    :data (apply (data-fn table)
;;                                                 (filter-params table)))))))

;; ;;; Table body

;; (defclass tbody ()
;;   ((table :accessor table :initarg :table)
;;    (data  :accessor data  :initarg :data) 
;;    (style :accessor style :initarg :style)))

;; (defmethod render ((tbody tbody) &key)
;;   (let ((row-style (getf (styles (table tbody)) :inactive-row)))
;;     (iter (for data-row in (data tbody))
;;           (render (make-instance row-class
;;                                  :table (table tbody)
;;                                  :data data-row
;;                                  :style row-style)))))

;; (defclass tbody-crud ()
;;   ())

;; (defmethod render ((tbody tbody-crud) &key)
;;   (let ((active-row-style (getf (styles (table tbody)) :active-row))
;;         (inactive-row-style (getf (styles (table tbody)) :inactive-row))
;;         (attention-row-style (getf (styles (table tbody)) :attention-row))
;;         (table (table tbody))) 
;;     (case (operation table)
;;       (:view 
;;        (iter (for data-row in (data tbody)) 
;;              (render (make-instance 'row-crud
;;                                     :table table
;;                                     :style (if (active-row-p table data-row)
;;                                                active-row-style
;;                                                inactive-row-style)
;;                                     :data data-row))))
;;       (:create
;;        (let ((rows (cons (make-instance 'row-crud
;;                                         :table table
;;                                         :style active-row-style
;;                                         :data (append (id-params table)
;;                                                       (payload-params table)))
;;                          (iter (for data-row in (data tbody)) 
;;                                (collect (make-instance 'row-crud
;;                                                        :table table
;;                                                        :style inactive-row-style
;;                                                        :data data-row))))))
;;          (render (make-form :submit-page (submit-page table)
;;                             :hidden (append (id-params table)
;;                                             (filter-params table)
;;                                             (aux-params table))
;;                             :body rows))))
;;       ((:update :delete) 
;;        (let ((rows
;;               (iter (for data-row in (data tbody))
;;                     (let* ((activep (active-row-p table data-row))
;;                            (row-style (if activep
;;                                           (if (eql (operation table) :delete)
;;                                               attention-row-style
;;                                               active-row-style)
;;                                           inactive-row-style))
;;                            (data (if activep
;;                                      (plist-union (append (id-params table)
;;                                                           (payload-params table))
;;                                                   data-row)
;;                                      data-row)))
;;                       (collect (make-instance 'row-crud
;;                                               :table table 
;;                                               :style row-style
;;                                               :data data)))))) 
;;          (render (make-form :submit-page (submit-page table) 
;;                             :hidden (append (id-params table)
;;                                             (filter-params table)
;;                                             (aux-params table))
;;                             :body rows)))))))

;; ;;; Rows

;; (defclass row (widget)
;;   ((table :accessor table :initarg :table)
;;    (data  :accessor data  :initarg :data) 
;;    (style :accessor style :initarg :style)))

;; (defmethod render ((row row) &key)
;;   (let ((cells-list (funcall (cells-fn (table row)) row)))
;;     (with-html
;;       (:tr :class (style row)
;;            (dolist (c cells-list)
;;              (htm (:td (render c))))))))

;; (defclass row-crud (row)
;;   ())

;; (defgeneric id-data (row-crud))

;; (defmethod id-data ((row row-crud))
;;   (plist-collect (id-keys (table row)) (data row)))

;; (defgeneric payload-data (row-crud))

;; (defmethod payload-data ((row row-crud))
;;   (plist-collect (payload-keys (table row)) (data row)))




;; ;;; ------------------------------------------------------------
;; ;;; Trees
;; ;;; ------------------------------------------------------------

;; (defclass tree (table)
;;   ((styles      :accessor styles      :initarg :styles) 
;;    (data-fn     :accessor data-fn     :initarg :data-fn) 
;;    (cells-fn    :accessor cells-fn    :initarg :cells-fn)))

;; (defclass tree-crud (tree page-interface-mixin crud-mixin)
;;   ())

;; (defmethod render ((tree tree) &key) 
;;   (with-html
;;     (:div :id (name tree)
;;           :class (getf (styles tree) :tree) 
;;           ;; body
;;           (render (make-instance (tbody-class table)
;;                                  :table table
;;                                  :style (getf (styles table) :tbody)
;;                                  :data (apply (data-fn table)
;;                                               (filter-params table)))))))




;; ;;; ------------------------------------------------------------
;; ;;; Table body
;; ;;; ------------------------------------------------------------

;; ;;; Non-CRUD

;; (defclass tbody ()
;;   ((table :accessor table :initarg :table)
;;    (data  :accessor data  :initarg :data) 
;;    (style :accessor style :initarg :style)))

;; (defclass tbody-normal (tbody)
;;   ())

;; (defmethod render ((tbody tbody-normal) &key)
;;   (with-html
;;     ))


;; (defclass tbody-ul (tbody)
;;   ())

;; (defmethod render ((tbody tbody-ul) &key)
;;   (with-html
;;     (:ul :class (style tbody)
;;          (render-tbody tbody (row-class (table tbody))))))




;; ;;; CRUD

;; (defclass tbody-normal-crud (tbody-normal)
;;   ())


;; (defmethod render ((tbody tbody-normal-crud) &key)
;;   (with-html
;;     (:tbody :style (style tbody)
;;             (render-tbody-crud tbody (row-class (table tbody))))))

;; (defclass tbody-ul-crud (tbody-ul)
;;   ())

;; (defmethod render ((tbody tbody-ul-crud) &key)
;;   (with-html
;;     (:ul :style (style tbody)
;;          (render-tbody-crud tbody (row-class (table tbody))))))





;; ;;; ------------------------------------------------------------
;; ;;; Rows
;; ;;; ------------------------------------------------------------

;; ;;; Non-CRUD

;; (defclass row (widget)
;;   ((table :accessor table :initarg :table)
;;    (style :accessor style :initarg :style) 
;;    (data  :accessor data  :initarg :data)))

;; (defclass row-normal (row)
;;   ())

;; (defmethod render ((row row-normal) &key)
;;   (let ((cells-list (funcall (cells-fn (table row)) row)))
;;     (with-html
;;       (:tr :class (style row)
;;            (render cells-list)))))

;; (defclass row-ul (row)
;;   ())

;; (defmethod render ((row row-ul) &key)
;;   (let ((cells-list (funcall (cells-fn (table row)) row)))
;;     (with-html
;;       (:li :class (style row)
;;            (render cells-list)))))

;; ;;; CRUD

;; (defclass row-crud (row)
;;   ())

;; (defclass row-normal-crud (row-normal row-crud)
;;   ())

;; (defclass row-ul-crud (row-ul row-crud)
;;   ())


;; ;;; Other row utilities

;; ;;; Compare: id-data <--> id-params
;; ;;; and payload-data <--> payload-params

;; (defgeneric id-data (row-crud))

;; (defmethod id-data ((row row-crud))
;;   (plist-collect (id-keys (table row)) (data row)))

;; (defgeneric payload-data (row-crud))

;; (defmethod payload-data ((row row-crud))
;;   (plist-collect (payload-keys (table row)) (data row)))



;; ;;; ------------------------------------------------------------
;; ;;; Cells -- Non-CRUD
;; ;;; ------------------------------------------------------------

;; (defclass cell (widget)
;;   ((style :accessor style :initarg :style)
;;    (value :accessor value :initarg :value)))

;; (defmethod render ((cell cell) &key)
;;   (with-html
;;     (str (lisp-to-html (value cell)))))



;; ;;; ------------------------------------------------------------
;; ;;; Cells -- CRUD
;; ;;; ------------------------------------------------------------

;; (defclass cell-crud (cell) 
;;   ((enabledp :accessor enabledp :initarg :enabledp)))


;; ;;; Selector

;; (defclass cell-selector (cell-crud)
;;   ((href-enabled  :accessor href-enabled  :initarg :href-enabled)
;;    (href-disabled :accessor href-disabled :initarg :href-disabled)))

;; (defmethod render ((cell cell-selector) &key)
;;   (with-html 
;;     (if (enabledp cell)
;;         (htm (:a :href href-enabled
;;                  (htm (:img :src (url "img/bullet_red.png")))))
;;         (htm (:a :href href-disabled
;;                  (htm (:img :src (url "img/bullet_blue.png"))))))))

;; (defmethod selector-href ((row row-crud))
;;   (let ((table (table row)))
;;     (apply (main-page table)
;;            (if barep
;;                (filter-params table)
;;                (append (id-data row) (filter-params table))))))

;; (defmethod input-box-style ((row row-crud))
;;   (let ((p (find (name cell) (params (table (row cell))) :key #'name)))
;;     (if (or (null p) (validp p)) "" "attention")))



;; ;;; Textbox

;; (labels ((param (cell)
;;            )
;;          (box-style ()
;;            )))

;; (defclass cell-textbox (cell-crud)
;;   ())

;; (defmethod render ((cell cell-textbox) &key) 
;;   (if (enabledp cell) 
;;       (with-html
;;         (textbox (name cell) 
;;                  :value (value cell)
;;                  :style (style cell)))
;;       (call-next-method)))



;; ;;; Dropdown

;; (defclass cell-dropdown (cell-crud)
;;   ((pairs :accessor pairs :initarg :pairs)))

;; (defmethod render ((cell cell-dropdown) &key)
;;   (if (enabledp cell)
;;       (with-html
;;         (dropdown (name cell)
;;                   (pairs cell)
;;                   :style style
;;                   :selected (value cell)))
;;       (call-next-method)))


;; ;; Submit

;; (defclass cell-submit (cell-crud)
;;   ())

;; (defmethod render ((cell cell-submit) &key)
;;   (if (enabledp cell)
;;       (with-html
;;         (:button :type "submit"
;;                  (:img :src (url "img/tick.png"))))
;;       (with-html
;;         "")))


;; ;; Cancel

;; (defclass cell-cancel (cell-crud)
;;   ((href :accessor href :initarg :href)))

;; (defmethod render ((cell cell-cancel) &key) 
;;   (if (enabledp cell)
;;       (with-html
;;         (:a :href (href cell)
;;             (:img :src (url "img/cancel.png"))))
;;       (with-html
;;         "")))



;; ;;; ------------------------------------------------------------
;; ;;; Utilities 
;; ;;; ------------------------------------------------------------

;; (defun active-row-p (table data)
;;   (set-equal (plist-collect (id-keys table) data)
;;              (id-params table)
;;              :test #'equal))




;; (defmacro filter (action id filter disabledp)
;;   `(with-form (,action :id ,id)
;;      (with-html
;;        (:p "Φίλτρο: " (textbox 'filter :value ,filter :disabledp ,disabledp) (ok-button)))))

