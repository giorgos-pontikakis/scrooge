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
  ((name      :accessor name      :initarg :name)))

(defgeneric render (widget &key)
  (:documentation "A function that produces html from the widget object"))

(defun find-widget (widget-name)
  (gethash widget-name *widgets*)) 

(defun register-widget (widget-name widget-class &rest args)
  (let ((widget (apply #'make-instance widget-class :name widget-name args)))
    (setf (gethash widget-name *widgets*) widget)
    widget))



;;; ------------------------------------------------------------
;;; Table-inline-form class
;;; ------------------------------------------------------------

(defclass table-inline-form (widget)
  (;; columns
   (cols              :accessor cols              :initarg :cols) 
   (id-cols           :accessor id-cols           :initarg :id-cols)
   (data-cols         :accessor data-cols         :initarg :data-cols) 
   (filter-keys       :accessor filter-keys       :initarg :filter-keys) 
   ;; urls
   (post-urls         :accessor post-urls         :initarg :post-urls)
   (get-urls          :accessor get-urls          :initarg :get-urls)
   ;; display
   (header            :accessor header            :initarg :header)
   (styles            :accessor styles            :initarg :styles)))

(defun make-table-inline-form (id-cols data-cols filter-keys post-urls get-urls header styles)
  (make-instance 'table-inline-form
                 :id-cols id-cols		
                 :data-cols data-cols
                 :filter-keys filter-keys
                 :post-urls post-urls	 
                 :get-urls get-urls		    
                 :header header	 
                 :styles styles))

(defmethod render ((obj table-inline-form) &key intent params)
  (flet ((row-id (row-db-data)
           (collectf (id-cols obj) row-db-data))
         (row-data (row-db-data)
           (collectf (data-cols obj) row-db-data)))
    (let* ((filters (params->plist params (filter-keys obj))) 
           (db-table (read-db obj :filters filters))) 
      (with-html
        (:table :id (conc (string-downcase (symbol-name (name obj))) "-table")
                :class (getf (styles obj) :table)
                (:thead
                 (:tr (dof (lambda (key label)
                             (declare (ignore key))
                             (htm (:th (str label))))
                           (header obj))))
                (:tbody 
                 (case intent
                   (:view 
                    (iter (for row-db-data in db-table) 
                          (simple-row obj
                                      (row-id row-db-data)
                                      (row-data row-db-data)
                                      intent
                                      params)))
                   (:create 
                    (form-row obj
                              (mapvalf #'val*
                                       (params->plist (id-cols obj) params))
                              (mapvalf #'val*
                                       (params->plist (data-cols obj) params))
                              intent
                              params) 
                    (iter (for row-db-data in db-table) 
                          (simple-row obj
                                      (row-id row-db-data)
                                      (row-data row-db-data)
                                      intent
                                      params)))
                   ((:update :delete)
                    (iter (for row-db-data in db-table)
                          (let ((row-id (row-id row-db-data))
                                (row-data (unionf (mapvalf #'val*
                                                           (params->plist (data-cols obj)
                                                                          params))
                                                  (collectf (data-cols obj) row-db-data))))
                            (if (active-row-p obj row-id params)
                                (form-row obj
                                          row-id
                                          row-data
                                          intent
                                          params) 
                                (simple-row obj
                                            row-id
                                            row-db-data
                                            intent
                                            params))))))))))))

(defgeneric read-db (table-inline-form &key filters))

;; Simple rows

(defgeneric simple-row (table row-id row-data intent params))

(defmethod simple-row ((obj table-inline-form) row-id row-data intent params)
  (let ((activep (active-row-p obj row-id params))
        (filters (params->plist (filter-keys obj) params))
        (viewfn (getf (get-urls obj) :view))) 
    (with-html
      (:tr :class (if activep "active" nil)
           (cell-selector obj
                          :col :id
                          :href (if activep
                                    (funcall viewfn)
                                    (apply viewfn (append row-id filters)))
                          :activep activep)
           (dof (lambda (col value) 
                  (cell-text obj
                             :col col
                             :value value))
                row-data)
           (:td :class "button" "")
           (:td :class "button" "")))))

;; Forms

(defgeneric form-row (table row-id row-data intent params))

(defmethod form-row ((obj table-inline-form) row-id row-data intent params) 
  (with-db
    (let ((actionfn (getf (post-urls obj) intent))
          (viewfn (getf (get-urls obj) :view)) 
          (activep (active-row-p obj row-id params))
          (filters (params->plist params (filter-keys obj))))
      (make-form (funcall actionfn)
                 (html ()
                   (:tr :style (if activep "active" nil)
                        (cell-selector obj
                                       :col :select
                                       :href (if activep
                                                 (funcall viewfn)
                                                 (apply viewfn (append row-id filters)))
                                       :activep activep)
                        (mapc (lambda (col value param)
                                (cell-textbox obj
                                              :col col
                                              :value value
                                              :style (if (validp param) nil "attention")))
                              (data-cols obj) row-data (params->plist (data-cols obj) params))
                        (cell-submit obj :col :submit)
                        (cell-anchor obj
                                     :col :cancel
                                     :href (funcall viewfn row-id filters)))))))) 




;;; ------------------------------------------------------------
;;; Table cells
;;; ------------------------------------------------------------

;; Utilities

(defgeneric active-row-p (table-inline-form row-id params))

(defmethod active-row-p ((obj table-inline-form) row-id params)
  (set-equal row-id
             (mapvalf #'val* (params->plist (id-cols obj) params))
             :test #'equal))

;; Selector cells

(defgeneric cell-style (container &key col)
  (:documentation "Get the CSS style of a cell."))

(defmethod cell-style ((obj table-inline-form) &key col)
  (getf (getf (styles obj) :cols) col))


;; Selector cells

(defgeneric cell-selector (container &key col)
  (:documentation "Render the cell of a table"))

(defmethod cell-selector ((obj table-inline-form) &key col href activep)
  (with-html
    (:td :class (cell-style obj :col col)
         (if activep 
             (htm (:a :href href
                      (htm (:img :src (url "img/bullet_red.png")))))
             (htm (:a :href href
                      (htm (:img :src (url "img/bullet_blue.png")))))))))


;; Text cells

(defgeneric cell-text (container &key col)
  (:documentation "Render a text-only cell."))

(defmethod cell-text ((obj table-inline-form) &key col value) 
  (with-html
    (:td :class (cell-style obj :col col)
         (str (lisp-to-html value)))))


;; Textbox cells

(defgeneric cell-textbox (container &key col)
  (:documentation "Render a cell with a text input box."))

(defmethod cell-textbox ((obj table-inline-form) &key col value style)
  (with-html
    (:td :class (cell-style obj :col col)
         (textbox col
                  :value value
                  :style style))))  


;; Dropdown cells

(defgeneric cell-dropdown (container &key col)
  (:documentation "Render a cell with a dropdown menu."))

(defmethod cell-dropdown ((obj table-inline-form) &key col value pairs)
  (with-html
    (:td :class (cell-style obj :col col)
         (dropdown col pairs
                   :selected value))))


;; Submit cells

(defgeneric cell-submit (container &key col)
  (:documentation "Render a cell with a submit button."))

(defmethod cell-submit ((obj table-inline-form) &key col)
  (with-html
      (:td :class (cell-style obj :col col)
           (:button :type "submit"
                    (:img :src (url "img/tick.png"))))))


;; Anchor cells

(defgeneric cell-anchor (container &key col)
  (:documentation "Render a cell with a dropdown menu."))

(defmethod cell-anchor ((obj table-inline-form) &key col href)
  (with-html
      (:td :class (cell-style obj :col col)
           (:a :href href
               (:img :src (url "img/cancel.png"))))))



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






;; (defmethod render ((cell cell-dropdown) &key id ids values pairs cell-style)
;;   (with-html
;;     (:td :class cell-style
;;          (dropdown id
;;                    (pairs cell)
;;                    :selected value))))

;; (defgeneric simple-row ())


;; (defmethod simple-row ((obj table-inline-form) get-url-fn ids data styles)
;;   (let ((activep (activep ids data)))
;;     (with-html
;;       (:tr :class (if activep "active" nil)
;;            (selector-td activep (if activep
;;                                     (funcall get-url-fn)
;;                                     (apply get-url-fn ids)))
;;            (row-cells obj data styles)
;;            (:td :class "button" "")
;;            (:td :class "button" "")))))

;; (defmethod form-row ((obj table-inline-form) intent get-url-fn post-url-fn ids data styles)
;;   (make-form (apply post-url-fn
;;                     (mapvalf #'val
;;                           (collectf (keyparams (page post-url-fn)) ids))) 
;;              (html () 
;;                (:tr :class (if (eql intent :delete) "attention" "active")
;;                     (selector-td t (funcall get-url-fn))
;;                     (row-cells obj data styles)
;;                     (:td :class "button" (ok-button))
;;                     (:td :class "button" (cancel-button (funcall get-url-fn)))))))


;; (defgeneric render (widget &rest args))

;; (defmethod render ((obj widget) &rest args)
;;   (apply (renderer obj) args))

;; (defmethod db-get ((obj widget) &key filters)
;;   (funcall (db-getter obj) filters))



;;; ------------------------------------------------------------
;;; Table with inline form
;;; ------------------------------------------------------------

;; (defmethod header ((obj table-inline-form))
;;   (cons "" (append (data-header obj) (list "" ""))))

;; (defmethod styles (params (obj table-inline-form))
;;   (mapvalf (lambda (param)
;;                (if (or (null param) (validp param))
;;                    nil
;;                    "attention"))
;;              (data params obj)))

;; (defmethod widgets (params (obj table-inline-form))
;;   (funcall (getf data-widgets param)))

;; (lambda (data-param)
;;   (getf data-widgets (name )))

;; (defmethod render ((obj table-inline-form) &key intent params)
;;   (let ((header (header obj))
;;         (filters (filters params obj))
;;         (ids (ids params obj)) 
;;         (data (data params obj)) 
;;         (styles (styles params obj))
;;         (widgets (widgets params obj))) 
;;     (with-html
;;       (:table :id (concatenate 'string
;;                                (string-downcase (symbol-name (name obj)))
;;                                "-table")
;;               :class (getf (table-styles obj) :table-style)
;;               (:thead
;;                (:tr (iter (for label in header) 
;;                           (htm (:th (str label))))))
;;               (:tbody
;;                (when (eql intent :create) 
;;                  (html-row obj
;;                            :intent intent
;;                            :ids ids
;;                            :data data
;;                            :styles styles
;;                            :widgets ))
;;                (iter (for db-data in (funcall (db-getter obj) filters)) 
;;                      (html-row obj
;;                                :intent intent
;;                                :ids ids
;;                                :data (if (activep ids db-data)
;;                                          (unionf data db-data)
;;                                          db-data)
;;                                :styles styles)))))))


;; (defmethod row-cells ((obj table-inline-form) data styles)
;;   (iter (for key in (data-keys obj))
;;         (let ((val (val* (getf data key)))
;;               (sty (getf styles key)) 
;;               (widget (let ((w (find (data-widgets obj) :key #'name)))
;;                         (if (functionp w)
;;                             (funcall w )))))
;;           (render widget
;;                   :value val
;;                   :style style
;;                   :allow-other-keys t))))




