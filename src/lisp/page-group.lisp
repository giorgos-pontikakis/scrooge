(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Auxiliary Functions
;;; ------------------------------------------------------------

(defun filters (params obj)
  (objects->plist params #'name (filter-keys obj)))

(defun ids (params obj)
  (objects->plist params #'name (id-keys obj)))

(defun data (params obj)
  (objects->plist params #'name (data-keys obj)))

(defun header (obj)
  (cons "" (append (data-header obj) (list "" ""))))

(defun styles (params obj)
  (plist-map (lambda (param)
               (if (or (null param) (validp param))
                   nil
                   "attention"))
             (data params obj)))

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
   (get-urls    :accessor get-urls    :initarg  :get-urls)))

(defun get-page-group (page-group-name)
  (find page-group-name *page-groups* :key #'name))

(defun define-page-group (page-group-name &rest args)
  (let ((obj (apply #'make-instance 'page-group :name page-group-name args)))
    (push obj *page-groups*)))



;;; ------------------------------------------------------------
;;; Widgets
;;; ------------------------------------------------------------

(defparameter *widgets* nil)

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
   (db-data         :accessor db-data         :initarg :dbdata)
   (renderer        :accessor renderer        :initarg :renderer)
   ;; auxiliary functions
   (html-row        :accessor html-row        :initarg :html-row)
   (simple-row      :accessor simple-row      :initarg :simple-row)
   (form-row        :accessor form-row        :initarg :form-row)
   (row-cells          :accessor row-cells          :initarg :row-cells)))

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
                   (iter (for db-data in (funcall (db-data obj) filters))
                         (funcall (html-row obj)
                                  intent
                                  :ids ids
                                  :data (if (activep ids db-data)
                                            (plist-union data db-data)
                                            db-data)
                                  :styles styles))))))))

(defmethod html-row ((obj table-inline-form))
  (with-slots (post-urls get-urls) obj
    #'(lambda (intent &key ids data styles)
        (case intent
          ((:view) (funcall (simple-row obj) 
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
                        (plist-map #'val*
                                   (plist-subset (append ids data)
                                                 (www-toolkit::keyparams (page post-fn)))))
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
                    (widget (getf data-widgets key)))
                (render widget val sty))))))

(defmethod define-db-data ((obj table-inline-form) fn)
  (setf (db-data obj) fn))


;;; ------------------------------------------------------------
;;; Table cells
;;; ------------------------------------------------------------
(defclass cell (widget)
  ((td-style :accessor td-style :initarg :td-style)))


;; String cells

(defclass cell-str (cell)
  ())

(defun make-cell-str (td-style)
  (make-instance 'cell-str :td-style td-style))

(defmethod renderer ((widget cell-str))
  (with-slots (td-style) widget
    #'(lambda (value) 
        (with-html
          (:td :class td-style
               (lisp-to-html value))))))

;; Textbox cells

(defclass cell-textbox (cell)
  ())

(defun make-cell-textbox (td-style)
  (make-instance 'cell-textbox :td-style td-style))

(defmethod renderer ((widget cell-textbox))
  (with-slots (td-style) widget
    #'(lambda (value) 
        (with-html
          (:td :class td-style
               (lisp-to-html value))))))


;; Dropdown cells

(defclass cell-dropdown (cell) (
   (pairs :accessor pairs :initarg :pairs)))

(defun make-cell-dropdown (td-style)
  (make-instance 'cell-dropdown
                 :td-style td-style))

(defmethod renderer ((widget cell-dropdown))
  (with-slots (name td-style pairs) widget
    #'(lambda (value)
        (with-html
          (:td :class td-style
               (dropdown name pairs
                         :selected value))))))

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
                                                    (plist-union data db-data)
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