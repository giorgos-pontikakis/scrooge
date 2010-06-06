(in-package :scrooge)


(defmacro with-ul ((&key id style) &body li-list)
  `(with-html
     (:ul :id ,id
	  :class ,style
	  ,@(iter (for option in li-list)
		  (collect `(:li (str (lisp-to-html ,option))))))))

;; (defmacro with-button ((&key name (value :null) disabledp style) &body body)
;;   (if name
;;       `(with-html
;; 	 (:button :class ,style
;; 		  :type "submit"
;; 		  :name (string-downcase ,name)
;; 		  :value (lisp-to-html ,value)
;; 		  :disabled ,disabledp
;; 		  ,@body))
;;       `(with-html
;; 	 (:button :class ,style
;; 		  :type "submit" 
;; 		  :disabled ,disabledp
;; 		  ,@body))))

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