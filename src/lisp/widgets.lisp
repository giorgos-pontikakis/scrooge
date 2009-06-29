(in-package :scrooge)



;; (defmacro with-ul ((&key id style) &body li-list)
;;   `(with-html
;;      (:ul :id ,id
;; 	  :class ,style
;; 	  ,@(iter (for option in li-list)
;; 		  (collect `(:li (str (lisp-to-html ,option))))))))

(defmacro with-ul ((&key id style) &body li-list)
  `(with-html
     (:ul :id ,id
	  :class ,style
	  ,@(iter (for option in li-list)
		  (collect `(:li (with-html
				   ,option)))))))

(defun link (label &key href style)
  (with-html
    (if href
	(htm (:a :class (getf style :a) :href href (str (lisp-to-html label))))
	(htm (:p :class (getf style :p) (str (lisp-to-html label)))))))


(defmacro with-table ((&key caption header id style)
		      &body body)
  `(with-html
     (:table :id ,id :class ,style
	     ,(when caption
		    `(:caption (str ,caption)))
	     ,(when header
		    `(:thead (:tr (iter (for label in ,header)
					(htm (:td (str label)))))))
	     ,(if body
		  `(:tbody 
		    ,@(iter (for row in body)
			    (let ((active-p (getf row :active-p)))
			      (remf row :active-p)
			      (collect `(:tr :class ,(if active-p "active" "inactive")
					     ,@(iter (for datum in row)
						     (collect `(:td (with-html ,datum)))))))))
		  `(:tbody (:tr (:td "No available data")))))))


;;; Tables
(defun table (table-data &key id-fn td-fn caption header href-fn active-id id style)
  (with-html 
    (:table :id id :class style
	    (when caption
	      (htm (:caption (str caption))))
	    (when header
	      (htm (:thead (:tr (iter (for label in header)
				      (htm (:td (str label))))))))
	    (if table-data
		(htm (:tbody
		      (iter (for row in table-data)
			    (for row-id = (if id-fn (funcall id-fn row) nil))
			    (for row-td = (if td-fn (funcall td-fn row) row))
			    (for href = (if href-fn (funcall href-fn row-id) nil))
			    (for pred = (equal active-id row-id))
			    (htm (:tr :class (if pred "active" "inactive")
				      (iter (for datum in row-td)
					    (if pred
						(htm (:td (:p (str (lisp-to-html datum)))))
						(htm (:td (:a :href href
							      (str (lisp-to-html datum))))))))))))
		(htm (:tbody (:tr (:td "No available data"))))))))



