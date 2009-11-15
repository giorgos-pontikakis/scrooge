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



