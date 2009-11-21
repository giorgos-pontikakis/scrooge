(in-package :scrooge)

(defun css (&rest files)
  (with-html
    (mapc (lambda (file)
	    (htm (:link :href (url "css/" file) :rel "stylesheet" :type "text/css")))
	  files)))

(defun logo ()
  (with-html
    (:h1 "Scrooge")))

(defun navbar (active-label)
  (with-html
    (:ul :id "navbar"
	 (iter (for (label href) in `(("Αρχική" ,(home))
				      ("Ρυθμίσεις" ,(config))
				      ("Εταιρίες" ,(companies))
				      ("Λογαριασμοί" ,(accounts))
				      ("Τύποι συναλλαγών" ,(transaction-types))
				      ("Συναλλαγές" ,(transactions)))) 
	       (htm (:li (:a :href href
			     :class (if (string-equal label active-label) "active" nil)
			     (str label))))))))

(defun footer ()
  (with-html
    (:div :id "footer" "Powered by lisp")))

(defun js-headers ()
  (insert-js-headers "lib/jquery/jquery-1.3.2.min.js"
	      "lib/jquery.autocomplete/jquery.autocomplete.pack.js"
	      "js/scrooge.js")
  (insert-css-headers "lib/jquery.autocomplete/jquery.autocomplete.css"))

(defun insert-css-headers (&rest files)
  (with-html
    (mapc (lambda (file)
	    (htm (:link :href (url file) :rel "stylesheet" :type "text/css")))
	  files)))

(defun insert-js-headers (&rest js-pathnames)
  (mapc #'(lambda (pathname)
	    (with-html (:script :type "text/javascript"
				:src (url pathname))))
	js-pathnames))

(defun see-other (target)
  (redirect target :code +http-see-other+))

;;; --- Queries --------------------

(defgeneric get-transactions (filter &optional id))

(defmethod get-transactions ((filter (eql 'company)) &optional id)
  (with-db
    (query (:select 'tx.id 'tx.tx-date 'company.title 'company-id
		    'tx-type.title 'tx-type.id
		    'tx.title 'tx.amount
		    :from 'tx 'company 'tx-type
		    :where (:and (:= 'company.id 'tx.company-id)
				 (:= 'company.id id)
				 (:= 'tx.tx-type-id 'tx-type.id))))))

;; (defmethod get-transactions ((filter (eql 'all)) &optional id)
;;   (declare (ignore id))
;;   (with-db
;;     (mapcar (lambda (row)
;; 	      (apply #'make-tx-view row)) 
;; 	    (query (:select 'tx.id 'tx.tx-date 'company.title 'company-id
;; 			    'tx-type.title 'tx-type.id
;; 			    'tx.title 'tx.amount
;; 			    :from 'tx 'company 'tx-type
;; 			    :where (:and (:= 'company.id 'tx.company-id)
;; 					 (:= 'tx.tx-type-id 'tx-type.id)))))))


(defmethod get-transactions ((filter (eql 'all)) &optional id)
  (declare (ignore id))
  (with-db
    (query (:select 'tx.id 'tx.tx-date 'company.title 'company-id
		    'tx-type.title 'tx-type.id
		    'tx.title 'tx.amount
		    :from 'tx 'company 'tx-type
		    :where (:and (:= 'company.id 'tx.company-id)
				 (:= 'tx.tx-type-id 'tx-type.id))))))

(defmethod get-transactions ((filter (eql 'account)) &optional id)
  (with-db
    (query
     (:order-by
      (:union (:select 'tx.id 'tx.tx-date 'company.title 'tx.title 'tx.amount :null
		       :from 'tx
		       :inner-join 'tx-type :on (:and (:= 'tx.tx-type-id 'tx-type.id)
						      (:= 'tx-type.debit-acc-id id))
		       :inner-join 'company :on (:= 'company.id 'tx.company-id))
	      (:select 'tx.id 'tx.tx-date 'company.title 'tx.title :null 'tx.amount
		       :from 'tx
		       :inner-join 'tx-type :on (:and (:= 'tx.tx-type-id 'tx-type.id)
						      (:= 'tx-type.credit-acc-id id))
		       :inner-join 'company :on (:= 'company.id 'tx.company-id)))
      'tx-date))))


