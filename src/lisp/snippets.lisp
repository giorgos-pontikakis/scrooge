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

#|(:union (:select 'tx.id 'tx.title 'tx.amount 
			:from 'tx   
			:left-join 'tx-type :on (:= 'tx-type.debit-acc-id account-id))
	       (:select 'tx.id 'tx.title 'tx.amount 
			:from 'tx 'tx-type  
			:left-join 'tx-type :on (:= 'tx-type.credit-acc-id account-id)))|#
;;; --- Menus --------------------

(defun company-menu (state &optional id)
  (with-db
    (let ((transactions-exist-p (and id (query (:select 'id
							:from 'tx
							:where (:= 'tx.company-id id))))))
      (flet ((summary (&optional id)
	       (with-html
		 (:li (:a :href (companies :company-id id)
			  (:img :src (url "img/table.png")) "Πίνακας εταιριών"))))
	     (insert () 
	       (with-html
		 (:li (:a :href (company/insert)
			  (:img :src (url "img/add.png")) "Δημιουργία"))))
	     (view (id)
	       (if id
		   (with-html
		     (:li (:a :href (company/view :company-id id)
			      (:img :src (url "img/magnifier.png")) "Προβολή")))
		   nil))
	     (edit (id)
	       (if id
		   (with-html
		     (:li (:a :href (company/edit :company-id id)
			      (:img :src (url "img/pencil.png")) "Επεξεργασία")))
		   nil))
	     (transactions (id)
	       (if id
		   (with-html
		     (:li (:a :href (transactions :company-id id)
			      (:img :src (url "img/money.png")) "Συναλλαγές")))
		   nil))
	     (kill (id)
	       (if (or (null id) transactions-exist-p)
		   nil
		   (with-html
		     (:li (:a :href (company/remove :company-id id)
			      (:img :src (url "img/delete.png")) "Διαγραφή")))))) 
	(with-html
	  (:ul :class "hmenu" 
	       (ecase state
		 ;;
		 (summary
		  (with-db 
		    (insert)
		    (when id
		      (view id)
		      (transactions id)
		      (edit id)
		      (unless transactions-exist-p
			(kill id)))))
		 ;;
		 (insert
		  (summary))
		 ;;
		 (view
		  (with-db
		    (summary id)
		    (transactions id)
		    (edit id)
		    (unless transactions-exist-p
		      (kill id))))
		 ;; 
		 (edit
		  (progn (summary id)
			 (transactions id)
			 (view id) 
			 (unless transactions-exist-p
			   (kill id))))
		 ;; 
		 (kill
		  (progn (summary id)
			 (transactions id)
			 (view id)
			 (edit id))))))))))

(defun account-menu (state &optional id)
  (with-db
    (let ((children-exist-p (and id (query (:select 'id
						    :from 'account
						    :where (:= 'parent-id id)))))
	  (tx-types-exist-p (and id (query (:select 'id
						    :from 'tx-type
						    :where (:or (:= 'debit-acc-id id)
								(:= 'credit-acc-id id))))))) 
      (flet ((summary (&optional id)
	       (with-html
		 (:li (:a :href (accounts :account-id id)
			  (:img :src (url "img/table.png")) "Πίνακας λογαριασμών"))))
	     (insert ()
	       (with-html
		 (:li (:a :href (account/insert)
			  (:img :src (url "img/add.png")) "Δημιουργία"))))
	     (insert-child (id)
	       (if id
		   (with-html
		     (:li (:a :href (account/insert :account-id id)
			      (:img :src (url "img/add.png")) "Δημιουργία υπολογαριασμού")))
		   nil))
	     (view (id)
	       (if id
		   (with-html
		     (:li (:a :href (account/view :account-id id)
			      (:img :src (url "img/magnifier.png")) "Προβολή")))
		   nil))
	     (edit (id)
	       (if id
		   (with-html
		     (:li (:a :href (account/edit :account-id id)
			      (:img :src (url "img/pencil.png")) "Επεξεργασία")))
		   nil))
	     (kill (id)
	       (if (or (null id) children-exist-p tx-types-exist-p)
		   nil
		   (with-html
		     (:li (:a :href (account/remove :account-id id)
			      (:img :src (url "img/delete.png")) "Διαγραφή"))))))
	(with-html
	  (:ul :class "hmenu" 
	       (ecase state
		 ;;
		 (summary (progn 
			    (insert)
			    (view id)
			    (edit id)
			    (insert-child id)
			    (kill id)))
		 ;;
		 (insert (summary))
		 ;;
		 (view (summary id)
		       (edit id)
		       (kill id))
		 ;; 
		 (edit (progn (summary id)
			      (view id)
			      (kill id)))
		 ;; 
		 (kill (progn (summary id)
			      (view id)
			      (edit id))))))))))

(defun tx-menu (state &optional id)
  (flet ((summary (&optional id)
	   (with-html
	     (:li (:a :href (transactions :tx-id id)
		      (:img :src (url "img/table.png")) "Πίνακας συναλλαγών"))))
	 (insert () 
	   (with-html
	     (:li (:a :href (transaction/insert)
		      (:img :src (url "img/add.png")) "Δημιουργία"))))
	 (view (id)
	   (with-html
	     (:li (:a :href (transaction/view :tx-id id)
		      (:img :src (url "img/magnifier.png")) "Προβολή"))))
	 (edit (id)
	   (with-html
	     (:li (:a :href (transaction/edit :tx-id id)
		      (:img :src (url "img/pencil.png")) "Επεξεργασία"))))
	 (kill (id)
	   (with-html
	     (:li (:a :href (transaction/remove :tx-id id)
		      (:img :src (url "img/delete.png")) "Διαγραφή")))))
    (with-db
      (let ((companies-exist-p (query (:select 'id :from 'company)))
	    (tx-types-exist-p (query (:select 'id :from 'tx-type)))) 
	(with-html
	  (:ul :class "hmenu" 
	       (ecase state
		 ;;
		 (summary
		  (if (and companies-exist-p tx-types-exist-p)
		      (progn (insert)
			     (when id
			       (view id)
			       (edit id)
			       (kill id)))
		      (htm
		       (:li (:p "Αδύνατη η δημιουργία συναλλαγών"))
		       (unless companies-exist-p
			 (htm
			  (:li
			   (:p "Δεν έχουν οριστεί εταιρίες"))))
		       (unless tx-types-exist-p
			 (htm
			  (:li
			   (:p "Δεν έχουν οριστεί τύποι συναλλαγών")))))))
		 ;;
		 (insert
		  (summary))
		 ;;
		 (view
		  (progn (summary id)
			 (edit id)
			 (kill id)))
		 ;; 
		 (edit
		  (progn (summary id)
			 (view id)
			 (kill id)))
		 ;; 
		 (kill
		  (progn (summary id)
			 (view id)
			 (edit id))))))))))

(defun tx-type-menu (state &optional id)
  (with-db
    (let ((accounts-exist-p (query (:select 'id :from 'account)))
	  (tx-exist-p (and id (query (:select 'id :from 'tx :where (:= id 'tx.tx-type-id))))))
      (flet ((summary (&optional id)
	       (with-html
		 (:li (:a :href (transaction-types :tx-type-id id)
			  (:img :src (url "img/table.png")) "Πίνακας τύπων συναλλαγών"))))
	     (insert () 
	       (with-html
		 (:li (:a :href (transaction-type/insert)
			  (:img :src (url "img/add.png")) "Δημιουργία"))))
	     (view (id)
	       (if id
		   (with-html
		     (:li (:a :href (transaction-type/view :tx-type-id id)
			      (:img :src (url "img/magnifier.png")) "Προβολή")))
		   nil))
	     (edit (id)
	       (if id
		   (with-html
		     (:li (:a :href (transaction-type/edit :tx-type-id id)
			      (:img :src (url "img/pencil.png")) "Επεξεργασία")))
		   nil))
	     (kill (id)
	       (if tx-exist-p
		   nil
		   (with-html
		     (:li (:a :href (transaction-type/remove :tx-type-id id)
			      (:img :src (url "img/delete.png")) "Διαγραφή")))))) 
	(with-html
	  (:ul :class "hmenu" 
	       (ecase state
		 ;;
		 (summary
		  (if accounts-exist-p
		      (progn (insert)
			     (when id
			       (view id)
			       (edit id)
			       (kill id)))
		      (htm (:li (:p "Αδύνατη η δημιουργία τύπων συναλλαγών"))
			   (:li (:p "Δεν έχουν οριστεί λογαριασμοί")))))
		 ;;
		 (insert
		  (summary))
		 ;;
		 (view
		  (progn (summary id)
			 (edit id)
			 (kill id)))
		 ;; 
		 (edit
		  (progn (summary id)
			 (view id)
			 (kill id)))
		 ;; 
		 (kill
		  (progn (summary id)
			 (view id)
			 (edit id))))))))))