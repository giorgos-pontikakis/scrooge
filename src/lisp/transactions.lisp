(in-package :scrooge)


;;; --- Actions --------------------

(define-dynamic-page insert-tx ((tx-date date)
				(company-title string #'valid-company-title-p)
				(tx-type-title string #'valid-tx-type-title-p)
				(title string)
				(amount integer #'positive-p))
    ("actions/transaction/insert" :request-type :post)
  (with-auth "root"
    (with-error-collection (errors) 
      (if errors
	  (redirect (transaction/insert :tx-date (or tx-date (getf errors 'tx-date))
					:company-title (or company-title (getf errors 'company-title))
					:tx-type-title (or tx-type-title (getf errors 'tx-type-title))
					:title title
					:amount (or amount (getf errors 'amount))))
	  (with-db
	    (let ((company-id (query (:select 'id :from 'company :where (:= 'title company-title))
				     :single))
		  (tx-type-id (query (:select 'id :from 'tx-type :where (:= 'title tx-type-title))
				     :single)))
	      (create-tx title tx-date amount tx-type-id company-id)
	      (redirect (transactions))))))))

(define-dynamic-page edit-tx ((tx-id         integer #'valid-tx-id-p)
			      (tx-date       date)
			      (company-title string  #'valid-company-title-p)
			      (tx-type-title string  #'valid-tx-type-title-p)
			      title
			      (amount        integer #'positive-p))
    ("actions/transaction/edit" :request-type :post) 
  (with-auth "root"
    (with-error-collection (errors)
      (if errors
	  (redirect (transaction/edit :tx-id (or tx-id (getf errors 'tx-id))
				      :tx-date (or tx-date (getf errors 'tx-date))
				      :company-title (or company-title (getf errors 'company-title))
				      :tx-type-title (or tx-type-title (getf errors 'tx-type-title))
				      :amount (or amount (getf errors 'amount))))
	  (with-db
	    (let ((company-id (query (:select 'id :from 'company :where (:= 'title company-title))
				     :single))
		  (tx-type-id (query (:select 'id :from 'tx-type :where (:= 'title tx-type-title))
				     :single)))
	      (update-tx tx-id
			 :company-id company-id
			 :title title
			 :tx-date tx-date
			 :amount amount
			 :tx-type-id tx-type-id)
	      (redirect (transactions))))))))

(define-dynamic-page remove-tx ((tx-id integer #'valid-tx-id-p))
    ("actions/transaction/remove" :request-type :post)
  (with-auth "root"
    (with-error-collection (errors)
      (if errors
	  (redirect (transaction/notfound))
	  (progn
	    (delete-tx tx-id)
	    (redirect (transactions)))))))


;;; --- Pages --------------------

(defun transactions-table (active-tx-id transactions header)
  (with-html
    (:table :class "dbtable"
	    (:thead
	     (:tr (iter (for label in header)
			(htm (:th (str label))))))
	    (:tbody 
	     (iter (for row in transactions)
		   (destructuring-bind (tx-id tx-date company-title company-id
					      tx-type-title tx-type-id tx-title tx-amount)
		       row
		     (let ((activep (and active-tx-id (= active-tx-id tx-id))))
		       (htm
			(:tr :class (if activep "active" nil)
			     (:td (:a :href (transactions :tx-id tx-id)
				      (:img :src (url (if activep
							  "img/bullet_red.png"
							  "img/bullet_blue.png")))))
			     (:td (:p (str (lisp-to-html tx-date))))
			     (:td (:a :href (company/view :company-id company-id)
				      (str (lisp-to-html company-title))))
			     (:td (:a :href (transaction-type/view :tx-type-id tx-type-id)
				      (str (lisp-to-html tx-type-title))))
			     (:td (:p (str (lisp-to-html tx-title))))
			     (:td (:p (str (lisp-to-html tx-amount)))))))))))))

(defun tx-errorbar (errors)
  (with-html 
    (:h3 "Υπάρχουν λάθη"
	 (:ul :class "errorbar" 
	      (when (getf errors 'tx-date)
		(htm (:li "Άκυρη ημερομηνία")))
	      (when (getf errors 'company-title)
		(htm (:li "Ανύπαρκτη εταιρία")))
	      (when (getf errors 'tx-type-title)
		(htm (:li "Ανύπαρκτος τύπος συναλλαγής")))
	      (when (getf errors 'amount)
		(htm (:li "Άκυρο χρηματικό ποσό")))))))

(define-dynamic-page transactions ((tx-id      integer #'valid-tx-id-p)
				   (company-id integer #'valid-company-id-p))
    ("transactions/")
  (with-error-collection (errors) 
    (cond ((getf errors 'tx-id) (redirect (transaction/notfound)))
	  ((getf errors 'company-id) (redirect (company/notfound)))
	  (t 
	   (with-db
	     (let ((transactions (if company-id
				     (get-transactions 'company company-id)
				     (get-transactions 'all)))
		   (header '("" "Ημερομηνία" "Εταιρία" "Τύπος" "Περιγραφή" "Ποσό"))) 
	       (with-page () 
		 (:head
		  (:title "Σκρουτζ: Συναλλαγές")
		  (css "reset.css" "scrooge.css"))
		 (:body
		  (:div :id "header"
			(logo)
			(navbar "Συναλλαγές"))
		  (:div :id "body"
			(:div :id "actions"
			      (tx-menu 'summary tx-id))
			(:div :id "content" :class "summary"
			      (transactions-table tx-id transactions header)))))))))))

(define-dynamic-page transaction/insert ((tx-date       date)
					 (company-title string  #'valid-company-title-p)
					 (tx-type-title string  #'valid-tx-type-title-p)
					 title
					 (amount        integer #'positive-p))
    ("transaction/insert")
  (with-error-collection (errors) 
    (with-db
      (with-page () 
	(:head
	 (:title "Σκρουτζ: Εισαγωγή συναλλαγής")
	 (css "reset.css" "scrooge.css")
	 (js-headers))
	(:body
	 (:div :id "header"
	       (logo)
	       (navbar "Συναλλαγές")) 
	 (:div :id "body"
	       (:div :id "actions"
		     (tx-menu 'insert))
	       (when errors
		 (tx-errorbar errors))
	       (:div :id "content" :class "simple-form"
		     (:h2 "Εισαγωγή συναλλαγής")
		     (with-form (insert-tx)
		       (with-table (:style "formtable")
			 ((label 'tx-date "Ημερομηνία")
			  (textbox 'tx-date
				   :value (or tx-date (getf errors 'tx-date) (current-date))
				   :style (if (getf errors 'tx-date) "invalid" nil)))
			 ((label 'company-title "Εταιρία")
			  (textbox 'company-title
				   :value (or company-title (getf errors 'company-title))
				   :style (if (getf errors 'company-title) "invalid" nil)))
			 ((label 'tx-type-title "Τύπος συναλλαγής")
			  (textbox 'tx-type-title
				   :value (or tx-type-title (getf errors 'tx-type-title))
				   :style (if (getf errors 'tx-type-title) "invalid" nil)))
			 ((label 'title "Περιγραφή")
			  (textbox 'title
				   :value title))
			 ((label 'amount "Ποσό")
			  (textbox 'amount
				   :value (or amount (getf errors 'amount))
				   :style (if (getf errors 'amount) "invalid" nil))))
		       (:ul :class "prompt hmenu"
			    (:li (submit "Δημιουργία συναλλαγής"))
			    (:li (:a :href (transactions) "Ακύρωση")))))
	       (footer)))))))

(define-dynamic-page transaction/view ((tx-id integer #'valid-tx-id-p))
    ("transaction/view")
  (with-error-collection (errors)
    (if (getf errors 'tx-id)
	(redirect (transaction/notfound))
	(with-db
	  (let* ((tx (get-dao 'tx tx-id))
		 (tx-type (query (:select 'title 
					  :from 'tx-type
					  :where (:= 'id (tx-type-id tx)))
				 :single))
		 (company (query (:select 'title 
					  :from 'company
					  :where (:= 'id (company-id tx)))
				 :single)))
	    (with-page ()
	      (:head
	       (:title "Σκρούτζ: Προβολή συναλλαγής")
	       (css "reset.css" "scrooge.css"))
	      (:body
	       (:div :id "header"
		     (logo)
		     (navbar "Συναλλαγές"))
	       (:div :id "body"
		     (:div :id "actions"
			   (tx-menu 'view tx-id))
		     (:div :id "content" :class "simple-form"
			   (:h2 "Προβολή συναλλαγής")
			   (with-table (:style "formtable")
			     ((label 'tx-date "Ημερομηνία")
			      (textbox 'tx-date :value (tx-date tx) :readonlyp t))
			     ((label 'company-id "Εταιρία")
			      (textbox 'company-id :value company :readonlyp t))
			     ((label 'tx-type-id "Τύπος συναλλαγής")
			      (textbox 'tx-type-id :value tx-type :readonlyp t))
			     ((label 'title "Περιγραφή")
			      (textbox 'title :value (title tx) :readonlyp t)) 
			     ((label 'amount "Ποσό")
			      (textbox 'amount :value (amount tx) :readonlyp t)))))
	       (footer))))))))

(define-dynamic-page transaction/edit ((tx-id         integer #'valid-tx-id-p)
				       (tx-date       date)
				       (company-title string  #'valid-company-title-p)
				       (tx-type-title string  #'valid-tx-type-title-p)
				       title
				       (amount        integer #'positive-p))
    ("transaction/edit") 
  (with-error-collection (errors)
    (if (getf errors 'tx-id)
	(redirect (transaction/notfound))
	(with-db
	  (let* ((tx (get-dao 'tx tx-id))
		 (company-title* (query (:select 'title :from 'company
						 :where (:= 'id (company-id tx)))
					:single))
		 (tx-type-title* (query (:select 'title :from 'tx-type
						 :where (:= 'id (tx-type-id tx)))
					:single)))
	    (with-page ()
	      (:head
	       (:title "Σκρούτζ: Επεξεργασία συναλλαγής")
	       (css "reset.css" "scrooge.css")
	       (js-headers)) 
	      (:body
	       (:div :id "header"
		     (logo)
		     (navbar "Συναλλαγές"))
	       (:div :id "body"
		     (:div :id "actions"
			   (tx-menu 'edit tx-id))
		     (when errors
		       (tx-errorbar errors))
		     (:div :id "content" :class "simple-form"
			   (:h2 "Επεξεργασία συναλλαγής")
			   (with-form (edit-tx :tx-id tx-id)
			     (with-table (:style "formtable")
			       ((label 'tx-date "Ημερομηνία")
				(textbox 'tx-date
					 :value (or (getf errors 'tx-date)
						    tx-date
						    (tx-date tx))
					 :style (if (getf errors 'tx-date) "invalid" nil)))
			       ((label 'company-title "Εταιρία")
				(textbox 'company-title
					 :value (or (getf errors 'company-title)
						    company-title
						    company-title*)
					 :style (if (getf errors 'company-title) "invalid" nil)))
			       ((label 'tx-type-title "Τύπος συναλλαγής")
				(textbox 'tx-type-title
					 :value (or (getf errors 'tx-type-title)
						    tx-type-title
						    tx-type-title*)
					 :style (if (getf errors 'tx-type-title) "invalid" nil)))
			       ((label 'title "Περιγραφή")
				(textbox 'title :value (or title (title tx))))
			       ((label 'amount "Ποσό")
				(textbox 'amount
					 :value (or (getf errors 'amount) amount (amount tx))
					 :style (if (getf errors 'amount) "invalid" nil))))
			     (:ul :class "prompt hmenu"
				  (:li (submit "Ενημέρωση"))
				  (:li (:a :href (transactions) "Άκυρο"))))))
	       (footer))))))))

(define-dynamic-page transaction/remove ((tx-id integer #'valid-tx-id-p)) ("transaction/remove")
  (no-cache)
  (with-error-collection (errors)
    (if (getf errors 'tx-id)
	(redirect (transaction/notfound))
	(with-db
	  (let* ((tx (get-dao 'tx tx-id))
		 (tx-type (query (:select 'title
					  :from 'tx-type
					  :where (:= 'id (tx-type-id tx)))
				 :single))
		 (company (query (:select 'title 
					  :from 'company
					  :where (:= 'id (company-id tx)))
				 :single)))
	    (with-page ()
	      (:head
	       (:title "Σκρούτζ: Διαγραφή συναλλαγής")
	       (css "reset.css" "scrooge.css"))
	      (:body
	       (:div :id "header"
		     (logo)
		     (navbar "Συναλλαγές"))
	       (:div :id "body"
		     (:div :id "actions"
			   (tx-menu 'kill tx-id))
		     (:div :id "content" :class "simple-form"
			   (:h2 "Διαγραφή συναλλαγής")
			   (with-form (remove-tx :tx-id tx-id)
			     (with-table (:style "formtable") 
			       ((label 'tx-date "Ημερομηνία")
				(textbox 'tx-date :value (tx-date tx) :readonlyp t))
			       ((label 'company-id "Εταιρία")
				(textbox 'company-id :value company :readonlyp t))
			       ((label 'tx-type-id "Τύπος συναλλαγής") 
				(textbox 'tx-type-id :value tx-type :readonlyp t))
			       ((label 'title "Περιγραφή")
				(textbox 'title :value (title tx) :readonlyp t))
			       ((label 'amount "Ποσό")
				(textbox 'amount :value (amount tx) :readonlyp t)))
			     (:ul :class "prompt hmenu"
				  (:li (submit "Διαγραφή"))
				  (:li (:a :href (transactions) "Άκυρο"))))))
	       (footer))))))))

(define-dynamic-page transaction/notfound () ("transaction/notfound")
  (with-page ()
    (:head
     (:title "Σκρούτζ: Άγνωστη συναλλαγή")
     (css "reset.css" "scrooge.css"))
    (:body
     (:div :id "header"
	   (logo)
	   (navbar "Συναλλαγές"))
     (:div :id "body"
	   (:div :id "content" :class "summary"
		 (:p "Η συναλλαγή που προσπαθείτε να προσπελάσετε δεν υπάρχει πια.")
		 (:p "Επιστρέψτε στο μενού των συναλλαγών και προσπαθήστε ξανά."))))))