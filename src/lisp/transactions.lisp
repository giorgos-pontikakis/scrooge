(in-package :scrooge)

;;; --- Snippets --------------------

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
		       (:p (str (conc "Αδύνατη η δημιουργία συναλλαγών. "
				      (unless companies-exist-p
					"Δεν έχουν οριστεί εταιρίες. ")
				      (unless tx-types-exist-p
					"Δεν έχουν οριστεί τύποι συναλλαγών.")))))))
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

(defun transactions-table (active-tx-id transactions header)
  (with-html
    (:table :class "dbtable"
	    (:thead
	     (:tr (iter (for label in header)
			(htm (:th (str label))))))
	    (:tbody 
	     (iter (for row in transactions)
		   (destructuring-bind (tx-id tx-date company company-id
					      tx-type tx-type-id tx-title tx-amount)
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
				      (str (lisp-to-html company))))
			     (:td (:a :href (transaction-type/view :tx-type-id tx-type-id)
				      (str (lisp-to-html tx-type))))
			     (:td (:p (str (lisp-to-html tx-title))))
			     (:td (:p (str (lisp-to-html tx-amount)))))))))))))

(defun tx-errorbar (tx-date company tx-type amount)
  (with-html 
    (:div :id "msg"
	  (:ul :class "errorbar" 
	       (when tx-date
		 (htm (:li "Άκυρη ημερομηνία")))
	       (when company
		 (htm (:li "Ανύπαρκτη εταιρία")))
	       (when tx-type
		 (htm (:li "Ανύπαρκτος τύπος συναλλαγής")))
	       (when amount
		 (htm (:li "Άκυρο χρηματικό ποσό")))))))


;;; --- Actions --------------------

(define-dynamic-page insert-tx ((tx-date date)
				(company string #'valid-company-p)
				(tx-type string #'valid-tx-type-p)
				(title string)
				(amount integer #'positive-p))
    ("actions/transaction/insert" :request-type :post)
  (no-cache)
  (with-auth "root"
    (with-error-plist (errors)
      (if errors
	  (redirect (transaction/insert :tx-date (or tx-date tx-date*)
					:company (or company company*)
					:tx-type (or tx-type tx-type*)
					:title title
					:amount (or amount amount*)))
	  (with-db
	    (let ((company-id (query (:select 'id :from 'company :where (:= 'title company))
				     :single))
		  (tx-type-id (query (:select 'id :from 'tx-type :where (:= 'title tx-type))
				     :single)))
	      (create-tx title tx-date amount tx-type-id company-id)
	      (redirect (transactions))))))))

(define-dynamic-page edit-tx ((tx-id         integer #'valid-tx-id-p)
			      (tx-date       date)
			      (company string  #'valid-company-p)
			      (tx-type string  #'valid-tx-type-p)
			      title
			      (amount        integer #'positive-p))
    ("actions/transaction/edit" :request-type :post) 
  (no-cache)
  (with-auth "root"
    (with-error-plist (errors)
      (if errors
	  (redirect (transaction/edit :tx-id (or tx-id tx-id*)
				      :tx-date (or tx-date tx-date*)
				      :company (or company company*)
				      :tx-type (or tx-type tx-type*)
				      :amount (or amount amount*)))
	  (with-db
	    (let ((company-id (query (:select 'id :from 'company :where (:= 'title company))
				     :single))
		  (tx-type-id (query (:select 'id :from 'tx-type :where (:= 'title tx-type))
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
  (no-cache)
  (with-auth "root"
    (with-error-plist (errors)
      (if errors
	  (redirect (transaction/notfound))
	  (progn
	    (delete-tx tx-id)
	    (redirect (transactions)))))))


;;; --- Pages --------------------

(define-dynamic-page transactions ((tx-id      integer #'valid-tx-id-p)
				   (company-id integer #'valid-company-id-p))
    ("transactions/")
  (no-cache)
  (cond (tx-id* (redirect (transaction/notfound)))
	(company-id* (redirect (company/notfound)))
	(t 
	 (with-db
	   (let ((transactions (if company-id
				   (get-transactions 'company company-id)
				   (get-transactions 'all)))
		 (header '("" "Ημερομηνία" "Εταιρία" "Τύπος Συναλλαγής" "Περιγραφή" "Ποσό"))) 
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
			    (transactions-table tx-id transactions header))))))))))

(define-dynamic-page transaction/insert ((tx-date date)
					 (company string  #'valid-company-p)
					 (tx-type string  #'valid-tx-type-p)
					 (title   string)
					 (amount  integer #'positive-p))
    ("transaction/insert")
  (no-cache)
  (with-error-plist (errors)
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
		 (tx-errorbar tx-date* company* tx-type* amount*))
	       (:div :id "content" :class "simple-form"
		     (:h2 "Εισαγωγή συναλλαγής")
		     (with-form (insert-tx)
		       (with-table (:style "formtable")
			 ((label 'tx-date "Ημερομηνία")
			  (textbox 'tx-date
				   :value (or tx-date tx-date* (current-date))
				   :style (if tx-date* "invalid" nil)))
			 ((label 'company "Εταιρία")
			  (textbox 'company
				   :value (or company company*)
				   :style (if company* "invalid" nil)))
			 ((label 'tx-type "Τύπος συναλλαγής")
			  (textbox 'tx-type
				   :value (or tx-type tx-type*)
				   :style (if tx-type* "invalid" nil)))
			 ((label 'title "Περιγραφή")
			  (textbox 'title :value title))
			 ((label 'amount "Ποσό")
			  (textbox 'amount
				   :value (or amount amount*)
				   :style (if amount* "invalid" nil))))
		       (:ul :class "prompt hmenu"
			    (:li (submit "Δημιουργία συναλλαγής"))
			    (:li (:a :href (transactions) "Ακύρωση")))))
	       (footer)))))))

(define-dynamic-page transaction/view ((tx-id integer #'valid-tx-id-p))
    ("transaction/view")
  (no-cache)
  (if tx-id*
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
	     (footer)))))))

(define-dynamic-page transaction/edit ((tx-id         integer #'valid-tx-id-p)
				       (tx-date       date)
				       (company string  #'valid-company-p)
				       (tx-type string  #'valid-tx-type-p)
				       title
				       (amount        integer #'positive-p))
    ("transaction/edit") 
  (no-cache)
  (if tx-id*
      (redirect (transaction/notfound))
      (with-db
	(let* ((tx (get-dao 'tx tx-id)))
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
		   (when (or tx-date* company* tx-type* amount)
		     (tx-errorbar tx-date* company* tx-type* amount*))
		   (:div :id "content" :class "simple-form"
			 (:h2 "Επεξεργασία συναλλαγής")
			 (with-form (edit-tx :tx-id tx-id)
			   (with-table (:style "formtable")
			     ((label 'tx-date "Ημερομηνία")
			      (textbox 'tx-date
				       :value (or tx-date
						  tx-date*
						  (tx-date tx))
				       :style (if tx-date* "invalid" nil)))
			     ((label 'company "Εταιρία")
			      (textbox 'company
				       :value (or company
						  company*
						  (query (:select 'title :from 'company
								  :where (:= 'id (company-id tx)))
							 :single))
				       :style (if company* "invalid" nil)))
			     ((label 'tx-type "Τύπος συναλλαγής")
			      (textbox 'tx-type
				       :value (or tx-type
						  tx-type*
						  (query (:select 'title :from 'tx-type
								  :where (:= 'id (tx-type-id tx)))
							 :single))
				       :style (if tx-type* "invalid" nil)))
			     ((label 'title "Περιγραφή")
			      (textbox 'title :value (or title (title tx))))
			     ((label 'amount "Ποσό")
			      (textbox 'amount
				       :value (or amount amount* (amount tx))
				       :style (if amount* "invalid" nil))))
			   (:ul :class "prompt hmenu"
				(:li (submit "Ενημέρωση"))
				(:li (:a :href (transactions) "Άκυρο"))))))
	     (footer)))))))

(define-dynamic-page transaction/remove ((tx-id integer #'valid-tx-id-p)) ("transaction/remove")
  (no-cache)
  (if tx-id*
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
	     (footer)))))))

(define-dynamic-page transaction/notfound () ("transaction/notfound")
  (with-page ()
    (:head
     (:title "Σκρούτζ: Άγνωστη συναλλαγή")
     (css "reset.css" "scrooge.css"))
    (:body
     (:div :id "header"
	   (logo)
	   (navbar nil))
     (:div :id "body"
	   (:div :id "content" :class "summary"
		 (:p "Η συναλλαγή που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
		 (:p "Επιστρέψτε στο μενού των συναλλαγών και προσπαθήστε ξανά."))))))