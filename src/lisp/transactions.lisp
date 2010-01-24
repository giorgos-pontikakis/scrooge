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
      (let ((companies-exist-p (query (:select 'id :from 'company)))) 
	(with-html
	  (:ul :class "hmenu" 
	       (ecase state
		 ;;
		 (summary
		  (if companies-exist-p
		      (progn (insert)
			     (when id
			       (view id)
			       (edit id)
			       (kill id)))
		      (htm
		       (:p (str (conc "Αδύνατη η δημιουργία συναλλαγών. "
				      (unless companies-exist-p
					"Δεν έχουν οριστεί εταιρίες. ")))))))
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
	     (iter (for tx in transactions)
		   (destructuring-bind (tx-id tx-date tx-title tx-amount
					      company-id company-title
					      debit-acc-id debit-acc-title
					      credit-acc-id credit-acc-title) tx
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
			     (:td (:a :href (account/view :account-id debit-acc-id)
				      (str (lisp-to-html debit-acc-title))))
			     (:td (:a :href (account/view :account-id credit-acc-id)
				      (str (lisp-to-html credit-acc-title)))) 
			     (:td (:p (str (lisp-to-html tx-title))))
			     (:td (:p (str (lisp-to-html tx-amount)))))))))))))

(defun tx-errorbar (tx-date company debit-acc credit-acc amount)
  (with-html 
    (:div :id "msg"
	  (:ul :class "errorbar" 
	       (when tx-date
		 (htm (:li "Άκυρη ημερομηνία")))
	       (when company
		 (htm (:li "Ανύπαρκτη εταιρία")))
	       (when debit-acc
		 (htm (:li "Ανύπαρκτος λογαριασμός χρέωσης")))
	       (when credit-acc
		 (htm (:li "Ανύπαρκτος λογαριασμός πίστωσης"))) 
	       (when amount
		 (htm (:li "Άκυρο χρηματικό ποσό")))))))


;;; --- Actions --------------------

(define-dynamic-page insert-tx ((tx-date    date)
				(company    string  #'valid-company-p)
				(debit-acc  string  #'account-exists-p)
				(credit-acc string  #'account-exists-p)
				(title      string)
				(amount     integer #'positive-p)
				(cheque-id  integer (lambda (val)
						     (or (eql :null val)
							 (valid-cheque-id-p val)))))
    ("actions/transaction/insert" :request-type :post)
  (no-cache)
  (with-auth "root"
    (with-error-plist (errors)
      (if errors
	  (see-other (transaction/insert :tx-date (or tx-date tx-date*)
					 :company (or company company*)
					 :debit-acc (or debit-acc debit-acc*)
					 :credit-acc (or credit-acc credit-acc*)
					 :title title
					 :amount (or amount amount*)
					 :cheque-id (or cheque-id cheque-id*)))
	  (with-db
	    (let ((company-id (query (:select 'id :from 'company :where (:= 'title company))
				     :single!))
		  (debit-acc-id (query (:select 'id :from 'account :where (:= 'title debit-acc))
				       :single))
		  (credit-acc-id (query (:select 'id :from 'account :where (:= 'title credit-acc))
					:single)))
	      (create-tx title tx-date amount debit-acc-id credit-acc-id company-id cheque-id)
	      (see-other (transactions))))))))

(define-dynamic-page edit-tx ((tx-id   integer #'valid-tx-id-p)
			      (tx-date date)
			      (company string  #'valid-company-p)
			      (debit-acc  string  #'account-exists-p)
			      (credit-acc string  #'account-exists-p)
			      title
			      (amount  integer #'positive-p)
			      (cheque-id integer (lambda (val)
						   (or (eql :null val)
						       (valid-cheque-id-p val)))))
    ("actions/transaction/edit" :request-type :post) 
  (no-cache)
  (with-auth "root"
    (with-error-plist (errors)
      (if errors
	  (see-other (transaction/edit :tx-id (or tx-id tx-id*)
				       :tx-date (or tx-date tx-date*)
				       :company (or company company*)
				       :debit-acc (or debit-acc debit-acc*)
				       :credit-acc (or credit-acc credit-acc*)
				       :amount (or amount amount*)
				       :cheque-id (or cheque-id cheque-id*)))
	  (with-db
	    (let ((company-id (query (:select 'id :from 'company :where (:= 'title company))
				     :single))
		  (debit-acc-id (query (:select 'id :from 'account :where (:= 'title debit-acc))
				       :single))
		  (credit-acc-id (query (:select 'id :from 'account :where (:= 'title credit-acc))
					:single)))
	      (update-tx tx-id
			 :company-id company-id
			 :title title
			 :debit-acc-id debit-acc-id
			 :credit-acc-id credit-acc-id
			 :amount amount 
			 :cheque-id cheque-id)
	      (see-other (transactions))))))))

(define-dynamic-page remove-tx ((tx-id integer #'valid-tx-id-p))
    ("actions/transaction/remove" :request-type :post)
  (no-cache)
  (with-auth "root"
    (with-error-plist (errors)
      (if errors
	  (see-other (transaction/notfound))
	  (progn
	    (delete-tx tx-id)
	    (see-other (transactions)))))))


;;; --- Pages --------------------

(define-dynamic-page transactions ((tx-id      integer #'valid-tx-id-p)
				   (company-id integer #'valid-company-id-p))
    ("transactions/") 
  (no-cache)
  (cond (tx-id* (see-other (transaction/notfound)))
	(company-id* (see-other (company/notfound)))
	(t 
	 (with-db
	   (let ((transactions (if company-id
				   (query (:select 'tx.id 'tx.tx-date 'tx.title 'tx.amount 'company.id 'company.title
						   'debacc.id 'debacc.title 'credacc.id 'credacc.title
						   :from 'tx
						   :left-join 'company :on (:= 'company.id 'tx.company-id)
						   :left-join (:as 'account 'debacc) :on (:= 'debacc.id 'tx.debit-acc-id)
						   :left-join (:as 'account 'credacc) :on (:= 'credacc.id 'tx.credit-acc-id)
						   :where (:= 'company.id company-id)))
				   (query (:select 'tx.id 'tx.tx-date 'tx.title 'tx.amount 'company.id 'company.title
						   'debacc.id 'debacc.title 'credacc.id 'credacc.title
						   :from 'tx
						   :left-join 'company :on (:= 'company.id 'tx.company-id)
						   :left-join (:as 'account 'debacc) :on (:= 'debacc.id 'tx.debit-acc-id)
						   :left-join (:as 'account 'credacc) :on (:= 'credacc.id 'tx.credit-acc-id)))))
		 (header '("" "Ημερομηνία" "Εταιρία" "Περιγραφή" "Λογαριασμός Πίστωσης" "Λογαριασμός Χρέωσης" "Ποσό"))) 
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

(define-dynamic-page transaction/insert ((tx-date    date)
					 (company    string  #'valid-company-p)
					 (debit-acc  string  #'account-exists-p)
					 (credit-acc string  #'account-exists-p)
					 (title      string)
					 (amount     integer #'positive-p)
					 (cheque-id  integer (lambda (val)
							      (or (eql :null val)
								  (valid-cheque-id-p val)))))
    ("transaction/insert")
  (no-cache)
  (with-error-plist (errors)
    (with-db
      (let* ((cheques (query (:select 'issuer 'due-date 'amount 'id :from 'cheque)))
	     (cheques-alist (iter (for row in cheques) 
				  (collect (list
					    (format nil "~A/~A/~A"
						    (lisp-to-html (first row))
						    (lisp-to-html (second row))
						    (lisp-to-html (third row)))
					    (fourth row)))))) 
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
		   (tx-errorbar tx-date* company* debit-acc* credit-acc* amount*))
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
			   ((label 'debit-acc "Λογαριασμός χρέωσης")
			    (textbox 'debit-acc
				     :value (or debit-acc debit-acc*)
				     :style (if debit-acc* "invalid" nil)))
			   ((label 'credit-acc "Λογαριασμός πίστωσης")
			    (textbox 'credit-acc
				     :value (or credit-acc credit-acc*)
				     :style (if credit-acc* "invalid" nil)))
			   ((label 'title "Περιγραφή")
			    (textbox 'title :value title))
			   ((label 'amount "Μετρητά")
			    (textbox 'amount
				     :value (or amount amount*)
				     :style (if amount* "invalid" nil)))
			   ((label 'cheque "Επιταγή") 
			    (dropdown 'cheque-id cheques-alist :selected cheque-id)))
			 (:ul :class "prompt hmenu"
			      (:li (submit "Δημιουργία συναλλαγής"))
			      (:li (:a :href (transactions) "Ακύρωση")))))
		 (footer))))))))

(define-dynamic-page transaction/view ((tx-id integer #'valid-tx-id-p))
    ("transaction/view")
  (no-cache) 
  (if tx-id*
      (see-other (transaction/notfound))
      (with-db
	(let* ((tx (get-dao 'tx tx-id))
	       (debit-acc  (query (:select 'title :from 'tx-type :where (:= 'id (debit-acc-id  tx))) :single))
	       (credit-acc (query (:select 'title :from 'tx-type :where (:= 'id (credit-acc-id tx))) :single))
	       (company (query (:select 'title 
					:from 'company
					:where (:= 'id (company-id tx)))
			       :single))
	       (cheques (query (:select 'issuer 'due-date 'amount 'id :from 'cheque)))
	       (cheques-alist (iter (for row in cheques) 
				    (collect (list
					      (format nil "~A/~A/~A"
						      (lisp-to-html (first row))
						      (lisp-to-html (second row))
						      (lisp-to-html (third row)))
					      (fourth row))))))
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
			   ((label 'debit-acc "Λογαριασμός χρέωσης")
			    (textbox 'debit-acc :value debit-acc :readonlyp t))
			   ((label 'credit-acc "Λογαριασμός πίστωσης")
			    (textbox 'credit-acc :value credit-acc :readonlyp t))
			   ((label 'title "Περιγραφή")
			    (textbox 'title :value (title tx) :readonlyp t)) 
			   ((label 'amount "Ποσό")
			    (textbox 'amount :value (amount tx) :readonlyp t))
			   ((label 'cheque-id "Μετρητά")
			    (dropdown 'cheque-id cheques-alist
				      :selected (cheque-id tx) :readonlyp t)))))
	     (footer)))))))

(define-dynamic-page transaction/edit ((tx-id      integer #'valid-tx-id-p)
				       (tx-date    date)
				       (company    string  #'valid-company-p)
				       (debit-acc  string  #'account-exists-p)
				       (credit-acc string  #'account-exists-p)
				       title
				       (amount     integer #'positive-p)
				       (cheque-id  integer (lambda (val)
							    (or (eql :null val)
								(valid-cheque-id-p val)))))
    ("transaction/edit") 
  (no-cache)
  (if tx-id*
      (see-other (transaction/notfound))
      (with-db
	(let* ((tx (get-dao 'tx tx-id))
	       (cheques (query (:select 'issuer 'due-date 'amount 'id :from 'cheque)))
	       (cheques-alist (iter (for row in cheques) 
				    (collect (list
					      (format nil "~A/~A/~A"
						      (lisp-to-html (first row))
						      (lisp-to-html (second row))
						      (lisp-to-html (third row)))
					      (fourth row))))))
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
		   (when (or tx-date* company* debit-acc* credit-acc* amount)
		     (tx-errorbar tx-date* company* debit-acc* credit-acc* amount*))
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
			     ((label 'debit-acc "Λογαριασμός χρέωσης")
			      (textbox 'debit-acc
				       :value (or debit-acc
						  debit-acc
						  (query (:select 'title :from 'account
								  :where (:= 'id (debit-acc-id tx)))
							 :single))
				       :style (if debit-acc* "invalid" nil)))
			     ((label 'credit-acc "Λογαριασμός χρέωσης")
			      (textbox 'credit-acc
				       :value (or credit-acc
						  credit-acc
						  (query (:select 'title :from 'account
								  :where (:= 'id (credit-acc-id tx)))
							 :single))
				       :style (if credit-acc* "invalid" nil)))
			     ((label 'title "Περιγραφή")
			      (textbox 'title :value (or title (title tx))))
			     ((label 'amount "Μετρητά")
			      (textbox 'amount
				       :value (or amount amount* (amount tx))
				       :style (if amount* "invalid" nil)))
			     ((label 'cheque-id "Επιταγή")
			      (dropdown 'cheque-id cheques-alist
					:selected (or cheque-id (cheque-id tx)))))
			   (:ul :class "prompt hmenu"
				(:li (submit "Ενημέρωση"))
				(:li (:a :href (transactions) "Άκυρο"))))))
	     (footer)))))))

(define-dynamic-page transaction/remove ((tx-id integer #'valid-tx-id-p)) ("transaction/remove")
  (no-cache)
  (if tx-id*
      (see-other (transaction/notfound))
      (with-db
	(let* ((tx (get-dao 'tx tx-id))
	       (debit-acc  (query (:select 'title :from 'tx-type :where (:= 'id (debit-acc-id  tx))) :single))
	       (credit-acc (query (:select 'title :from 'tx-type :where (:= 'id (credit-acc-id tx))) :single))
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
			     ((label 'debit-acc "Λογαριασμός χρέωσης")
			    (textbox 'debit-acc :value debit-acc :readonlyp t))
			   ((label 'credit-acc "Λογαριασμός πίστωσης")
			    (textbox 'credit-acc :value credit-acc :readonlyp t))
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