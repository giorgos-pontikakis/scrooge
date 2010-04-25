(in-package :scrooge)

(defun tx-type-errorbar (title debit-acc credit-acc)
  (with-html 
    (:div :id "msg"
	  (:ul :class "errorbar" 
	       (when title
		 (htm (:li "Άκυρος τίτλος τύπου συναλλαγής.")))
	       (when debit-acc
		 (htm (:li "Ανύπαρκτος χρεωστικός λογαριασμός.")))
	       (when credit-acc
		 (htm (:li "Ανύπαρκτος πιστωτικός λογαριασμός.")))))))

(defun transaction-types-table (active-id transaction-types header)
  (with-html
    (:table :class "dbtable"
	    (:thead
	     (:tr (iter (for label in header)
			(htm (:th (str label))))))
	    (:tbody
	     (iter (for row in transaction-types)
		   (destructuring-bind (id title debit-id debit-title credit-id credit-title) row
		     (let ((activep (and active-id (= active-id id))))
		       (htm
			(:tr :class (if activep "active" nil)
			     (:td (:a :href (transaction-types :tx-type-id id)
				      (:img :src (url (if activep
							  "img/bullet_red.png"
							  "img/bullet_blue.png")))))
			     (:td (:p (str title)))
			     (:td (:a :href (account/view :account-id debit-id)
				      (str debit-title)))
			     (:td (:a :href (account/view :account-id credit-id)
				      (str credit-title))))))))))))

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
		      (htm (:p "Αδύνατη η δημιουργία τύπων συναλλαγών. Δεν έχουν οριστεί λογαριασμοί."))))
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

;;; --- Actions --------------------

(define-dynamic-page insert-tx-type ((title      string #'not-db-null-p)
				     (debit-acc  string #'account-exists-p)
				     (credit-acc string #'account-exists-p))
    ("actions/transaction-type/insert" :request-type :post)
  (no-cache)
  (with-auth "root"
    (with-error-plist (errors)
      (if errors
	  (see-other (transaction-type/insert :title (or title title*)
					      :debit-acc (or debit-acc debit-acc*)
					      :credit-acc (or credit-acc credit-acc*)))
	  (with-db
	    (let ((debit-acc-id (query (:select 'id
						:from 'account
						:where (:= 'title debit-acc))
				       :single))
		  (credit-acc-id (query (:select 'id
						 :from 'account
						 :where (:= 'title credit-acc))
					:single)))
	      (create-tx-type title debit-acc-id credit-acc-id)
	      (see-other (transaction-types))))))))

(define-dynamic-page edit-tx-type ((tx-type-id integer #'valid-tx-type-id-p)
				   (title      string  #'not-db-null-p)
				   (debit-acc  string  #'account-exists-p)
				   (credit-acc string  #'account-exists-p))
    ("actions/transaction-type/edit" :request-type :post)
  (no-cache)
  (with-auth "root"
    (with-error-plist (errors)
      (if errors
	  (see-other (transaction-type/edit :tx-type-id tx-type-id
					    :title (or title title*)
					    :debit-acc (or debit-acc debit-acc*)
					    :credit-acc (or credit-acc credit-acc*)))
	  (with-db
	    (let ((debit-acc-id (query (:select 'id
						:from 'account
						:where (:= 'title debit-acc))
				       :single))
		  (credit-acc-id (query (:select 'id
						 :from 'account
						 :where (:= 'title credit-acc))
					:single)))
	      (update-tx-type tx-type-id
			      :title title
			      :debit-acc-id debit-acc-id
			      :credit-acc-id credit-acc-id)
	      (see-other (transaction-types))))))))

(define-dynamic-page remove-tx-type ((tx-type-id integer))
    ("actions/transaction-type/remove" :request-type :post)
  (no-cache)
  (with-auth "root"
    (if tx-type-id*
	(see-other (transaction-type/notfound))
	(progn
	  (delete-tx-type tx-type-id)
	  (see-other (transaction-types))))))


;;; --- Pages --------------------

(define-dynamic-page transaction-types ((tx-type-id integer #'valid-tx-type-id-p))
    ("transaction-types/")
  (no-cache)
  (if tx-type-id*
      (see-other (transaction/notfound)) 
      (with-db
	(let ((transaction-types
	       (query
		(:select 'tx-type.id 'tx-type.title 'acc1.id 'acc1.title 'acc2.id 'acc2.title
			 :from 'tx-type
			 :inner-join (:as 'account 'acc1) :on (:= 'acc1.id 'tx-type.debit-acc-id)
			 :inner-join (:as 'account 'acc2) :on (:= 'acc2.id 'tx-type.credit-acc-id))))
	      (header '("" "Περιγραφή" "Λογαριασμός χρέωσης" "Λογαριασμός πίστωσης")))
	  (with-page ()
	    (:head
	     (:title "Σκρούτζ: Τύποι συναλλαγών")
	     (css-headers "css/reset.css" "css/scrooge.css"))
	    (:body
	     (:div :id "header"
		   (logo)
		   (navbar "Τύποι συναλλαγών"))
	     (:div :id "body"
		   (:div :id "actions"
			 (tx-type-menu 'summary tx-type-id))
		   (:div :id "content" :class "summary"
			 (transaction-types-table tx-type-id transaction-types header)))))))))

(define-dynamic-page transaction-type/insert ((title      string #'not-db-null-p)
					      (debit-acc  string #'account-exists-p)
					      (credit-acc string #'account-exists-p))
    ("transaction-type/insert")
  (no-cache)
  (with-db
    (with-page ()
      (:head
       (:title "Σκρούτζ: Εισαγωγή τύπου συναλλαγής")
       (css-headers "css/reset.css" "css/scrooge.css")
       (js-standard-headers))
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Τύποι συναλλαγών"))
       (:div :id "body"
	     (:div :id "actions"
		   (tx-type-menu 'insert))
	     (when (or title* debit-acc* credit-acc*)
	       (tx-type-errorbar title* debit-acc* credit-acc*))
	     (:div :id "content" :class "simple-form"
		   (with-form (insert-tx-type)
		     (with-table (:style "formtable")
		       ((label 'title "Περιγραφή")
			(textbox 'title
				 :value title
				 :style (if title* "invalid" nil)))
		       ((label 'debit-acc "Λογαριασμός χρέωσης")
			(textbox 'debit-acc
				 :value (or debit-acc debit-acc*)
				 :style (if debit-acc* "invalid" nil)))
		       ((label 'credit-acc "Λογαριασμός πίστωσης")
			(textbox 'credit-acc 
				 :value (or credit-acc credit-acc*)
				 :style (if credit-acc* "invalid" nil))))
		     (:ul :class "prompt hmenu"
			  (:li (submit "Δημιουργία"))
			  (:li (:a :href (transaction-types) "Άκυρο"))))))
       (footer)))))

(define-dynamic-page transaction-type/view ((tx-type-id integer #'valid-tx-type-id-p))
    ("transaction-type/view")
  (no-cache)
  (if tx-type-id*
      (see-other (transaction-type/notfound))
      (with-db
	(let* ((tx-type (get-dao 'tx-type tx-type-id))
	       (debit-acc (query
			   (:select 'title :from 'account :where (:= 'id (debit-acc-id tx-type)))
			   :single))
	       (credit-acc (query
			    (:select 'title :from 'account :where (:= 'id (credit-acc-id tx-type)))
			    :single)))
	  (with-page ()
	    (:head
	     (:title "Σκρούτζ: Προβολή τύπου συναλλαγής")
	     (css-headers "css/reset.css" "css/scrooge.css"))
	    (:body
	     (:div :id "header"
		   (logo)
		   (navbar "Τύποι συναλλαγών"))
	     (:div :id "body"
		   (:div :id "actions"
			 (tx-type-menu 'view tx-type-id))
		   (:div :id "content" :class "simple-form"
			 (:h2 "Προβολή τύπου συναλλαγής")
			 (with-table (:style "formtable")
			   ((label 'title "Περιγραφή")
			    (textbox 'title :value (title tx-type) :readonlyp t))
			   ((label 'debit-acc "Λογαριασμός χρέωσης")
			    (textbox 'debit-acc :value debit-acc :readonlyp t))
			   ((label 'credit-acc "Λογαριασμός πίστωσης")
			    (textbox 'credit-acc :value credit-acc :readonlyp t)))))
	     (footer)))))))

(define-dynamic-page transaction-type/edit ((tx-type-id integer #'valid-tx-type-id-p)
					    (title      string  #'not-db-null-p)
					    (debit-acc  string  #'account-exists-p)
					    (credit-acc string  #'account-exists-p))
    ("transaction-type/edit")
  (no-cache)
  (if tx-type-id*
      (see-other (transaction-type/notfound))
      (with-db
	(let ((tx-type (get-dao 'tx-type tx-type-id)))
	  (with-page ()
	    (:head
	     (:title "Σκρούτζ: Επεξεργασία τύπου συναλλαγής")
	     (css-headers "css/reset.css" "css/scrooge.css")
	     (js-standard-headers))
	    (:body
	     (:div :id "header"
		   (logo)
		   (navbar "Τύποι συναλλαγών"))
	     (:div :id "body"
		   (:div :id "actions"
			 (tx-type-menu 'edit tx-type-id))
		   (when (or title* debit-acc* credit-acc*)
		     (tx-type-errorbar title* debit-acc* credit-acc*))
		   (:div :id "content" :class "simple-form"
			 (:h2 "Επεξεργασία τύπου συναλλαγής")
			 (with-form (edit-tx-type :tx-type-id tx-type-id)
			   (with-table (:style "formtable")
			     ((label 'title "Περιγραφή")
			      (textbox 'title
				       :value (or title title* (title tx-type))
				       :style (if title* "invalid" nil)))
			     ((label 'debit-acc "Λογαριασμός χρέωσης")
			      (textbox 'debit-acc
				       :value (or debit-acc
						  debit-acc*
						  (query
						   (:select 'title
							    :from 'account
							    :where (:= 'id
								       (debit-acc-id tx-type)))
						   :single))
				       :style (if debit-acc* "invalid" nil)))
			     ((label 'credit-acc "Λογαριασμός πίστωσης")
			      (textbox 'credit-acc
				       :value (or credit-acc
						  credit-acc*
						  (query
						   (:select 'title
							    :from 'account
							    :where (:= 'id
								       (credit-acc-id tx-type)))
						   :single))
				       :style (if credit-acc* "invalid" nil))))
			   (:ul :class "prompt hmenu"
				(:li (submit "Ενημέρωση"))
				(:li (:a :href (transaction-types) "Άκυρο"))))))
	     (footer)))))))

(define-dynamic-page transaction-type/remove ((tx-type-id integer #'valid-tx-type-id-p))
    ("transaction-type/remove")
  (no-cache)
  (if tx-type-id*
      (see-other (transaction-type/notfound))
      (with-db
	(let* ((tx-type (get-dao 'tx-type tx-type-id))
	       (debit-acc (query
			   (:select 'title :from 'account :where (:= 'id (debit-acc-id tx-type)))
			   :single))
	       (credit-acc (query
			    (:select 'title :from 'account :where (:= 'id (credit-acc-id tx-type)))
			    :single)))
	  (with-page ()
	    (:head
	     (:title "Σκρούτζ: Διαγραφή τύπου συναλλαγής")
	     (css-headers "css/reset.css" "css/scrooge.css"))
	    (:body
	     (:div :id "header"
		   (logo)
		   (navbar "Τύποι συναλλαγών"))
	     (:div :id "body"
		   (:div :id "actions"
			 (tx-type-menu 'kill tx-type-id))
		   (:div :id "content" :class "simple-form"
			 (:h2 "Διαγραφή τύπου συναλλαγής")
			 (with-form (remove-tx-type :tx-type-id tx-type-id)
			   (with-table (:style "formtable")
			     ((label 'title "Περιγραφή")
			      (textbox 'title :value (title tx-type) :readonlyp t))
			     ((label 'debit-acc "Λογαριασμός χρέωσης")
			      (textbox 'debit-acc :value debit-acc :readonlyp t))
			     ((label 'credit-acc "Λογαριασμός πίστωσης")
			      (textbox 'credit-acc :value credit-acc :readonlyp t)))
			   (:ul :class "prompt hmenu"
				(:li (submit "Διαγραφή"))
				(:li (:a :href (transaction-type/view :tx-type-id tx-type-id)
					 "Ακύρωση"))))))
	     (footer)))))))


(define-dynamic-page transaction-type/notfound () ("transaction-type/notfound")
  (with-page ()
    (:head
     (:title "Σκρούτζ: Άγνωστος τύπος συναλλαγής")
     (css-headers "css/reset.css" "css/scrooge.css"))
    (:body
     (:div :id "header"
	   (logo)
	   (navbar nil))
     (:div :id "body"
	   (:div :id "content" :class "summary"
		 (:p "Ο τύπος συναλλαγής που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
		 (:p "Επιστρέψτε στο μενού των τύπων συναλλαγών και προσπαθήστε ξανά."))))))


