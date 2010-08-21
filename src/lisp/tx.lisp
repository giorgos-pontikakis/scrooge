(in-package :scrooge)


(declaim (optimize (speed 0) (debug 3)))

(defun manual-tx-p (tx-id)
  (if tx-id
      (with-db
	(eql :null (query (:select 'src-id :from 'tx :where (:= 'id tx-id)) :single)))
      nil))

(define-dynamic-page actions/transaction/create ((tx-date date)
						 (description string)
						 (company string #'valid-company-p)
						 (amount integer #'positive-p)
						 (debit-acc string #'acc-exists-p)
						 (credit-acc string #'acc-exists-p))
    ("actions/transaction/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params) 
	(with-parameter-rebinding #'val 
	  (let ((company-id (company-id company))
		(debit-acc-id (account-id debit-acc))
		(credit-acc-id (account-id credit-acc)))
	    (with-db
	      (insert-dao (make-instance 'tx
					 :tx-date tx-date
					 :description description
					 :company-id company-id 
					 :amount amount
					 :credit-acc-id credit-acc-id
					 :debit-acc-id debit-acc-id))
	      (see-other (transactions)))))
	(with-parameter-rebinding #'raw 
	  (see-other (transaction/create :tx-date tx-date
					:description description
					:company company 
					:amount amount
					:debit-acc debit-acc
					:credit-acc credit-acc))))))

(define-dynamic-page actions/transaction/delete ((tx-id integer #'valid-tx-id-p t))
    ("actions/transaction/delete" :request-type :post)
  (no-cache)
  (if (validp tx-id)
      (with-db
	(let ((tx (get-dao 'tx (val tx-id))))
	  (if (eql :null (src-id tx))
	      ;; Ok, this tx is not autogenerated, delete it
	      (progn
		(delete-dao tx) 
		(see-other (transactions)))
	      ;; This /is/ auto-generated, redirect to the error page
	      (see-other (error-page)))))
      (see-other (transaction/notfound))))

(define-dynamic-page actions/transaction/update ((tx-id integer #'valid-tx-id-p)
						 (tx-date date)
						 (description string)
						 (company string #'valid-company-p)
						 (amount integer #'positive-p)
						 (debit-acc string #'acc-exists-p)
						 (credit-acc string #'acc-exists-p))
    ("actions/transaction/update" :request-type :post)
  (no-cache) 
  (with-parameter-list params 
    (if (every #'validp params) 
	(with-parameter-rebinding #'val
	  (let ((company-id (company-id company))
		(debit-acc-id (account-id debit-acc))
		(credit-acc-id (account-id credit-acc)))
	    (with-db
	      (execute (:update 'tx :set
				'tx-date tx-date
				'description description
				'company-id company-id
				'amount amount
				'debit-acc-id debit-acc-id
				'credit-acc-id credit-acc-id
				:where (:= 'id tx-id)))
	      (see-other (transactions :tx-id tx-id)))))
	(with-parameter-rebinding #'raw
	  (see-other (transaction/update :tx-id tx-id
					:tx-date tx-date
					:description description
					:company company 
					:amount amount
					:debit-acc debit-acc
					:credit-acc credit-acc))))))


;;; ------------------------------------------------------------

(define-dynamic-page transactions ((tx-id integer #'valid-tx-id-p))
    ("transactions/")
  (no-cache)
  (if (validp tx-id)
      (with-parameter-rebinding #'val
	(with-page ()
	  (:head
	   (:title "Συναλλαγές")
	   (css-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'transactions)) 
	   (:div :id "body" 
		 (:div :id "transactions" :class "window" 
		       (apply #'tx-menu tx-id (if (manual-tx-p tx-id)
						  '(:create :update :delete)
						  '(:create))) 
		       (:h2 "Κατάλογος συναλλαγών") 
		       (tx-table tx-id 'view))
		 (footer)))))
      (see-other (transaction/notfound))))

(define-dynamic-page transaction/create ((tx-date     date)
					 (description string)
					 (company     string  #'valid-company-p)
					 (amount      integer #'positive-p)
					 (debit-acc   string  #'acc-exists-p)
					 (credit-acc  string  #'acc-exists-p))
    ("transaction/create")
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-list params 
	  (with-page ()
	    (:head
	     (:title "Συναλλαγές")
	     (css-standard-headers)
	     (js-standard-headers))
	    (:body
	     (:div :id "header"
		   (logo)
		   (primary-navbar 'transactions)) 
	     (:div :id "body" 
		   (:div :id "transactions" :class "window"
			 (tx-menu nil)
			 (:h2 "Δημιουργία συναλλαγής") 
			 (tx-table nil 'create params))
		   (footer)))))
	(see-other (transaction/notfound)))))

(define-dynamic-page transaction/update ((tx-id       integer #'valid-tx-id-p t)
					 (tx-date     date)
					 (description string)
					 (company     string  #'valid-company-p)
					 (amount      integer #'positive-p)
					 (debit-acc   string  #'acc-exists-p)
					 (credit-acc  string  #'acc-exists-p))
    ("transaction/update")
  (no-cache)
  (if (and (validp tx-id) (manual-tx-p (val tx-id)))
      (with-parameter-list params
	(with-page ()
	  (:head
	   (:title "Συναλλαγές")
	   (css-standard-headers)
	   (js-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'transactions)) 
	   (:div :id "body"
		 #|(tx-errorbar bank company amount status due-date)|#
		 (:div :id "transactions" :class "window"
		       (tx-menu (val tx-id) :view :delete)
		       (:h2 "Επεξεργασία συναλλαγής")
		       ;; first of params list is id, which we ignore 
		       (tx-table (val tx-id) 'update (rest params)))
		 (footer)))))
      (see-other (error-page))))

(define-dynamic-page transaction/delete ((tx-id integer #'valid-tx-id-p t))
    ("transaction/delete")
  (no-cache)
  (if (and (validp tx-id) (manual-tx-p (val tx-id)))
      (with-parameter-rebinding #'val
	(with-page ()
	  (:head
	   (:title "Συναλλαγές")
	   (css-standard-headers)
	   (js-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'transactions)) 
	   (:div :id "body" 
		 (:div :id "transactions" :class "window"
		       (tx-menu tx-id :view :update)
		       (:h2 "Διαγραφή συναλλαγής")
		       (tx-table tx-id 'delete))
		 (footer)))))
      (see-other (error-page))))

(defun tx-table (active-id intent &optional params)
  (flet ((normal-row (tx-id data aux &optional activep)
	   (bind (((tx-date description company amount debit-acc credit-acc) data)
		  ((company-id) aux)) 
	     (with-html
	       (:tr :class (if activep "active" nil)
		    (:td :class "select"
			 (if activep
			     (htm (:a :href (transactions)
				      (:img :src (url "img/bullet_red.png"))))
			     (htm (:a :href (transactions :tx-id tx-id)
				      (:img :src (url "img/bullet_blue.png"))))))
		    (:td :class "data" (str (lisp-to-html tx-date)))
		    (:td :class "data" (str (lisp-to-html description)))
		    (:td :class "data"
			 (:a :class "data" :href (company/view :id company-id)
			     (str (lisp-to-html company))))
		    (:td :class "data" (str (lisp-to-html amount))) 
		    (:td :class "data" (str (lisp-to-html debit-acc)))
		    (:td :class "data" (str (lisp-to-html credit-acc)))
		    (:td :class "button" "")
		    (:td :class "button" "")))))
	 (form-row-create (data styles)
	   (bind (((tx-date description company amount debit-acc credit-acc) data)
		  ((tx-date% description% company% amount% debit-acc% credit-acc%) styles)) 
	     (with-form (actions/transaction/create)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (transactions)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (textbox 'tx-date :value tx-date :style tx-date%))
		    (:td :class "data" (textbox 'description :value description :style description%))
		    (:td :class "data" (textbox 'company :value company :style company%))
		    (:td :class "data" (textbox 'amount :value amount :style amount%))
		    (:td :class "data" (textbox 'debit-acc :value debit-acc :style debit-acc%))
		    (:td :class "data" (textbox 'credit-acc :value credit-acc :style credit-acc%)) 
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (transactions)))))))
	 (form-row-update (tx-id data styles)
	   (bind (((tx-date description company amount debit-acc credit-acc) data)
		  ((tx-date% description% company% amount% debit-acc% credit-acc%) styles)) 
	     (with-form (actions/transaction/update :tx-id tx-id)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (transactions :tx-id tx-id)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (textbox 'tx-date :value tx-date :style tx-date%))
		    (:td :class "data" (textbox 'description :value description :style description%))
		    (:td :class "data" (textbox 'company :value company :style company%))
		    (:td :class "data" (textbox 'amount :value amount :style amount%))
		    (:td :class "data" (textbox 'debit-acc :value debit-acc :style debit-acc%))
		    (:td :class "data" (textbox 'credit-acc :value credit-acc :style credit-acc%))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (transactions :tx-id tx-id)))))))
	 (form-row-delete (tx-id data aux)
	   (bind (((tx-date description company amount debit-acc credit-acc) data)
		  ((company-id) aux))
	     (with-form (actions/transaction/delete :tx-id tx-id)
	       (:tr :class "attention"
		    (:td :class "select"
			 (:a :href (transactions :tx-id tx-id)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (str (lisp-to-html tx-date)))
		    (:td :class "data" (str (lisp-to-html description)))
		    (:td :class "data"
			 (:a :class "data" :href (companies :id company-id)
			     (str (lisp-to-html company))))
		    (:td :class "data" (str (lisp-to-html amount)))
		    (:td :class "data" (str (lisp-to-html debit-acc)))
		    (:td :class "data" (str (lisp-to-html credit-acc)))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (transactions :tx-id tx-id))))))))
    (with-db
      (let ((transactions (query (:select 'tx.id 'tx.tx-date 'tx.description 'company.title 'tx.amount
					  'debit-account.title 'credit-account.title 'tx.company-id
					  :from 'tx
					  :left-join 'company
					  :on (:= 'company.id 'tx.company-id)
					  :left-join (:as 'account 'debit-account)
					  :on (:= 'debit-account.id 'tx.debit-acc-id)
					  :left-join (:as 'account 'credit-account)
					  :on (:= 'credit-account.id 'tx.credit-acc-id)))) 
	    (header '("" "Ημερομηνία" "Περιγραφή" "Εταιρία" "Ποσό"
		      "Χρέωστικός λογαρ." "Πιστωτικός λογαρ."))
	    (hstyles '("select" "data" "data" "data" "data" "data" "data" "button" "button"))
	    (inp (mapcar #'val* params))
	    (styles (mapcar (lambda (p) (if (validp p) nil "attention")) params))) 
	(with-html
	  (:table :id "transactions-table" :class "forms-in-row"
		  (:thead
		   (:tr (iter (for label in header)
			      (for sty in hstyles)
			      (htm (:th :class sty (str label))))))
		  (:tbody 
		   (when (eql intent :create)
		     (form-row-create inp styles))
		   (iter (for tx in transactions)
			 (for id = (first tx))
			 (for def = (subseq tx 1 7))
			 (for aux = (subseq tx 7))
			 (for activep = (and active-id (= active-id id))) 
			 (if activep
			     (let ((data (merge-nonnull def inp)))
			      (case intent
				(view (normal-row id data aux activep)) 
				(update (form-row-update id data styles))
				(delete (form-row-delete id data aux))))
			     (normal-row id def aux))))))))))

(define-menu tx-menu (tx-id) ()
  (:create (lambda () 
	     (with-html
	       (:li (:a :href (transaction/create)
			(:img :src (url "img/add.png")) "Δημιουργία")))))
  (:view (lambda () 
	   (with-html
	     (:li (:a :href (transactions :tx-id tx-id)
		      (:img :src (url "img/magnifier.png")) "Προβολή")))))
  (:update (lambda ()
	     (with-html
	       (:li (:a :href (transaction/update :tx-id tx-id)
			(:img :src (url "img/pencil.png")) "Επεξεργασία")))))
  (:delete (lambda ()
	     (with-html
	       (:li (:a :href (transaction/delete :tx-id tx-id)
			(:img :src (url "img/delete.png")) "Διαγραφή"))))))

(define-dynamic-page transaction/notfound () ("transaction/notfound")
  (no-cache)
  (with-page ()
    (:head
     (:title "Άγνωστη συναλλαγή")
     (css-standard-headers))
    (:body
     (:div :id "header"
	   (logo)
	   (primary-navbar 'transactions))
     (:div :id "body"
	   (:div :id "content" :class "window"
		 (:p "Η συναλλαγή που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
		 (:p "Επιστρέψτε στο μενού των συναλλαγών και προσπαθήστε ξανά."))))))


#|(defun tx-errorbar (company amount tx-date)
  (unless (every #'validp (list bank company amount status due-date))
    (with-html
      (:div :id "msg"
	    (:ul :class "errorbar" 
		 (unless (validp company)
		   (htm (:li "Άκυρο όνομα εταιρίας")))
		 (unless (validp amount)
		   (htm (:li "Άκυρο ποσό"))) 
		 (unless (validp tx-date)
		   (htm (:li "Άκυρη ημερομηνία"))))))))|#