(in-package :scrooge)

(defun get-account-id (title)
  (with-db
    (query (:select 'id :from 'account :where (:= 'title title))
	   :single)))

(define-dynamic-page actions/transaction/create ((tx-date date)
						 (description string)
						 (company string #'valid-company-p)
						 (amount integer #'positive-p)
						 (debit-acc string #'account-exists-p)
						 (credit-acc string #'account-exists-p))
    ("actions/transaction/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params) 
	(with-parameter-rebinding #'val 
	  (let ((company-id (get-company-id company))
		(debit-acc-id (get-account-id debit-acc))
		(credit-acc-id (get-account-id credit-acc)))
	    (with-db
	      (insert-dao (make-instance 'tx
					 :tx-date tx-date
					 :description description
					 :company-id company-id 
					 :amount amount
					 :credit-acc-id credit-acc-id
					 :debit-acc-id debit-acc-id))
	      (redirect (transactions) :code +http-see-other+))))
	(with-parameter-rebinding #'raw 
	  (redirect (transaction/create :tx-date tx-date
					:description description
					:company company 
					:amount amount
					:debit-acc debit-acc
					:credit-acc credit-acc)
		    :code +http-see-other+)))))

(define-dynamic-page actions/transaction/delete ((tx-id integer #'valid-tx-id-p))
    ("actions/transaction/delete" :request-type :post)
  (no-cache)
  (if (validp tx-id)
      (with-db
	(delete-dao (get-dao 'tx (val tx-id))) 
	(redirect (transactions) :code +http-see-other+))
      (redirect (transaction/notfound) :code +http-see-other+)))

(define-dynamic-page actions/transaction/update ((tx-id integer #'valid-tx-id-p)
						 (tx-date date)
						 (description string)
						 (company string #'valid-company-p)
						 (amount integer #'positive-p))
    ("actions/transaction/update" :request-type :post)
  (no-cache) 
  (with-parameter-list params 
    (if (every #'validp params) 
	(with-parameter-rebinding #'val
	  (let ((company-id (get-company-id company)))
	    (with-db
	      (execute (:update 'tx :set
				'tx-date tx-date
				'description description
				'company-id company-id
				'amount amount 
				:where (:= 'id tx-id)))
	      (redirect (transactions :tx-id tx-id) :code +http-see-other+))))
	(with-parameter-rebinding #'raw
	  (redirect (transaction/update :tx-id tx-id
					:tx-date tx-date
					:description description
					:company company 
					:amount amount)
		    :code +http-see-other+)))))


;;; ------------------------------------------------------------

(define-dynamic-page transactions ((tx-id integer #'valid-tx-id-p))
    ("transactions/")
  (no-cache)
  (if (validp tx-id) 
      (with-page ()
	(:head
	 (:title "Συναλλαγές")
	 (css "reset.css" "main.css"))
	(:body
	 (:div :id "header"
	       (logo)
	       (primary-navbar 'transactions)) 
	 (:div :id "body" 
	       (:div :id "transactions" :class "window" 
		     (tx-menu (val tx-id) :create :edit :delete) 
		     (:h2 "Κατάλογος συναλλαγών") 
		     (tx-table (val tx-id) :view))
	       (footer))))
      (redirect (transaction/notfound) :code +http-see-other+)))

(define-dynamic-page transaction/create ((tx-date     date)
					 (description string)
					 (company     string  #'valid-company-p)
					 (amount      integer #'positive-p)
					 (debit-acc   string  #'account-exists-p)
					 (credit-acc  string  #'account-exists-p))
    ("transaction/create")
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-list params 
	  (with-page ()
	    (:head
	     (:title "Συναλλαγές")
	     (css "reset.css" "main.css")
	     (js-headers))
	    (:body
	     (:div :id "header"
		   (logo)
		   (primary-navbar 'transactions)) 
	     (:div :id "body" 
		   (:div :id "transactions" :class "window"
			 (tx-menu nil)
			 (:h2 "Δημιουργία συναλλαγής") 
			 (tx-table nil :create params))
		   (footer)))))
	(redirect (transaction/notfound) :code +http-see-other+))))

(define-dynamic-page transaction/update ((tx-id       integer #'valid-tx-id-p)
					 (tx-date     date)
					 (description string)
					 (company     string  #'valid-company-p)
					 (amount      integer #'positive-p)
					 (debit-acc   string  #'account-exists-p)
					 (credit-acc  string  #'account-exists-p))
    ("transaction/update")
  (no-cache)
  (if (validp tx-id)
      (with-parameter-list params
	(with-page ()
	  (:head
	   (:title "Συναλλαγές")
	   (css "reset.css" "main.css")
	   (js-headers))
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
		       (tx-table (val tx-id) :edit (rest params)))
		 (footer)))))
      (redirect (transaction/notfound) :code +http-see-other+)))

(define-dynamic-page transaction/delete ((tx-id integer #'valid-tx-id-p))
    ("transaction/delete")
  (no-cache)
  (if (validp tx-id)
      (with-page ()
	(:head
	 (:title "Συναλλαγές")
	 (css "reset.css" "main.css")
	 (js-headers))
	(:body
	 (:div :id "header"
	       (logo)
	       (primary-navbar 'transactions)) 
	 (:div :id "body" 
	       (:div :id "transactions" :class "window"
		     (tx-menu (val tx-id) :view :edit)
		     (:h2 "Διαγραφή συναλλαγής")
		     (tx-table (val tx-id) :delete))
	       (footer))))
      (redirect (transaction/notfound) :code +http-see-other+)))

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
	 (form-row-create (params)
	   (bind (((tx-date description company amount debit-acc credit-acc) (mapcar #'val* params))
		  ((tx-date% description% company% amount% debit-acc% credit-acc%)
		   (mapcar #'(lambda (p) (if (validp p) nil "attention")) params))) 
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
	 (form-row-update (tx-id data params)
	   (bind (((tx-date description company amount debit-acc credit-acc)
		   (mapcar (lambda (param default)
			     (cond ((not (suppliedp param)) default)
				   ((validp param) (val param))
				   (t (raw param))))
			   params data))
		  ((tx-date% description% company% amount% debit-acc% credit-acc%)
		   (mapcar #'(lambda (p) (if (validp p) nil "attention")) params))) 
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
		      "Χρέωστικός λογαρ." "Πιστωτικός λογαρ."))) 
	(with-html
	  (:table :id "transactions-table" :class "forms-in-row"
		  (:thead
		   (:tr (iter (for label in header)
			      (htm (:th (str label))))))
		  (:tbody 
		   (when (eql intent :create)
		     (form-row-create params))
		   (iter (for tx in transactions)
			 (for id = (first tx))
			 (for data = (subseq tx 1 7))
			 (for aux = (subseq tx 7))
			 (for activep = (and active-id (= active-id id))) 
			 (if activep
			     (case intent
			       (:view (normal-row id data aux activep)) 
			       (:edit (form-row-update id data params))
			       (:delete (form-row-delete id data aux)))
			     (normal-row id data aux))))))))))

(defun tx-menu (tx-id &rest opt-list)
  (let ((options
	 (list :create (lambda () 
			 (with-html
			   (:li (:a :href (transaction/create)
				    (:img :src (url "img/add.png")) "Δημιουργία"))))
	       :view (lambda () 
		       (if tx-id
			   (with-html
			     (:li (:a :href (transactions :tx-id tx-id)
				      (:img :src (url "img/magnifier.png")) "Προβολή")))
			   nil))
	       :edit (lambda ()
		       (if tx-id
			   (with-html
			     (:li (:a :href (transaction/update :tx-id tx-id)
				      (:img :src (url "img/pencil.png")) "Επεξεργασία")))
			   nil))
	       :delete (lambda ()
			 (if tx-id
			     (with-html
			       (:li (:a :href (transaction/delete :tx-id tx-id)
					(:img :src (url "img/delete.png")) "Διαγραφή")))
			     nil)))))
    (with-html
      (:div :class "actions"
	    (:ul :class "hmenu"
		 (iter (for opt in opt-list)
		       (funcall (getf options opt))))))))

(define-dynamic-page transaction/notfound () ("transaction/notfound")
  (no-cache)
  (with-page ()
    (:head
     (:title "Άγνωστη συναλλαγή")
     (css "reset.css" "main.css"))
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