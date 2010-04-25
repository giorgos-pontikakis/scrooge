(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

(define-navbar cheque-navbar () (:id "subnavbar" :style "hmenu")
  (receivable (cheques)              "Προς είσπραξη")
  (payable    (cheques :payable-p t) "Προς πληρωμή"))

(defparameter *cheque-statuses*
  (with-db
    (query (:select 'description 'status :from 'cheque-status))))

(defun get-bank-id (title)
  (with-db
    (query (:select 'id :from 'bank :where (:= 'title title))
	   :single)))

(defun get-company-id (title)
  (with-db
    (query (:select 'id :from 'company :where (:= 'title title))
	   :single)))

(defun cheque-payable-p (cheque-id)
  (with-db
    (query (:select 'payable-p :from 'cheque :where (:= 'id cheque-id)) :single)))

(defun next-statuses (cheque-id)
  (if cheque-id
      (with-db
	(mapcar (compose #'make-keyword #'string-upcase)
		(query (:select 'new-status
				:from 'cheque-fsm
				:where (:= 'old-status
					   (:select 'status
						    :from 'cheque
						    :where (:= 'id cheque-id))))
		       :column)))
      nil))

;;; Actions

(define-dynamic-page actions/cheque/create ((payable-p boolean)
					    (bank string #'valid-bank-p t)
					    (due-date date #'valid-due-date-p t)
					    (company string #'valid-company-p t) 
					    (amount integer #'positive-p t))
    ("actions/cheque/create" :request-type :post)
  (no-cache) 
  (with-parameter-list params
    (if (every #'validp params) 
	(with-parameter-rebinding #'val 
	  (with-db 
	    (let* ((status "pending")
		   (bank-id (get-bank-id bank))
		   (company-id (get-company-id company))
		   (fsm-data (query (:select 'debit-acc-id 'credit-acc-id 'description
					     :from 'cheque-fsm
					     :where (:and (:is-null 'old-status)
							  (:= 'new-status status)))
				    :row)))
	      (if fsm-data
		  (destructuring-bind (debit-acc-id credit-acc-id description) fsm-data
		    (with-transaction ()
		      ;; Store the dao of the new cheque, we need its id
		      ;; for the auto-generated transaction 
		      (let ((cheque-dao (insert-dao (make-instance 'cheque
								   :bank-id bank-id
								   :company-id company-id
								   :due-date due-date
								   :amount amount
								   :status status
								   :payable-p payable-p))))
			(insert-dao (make-instance 'tx
						   :tx-date (current-date)
						   :description description
						   :company-id company-id
						   :amount amount
						   :credit-acc-id credit-acc-id
						   :debit-acc-id debit-acc-id
						   :src-tbl "cheque"
						   :src-id (id cheque-dao)))))
		    (redirect (cheques :payable-p payable-p) :code +http-see-other+))
		  (redirect (no-fsm-data) :code +http-see-other+)))))
	(with-parameter-rebinding #'raw 
	  (redirect (cheque/create :bank bank
				   :company company
				   :due-date due-date
				   :amount amount 
				   :payable-p payable-p)
		    :code +http-see-other+)))))

(define-dynamic-page actions/cheque/delete ((cheque-id integer #'valid-cheque-id-p t))
    ("actions/cheque/delete" :request-type :post)
  (no-cache)
  (if (validp cheque-id)
      (with-parameter-rebinding #'val
	(with-db
	  (with-transaction ()
	    ;; First update /all/ transactions that have originated
	    ;; from the cheque with this particular cheque-id
	    (execute (:delete-from 'tx :where (:and (:= 'src-tbl "cheque")
						    (:= 'src-id cheque-id))))
	    ;; Then delete the cheque itself 
	    (execute (:delete-from 'cheque :where (:= 'id cheque-id)))) 
	  (redirect (cheques :payable-p (cheque-payable-p cheque-id))
		    :code +http-see-other+)))
      (redirect (cheque/notfound) :code +http-see-other+)))

(define-dynamic-page actions/cheque/update ((cheque-id integer #'valid-cheque-id-p t)
					    (bank string #'valid-bank-p t)
					    (company string #'valid-company-p t)
					    (due-date date #'valid-due-date-p t)
					    (amount integer #'positive-p t))
    ("actions/cheque/update" :request-type :post)
  (no-cache) 
  (with-parameter-list params 
    (if (every #'validp params) 
	(with-parameter-rebinding #'val
	  (with-db 
	    (let* ((bank-id (get-bank-id bank))
		   (company-id (get-company-id company)))
	      (with-transaction ()
		;; First update /all/ transactions that have originated
		;; from the cheque with this particular cheque-id
		(execute (:update 'tx :set 
				  'company-id company-id 
				  'amount amount
				  :where (:and (:= 'src-tbl "cheque")
					       (:= 'src-id cheque-id))))
		;; Then update the cheque itself
		(execute (:update 'cheque :set
				  'bank-id bank-id
				  'company-id company-id
				  'due-date due-date
				  'amount amount 
				  :where (:= 'id cheque-id))))
	      (redirect (cheques :cheque-id cheque-id :payable-p (cheque-payable-p cheque-id))
			:code +http-see-other+))))
	(with-parameter-rebinding #'raw
	  (redirect (cheque/update :cheque-id cheque-id
				   :bank bank
				   :company company
				   :due-date due-date
				   :amount amount)
		    :code +http-see-other+)))))

(define-dynamic-page actions/cheque/chstat ((cheque-id integer #'valid-cheque-id-p)
					    (new-status string))
    ("actions/cheque/chstat" :request-type :post)
  (no-cache)
  (with-parameter-list params 
    (if (every #'validp params) 
	(with-parameter-rebinding #'val
	  (with-db 
	    (bind (((old-status company-id amount) (query (:select 'status 'company-id 'amount
								   :from 'cheque
								   :where (:= 'id cheque-id))
							  :row))
		   (fsm-data (query (:select 'debit-acc-id 'credit-acc-id 'description
					     :from 'cheque-fsm
					     :where (:and (:= 'old-status old-status)
							  (:= 'new-status new-status)))
				    :row)))
	      (if fsm-data 
		  (destructuring-bind (debit-acc-id credit-acc-id description) fsm-data
		    (with-transaction ()
		      ;; Create a new transaction which points to our cheque
		      (execute (:insert-into 'tx :set
					     'tx-date (current-date)
					     'description description
					     'company-id company-id
					     'amount amount
					     'credit-acc-id credit-acc-id
					     'debit-acc-id debit-acc-id
					     'src-tbl "cheque"
					     'src-id cheque-id))
		      ;; Then update the cheque itself
		      (execute (:update 'cheque :set 
					'status new-status
					:where (:= 'id cheque-id))))
		    (redirect (cheques :cheque-id cheque-id :payable-p (cheque-payable-p cheque-id))
			      :code +http-see-other+))
		  (redirect (no-fsm-data) :code +http-see-other+)))))
	(with-parameter-rebinding #'raw
	  (redirect (cheque/chstat :cheque-id cheque-id
				   :new-status new-status)
		    :code +http-see-other+)))))

;; Pages

(define-dynamic-page cheques ((cheque-id integer #'valid-cheque-id-p)
			      (payable-p boolean))
    ("cheques")
  (no-cache) 
  (if (and (validp cheque-id)
	   (validp payable-p)
	   (if (and (suppliedp cheque-id) (suppliedp payable-p))
	       (eql (cheque-payable-p (val cheque-id))
		    (val payable-p))
	       t))
      (with-parameter-rebinding #'val
	(let ((next-statuses (next-statuses cheque-id))) 
	  (with-page () 
	    (:head
	     (:title "Επιταγές")
	     (css-standard-headers))
	    (:body
	     (:div :id "header"
		   (logo)
		   (primary-navbar 'cheques)
		   (cheque-navbar (if payable-p 'payable 'receivable)) 
		   (:div :id "body" 
			 (:div :id "cheques" :class "window"
			       (cheque-menu cheque-id
					    (append (list :create :update :delete)
						    next-statuses) 
					    payable-p) 
			       (:h2 "Κατάλογος επιταγών") 
			       (cheques-table cheque-id
					      'view
					      :payable-p payable-p))
			 (footer)))))))
      (redirect (cheque/notfound) :code +http-see-other+)))

(define-dynamic-page cheque/create ((payable-p boolean)
				    (bank string #'valid-bank-p)
				    (company string #'valid-company-p)
				    (amount integer #'positive-p) 
				    (due-date date #'valid-due-date-p))
    ("cheque/create")
  (no-cache)
  (with-parameter-list params 
    (with-page ()
      (:head
       (:title "Επιταγές")
       (css-standard-headers)
       (js-standard-headers))
      (:body
       (:div :id "header"
	     (logo)
	     (primary-navbar 'cheques)
	     (cheque-navbar (if (val payable-p) 'payable 'receivable)))
       (:div :id "body"
	     (cheque-errorbar bank company amount due-date)
	     (:div :id "cheques" :class "window"
		   (cheque-menu nil nil (val payable-p))
		   (:h2 "Δημιουργία επιταγής") 
		   (cheques-table nil
				  'create
				  :params (rest params)
				  :payable-p (val payable-p)))
	     (footer))))))

(define-dynamic-page cheque/update ((cheque-id integer #'valid-cheque-id-p t) 
				    (bank string #'valid-bank-p)
				    (company string #'valid-company-p)
				    (amount integer #'positive-p) 
				    (due-date date #'valid-due-date-p))
    ("cheque/update")
  (no-cache)
  (if (validp cheque-id)
      (let ((payable-p (cheque-payable-p (val cheque-id))))
	(with-parameter-list params
	  (with-page ()
	    (:head
	     (:title "Επιταγές")
	     (css-standard-headers)
	     (js-standard-headers))
	    (:body
	     (:div :id "header"
		   (logo)
		   (primary-navbar 'cheques)
		   (cheque-navbar (if payable-p 'payable 'receivable))) 
	     (:div :id "body"
		   (cheque-errorbar bank company amount due-date)
		   (:div :id "cheques" :class "window"
			 (cheque-menu (val cheque-id)
				      '(:view :delete)
				      payable-p)
			 (:h2 "Επεξεργασία επιταγής")
			 ;; first of params list is id, which we ignore 
			 (cheques-table (val cheque-id)
					'update
					:params (rest params)
					:payable-p payable-p))
		   (footer))))))
      (redirect (cheque/notfound) :code +http-see-other+)))

(define-dynamic-page cheque/delete ((cheque-id integer #'valid-cheque-id-p t))
    ("cheque/delete")
  (no-cache)
  (if (validp cheque-id)
      (let ((payable-p (cheque-payable-p (val cheque-id))))
	(with-page ()
	  (:head
	   (:title "Επιταγές")
	   (css-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'cheques)
		 (cheque-navbar (if payable-p 'payable 'receivable))) 
	   (:div :id "body" 
		 (:div :id "cheques" :class "window"
		       (cheque-menu (val cheque-id) '(:view :update) payable-p)
		       (:h2 "Διαγραφή επιταγής")
		       (cheques-table (val cheque-id)
				      'delete
				      :payable-p payable-p))
		 (footer)))))
      (redirect (cheque/notfound) :code +http-see-other+)))

(define-dynamic-page cheque/chstat ((cheque-id integer #'valid-cheque-id-p t)
				    (new-status symbol #'valid-cheque-status-p t))
    ("cheque/chstat")
  (no-cache)
  (if (and (validp cheque-id) (validp new-status))
      (with-parameter-rebinding #'val
	(let ((payable-p (cheque-payable-p cheque-id)))
	  (with-page ()
	    (:head
	     (:title "Επιταγές")
	     (css-standard-headers))
	    (:body
	     (:div :id "header"
		   (logo)
		   (primary-navbar 'cheques)
		   (cheque-navbar (if payable-p 'payable 'receivable))) 
	     (:div :id "body" 
		   (:div :id "cheques" :class "window"
			 (cheque-menu cheque-id '(:view) payable-p)
			 (:h2 (str (case new-status
				     (paid "Πληρωμή επιταγής")
				     (bounced "Σφράγισμα επιταγής")
				     (returned "Επιστροφή επιταγής"))))
			 (cheques-table cheque-id
					new-status
					:payable-p payable-p))
		   (footer))))))
      (redirect (cheque/notfound) :code +http-see-other+)))

(define-dynamic-page cheque/notfound () ("cheque/notfound")
  (no-cache)
  (with-page ()
    (:head
     (:title "Άγνωστη εταιρία")
     (css-standard-headers))
    (:body
     (:div :id "header"
	   (logo)
	   (primary-navbar 'companies))
     (:div :id "body"
	   (:div :id "content" :class "window"
		 (:p "Η επιταγή που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
		 (:p "Επιστρέψτε στο μενού των επιταγών και προσπαθήστε ξανά."))))))

;;; Snippets

(defun status-label (status)
  (first (find status *cheque-statuses* :key #'second :test #'string-equal)))

(defun cheques-table (active-id intent &key payable-p params)
  (flet ((normal-row (cheque-id data aux &optional activep)
	   (bind (((bank company amount due-date) data)
		  ((company-id status) aux)) 
	     (with-html
	       (:tr :class (if activep "active" nil)
		    (:td :class "select"
			 (if activep
			     (htm (:a :href (cheques :payable-p payable-p)
				      (:img :src (url "img/bullet_red.png"))))
			     (htm (:a :href (cheques :cheque-id cheque-id :payable-p payable-p)
				      (:img :src (url "img/bullet_blue.png"))))))
		    (:td :class "data" (str (lisp-to-html bank)))
		    (:td :class "data"
			 (:a :class "data" :href (company/view :id company-id)
			     (str (lisp-to-html company))))
		    (:td :class "data" (str (lisp-to-html amount)))
		    (:td :class "data" (str (lisp-to-html (status-label status))))
		    (:td :class "data" (str (lisp-to-html due-date)))
		    (:td :class "button" "")
		    (:td :class "button" "")))))
	 (form-row-create (data styles)
	   (bind (((&optional bank company amount due-date) data)
		  ((&optional bank% company% amount% due-date%) styles)
		  (status "pending")) 
	     (with-form (actions/cheque/create :payable-p payable-p)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (cheques :payable-p payable-p)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (textbox 'bank :value bank :style bank%))
		    (:td :class "data" (textbox 'company :value company :style company%))
		    (:td :class "data" (textbox 'amount :value amount :style amount%))
		    (:td :class "data" (str (lisp-to-html (status-label status))))
		    (:td :class "data" (textbox 'due-date :value due-date :style due-date%))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (cheques :payable-p payable-p)))))))
	 (form-row-update (cheque-id data aux styles) 
	   (bind (((&optional bank company amount due-date) data)
		  ((&optional bank% company% amount% due-date%) styles)
		  ((company-id status) aux))
	     (declare (ignore company-id))
	     (with-form (actions/cheque/update :cheque-id cheque-id)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (cheques :cheque-id cheque-id :payable-p payable-p)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (textbox 'bank :value bank :style bank%))
		    (:td :class "data" (textbox 'company :value company :style company%))
		    (:td :class "data" (textbox 'amount :value amount :style amount%))
		    (:td :class "data" (str (lisp-to-html (status-label status))))
		    (:td :class "data" (textbox 'due-date :value due-date :style due-date%))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (cheques :cheque-id cheque-id
								 :payable-p payable-p)))))))
	 (form-row-chstat (cheque-id data aux) 
	   (bind (((bank company amount due-date) data)
		  ((company-id status) aux)) 
	     (with-form (actions/cheque/chstat :cheque-id cheque-id :new-status intent)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (cheques :cheque-id cheque-id :payable-p payable-p)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (str (lisp-to-html bank)))
		    (:td :class "data"
			 (:a :class "data" :href (company/view :id company-id)
			     (str (lisp-to-html company)))) 
		    (:td :class "data" (str (lisp-to-html amount)))
		    (:td :class "data" (str (lisp-to-html (status-label status))))
		    (:td :class "data" (str (lisp-to-html due-date)))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (cheques :cheque-id cheque-id
								 :payable-p payable-p)))))))
	 (form-row-delete (cheque-id data aux)
	   (bind (((bank company amount due-date) data)
		  ((company-id status) aux))
	     (with-form (actions/cheque/delete :cheque-id cheque-id :payable-p payable-p)
	       (:tr :class "attention"
		    (:td :class "select"
			 (:a :href (cheques :cheque-id cheque-id)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (str (lisp-to-html bank)))
		    (:td :class "data"
			 (:a :class "data" :href (company/view :id company-id)
			     (str (lisp-to-html company))))
		    (:td :class "data" (str (lisp-to-html amount)))
		    (:td :class "data" (str (lisp-to-html (status-label status))))
		    (:td :class "data" (str (lisp-to-html due-date)))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (cheques :cheque-id cheque-id
								 :payable-p payable-p))))))))
    (with-db
      (let ((cheques (query (:select 'cheque.id 'bank.title 'company.title
				     'cheque.amount 'cheque.due-date
				     'company-id 'cheque.status
				     :from 'cheque
				     :left-join 'company
				     :on (:= 'company.id 'cheque.company-id)
				     :left-join 'bank
				     :on (:= 'bank.id 'cheque.bank-id)
				     :where (:= 'cheque.payable-p payable-p)))) 
	    (header '("" "Τράπεζα" "Εταιρία" "Ποσόν" "Κατάσταση" "Ημερομηνία πληρωμής"))
	    (hstyles '("select" "data" "data" "data" "data" "data" "button" "button"))
	    (inp (mapcar #'val* params))
	    (styles (mapcar (lambda (p) (if (validp p) nil "attention")) params))) 
	(with-html
	  (:table :id "cheques-table" :class "forms-in-row"
		  (:thead
		   (:tr (iter (for label in header)
			      (for sty in hstyles)
			      (htm (:th :class sty (str label))))))
		  (:tbody 
		   (when (eql intent 'create)
		     (form-row-create inp styles))
		   (iter (for row in cheques)
			 (for id = (first row))
			 (for def = (subseq row 1 5))
			 (for aux = (subseq row 5 7))
			 (for activep = (and active-id (= active-id id))) 
			 (if activep 
			     (let ((data (merge-nonnull def inp)))
			       (case intent
				 (view (normal-row id data aux activep)) 
				 (update (form-row-update id data aux styles))
				 (delete (form-row-delete id data aux))
				 ((paid bounced returned) (form-row-chstat id data aux))))
			     (normal-row id def aux))))))))))


(defun cheque-menu (cheque-id opt-list payable-p)
  (let ((options
	 (list :create (lambda ()
			 (with-html
			   (:li (:a :href (cheque/create :payable-p payable-p)
				    (:img :src (url "img/add.png")) "Δημιουργία")))) 
	       :paid (lambda () 
		      (with-html
			(:li (:a :href (cheque/chstat :cheque-id cheque-id :new-status 'paid)
				 (:img :src (url "img/magnifier.png")) "Πληρωμή"))))
	       :bounced (lambda () 
			 (with-html
			   (:li (:a :href (cheque/chstat :cheque-id cheque-id :new-status 'bounced)
				    (:img :src (url "img/magnifier.png")) "Σφράγισμα"))))
	       :returned (lambda () 
			 (with-html
			   (:li (:a :href (cheque/chstat :cheque-id cheque-id :new-status 'returned)
				    (:img :src (url "img/magnifier.png")) "Επιστροφή"))))
	       :view (lambda () 
		       (with-html
			 (:li (:a :href (cheques :cheque-id cheque-id :payable-p payable-p)
				  (:img :src (url "img/magnifier.png")) "Προβολή"))))
	       :update (lambda ()
			 (with-html
			   (:li (:a :href (cheque/update :cheque-id cheque-id)
				    (:img :src (url "img/pencil.png")) "Επεξεργασία")))) 
	       :delete (lambda ()
			 (with-html
			   (:li (:a :href (cheque/delete :cheque-id cheque-id)
				    (:img :src (url "img/delete.png")) "Διαγραφή")))))))
    (with-html
      (:div :class "actions"
	    (:ul :class "hmenu"
		 (iter (for opt in opt-list)
		       (funcall (getf options opt))))))))


(defun cheque-errorbar (bank company amount due-date)
  (unless (every #'validp (list bank company amount due-date))
    (with-html
      (:div :id "msg"
	    (:ul :class "errorbar"
		 (unless (validp bank)
		   (htm (:li "Άκυρο όνομα τράπεζας")))
		 (unless (validp company)
		   (htm (:li "Άκυρο όνομα εταιρίας")))
		 (unless (validp amount)
		   (htm (:li "Άκυρο ποσό")))
		 #|(unless (validp status)
		   (htm (:li "Άκυρη κατάσταση επιταγής")))|#
		 (unless (validp due-date)
		   (htm (:li "Άκυρη ημερομηνία"))))))))


