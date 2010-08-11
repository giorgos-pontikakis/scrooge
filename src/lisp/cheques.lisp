(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))


;;; ------------------------------------------------------------
;;; Actions
;;; ------------------------------------------------------------

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
	    (let* ((status "pending") ;; default status for new cheques
		   (bank-id (bank-id bank))
		   (company-id (company-id company))
		   (stran-data (query (:select 'debit-acc-id 'credit-acc-id 'description
                                               :from 'cheque-stran
                                               :where (:and (:is-null 'old-status)
                                                            (:= 'new-status status)))
                                      :plist)))
	      (if stran-data
		  (with-transaction ()
                    ;; Store the dao of the new cheque, we need its id
                    ;; for the auto-generated transaction below
                    (let ((cheque-dao (insert-dao (make-instance 'cheque
                                                                 :bank-id bank-id
                                                                 :company-id company-id
                                                                 :due-date due-date
                                                                 :amount amount
                                                                 :status status
                                                                 :payable-p payable-p))))
                      (insert-dao (make-instance 'tx
                                                 :tx-date (current-date)
                                                 :description (getf stran-data :description)
                                                 :company-id company-id
                                                 :amount amount
                                                 :credit-acc-id (getf stran-data :credit-acc-id)
                                                 :debit-acc-id (getf stran-data :debit-acc-id)
                                                 :src-tbl "cheque"
                                                 :src-id (id cheque-dao)))))
		  (redirect (notfound) :code +http-see-other+)))))
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
	    (let* ((bank-id (bank-id bank))
		   (company-id (company-id company)))
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
		   (stran-data (query (:select 'debit-acc-id 'credit-acc-id 'description
					     :from 'cheque-stran
					     :where (:and (:= 'old-status old-status)
							  (:= 'new-status new-status)))
				    :row)))
	      (if stran-data 
		  (destructuring-bind (debit-acc-id credit-acc-id description) stran-data
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
		  (redirect (notfound) :code +http-see-other+)))))
	(with-parameter-rebinding #'raw
	  (redirect (cheque/chstat :cheque-id cheque-id
				   :new-status new-status)
		    :code +http-see-other+)))))

;;; ------------------------------------------------------------
;;; Definition
;;; ------------------------------------------------------------

(define-page-group 'cheques
    :id-keys '(:id)
    :data-keys '(:bank :due-date :company :amount :status) 
    :filter-keys '(:payable-p)
    :post-urls (list :create 'actions/cheque/create
                     :update 'actions/cheque/update
                     :delete 'actions/cheque/delete
                     :chstat 'actions/cheque/chstat) 
    :get-urls (list :view 'cheques
                    :create 'cheque/create
                    :update 'cheque/update
                    :delete 'cheque/delete
                    :chstat 'cheque/chstat))

(define-widget 'cheques-table 'table-inline-form 
  :id-keys '(:id)
  :data-keys '(:bank :due-date :company :amount :status) 
  :filter-keys '(:payable-p)
  :post-urls (list :create 'actions/cheque/create
                   :update 'actions/cheque/update
                   :delete 'actions/cheque/delete
                   :chstat 'actions/cheque/chstat) 
  :get-urls (list :view 'cheques
                  :create 'cheque/create
                  :update 'cheque/update
                  :delete 'cheque/delete
                  :chstat 'cheque/chstat)
  :page-group-name 'cheques 
  :data-header '("Τράπεζα" "Εταιρία" "Ποσόν" "Κατάσταση" "Ημερομηνία πληρωμής") 
  :data-widgets (list :bank (make-cell-str "data")
                      :due-date (make-cell-str "data")
                      :company (make-cell-str "data")
                      :amount (make-cell-str "data")
                      :status (make-cell-str "data"))
  :table-styles '(:table-style "forms-in-row"))

(define-db-data (get-widget 'cheques-table)
    #'(lambda (filters)
        (with-db
          (query (:select 'cheque.id 'bank.title 'company.title
                          'cheque.amount 'cheque.due-date
                          'company-id 'cheque.status
                          :from 'cheque
                          :left-join 'company
                          :on (:= 'company.id 'cheque.company-id)
                          :left-join 'bank
                          :on (:= 'bank.id 'cheque.bank-id)
                          :where (:= 'cheque.payable-p
                                     (val* (getf filters :payable-p))))
                 :plists))))

;;; ------------------------------------------------------------
;;; Snippets
;;; ------------------------------------------------------------

(define-navbar cheque-navbar () (:id "subnavbar" :ul-style "hmenu")
  (receivable (cheques)              "Προς είσπραξη")
  (payable    (cheques :payable-p t) "Προς πληρωμή"))

(defparameter *cheque-statuses*
  (with-db
    (query (:select 'description 'status :from 'cheque-status))))

(defun cheque-payable-p (cheque-id)
  (with-db
    (query (:select 'payable-p :from 'cheque :where (:= 'id cheque-id)) :single)))

(defun next-statuses (cheque-id)
  (if cheque-id
      (with-db
	(mapcar (compose #'make-keyword #'string-upcase)
		(query (:select 'new-status
				:from 'cheque-stran
				:where (:= 'old-status
					   (:select 'status
						    :from 'cheque
						    :where (:= 'id cheque-id))))
		       :column)))
      nil))




;;; ------------------------------------------------------------
;;; Pages
;;; ------------------------------------------------------------
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
      (with-parameter-list params
        (let ((next-statuses (next-statuses (val cheque-id)))) 
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
                               (apply #'cheque-menu (val cheque-id) (val payable-p)
                                      :create :update :delete next-statuses)
                               (:h2 "Κατάλογος επιταγών") 
                               (render (get-widget 'cheques-table) params :view))
                         (footer)))))))
      (redirect (cheque/notfound) :code +http-see-other+)))

(define-dynamic-page cheque/create ((payable-p boolean)
				    (bank      string  #'valid-bank-p)
				    (company   string  #'valid-company-p)
				    (amount    integer #'positive-p) 
				    (due-date  date    #'valid-due-date-p))
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
		   (cheque-menu nil (val payable-p))
		   (:h2 "Δημιουργία επιταγής")
                   (render (get-widget 'cheques-table) params :create))
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
				      payable-p
				      :view :delete)
			 (:h2 "Επεξεργασία επιταγής")
			 (render (get-widget 'cheques-table) :params params :intent :update))
		   (footer))))))
      (redirect (cheque/notfound) :code +http-see-other+)))

(define-dynamic-page cheque/delete ((cheque-id integer #'valid-cheque-id-p t))
    ("cheque/delete")
  (no-cache)
  (if (validp cheque-id)
      (with-parameter-list params
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
                         (cheque-menu (val cheque-id) payable-p :view :update)
                         (:h2 "Διαγραφή επιταγής")
                         (render (get-widget 'cheques-table) :params params :intent :delete))
                   (footer))))))
      (redirect (cheque/notfound) :code +http-see-other+)))

(define-dynamic-page cheque/chstat ((cheque-id integer #'valid-cheque-id-p t)
				    (new-status symbol #'valid-cheque-status-p t))
    ("cheque/chstat")
  (no-cache)
  (if (and (validp cheque-id) (validp new-status))
      (with-parameter-list params
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
                         (cheque-menu (val cheque-id) (val payable-p) :view)
                         (:h2 (str (case new-status
                                     (paid "Πληρωμή επιταγής")
                                     (bounced "Σφράγισμα επιταγής")
                                     (returned "Επιστροφή επιταγής"))))
                         (render (get-widget 'cheques-table) :params params :intent :chstat))
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

(define-menu cheque-menu (cheque-id payable-p) ()
  (:create (with-html
	     (:li (:a :href (cheque/create :payable-p payable-p)
		      (:img :src (url "img/add.png")) "Δημιουργία")))) 
  (:paid (with-html
	   (:li (:a :href (cheque/chstat :cheque-id cheque-id :new-status 'paid)
		    (:img :src (url "img/magnifier.png")) "Πληρωμή"))))
  (:bounced (with-html
	      (:li (:a :href (cheque/chstat :cheque-id cheque-id :new-status 'bounced)
		       (:img :src (url "img/magnifier.png")) "Σφράγισμα"))))
  (:returned (with-html
	       (:li (:a :href (cheque/chstat :cheque-id cheque-id :new-status 'returned)
			(:img :src (url "img/magnifier.png")) "Επιστροφή"))))
  (:view (with-html
	   (:li (:a :href (cheques :cheque-id cheque-id :payable-p payable-p)
		    (:img :src (url "img/magnifier.png")) "Προβολή"))))
  (:update (with-html
	     (:li (:a :href (cheque/update :cheque-id cheque-id)
		      (:img :src (url "img/pencil.png")) "Επεξεργασία")))) 
  (:delete (with-html
	     (:li (:a :href (cheque/delete :cheque-id cheque-id)
		      (:img :src (url "img/delete.png")) "Διαγραφή")))))

(define-errorbar cheque-errorbar ()
  (bank "Άκυρο όνομα τράπεζας")
  (company "Άκυρο όνομα εταιρίας")
  (amount "Άκυρο ποσό") 
  (due-date "Άκυρη ημερομηνία"))


