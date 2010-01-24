(in-package :scrooge)

(defun get-bank-id (title)
  (with-db
    (query (:select 'id :from 'bank :where (:= 'title title))
	   :single)))

(defun get-company-id (title)
  (with-db
    (query (:select 'id :from 'company :where (:= 'title title))
	   :single)))

(define-dynamic-page actions/cheque/create ((bank string #'valid-bank-p)
					    (due-date date #'valid-due-date-p)
					    (company string #'valid-company-p)
					    (status string)
					    (amount integer #'positive-p))
    ("actions/cheque/create")
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params) 
	(with-parameter-rebinding #'val 
	  (let ((bank-id (get-bank-id (val bank)))
		(company-id (get-company-id (val company))))
	    (with-db
	      (insert-dao (make-instance 'cheque
					 :bank-id bank-id
					 :company-id company-id
					 :due-date due-date
					 :amount amount
					 :status status))
	      (redirect (cheques) :code +http-see-other+))))
	(with-parameter-rebinding #'raw 
	  (redirect (cheque/create :bank bank
				   :company company
				   :due-date due-date
				   :amount amount
				   :status status)
		    :code +http-see-other+)))))

(define-dynamic-page actions/cheque/delete ((cheque-id integer #'valid-cheque-id-p))
    ("actions/cheque/delete" :request-type :post)
  (no-cache)
  (if (validp cheque-id)
      (with-db
	(delete-dao (get-dao 'cheque (val cheque-id))) 
	(redirect (cheques) :code +http-see-other+))
      (redirect (cheque/notfound) :code +http-see-other+)))

(define-dynamic-page actions/cheque/update ((cheque-id integer #'valid-cheque-id-p)
					    (bank string #'valid-bank-p)
					    (company string #'valid-company-p)
					    (due-date date #'valid-due-date-p)
					    (amount integer #'positive-p)
					    (status string))
    ("actions/cheque/update" :request-type :post)
  (no-cache) 
  (with-parameter-list params
    (let ((bank-id (get-bank-id (val bank)))
	  (company-id (get-company-id (val company))))
      (if (and bank-id company-id (every #'validp params)) 
	  (with-parameter-rebinding #'val 
	    (with-db
	      (execute (:update 'cheque :set
				'bank-id bank-id
				'company-id company-id
				'due-date due-date
				'amount amount
				'status status
				:where (:= 'id cheque-id)))
	      (redirect (cheques :cheque-id cheque-id) :code +http-see-other+)))
	  (with-parameter-rebinding #'raw
	    (redirect (cheque/update :cheque-id cheque-id
				     :bank bank
				     :company company
				     :due-date due-date
				     :amount amount
				     :status status)
		      :code +http-see-other+))))))

;; Pages

(define-dynamic-page cheques ((cheque-id integer #'valid-cheque-id-p))
    ("cheques/")
  (no-cache)
  (if (validp cheque-id)
      (with-page ()
	(:head
	 (:title "Επιταγές")
	 (css "reset.css" "main.css"))
	(:body
	 (:div :id "header"
	       (logo)
	       (primary-navbar 'cheques)) 
	 (:div :id "body" 
	       (:div :id "cheques" :class "window" 
		     (cheque-menu (val cheque-id) :create :edit :delete)
		     (:h2 "Κατάλογος επιταγών") 
		     (cheques-table (val cheque-id) :view))
	       (footer))))
      (redirect (cheque/notfound) :code +http-see-other+)))

(define-dynamic-page cheque/create ((bank string #'valid-bank-p)
				    (company string #'valid-company-p)
				    (due-date date #'valid-due-date-p)
				    (status string)
				    (amount integer #'positive-p))
    ("cheque/create")
  (no-cache) 
  (with-parameter-list params 
    (with-page ()
      (:head
       (:title "Επιταγές")
       (css "reset.css" "main.css"))
      (:body
       (:div :id "header"
	     (logo)
	     (primary-navbar 'cheques)) 
       (:div :id "body" 
	     (:div :id "cheques" :class "window"
		   (cheque-menu nil)
		   (:h2 "Δημιουργία επιταγής") 
		   (cheques-table nil :create params))
	     (footer))))))

(define-dynamic-page cheque/update ((cheque-id integer #'valid-cheque-id-p)
				    (bank string #'valid-bank-p)
				    (company string #'valid-company-p)
				    (due-date date #'valid-due-date-p)
				    (status string)
				    (amount integer #'positive-p))
    ("cheque/update")
  (no-cache)
  (if (validp cheque-id)
      (with-parameter-list params
	(with-page ()
	  (:head
	   (:title "Επιταγές")
	   (css "reset.css" "main.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'cheques)) 
	   (:div :id "body" 
		 (:div :id "cheques" :class "window"
		       (cheque-menu (val cheque-id) :view :delete)
		       (:h2 "Επεξεργασία επιταγής")
		       ;; first of params list is id, which we ignore 
		       (cheques-table (val cheque-id) :edit (rest params)))
		 (footer)))))
      (redirect (cheque/notfound) :code +http-see-other+)))

(define-dynamic-page cheque/delete ((cheque-id integer #'valid-cheque-id-p))
    ("cheque/delete")
  (no-cache)
  (if (validp cheque-id)
      (with-page ()
	(:head
	 (:title "Επιταγές")
	 (css "reset.css" "main.css"))
	(:body
	 (:div :id "header"
	       (logo)
	       (primary-navbar 'cheques)) 
	 (:div :id "body" 
	       (:div :id "cheques" :class "window"
		     (cheque-menu (val cheque-id) :view :edit)
		     (:h2 "Διαγραφή επιταγής")
		     (cheques-table (val cheque-id) :delete))
	       (footer))))
      (redirect (cheque/notfound) :code +http-see-other+)))

(define-dynamic-page cheque/notfound () ("cheque/notfound")
  (no-cache)
  (with-page ()
    (:head
     (:title "Άγνωστη εταιρία")
     (css "reset.css" "main.css"))
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
  (first (find status (cheque-statuses) :key #'second :test #'string-equal)))

(defun cheques-table (active-id intent &optional params)
  (flet ((normal-row (cheque-id data aux &optional activep)
	   (bind (((bank company amount status due-date) data)
		  ((company-id) aux)) 
	     (with-html
	       (:tr :class (if activep "active" nil)
		    (:td :class "select"
			 (:a :href (cheques :cheque-id cheque-id)
			     (:img :src (if activep
					    (url "img/bullet_red.png")
					    (url "img/bullet_blue.png")))))
		    (:td :class "data" (str (lisp-to-html bank)))
		    (:td :class "data"
			 (:a :class "data" :href (companies :id company-id)
			     (str (lisp-to-html company))))
		    (:td :class "data" (str (lisp-to-html amount)))
		    (:td :class "data" (str (lisp-to-html (status-label status))))
		    (:td :class "data" (str (lisp-to-html due-date)))
		    (:td :class "button" "")
		    (:td :class "button" "")))))
	 (form-row-create (params)
	   (bind (((bank company amount status due-date) (mapcar #'val* params))
		  ((bank% company% amount% status% due-date%)
		   (mapcar #'(lambda (p) (if (validp p) nil "attention")) params))) 
	     (with-form (actions/cheque/create)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (cheques)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (textbox 'bank :value bank :style bank%))
		    (:td :class "data" (textbox 'company :value company :style company%))
		    (:td :class "data" (textbox 'amount :value amount :style amount%))
		    (:td :class "data" (dropdown 'status (cheque-statuses)
						 :selected status :style status%))
		    (:td :class "data" (textbox 'due-date :value due-date :style due-date%))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (cheques)))))))
	 (form-row-update (cheque-id data params) 
	   (bind (((bank company amount status due-date)
		   (mapcar (lambda (param default)
			     (cond ((not (suppliedp param)) default)
				   ((validp param) (val param))
				   (t (raw param))))
			   params data))
		  ((bank% company% amount% status% due-date%)
		   (mapcar #'(lambda (p) (if (validp p) nil "attention")) params))) 
	     (with-form (actions/cheque/update :cheque-id cheque-id)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (cheques :cheque-id cheque-id)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (textbox 'bank :value bank :style bank%))
		    (:td :class "data" (textbox 'company :value company :style company%))
		    (:td :class "data" (textbox 'amount :value amount :style amount%))
		    (:td :class "data" (dropdown 'status (cheque-statuses) :selected status :style status%))
		    (:td :class "data" (textbox 'due-date :value due-date :style due-date%))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (cheques :cheque-id cheque-id)))))))
	 (form-row-delete (cheque-id data aux)
	   (bind (((bank company amount status due-date) data)
		  ((company-id) aux))
	     (with-form (actions/cheque/delete :cheque-id cheque-id)
	       (:tr :class "attention"
		    (:td :class "select"
			 (:a :href (cheques :cheque-id cheque-id)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (str (lisp-to-html bank)))
		    (:td :class "data"
			 (:a :class "data" :href (companies :id company-id)
			     (str (lisp-to-html company))))
		    (:td :class "data" (str (lisp-to-html amount)))
		    (:td :class "data" (str (lisp-to-html (status-label status))))
		    (:td :class "data" (str (lisp-to-html due-date)))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (cheques :cheque-id cheque-id))))))))
    (with-db
      (let ((cheques (query (:select 'cheque.id 'bank.title 'company.title
				     'cheque.amount 'cheque.status 'cheque.due-date
				     'company-id 
				     :from 'cheque
				     :left-join 'company
				     :on (:= 'company.id 'cheque.company-id)
				     :left-join 'bank
				     :on (:= 'bank.id 'cheque.bank-id)))) 
	    (header '("" "Τράπεζα" "Εταιρία" "Ποσόν" "Κατάσταση" "Ημερομηνία πληρωμής"))) 
	(with-html
	  (:table :id "cheques-table" :class "forms-in-row"
		  (:thead
		   (:tr (iter (for label in header)
			      (htm (:th (str label))))))
		  (:tbody 
		   (if (eql intent :create) (form-row-create params))
		   (iter (for cheque in cheques)
			 (for id = (first cheque))
			 (for data = (subseq cheque 1 6))
			 (for aux = (subseq cheque 6 7))
			 (for activep = (and active-id (= active-id id))) 
			 (if activep
			     (case intent
			       (:view (normal-row id data aux activep))
			       (:create (form-row-create params))
			       (:edit (form-row-update id data params))
			       (:delete (form-row-delete id data aux)))
			     (normal-row id data aux))))))))))

(defun cheque-menu (cheque-id &rest opt-list)
  (let ((options
	 (list :create (lambda () 
			 (with-html
			   (:li (:a :href (cheque/create)
				    (:img :src (url "img/add.png")) "Δημιουργία"))))
	       :view (lambda () 
		       (if cheque-id
			   (with-html
			     (:li (:a :href (cheques :cheque-id cheque-id)
				      (:img :src (url "img/magnifier.png")) "Προβολή")))
			   nil))
	       :edit (lambda ()
		       (if cheque-id
			   (with-html
			     (:li (:a :href (cheque/update :cheque-id cheque-id)
				      (:img :src (url "img/pencil.png")) "Επεξεργασία")))
			   nil))
	       :delete (lambda ()
			 (if cheque-id
			     (with-html
			       (:li (:a :href (cheque/delete :cheque-id cheque-id)
					(:img :src (url "img/delete.png")) "Διαγραφή")))
			     nil)))))
    (with-html
      (:div :class "actions"
	    (:ul :class "hmenu"
		 (iter (for opt in opt-list)
		       (funcall (getf options opt))))))))