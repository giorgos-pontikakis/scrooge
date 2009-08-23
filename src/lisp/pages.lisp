(in-package :scrooge)

(disable-sql-reader-syntax)
(enable-sql-reader-syntax)

(define-dynamic-page debug-page (variable value) ("debug")
  (with-page
    (:body
     (:p "Variable " variable " = " value))))


(define-dynamic-page unauthorized () ("unauthorized")
  (with-page
    (:head
     :title "Σκρουτζ: Πρόβλημα εξουσιοδότησης")
    (:body
     (:p "Δεν έχετε εξουσιοδότηση για να δείτε αυτή τη σελίδα"))))

(define-dynamic-page login () ("login")
  (with-page
    (:head
     :title "Σκρουτζ: Login")
    (:body
     (with-form (verify-login)
       (:p (label 'username "Username:"))
       (:p (text 'username))
       (:p (label 'scrooge-password "Password:"))
       (:p (text 'scrooge-password :passwordp t))
       (:p (submit "Login"))))))

(define-dynamic-page home () ("")
  (with-auth "root"
    (with-page
      (:head
       (:title "Σκρουτζ: Αρχική")
       (css "reset.css" "scrooge.css"))
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Αρχική"))
       (:div :id "content"
	     (:p "Home content not yet available"))))))

(defun config-head ()
  (with-html
    (:head
     (:title "Σκρουτζ: Ρυθμίσεις")
     (css "reset.css" "scrooge.css"))))

(defun config-tables (&key bank-id city-id tof-id)
  (with-db
    (let ((bank-data (select [id] [title] :from [bank] :order-by [title] :flatp t))
	  (city-data (select [id] [title] :from [city] :order-by [title] :flatp t))
	  (tof-data  (select [id] [title] :from [tof]  :order-by [title] :flatp t)))
      (with-html
	(:div :id "content"
	      (:div :class "column"
		    (with-ul (:style "inline")
		      (link "Εισαγωγή" :href (bank/insert))
		      (link "Επεξεργασία" :href (if bank-id
						    (bank/edit :bank-id bank-id)
						    nil))
		      (link "Διαγραφή" :href (if bank-id
						 (bank/remove :bank-id bank-id)
						 nil)))
		    (table bank-data
			   :id-fn #'first
			   :td-fn #'rest 
			   :header '("Τράπεζες") 
			   :href-fn (lambda (bank-id)
				      (config :bank-id bank-id))
			   :active-id bank-id
			   :style "dbtable"
			   :id "banks"))
	      (:div :class "column"
		    (with-ul (:style "inline")
		      (link "Εισαγωγή" :href (city/insert))
		      (link "Επεξεργασία" :href (if city-id
						    (city/edit :city-id city-id)
						    nil))
		      (link "Διαγραφή" :href (if city-id
						 (city/remove :city-id city-id)
						 nil)))
		    (table city-data   
			   :id-fn #'first
			   :td-fn #'rest 
			   :header '("Πόλεις")
			   :href-fn (lambda (city-id)
				      (config :city-id city-id))
			   :active-id city-id
			   :style "dbtable"
			   :id "cities")) 
	      (:div :class "column"
		    (with-ul (:style "inline")
		      (link "Εισαγωγή" :href (tof/insert))
		      (link "Επεξεργασία" :href (if tof-id
						    (tof/edit :tof-id tof-id)
						    nil))
		      (link "Διαγραφή" :href (if tof-id
						 (tof/remove :tof-id tof-id)
						 nil)))
		    (table tof-data  
			   :id-fn #'first
			   :td-fn #'rest 
			   :header '("Δ.Ο.Υ.")
			   :href-fn (lambda (tof-id)
				      (config :tof-id tof-id))
			   :active-id tof-id
			   :style "dbtable"
			   :id "tofs")))))))

(define-dynamic-page config ((bank-id integer) (city-id integer) (tof-id integer)) ("config/") 
  (with-auth "root"
    (with-page
      (config-head)
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Ρυθμίσεις"))
       (config-tables :bank-id bank-id :city-id city-id :tof-id tof-id)))))

(define-dynamic-page bank/insert () ("config/bank/insert")
  (with-auth "root"
    (with-page
      (config-head)
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Ρυθμίσεις"))
       (:div :id "prompt"
	     (with-form (insert-bank) 
	       (with-ul (:style "inline") 
		 (label 'title "Νέα τράπεζα: ")
		 (text 'title)
		 (submit "Εισαγωγή")
		 (link "Ακύρωση" :href (config)))))
       (config-tables)))))

(define-dynamic-page city/insert () ("config/city/insert")
  (with-auth "root"
    (with-page
      (config-head)
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Ρυθμίσεις"))
       (:div :id "prompt"
	     (with-form (insert-city) 
	       (with-ul (:style "inline") 
		 (label 'title "Νέα πόλη:")
		 (text 'title)
		 (submit "Εισαγωγή")
		 (link "Ακύρωση" :href (config)))))
       (config-tables)))))

(define-dynamic-page tof/insert () ("config/tof/insert")
  (with-auth "root"
    (with-page
      (config-head)
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Ρυθμίσεις"))
       (:div :id "prompt"
	     (with-form (insert-tof) 
	       (with-ul (:style "inline") 
		 (label 'title "Νέα Δ.Ο.Υ.:")
		 (text 'title)
		 (submit "Εισαγωγή")
		 (link "Ακύρωση" :href (config)))))
       (config-tables)))))


(define-dynamic-page bank/edit ((bank-id integer)) ("config/bank/edit")
  (with-auth "root"
    (with-db
      (let ((bank (select-unique 'bank :where [= [id] bank-id] :flatp t)))
	(with-page 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (edit-bank :id bank-id) 
		   (with-ul (:style "inline") 
		     (label 'title "Επεξεργασία τράπεζας: ")
		     (text 'title :value (title bank))
		     (submit "Ενημέρωση")
		     (link "Ακύρωση" :href (config :bank-id bank-id)))))
	   (config-tables :bank-id bank-id)))))))

(define-dynamic-page city/edit ((city-id integer)) ("config/city/edit")
  (with-auth "root"
    (with-db
      (let ((city (select-unique 'city :where [= [id] city-id] :flatp t)))
	(with-page 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (edit-city :id city-id) 
		   (with-ul (:style "inline") 
		     (label 'title "Επεξεργασία πόλης: ")
		     (text 'title :value (title city))
		     (submit "Ενημέρωση")
		     (link "Ακύρωση" :href (config :city-id city-id)))))
	   (config-tables :city-id city-id)))))))

(define-dynamic-page tof/edit ((tof-id integer)) ("config/tof/edit")
  (with-auth "root"
    (with-db
      (let ((tof (select-unique 'tof :where [= [id] tof-id] :flatp t)))
	(with-page 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (edit-tof :id tof-id) 
		   (with-ul (:style "inline") 
		     (label 'title "Επεξεργασία Δ.Ο.Υ.: ")
		     (text 'title :value (title tof))
		     (submit "Ενημέρωση")
		     (link "Ακύρωση" :href (config :tof-id tof-id)))))
	   (config-tables :tof-id tof-id)))))))


(define-dynamic-page bank/remove ((bank-id integer)) ("config/bank/remove")
  (with-auth "root"
    (with-db
      (let ((bank (select-unique 'bank :where [= [id] bank-id] :flatp t)))
	(with-page 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (remove-bank :id bank-id) 
		   (with-ul (:style "inline") 
		     (label 'title "Διαγραφή τράπεζας: ")
		     (text 'title :disabledp t :value (title bank))
		     (submit "Διαγραφή")
		     (link "Ακύρωση" :href (config :bank-id bank-id)))))
	   (config-tables :bank-id bank-id)))))))

(define-dynamic-page city/remove ((city-id integer)) ("config/city/remove")
  (with-auth "root"
    (with-db
      (let ((city (select-unique 'city :where [= [id] city-id] :flatp t)))
	(with-page 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (remove-city :id city-id) 
		   (with-ul (:style "inline") 
		     (label 'title "Διαγραφή πόλης: ")
		     (text 'title :disabledp t :value (title city))
		     (submit "Διαγραφή")
		     (link "Ακύρωση" :href (config :city-id city-id)))))
	   (config-tables :city-id city-id)))))))

(define-dynamic-page tof/remove ((tof-id integer)) ("config/tof/remove")
  (with-auth "root"
    (with-db
      (let ((tof (select-unique 'tof :where [= [id] tof-id] :flatp t)))
	(with-page 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (remove-tof :id tof-id) 
		   (with-ul (:style "inline") 
		     (label 'title "Διαγραφή Δ.Ο.Υ.: ")
		     (text 'title :disabledp t :value (title tof))
		     (submit "Διαγραφή")
		     (link "Ακύρωση" :href (config :tof-id tof-id)))))
	   (config-tables :tof-id tof-id)))))))


(define-dynamic-page companies ((company-id integer)) ("companies/")
  (with-auth "root"
    (with-db
      (let ((companies (select [company.id] [company.title] [company.tin] [tof.title]
			       :from '([company] [tof])
			       :where [= [tof.id] [company.tof-id]]
			       :flatp t))
	    (header '("Επωνυμία" "Α.Φ.Μ" "Δ.Ο.Υ.")))
	(with-page
	  (:head
	   (:title "Σκρουτζ: Συναλλασσόμενοι")
	   (css "reset.css" "scrooge.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Συναλλασσόμενοι"))
	   (:div :id "content"
		 (:div :id "prompt"
		       (with-ul (:style "inline")
			 (link "Εισαγωγή συναλλασσόμενου" :href (company/insert))
			 #|(with-form (company/search)
			 (label 'search-item "Search: ")
			 (text 'search-item))|#))	       
	       (table companies
		      :id-fn #'first
		      :td-fn #'rest
		      :caption "ΣΥΝΑΛΛΑΣΣΟΜΕΝΟΙ"
		      :header header
		      :href-fn (lambda (company-id)
				 (company/view :company-id company-id))
		      :active-id company-id
		      :id "companies"
		      :style "dbtable"))))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro company-form (purpose)
    ;; Captures company-id from the containing environment
    (with-gensyms (company tofs cities)
      (let ((form-fn (ecase purpose
		       (view 'company/edit)
		       (insert 'insert-company)
		       (edit 'edit-company)
		       (remove 'remove-company)))
	    (readonlyp (ecase purpose
			 (view t)
			 (insert nil)
			 (edit nil)
			 (remove t))))
	`(with-db
	   (let* ((,company ,(ecase purpose
			      ((view edit remove) `(select-unique 'company
								  :where [= [id] company-id]
								  :flatp t))
			      (insert `(make-instance 'company))))
		  (,tofs (select [title] [id] :from [tof] :flatp t))
		  (,cities (select [title] [id] :from [city] :flatp t))) 
	     (with-form (,form-fn :company-id company-id)
	       (:div :id "prompt"
		     (with-ul (:style "inline") 
		       ,@(ecase purpose
				(view `((:a :href (company/edit   :company-id company-id) "Επεξεργασία")
					(:a :href (company/remove :company-id company-id) "Διαγραφή")
					(:a :href (companies) "Πίνακας εταιριών")))
				(edit `((submit "Ενημέρωση")
					(:a :href (company/view :company-id company-id) "Ακύρωση")))
				(insert `((submit "Δημιουργία συναλλασσόμενου")
					  (:a :href (companies) "Ακύρωση")))
				(remove `((submit "Διαγραφή συναλλασσόμενου")
					  (:a :href (company/view :company-id company-id) "Ακύρωση"))))))
	       (with-table (:style "dbtable")
		 ((label 'title "Επωνυμία") (text 'title :readonlyp ,readonlyp :value (title ,company))))
	       (:fieldset
		(:legend "Φορολογικά στοιχεία")
		(with-table (:style "dbtable")
		  ((label 'occupation "Επάγγελμα") (text 'occupation :readonlyp ,readonlyp :value (occupation ,company)))
		  ((label 'tin "Α.Φ.Μ.") (text 'tin :readonlyp ,readonlyp :value (tin ,company)))
		  ((label 'tof "Δ.Ο.Υ.") ,(ecase purpose
						 ((view remove)
						  `(text 'tof-id :readonlyp ,readonlyp
							 :value (car (rassoc (tof-id ,company) ,tofs :key #'car))))
						 ((edit insert)
						  `(dropdown 'tof-id ,tofs :selected (tof-id ,company)))))))
	       (:fieldset
		(:legend "Διεύθυνση")
		(with-table (:style "dbtable")
		  ((label 'address "Διεύθυνση:") (text 'address :readonlyp ,readonlyp :value (address ,company)))
		  ((label 'city "Πόλη") ,(ecase purpose
						((view remove)
						 `(text 'address :readonlyp ,readonlyp
							:value (car (rassoc (city-id ,company) ,cities :key #'car))))
						((edit insert)
						 `(dropdown 'city-id ,cities :selected (city-id ,company)))))
		  ((label 'zipcode "Ταχυδρομικός κώδικας") (text 'zipcode :readonlyp ,readonlyp :value (zipcode ,company)))
		  ((label 'pobox "Ταχυδρομική θυρίδα") (text 'pobox :readonlyp ,readonlyp :value (pobox ,company))))))))))))


(define-dynamic-page company/edit ((company-id integer)) ("company/edit")
  (with-auth "root"
    (with-db
      (let ((company (select-unique 'company
				    :where [= [id] company-id]
				    :flatp t)))
	(with-page
	  (:head
	   (:title "Σκρουτζ: Επεξεργασία συναλλασσόμενου: " (title company))
	   (css "reset.css" "scrooge.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Συναλλασσόμενοι"))
	   (:div :id "content"
		 (company-form edit))))))))

(define-dynamic-page company/view ((company-id integer)) ("company/view")
  (with-auth "root"
    (with-db
      (let ((company (select-unique 'company
				    :where [= [id] company-id]
				    :flatp t)))
	(with-page
	  (:head
	   (:title "Σκρουτζ: " (str (title company)))
	   (css "reset.css" "scrooge.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Συναλλασσόμενοι"))
	   (:div :id "content"
		 (company-form view))))))))

(define-dynamic-page company/insert () ("company/insert")
  (with-auth "root"
    (with-page
      (:head
       (:title "Σκρουτζ: Εισαγωγή συναλλασσόμενου")
       (css "reset.css" "scrooge.css"))
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Συναλλασσόμενοι"))
       (:div :id "content"
	     (company-form insert))))))

(define-dynamic-page company/remove ((company-id integer)) ("company/remove")
  (with-auth "root"
    (with-page
      (:head
       (:title "Σκρουτζ: Εισαγωγή συναλλασσόμενου")
       (css "reset.css" "scrooge.css"))
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Συναλλασσόμενοι"))
       (:div :id "content"
	     (company-form remove))))))

