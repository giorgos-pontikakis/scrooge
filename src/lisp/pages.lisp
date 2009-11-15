(in-package :scrooge)

;;; --- Autocomplete --------------------

(define-dynamic-page autocomplete (table q) ("autocomplete" :content-type "text/plain")
  (with-db
    (let ((results (cond ((string-equal table "company")
			  (query (:select 'id 'title :from 'company)))
			 ((string-equal table "tx-type")
			  (query (:select 'id 'title :from 'tx-type)))
			 (t nil))))
      (if results
	  (with-html-output (*standard-output* nil :indent nil :prologue nil) 
	    (iter (for (key val) in results)
		  (when (search (string-upcase-gr q) (string-upcase-gr val))
		    (fmt "~A|~A~&" val key))))
	  (with-html-output (*standard-output* nil :indent nil :prologue nil)
	    "|")))))

;;; --- Authorization --------------------

(define-dynamic-page debug-page (variable value) ("debug")
  (with-page ()
    (:body
     (:p "Variable " variable " = " value))))

(define-dynamic-page unauthorized () ("unauthorized")
  (with-page ()
    (:head
     :title "Σκρουτζ: Πρόβλημα εξουσιοδότησης")
    (:body
     (:p "Δεν έχετε εξουσιοδότηση για να δείτε αυτή τη σελίδα"))))

(define-dynamic-page login () ("login")
  (with-page ()
    (:head
     :title "Σκρουτζ: Login")
    (:body
     (with-form (verify-login)
       (:p (label 'username "Username:"))
       (:p (textbox 'username))
       (:p (label 'scrooge-password "Password:"))
       (:p (textbox 'scrooge-password :passwordp t))
       (:p (submit "Login"))))))

(define-dynamic-page home () ("")
  (with-auth "root"
    (with-page ()
      (:head
       (:title "Σκρουτζ: Αρχική")
       (css "reset.css" "scrooge.css"))
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Αρχική"))
       (:div :id "body"
	     (:p "Home content not yet available"))))))

(defun config-head ()
  (with-html
    (:head
     (:title "Σκρουτζ: Ρυθμίσεις")
     (css "reset.css" "scrooge.css"))))

(defun config-tables (&key bank-id city-id tof-id)
  (with-db
    (let ((bank-data (query (:order-by (:select 'id 'title :from 'bank) 'title)))
	  (city-data (query (:order-by (:select 'id 'title :from 'city) 'title)))
	  (tof-data  (query (:order-by (:select 'id 'title :from 'tof) 'title))))
      (with-html
	(:div :id "body"
	      (:div :class "column"
		    (:ul :class "inline"
			 (:li (link "Εισαγωγή" :href (bank/insert)))
			 (:li (link "Επεξεργασία" :href (if bank-id
							    (bank/edit :bank-id bank-id)
							    nil)))
			 (:li (link "Διαγραφή" :href (if bank-id
							 (bank/remove :bank-id bank-id)
							 nil))))
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
		    (:ul :class "inline"
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
		    (:ul :class "inline"
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
    (with-page ()
      (config-head)
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Ρυθμίσεις"))
       (config-tables :bank-id bank-id :city-id city-id :tof-id tof-id)))))


;;; --- Banks, cities, tofs --------------------

(define-dynamic-page bank/insert () ("config/bank/insert")
  (with-auth "root"
    (with-page ()
      (config-head)
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Ρυθμίσεις"))
       (:div :id "prompt"
	     (with-form (insert-bank) 
	       (:ul :class "inline" 
		    (label 'title "Νέα τράπεζα: ")
		    (textbox 'title)
		    (submit "Εισαγωγή")
		    (link "Ακύρωση" :href (config)))))
       (config-tables)))))

(define-dynamic-page city/insert () ("config/city/insert")
  (with-auth "root"
    (with-page ()
      (config-head)
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Ρυθμίσεις"))
       (:div :id "prompt"
	     (with-form (insert-city) 
	       (:ul :class "inline" 
		    (label 'title "Νέα πόλη:")
		    (textbox 'title)
		    (submit "Εισαγωγή")
		    (link "Ακύρωση" :href (config)))))
       (config-tables)))))

(define-dynamic-page tof/insert () ("config/tof/insert")
  (with-auth "root"
    (with-page ()
      (config-head)
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Ρυθμίσεις"))
       (:div :id "prompt"
	     (with-form (insert-tof) 
	       (:ul :class "inline" 
		    (label 'title "Νέα Δ.Ο.Υ.:")
		    (textbox 'title)
		    (submit "Εισαγωγή")
		    (link "Ακύρωση" :href (config)))))
       (config-tables)))))


(define-dynamic-page bank/edit ((bank-id integer)) ("config/bank/edit")
  (with-auth "root"
    (with-db
      (let ((bank (get-dao 'bank bank-id)))
	(with-page () 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (edit-bank :id bank-id) 
		   (:ul :class "inline" 
			(label 'title "Επεξεργασία τράπεζας: ")
			(textbox 'title :value (title bank))
			(submit "Ενημέρωση")
			(link "Ακύρωση" :href (config :bank-id bank-id)))))
	   (config-tables :bank-id bank-id)))))))

(define-dynamic-page city/edit ((city-id integer)) ("config/city/edit")
  (with-auth "root"
    (with-db
      (let ((city (get-dao 'city city-id)))
	(with-page () 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (edit-city :id city-id) 
		   (:ul :class "inline" 
			(label 'title "Επεξεργασία πόλης: ")
			(textbox 'title :value (title city))
			(submit "Ενημέρωση")
			(link "Ακύρωση" :href (config :city-id city-id)))))
	   (config-tables :city-id city-id)))))))

(define-dynamic-page tof/edit ((tof-id integer)) ("config/tof/edit")
  (with-auth "root"
    (with-db
      (let ((tof (get-dao 'tof tof-id)))
	(with-page () 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (edit-tof :id tof-id) 
		   (:ul :class "inline" 
			(label 'title "Επεξεργασία Δ.Ο.Υ.: ")
			(textbox 'title :value (title tof))
			(submit "Ενημέρωση")
			(link "Ακύρωση" :href (config :tof-id tof-id)))))
	   (config-tables :tof-id tof-id)))))))


(define-dynamic-page bank/remove ((bank-id integer)) ("config/bank/remove")
  (with-auth "root"
    (with-db
      (let ((bank (get-dao 'bank  bank-id)))
	(with-page () 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (remove-bank :id bank-id) 
		   (:ul :class "inline" 
			(label 'title "Διαγραφή τράπεζας: ")
			(textbox 'title :disabledp t :value (title bank))
			(submit "Διαγραφή")
			(link "Ακύρωση" :href (config :bank-id bank-id)))))
	   (config-tables :bank-id bank-id)))))))

(define-dynamic-page city/remove ((city-id integer)) ("config/city/remove")
  (with-auth "root"
    (with-db
      (let ((city (get-dao 'city city-id)))
	(with-page () 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (remove-city :id city-id) 
		   (:ul :class "inline" 
			(label 'title "Διαγραφή πόλης: ")
			(textbox 'title :disabledp t :value (title city))
			(submit "Διαγραφή")
			(link "Ακύρωση" :href (config :city-id city-id)))))
	   (config-tables :city-id city-id)))))))

(define-dynamic-page tof/remove ((tof-id integer)) ("config/tof/remove")
  (with-auth "root"
    (with-db
      (let ((tof (get-dao 'tof tof-id)))
	(with-page () 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (remove-tof :id tof-id) 
		   (:ul :class "inline" 
			(label 'title "Διαγραφή Δ.Ο.Υ.: ")
			(textbox 'title :disabledp t :value (title tof))
			(submit "Διαγραφή")
			(link "Ακύρωση" :href (config :tof-id tof-id)))))
	   (config-tables :tof-id tof-id)))))))


;;; --- Companies --------------------

(define-dynamic-page companies ((company-id integer)) ("companies/")
  (with-auth "root"
    (with-db
      (let ((companies (query (:select 'company.id 'company.title 'company.tin 'tof.title
				       :from 'company 'tof
				       :where (:= 'tof.id 'company.tof-id))))
	    (header '("" "Επωνυμία" "Α.Φ.Μ" "Δ.Ο.Υ.")))
	(with-page ()
	  (:head
	   (:title "Σκρουτζ: Συναλλασσόμενοι")
	   (css "reset.css" "scrooge.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Εταιρίες"))
	   (:div :id "body"
		 (:div :id "actions"
		       (company-menu 'summary company-id))	       
		 (:div :id "content" :class "summary"
		       (companies-table company-id companies header))
		 (footer))))))))

(defun companies-table (active-id companies header)
  (with-html
    (:table :class "dbtable"
	    (:thead
	     (:tr (iter (for label in header)
			(htm (:th (str label))))))
	    (:tbody
	     (iter (for row in companies) 
		   (destructuring-bind (id title tin doy) row
		     (let ((activep (and active-id (= active-id id))))
		       (htm
			(:tr :class (if activep "active" nil)
			     (:td (:a :href (companies :company-id id)
				      (:img :src (url (if activep
							  "img/bullet_red.png"
							  "img/bullet_blue.png")))))
			     (:td (:a :href (company/view :company-id id) (str title)))
			     (:td (:p (str tin)))
			     (:td (:p (str doy))))))))))))

(define-dynamic-page company/insert () ("company/insert")
  (with-auth "root"
    (with-db
      (let ((cities (query (:select 'title 'id :from 'city)))
	    (tofs (query (:select 'title 'id :from 'tof))))
	(with-page ()
	  (:head
	   (:title "Σκρουτζ: Εισαγωγή εταιρίας")
	   (css "reset.css" "scrooge.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Εταιρίες"))
	   (:div :id "body"
		 (:div :id "actions" 
		       (company-menu 'insert))
		 (:div :id "content" :class "simple-form"
		       (:h2 "Εισαγωγή εταιρίας")
		       (with-form (insert-company :company-id company-id)
			 (with-table (:style "formtable") 
			   ((:p (label 'title "Επωνυμία" :style "strong"))
			    (textbox 'title)))
			 (:fieldset
			  (:legend :class "strong" "Φορολογικά στοιχεία")
			  (with-table (:style "formtable")
			    ((label 'occupation "Επάγγελμα")
			     (textbox 'occupation))
			    ((label 'tin "Α.Φ.Μ.")
			     (textbox 'tin))
			    ((label 'tof-id "Δ.Ο.Υ.")
			     (dropdown 'tof-id tofs))))
			 (:fieldset
			  (:legend :class "strong" "Στοιχεία Έδρας")
			  (with-table (:style "formtable")
			    ((label 'address "Διεύθυνση")
			     (textbox 'address))
			    ((label 'city "Πόλη")
			     (dropdown 'city-id cities))
			    ((label 'zipcode "Ταχυδρομικός κώδικας")
			     (textbox 'zipcode))
			    ((label 'pobox "Ταχυδρομική θυρίδα")
			     (textbox 'pobox))))
			 (:ul :class "prompt hmenu" 
			      (:li (submit "Δημιουργία"))
			      (:li (:a :href (companies) "Ακύρωση")))))
		 (footer))))))))

(define-dynamic-page company/view ((company-id integer)) ("company/view")
  (with-auth "root"
    (with-db
      (let* ((company (get-dao 'company company-id))
	     (city (query (:select 'title
				   :from 'city
				   :where (:= (city-id company) 'id))
			  :single))
	     (tof (query (:select 'title
				  :from 'tof
				  :where (:= (tof-id company) 'id))
			 :single)))
	(with-page ()
	  (:head
	   (:title "Σκρουτζ: " (str (title company)))
	   (css "reset.css" "scrooge.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Εταιρίες")) 
	   (:div :id "body" 
		 (:div :id "actions"
		       (company-menu 'view company-id))
		 (:div :id "content" :class "simple-form"
		       (:h2 "Στοιχεία εταιρίας")
		       (with-form (edit-company :company-id company-id) 
			 (with-table (:style "formtable")
			   ((:p (label 'title "Επωνυμία" :style "strong"))
			    (textbox 'title :value (title company) :readonlyp t)))
			 (:fieldset
			  (:legend "Φορολογικά στοιχεία")
			  (with-table (:style "formtable")
			    ((label 'occupation "Επάγγελμα")
			     (textbox 'occupation :value (occupation company) :readonlyp t))
			    ((label 'tin "Α.Φ.Μ.")
			     (textbox 'tin :value (tin company) :readonlyp t))
			    ((label 'tof "Δ.Ο.Υ.")
			     (textbox 'tof :value tof :readonlyp t))))
			 (:fieldset
			  (:legend "Διεύθυνση")
			  (with-table (:style "formtable")
			    ((label 'address "Διεύθυνση:")
			     (textbox 'address :value (address company) :readonlyp t))
			    ((label 'city "Πόλη")
			     (textbox 'address :value city :readonlyp t))
			    ((label 'zipcode "Ταχυδρομικός κώδικας")
			     (textbox 'zipcode :value (zipcode company) :readonlyp t))
			    ((label 'pobox "Ταχυδρομική θυρίδα")
			     (textbox 'pobox :value (pobox company) :readonlyp t))))))
		 (footer))))))))

(define-dynamic-page company/edit ((company-id integer)) ("company/edit")
  (with-auth "root"
    (with-db
      (let ((company (get-dao 'company company-id))
	    (cities (query (:select 'title 'id :from 'city)))
	    (tofs (query (:select 'title 'id :from 'tof))))
	(with-page ()
	  (:head
	   (:title "Σκρουτζ: Επεξεργασία συναλλασσόμενου: " (title company))
	   (css "reset.css" "scrooge.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Εταιρίες"))
	   (:div :id "body"
		 (:div :id "actions"
		       (company-menu 'edit company-id))
		 (:div :id "content" :class "simple-form"
		       (:h2 "Επεξεργασία εταιρίας")
		       (with-form (edit-company :company-id company-id) 
			 (with-table (:style "formtable")
			   ((:p (label 'title "Επωνυμία" :style "strong"))
			    (textbox 'title :value (title company))))
			 (:fieldset
			  (:legend "Φορολογικά στοιχεία")
			  (with-table (:style "formtable")
			    ((label 'occupation "Επάγγελμα")
			     (textbox 'occupation :value (occupation company)))
			    ((label 'tin "Α.Φ.Μ.")
			     (textbox 'tin :value (tin company)))
			    ((label 'tof-id "Δ.Ο.Υ.")
			     (dropdown 'tof-id tofs :selected (tof-id company)))))
			 (:fieldset
			  (:legend "Διεύθυνση")
			  (with-table (:style "formtable")
			    ((label 'address "Διεύθυνση:")
			     (textbox 'address  :value (address company)))
			    ((label 'city "Πόλη")
			     (dropdown 'city-id cities :selected (city-id company)))
			    ((label 'zipcode "Ταχυδρομικός κώδικας")
			     (textbox 'zipcode :value (zipcode company)))
			    ((label 'pobox "Ταχυδρομική θυρίδα")
			     (textbox 'pobox :value (pobox company)))))
			 (:ul :class "prompt hmenu"
			      (:li (submit "Ενημέρωση"))
			      (:li (:a :href (company/view :company-id company-id) "Ακύρωση")))))
		 (footer))))))))

(define-dynamic-page company/remove ((company-id integer)) ("company/remove")
  (with-auth "root"
    (with-db
      (let* ((company (get-dao 'company company-id))
	     (city (query (:select 'title
				   :from 'city
				   :where (:= (city-id company) 'id))
			  :single))
	     (tof (query (:select 'title
				  :from 'tof
				  :where (:= (tof-id company) 'id))
			 :single)))
	(with-page ()
	  (:head
	   (:title "Σκρουτζ: Διαγραφή εταιρίας")
	   (css "reset.css" "scrooge.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Εταιρίες")) 
	   (:div :id "body"
		 (:div :id "actions"
		       (company-menu 'kill company-id))
		 (:div :id "content" :class "simple-form"
		       (:h2 "Διαγραφή εταιρίας")
		       (with-form (remove-company :company-id company-id)
			 (with-table (:style "formtable")
			   ((:p (label 'title "Επωνυμία" :style "strong"))
			    (textbox 'title :value (title company))))
			 (:fieldset
			  (:legend "Φορολογικά στοιχεία")
			  (with-table (:style "formtable")
			    ((label 'occupation "Επάγγελμα")
			     (textbox 'occupation :value (occupation company) :readonlyp t))
			    ((label 'tin "Α.Φ.Μ.")
			     (textbox 'tin :value (tin company) :readonlyp t))
			    ((label 'tof "Δ.Ο.Υ.")
			     (textbox 'tof-id :value tof :readonlyp t))))
			 (:fieldset
			  (:legend "Διεύθυνση")
			  (with-table (:style "formtable")
			    ((label 'address "Διεύθυνση:")
			     (textbox 'address :value (address company) :readonlyp t))
			    ((label 'city "Πόλη")
			     (textbox 'address :value city :readonlyp t))
			    ((label 'zipcode "Ταχυδρομικός κώδικας")
			     (textbox 'zipcode :value (zipcode company) :readonlyp t))
			    ((label 'pobox "Ταχυδρομική θυρίδα")
			     (textbox 'pobox :value (pobox company) :readonlyp t))))
			 (:ul :class "prompt hmenu"
			      (:li (submit "Διαγραφή"))
			      (:li (:a :href (company/view :company-id company-id) "Ακύρωση")))))
		 (footer))))))))


;;; --- Accounts --------------------

(define-dynamic-page accounts ((account-id integer)) ("accounts/")
  (labels ((display-accounts (accounts-list active-id &optional (level 0))
	     (with-html
	       (:ul :class "indent"
		    (iter (for account in accounts-list)
			  (let ((subs (select-dao 'account (:= 'parent-id (id account)))))
			    (if (null subs)
				(htm
				 (:li (:a :href (accounts :account-id (id account))
					  :class (if (eql (id account) active-id) "active" nil)
					  (str (title account)))))
				(htm
				 (:li (:a :href (accounts :account-id (id account))
					  :class (if (eql (id account) active-id) "active" nil)
					  (str (title account)))
				      (display-accounts subs active-id (1+ level)))))))))))
    (with-db
      (let ((root-debit-accounts (select-dao 'account
					     (:and (:= 'debit-p t)
						   (:is-null 'parent-id))))
	    (root-credit-accounts (select-dao 'account (:and (:= 'debit-p nil)
							     (:is-null 'parent-id)))))
	(with-page ()
	  (:head
	   (:title "Σκρουτζ: Λογαριασμοί")
	   (css "reset.css" "scrooge.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Λογαριασμοί"))
	   (:div :id "body"
		 (:div :id "actions"
		       (account-menu 'summary account-id))
		 (:div :id "content" :class "summary"
		       (:div :class "vmenu" :id "debit-accounts"
			     (:h3 "Χρεωστικοί λογαριασμοί")
			     (if root-debit-accounts
				 (display-accounts root-debit-accounts account-id)
				 (htm (:p "Δεν έχουν οριστεί χρεωστικοί λογαριασμοί"))))
		       (:div :class "vmenu" :id "credit-accounts"
			     (:h3 "Πιστωτικοί λογαριασμοί") 
			     (if root-credit-accounts
				 (display-accounts root-credit-accounts account-id)
				 (htm (:p "Δεν έχουν οριστεί πιστωτικοί λογαριασμοί")))))
		 (footer))))))))

(define-dynamic-page account/insert ((account-id integer)) ("account/insert")
  (with-page ()
    (:head
     (:title "Σκρουτζ: Εισαγωγή λογαριασμού")
     (css "reset.css" "scrooge.css"))
    (:body
     (:div :id "header"
	   (logo)
	   (navbar "Λογαριασμοί"))
     (:div :id "body" 
	   (:div :id "actions" 
		 (account-menu 'insert account-id)) 
	   (:div :id "content" :class "simple-form" 
		 ;; TODO: basename defaults to :null because it should
		 ;; be removed, it does not seem useful
		 (if (null account-id)
		     (htm
		      (:h2 "Εισαγωγή λογαριασμού")
		      (with-form (insert-account :parent-id :null :basename :null)
			(with-table (:style "formtable")
			  ((label 'title "Ονομασία λογαριασμού")
			   (textbox 'title))
			  ((label 'debit-p "Είδος λογαριασμού")
			   (radio 'debit-p '(("Χρεωστικός" t)
					     ("Πιστωτικός" nil))
				  :checked t)))
			(:ul :class "prompt hmenu"
			     (:li (submit "Δημιουργία"))
			     (:li (:a :href (accounts) "Ακύρωση")))))
		     (with-db 
		       (let ((parent-account (get-dao 'account account-id)))
			 (htm
			  (:h2 "Εισαγωγή υπο-λογαριασμού")
			  (with-form (insert-account :parent-id (id parent-account)
						     :debit-p (debit-p parent-account)
						     :basename :null)
			    (with-table (:style "formtable")
			      ((label 'parent-title "Ονομασία λογαριασμού-γονέα")
			       (textbox 'parent-title :value (title parent-account) :disabledp t))
			      ((label 'title "Ονομασία λογαριασμού")
			       (textbox 'title)))
			    (:ul :class "prompt hmenu"
				 (:li (submit "Δημιουργία"))
				 (:li (:a :href (accounts) "Ακύρωση")))))))))
	   (footer)))))

(define-dynamic-page account/view ((account-id integer)) ("account/view")
  (with-db
    (let* ((account (get-dao 'account account-id))
	   (parent-account (get-dao 'account (parent-id account)))
	   (transactions (get-transactions 'account account-id)) 
	   (debits-sum (reduce #'+ (remove-if-not #'integerp transactions :key #'fifth)
			       :key #'fifth)) 
	   (credits-sum (reduce #'+ (remove-if-not #'integerp transactions :key #'sixth)
				:key #'sixth)))
      (with-page ()
	(:head
	 (:title "Σκρουτζ: Λογαριασμός > " (str (title account)))
	 (css "reset.css" "scrooge.css"))
	(:body
	 (:div :id "header"
	       (logo)
	       (navbar "Λογαριασμοί"))
	 (:div :id "body" 
	       (:div :id "actions"
		     (account-menu 'view account-id)) 
	       (:div :id "content" :class "simple-form"
		     (:h2 "Στοιχεία λογαριασμού")
		     (with-table (:style "formtable")
		       ((label 'title "Ονομασία λογαριασμού")
			(textbox 'title :value (title account) :readonlyp t))
		       ((label 'parent-title "Γονέας")
			(textbox 'parent-title
				 :value (if parent-account (title parent-account) :null)
				 :readonlyp t))
		       ((label 'debit-p "Είδος λογαριασμού")
			(radio 'debit-p '(("Χρεωστικός" t)
					  ("Πιστωτικός" nil))
			       :checked (debit-p account)
			       :readonlyp t))))
	       (:div :class "dbtable"
		     (table transactions
			    :header '("Ημερομηνία" "Εταιρία" "Περιγραφή" "Χρέωση" "Πίστωση")
			    :caption "Συναλλαγές Λογαριασμού"
			    :id-fn #'first
			    :td-fn #'rest
			    :style "dbtable")
		     (:p "Σύνολο χρεώσεων = " (str debits-sum)) 
		     (:p "Σύνολο πιστώσεων = " (str credits-sum))
		     (:p "Ισοζύγιο λογαριασμού: " (str (if (debit-p account)
							   (- debits-sum credits-sum)
							   (- credits-sum debits-sum)))))
	       (footer)))))))

(define-dynamic-page account/edit ((account-id integer)) ("account/edit")
  (with-db
    (let ((account (get-dao 'account account-id)))
      (with-page ()
	(:head
	 (:title "Σκρουτζ: Επεξεργασία λογαριασμού: " (title account))
	 (css "reset.css" "scrooge.css"))
	(:body
	 (:div :id "header"
	       (logo)
	       (navbar "Λογαριασμοί"))
	 (:div :id "body" 
	       (:div :id "actions"
		     (account-menu 'edit account-id))
	       (:div :id "content" :class "simple-form"
		     (:h2 "Επεξεργασία λογαριασμού")
		     ;; TODO: basename defaults to :null because it should
		     ;; be removed, it does not seem useful 
		     (with-form (edit-account :account-id account-id
					      :parent-id (parent-id account)
					      :basename :null) 
		       (with-table (:style "formtable")
			 ((label 'title "Ονομασία λογαριασμού")
			  (textbox 'title :value (title account)))
			 #|((label 'debit-p "Είδος λογαριασμού")
			 (radio 'debit-p '(("Χρεωστικός" t)
			 ("Πιστωτικός" nil))
			 :checked (debit-p account)))|#)
		       (:ul :class "prompt hmenu"
			    (:li (submit "Ενημέρωση"))
			    (:li (:a :href (account/view :account-id account-id)
				     "Ακύρωση")))))
	       (footer)))))))

(define-dynamic-page account/remove ((account-id integer)) ("account/remove")
  (with-db
    (let* ((account (get-dao 'account account-id))
	   (parent-account (get-dao 'account (parent-id account))))
      (with-page ()
	(:head
	 (:title "Σκρουτζ: Λογαριασμός" (str (title account)))
	 (css "reset.css" "scrooge.css"))
	(:body
	 (:div :id "header"
	       (logo)
	       (navbar "Λογαριασμοί"))
	 (:div :id "body" 
	       (:div :id "actions"
		     (account-menu 'kill account-id))
	       (:div :id "content" :class "simple-form"
		     (:h2 "Διαγραφή λογαριασμού")
		     (with-form (remove-account :account-id account-id) 
		       (with-table (:style "formtable")
			 ((label 'title "Ονομασία λογαριασμού")
			  (textbox 'title
				   :value (title account)
				   :readonlyp t))
			 ((label 'parent-title "Γονέας")
			  (textbox 'parent-title
				   :value (if parent-account (title parent-account) :null)
				   :readonlyp t))
			 ((label 'debit-p "Είδος λογαριασμού")
			  (radio 'debit-p '(("Χρεωστικός" t)
					    ("Πιστωτικός" nil))
				 :checked (debit-p account)
				 :readonlyp t)))
		       (:ul :class "prompt hmenu"
			    (:li (submit "Διαγραφή λογαριασμού")) 
			    (:li (:a :href (account/view :account-id account-id)
				     "Ακύρωση")))))))))))





(defmacro with-defaults (args &body body)
  (let ((params (mapcar #'(lambda (sym)
			    (concatenate 'string (string-downcase sym) "-default"))
			args)))
    `(let ,(mapcar #'(lambda (arg param)
		       `(,arg (get-parameter ,param)))
		   args params)
       ,@body)))


;;; --- Transaction types --------------------

(define-dynamic-page transaction-types ((tx-type-id integer)) ("transaction-types/") 
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
	 (css "reset.css" "scrooge.css"))
	(:body
	 (:div :id "header"
	       (logo)
	       (navbar "Τύποι συναλλαγών"))
	 (:div :id "body"
	       (:div :id "actions"
		     (tx-type-menu 'summary tx-type-id))
	       (:div :id "content" :class "summary"
		     (transaction-types-table tx-type-id transaction-types header))))))))

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

(define-dynamic-page transaction-type/insert () ("transaction-type/insert")
  (with-db
    (let* ((accounts (mapcar (lambda (rec)
			       (destructuring-bind (paracc acc id) rec
				 (if (eql :null paracc)
				     (list acc id)
				     (list (conc paracc ">" acc) id))))
			     (query
			      (:select 'parent.title 'account.title 'account.id 
				       :from 'account
				       :left-join (:as 'account 'parent)
				       :on (:= 'parent.id 'account.parent-id))))))
      (with-page ()
	(:head
	 (:title "Σκρούτζ: Εισαγωγή τύπου συναλλαγής")
	 (css "reset.css" "scrooge.css"))
	(:body
	 (:div :id "header"
	       (logo)
	       (navbar "Τύποι συναλλαγών"))
	 (:div :id "body"
	       (:div :id "actions"
		     (tx-type-menu 'insert))
	       (:div :id "content" :class "simple-form"
		     (with-form (insert-tx-type)
		       (with-table (:style "formtable")
			 ((label 'title "Περιγραφή")
			  (textbox 'title))
			 ((label 'debit-acc-id "Λογαριασμός χρέωσης")
			  (dropdown 'debit-acc-id accounts))
			 ((label 'credit-acc-id "Λογαριασμός πίστωσης")
			  (dropdown 'credit-acc-id accounts)))
		       (:ul :class "prompt hmenu"
			    (:li (submit "Δημιουργία"))
			    (:li (:a :href (transaction-types) "Άκυρο"))))))
	 (footer))))))

(define-dynamic-page transaction-type/view ((tx-type-id integer)) ("transaction-type/view")
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
	 (css "reset.css" "scrooge.css"))
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
	 (footer))))))

(define-dynamic-page transaction-type/edit ((tx-type-id integer)) ("transaction-type/edit")
  (with-db
    (let ((accounts (query (:select 'title 'id :from 'account)))
	  (tx-type (get-dao 'tx-type tx-type-id)))
      (with-page ()
	(:head
	 (:title "Σκρούτζ: Επεξεργασία τύπου συναλλαγής")
	 (css "reset.css" "scrooge.css"))
	(:body
	 (:div :id "header"
	       (logo)
	       (navbar "Τύποι συναλλαγών"))
	 (:div :id "body"
	       (:div :id "actions"
		     (tx-type-menu 'edit tx-type-id))
	       (:div :id "content" :class "simple-form"
		     (:h2 "Επεξεργασία τύπου συναλλαγής")
		     (with-form (edit-tx-type :tx-type-id tx-type-id)
		       (with-table (:style "formtable")
			 ((label 'title "Περιγραφή")
			  (textbox 'title :value (title tx-type)))
			 ((label 'debit-acc-id "Λογαριασμός χρέωσης")
			  (dropdown 'debit-acc-id accounts :selected (debit-acc-id tx-type)))
			 ((label 'credit-acc-id "Λογαριασμός πίστωσης")
			  (dropdown 'credit-acc-id accounts :selected (credit-acc-id tx-type))))
		       (:ul :class "prompt hmenu"
			    (:li (submit "Ενημέρωση"))
			    (:li (:a :href (transaction-types) "Άκυρο"))))))
	 (footer))))))

(define-dynamic-page transaction-type/remove ((tx-type-id integer)) ("transaction-type/remove")
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
	 (css "reset.css" "scrooge.css"))
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
	 (footer))))))





