(in-package :scrooge)


;;; --- Snippets --------------------

(defun company-menu (state &optional id)
  (with-db
    (let ((transactions-exist-p (and id (query (:select 'id
							:from 'tx
							:where (:= 'tx.company-id id))))))
      (flet ((summary (&optional id)
	       (with-html
		 (:li (:a :href (companies :company-id id)
			  (:img :src (url "img/table.png")) "Πίνακας εταιριών"))))
	     (insert () 
	       (with-html
		 (:li (:a :href (company/insert)
			  (:img :src (url "img/add.png")) "Δημιουργία"))))
	     (view (id)
	       (if id
		   (with-html
		     (:li (:a :href (company/view :company-id id)
			      (:img :src (url "img/magnifier.png")) "Προβολή")))
		   nil))
	     (edit (id)
	       (if id
		   (with-html
		     (:li (:a :href (company/edit :company-id id)
			      (:img :src (url "img/pencil.png")) "Επεξεργασία")))
		   nil))
	     (transactions (id)
	       (if id
		   (with-html
		     (:li (:a :href (transactions :company-id id)
			      (:img :src (url "img/money.png")) "Συναλλαγές")))
		   nil))
	     (kill (id)
	       (if (or (null id) transactions-exist-p)
		   nil
		   (with-html
		     (:li (:a :href (company/remove :company-id id)
			      (:img :src (url "img/delete.png")) "Διαγραφή")))))) 
	(with-html
	  (:ul :class "hmenu" 
	       (ecase state
		 ;;
		 (summary
		  (with-db 
		    (insert)
		    (when id
		      (view id)
		      (transactions id)
		      (edit id)
		      (unless transactions-exist-p
			(kill id)))))
		 ;;
		 (insert
		  (summary))
		 ;;
		 (view
		  (with-db
		    (summary id)
		    (transactions id)
		    (edit id)
		    (unless transactions-exist-p
		      (kill id))))
		 ;; 
		 (edit
		  (progn (summary id)
			 (transactions id)
			 (view id) 
			 (unless transactions-exist-p
			   (kill id))))
		 ;; 
		 (kill
		  (progn (summary id)
			 (transactions id)
			 (view id)
			 (edit id))))))))))

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
			     (:td (:p (str (lisp-to-html tin))))
			     (:td (:p (str (lisp-to-html doy)))))))))))))

(defun company-errorbar (title tof tin city pobox zipcode)
  (with-html 
    (:div :id "msg"
	  (:ul :class "errorbar"
	       (when title
		 (htm (:li "Άκυρο όνομα εταιρίας")))
	       (when tof
		 (htm (:li "Η Δ.Ο.Υ. αυτή δεν έχει οριστεί.")))
	       (when tin
		 (htm (:li "Άκυρος Α.Φ.Μ.")))
	       (when city
		 (htm (:li "Η πόλη αυτή δεν έχει οριστεί.")))
	       (when pobox
		 (htm (:li "Μη αποδεκτός αριθμός ταχυδρομικής θυρίδας.")))
	       (when zipcode
		 (htm (:li "Μη αποδεκτός ταχυδρομικός κωδικός.")))))))


;;; --- Actions --------------------

(define-dynamic-page insert-company ((title      string  #'not-db-null-p)
				     (occupation string)
				     (tof        string  #'valid-tof-p)
				     (tin        string  #'valid-tin-p)
				     (address    string)
				     (city       string  #'valid-city-p)
				     (pobox      integer #'positive-p)
				     (zipcode    integer #'positive-p))
    ("actions/company/insert" :request-type :post)
  (no-cache) 
  (with-auth "root"
    (with-error-plist (errors) 
      (if errors
	  (see-other (company/insert :title (or title title*)
				     :occupation occupation
				     :tof (or tof tof*)
				     :tin (or tin tin*)
				     :address address
				     :city (or city city*) 
				     :zipcode (or zipcode zipcode*)
				     :pobox (or pobox pobox*)))
	  (with-db
	    (let ((tof-id (or (query (:select 'id
					      :from 'tof
					      :where (:= 'title tof))
				     :single)
			      :null))
		  (city-id (or (query (:select 'id
					       :from 'city
					       :where (:= 'title city))
				      :single)
			       :null)))
	      (create-company title occupation tof-id tin address city-id pobox zipcode)
	      (see-other (companies))))))))

(define-dynamic-page edit-company ((company-id integer #'valid-company-id-p)
				   (title      string)
				   (occupation string) 
				   (tof        string  #'valid-tof-p)
				   (tin        string  #'valid-tin-p)
				   (address    string)
				   (city       integer #'valid-city-p)
				   (pobox      integer #'positive-p)
				   (zipcode    integer #'positive-p))
    ("actions/company/edit" :request-type :post)
  (no-cache)
  (with-auth "root" 
    (with-error-plist (errors)
      (if errors
	  (see-other (company/edit :company-id company-id
				   :title (or title title*)
				   :occupation occupation
				   :tof (or tof tof*)
				   :tin (or tin tin*)
				   :address address
				   :city (or city city*)
				   :zipcode (or zipcode zipcode*)
				   :pobox (or pobox pobox*)))
	  (let ((tof-id (or (query (:select 'id
					    :from 'tof
					    :where (:= 'title tof))
				   :single)
			    :null))
		(city-id (or (query (:select 'id
					     :from 'city
					     :where (:= 'title city))
				    :single)
			     :null)))
	    
	    (update-company company-id
			    :title title
			    :occupation occupation
			    :tof-id tof-id
			    :tin tin
			    :address address
			    :city-id city-id
			    :pobox pobox
			    :zipcode zipcode)
	    (see-other (company/view :company-id company-id)))))))

(define-dynamic-page remove-company ((company-id integer))
    ("actions/company/remove" :request-type :post)
  (no-cache)
  (with-error-plist (errors)
    (if errors
	(see-other (company/notfound))
	(with-auth "root"
	  (delete-company company-id)
	  (see-other (companies))))))


;;; --- Pages --------------------

(define-dynamic-page companies ((company-id integer)) ("companies/")
  (no-cache)
  (if company-id*
      (see-other (company/notfound))
      (with-auth "root"
	(with-db
	  (let ((companies (query (:select 'company.id 'company.title 'company.tin 'tof.title
					   :from 'company
					   :left-join 'tof
					   :on (:= 'company.tof-id 'tof.id))))
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
		     (footer)))))))))

(define-dynamic-page company/insert ((title      string  #'not-db-null-p)
				     (occupation string)
				     (tof        string #'valid-tof-p)
				     (tin        string  #'valid-tin-p)
				     (address    string)
				     (city       string  #'valid-city-p)
				     (zipcode    integer #'positive-p)
				     (pobox      integer #'positive-p))
    ("company/insert") 
  (no-cache) 
  (with-auth "root"
    (with-db
      (with-page ()
	(:head
	 (:title "Σκρουτζ: Εισαγωγή εταιρίας")
	 (css "reset.css" "scrooge.css")
	 (js-headers))
	(:body
	 (:div :id "header"
	       (logo)
	       (navbar "Εταιρίες")) 
	 (:div :id "body"
	       (:div :id "actions" 
		     (company-menu 'insert))
	       (when (or title* tof* tin* city* pobox* zipcode*)
		 (company-errorbar title* tof* tin* city* pobox* zipcode*))
	       (:div :id "content" :class "simple-form"
		     (:h2 "Εισαγωγή εταιρίας")
		     (with-form (insert-company)
		       (with-table (:style "formtable") 
			 ((:p (label 'title "Επωνυμία" :style "strong"))
			  (textbox 'title :value (or title title*)
				   :style (if title* "invalid" nil))))
		       (:fieldset
			(:legend :class "strong" "Φορολογικά στοιχεία")
			(with-table (:style "formtable")
			  ((label 'occupation "Επάγγελμα")
			   (textbox 'occupation :value occupation))
			  ((label 'tin "Α.Φ.Μ.")
			   (textbox 'tin
				    :value (or tin tin*)
				    :style (if tin* "invalid" nil)))
			  ((label 'tof "Δ.Ο.Υ.")
			   (textbox 'tof
				    :value (or tof tof*)
				    :style (if tof* "invalid" nil)))))
		       (:fieldset
			(:legend :class "strong" "Στοιχεία Έδρας")
			(with-table (:style "formtable")
			  ((label 'address "Διεύθυνση")
			   (textbox 'address :value address))
			  ((label 'city "Πόλη")
			   (textbox 'city
				    :value (or city city*)
				    :style (if city* "invalid" nil))) 
			  ((label 'zipcode "Ταχυδρομικός κώδικας")
			   (textbox 'zipcode
				    :value (or zipcode zipcode*)
				    :style (if zipcode* "invalid" nil)))
			  ((label 'pobox "Ταχυδρομική θυρίδα")
			   (textbox 'pobox
				    :value (or pobox pobox*)
				    :style (if pobox* "invalid" nil)))))
		       (:ul :class "prompt hmenu" 
			    (:li (submit "Δημιουργία"))
			    (:li (:a :href (companies) "Ακύρωση")))))
	       (footer)))))))

(define-dynamic-page company/view ((company-id integer)) ("company/view")
  (no-cache)
  (if company-id*
      (see-other (company/notfound))
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
		     (footer)))))))))

(define-dynamic-page company/edit ((company-id integer)
				   (title      string  #'not-db-null-p)
				   (occupation string)
				   (tof        string  #'valid-tof-p)
				   (tin        string  #'valid-tin-p)
				   (address    string)
				   (city       string  #'valid-city-p)
				   (zipcode    integer #'positive-p)
				   (pobox      integer #'positive-p))
    ("company/edit")
  (no-cache) 
  (with-auth "root"
    (if company-id*
	(see-other (company/notfound))
	(with-db
	  (let ((company (get-dao 'company company-id)))
	    (with-page ()
	      (:head
	       (:title "Σκρουτζ: Επεξεργασία συναλλασσόμενου: " (title company))
	       (css "reset.css" "scrooge.css")
	       (js-headers))
	      (:body
	       (:div :id "header"
		     (logo)
		     (navbar "Εταιρίες"))
	       (:div :id "body"
		     (:div :id "actions"
			   (company-menu 'edit company-id))
		     (when (or title* tof* tin* city* pobox* zipcode*)
		       (company-errorbar title* tof* tin* city* pobox* zipcode*))
		     (:div :id "content" :class "simple-form"
			   (:h2 "Επεξεργασία εταιρίας")
			   (with-form (edit-company :company-id company-id) 
			     (with-table (:style "formtable")
			       ((:p (label 'title "Επωνυμία" :style "strong"))
				(textbox 'title :value (or title (title company)))))
			     (:fieldset
			      (:legend "Φορολογικά στοιχεία")
			      (with-table (:style "formtable")
				((label 'occupation "Επάγγελμα")
				 (textbox 'occupation
					  :value (or occupation (occupation company))))
				((label 'tin "Α.Φ.Μ.")
				 (textbox 'tin
					  :value (or tin tin* (tin company))
					  :style (if tin* "invalid" nil)))
				((label 'tof "Δ.Ο.Υ.")
				 (textbox 'tof
					  :value (or tof
						     tof*
						     (query
						      (:select 'title
							       :from 'tof
							       :where (:= 'id (tof-id company)))
						      :single))
					  :style (if tof* "invalid" nil)))))
			     (:fieldset
			      (:legend "Διεύθυνση")
			      (with-table (:style "formtable")
				((label 'address "Διεύθυνση:")
				 (textbox 'address  :value (or address (address company))))
				((label 'city "Πόλη")
				 (textbox 'city
					  :value (or city
						     city*
						     (query
						      (:select 'title
							       :from 'city
							       :where (:= 'id (city-id company)))
						      :single))
					  :style (if city* "invalid" nil)))
				((label 'zipcode "Ταχυδρομικός κώδικας")
				 (textbox 'zipcode
					  :value (or zipcode zipcode* (zipcode company))
					  :style (if zipcode* "invalid" nil)))
				((label 'pobox "Ταχυδρομική θυρίδα")
				 (textbox 'pobox
					  :value (or pobox pobox* (pobox company))
					  :style (if pobox* "invalid" nil)))))
			     (:ul :class "prompt hmenu"
				  (:li (submit "Ενημέρωση"))
				  (:li (:a :href (company/view :company-id company-id) "Ακύρωση")))))
		     (footer)))))))))

(define-dynamic-page company/remove ((company-id integer #'valid-company-id-p)) ("company/remove")
  (no-cache)
  (with-auth "root"
    (if company-id*
	(see-other (company/notfound)) 
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
				(textbox 'title :value (title company) :readonlyp t)))
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
		     (footer)))))))))

(define-dynamic-page company/notfound () ("company/notfound")
  (no-cache)
  (with-page ()
    (:head
     (:title "Σκρούτζ: Άγνωστη εταιρία")
     (css "reset.css" "scrooge.css"))
    (:body
     (:div :id "header"
	   (logo)
	   (navbar "Εταιρίες"))
     (:div :id "body"
	   (:div :id "content" :class "summary"
		 (:p "Η εταιρία που προσπαθείτε να προσπελάσετε δεν υπάρχει πια.")
		 (:p "Επιστρέψτε στο μενού των εταιριών και προσπαθήστε ξανά."))))))