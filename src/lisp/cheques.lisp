(in-package :scrooge)

;;; --- Snippets --------------------

(defun cheque-menu (state &optional id)
  (with-db
    (let ((transactions-exist-p (and id (query (:select 'id
							:from 'tx
							:where (:= 'cheque-id id))))))
      (flet ((summary (&optional id)
	       (with-html
		 (:li (:a :href (cheques :cheque-id id)
			  (:img :src (url "img/table.png")) "Πίνακας επιταγών"))))
	     (insert () 
	       (with-html
		 (:li (:a :href (cheque/insert)
			  (:img :src (url "img/add.png")) "Δημιουργία ")))) 
	     (view (id)
	       (if id
		   (with-html
		     (:li (:a :href (cheque/view :cheque-id id)
			      (:img :src (url "img/magnifier.png")) "Προβολή")))
		   nil))
	     (edit (id)
	       (if id
		   (with-html
		     (:li (:a :href (cheque/edit :cheque-id id)
			      (:img :src (url "img/pencil.png")) "Επεξεργασία")))
		   nil)) 
	     (kill (id)
	       (if (or (null id) transactions-exist-p)
		   nil
		   (with-html
		     (:li (:a :href (cheque/remove :cheque-id id)
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
		    (edit id)
		    (unless transactions-exist-p
		      (kill id))))
		 ;; 
		 (edit
		  (progn (summary id) 
			 (view id) 
			 (unless transactions-exist-p
			   (kill id))))
		 ;; 
		 (kill
		  (progn (summary id) 
			 (view id)
			 (edit id))))))))))

(defun cheques-table (active-id cheques header)
  (with-html
    (:table :class "dbtable"
	    (:thead
	     (:tr (iter (for label in header)
			(htm (:th (str label))))))
	    (:tbody
	     (iter (for row in cheques) 
		   (destructuring-bind (id bank issuer payee due-date amount) row
		     (let ((activep (and active-id (= active-id id))))
		       (htm
			(:tr :class (if activep "active" nil)
			     (:td (:a :href (cheques :cheque-id id)
				      (:img :src (url (if activep
							  "img/bullet_red.png"
							  "img/bullet_blue.png"))))) 
			     (:td (:p (str (lisp-to-html bank))))
			     (:td (:p (str (lisp-to-html issuer))))
			     (:td (:p (str (lisp-to-html payee))))
			     (:td (:p (str (lisp-to-html due-date))))
			     (:td (:p (str (lisp-to-html amount)))))))))))))


(defun cheque-errorbar (bank due-date amount)
  (with-html 
    (:div :id "msg"
	  (:ul :class "errorbar"
	       (when due-date
		 (htm (:li "Άκυρη ημερομηνία")))
	       (when bank
		 (htm (:li "Η τράπεζα αυτή δεν έχει οριστεί.")))
	       (when amount
		 (htm (:li "Μη αποδεκτό χρηματικό ποσό")))))))

;;; --- Actions --------------------

(define-dynamic-page insert-cheque ((bank string #'bank-exists-p)
				    issuer
				    (due-date date)
				    (amount integer #'positive-p))
    ("actions/cheque/insert" :request-type :post)
  (no-cache)
  (with-auth "root"
    (with-error-plist (errors)
      (if errors
	  (see-other (cheque/insert :bank (or bank bank*)
				    :issuer issuer
				    :due-date (or due-date due-date*)
				    :amount (or amount amount*)))
	  (with-db
	    (let ((bank-id (query (:select 'id
					   :from 'bank
					   :where (:= 'title bank))
				  :single!)))
	      (create-cheque issuer bank-id due-date amount)
	      (see-other (cheques))))))))

(define-dynamic-page edit-cheque ((cheque-id integer #'valid-cheque-id-p)
				  (bank string #'bank-exists-p)
				  issuer
				  (due-date date)
				  (amount integer #'positive-p))
    ("actions/cheque/edit" :request-type :post)
  (no-cache)
  (with-auth "root"
    (with-error-plist (errors)
      (if errors
	  (see-other (cheque/edit :cheque-id cheque-id
				  :bank (or bank bank*)
				  :issuer issuer
				  :due-date (or due-date due-date*)
				  :amount (or amount amount*)))
	  (with-db
	    (let ((bank-id (query (:select 'id
					   :from 'bank
					   :where (:= 'title bank))
				  :single!)))
	      (update-cheque cheque-id
			     :issuer issuer
			     :bank-id bank-id
			     :due-date due-date
			     :amount amount)
	      (see-other (cheque/view :cheque-id cheque-id))))))))

(define-dynamic-page remove-cheque ((cheque-id integer #'valid-cheque-id-p))
    ("actions/cheque/remove" :request-type :post)
  (no-cache)
  (with-error-plist (errors)
    (if errors
	(see-other (cheque/notfound))
	(with-auth "root"
	  (delete-cheque cheque-id)
	  (see-other (cheques))))))

;;; --- Pages --------------------

(define-dynamic-page cheques ((cheque-id integer)) ("cheques/")
  (no-cache)
  (if cheque-id*
      (see-other (cheque/notfound))
      (with-auth "root"
	(with-db
	  (let ((cheques
		 (query (:union (:select 'cheque.id 'bank.title "gnp" 'payee.title 'due-date 'amount
					 :from 'cheque
					 :inner-join (:as 'company 'payee) :on (:= 'payee.id 'payee-id) 
					 :inner-join 'bank :on (:= 'bank.id 'cheque.bank-id)
					 :where (:is-null 'issuer-id))
				(:select 'cheque.id 'bank.title 'issuer.title "gnp" 'due-date 'amount
					 :from 'cheque
					 :inner-join (:as 'company 'issuer) :on (:= 'issuer.id 'issuer-id) 
					 :inner-join 'bank :on (:= 'bank.id 'cheque.bank-id)
					 :where (:is-null 'payee-id)))))
		(header '("" "Τράπεζα" "Εκδότης" "Δικαιούχος" "Ημερομηνία" "Ποσό")))
	    (with-page ()
	      (:head
	       (:title "Σκρουτζ: Συναλλασσόμενοι")
	       (css "reset.css" "scrooge.css"))
	      (:body
	       (:div :id "header"
		     (logo)
		     (navbar "Επιταγές"))
	       (:div :id "body"
		     (:div :id "actions"
			   (cheque-menu 'summary cheque-id))	       
		     (:div :id "content" :class "summary"
			   (cheques-table cheque-id cheques header))
		     (footer)))))))))

(define-dynamic-page cheque/insert ((bank string #'bank-exists-p)
				    issuer
				    (due-date date)
				    (amount integer #'positive-p))
    ("cheque/insert")
  (no-cache)
  (with-auth "root"
    (with-db
      (with-page ()
	(:head
	 (:title "Σκρούτζ: Εισαγωγή επιταγής")
	 (css "reset.css" "scrooge.css")
	 (js-headers))
	(:body
	 (:div :id "header"
	       (logo)
	       (navbar "Επιταγές"))
	 (:div :id "body"
	       (:div :id "actions"
		     (cheque-menu 'insert))
	       (when (or bank* due-date* amount*)
		 (cheque-errorbar bank* due-date* amount*))
	       (:div :id "content" :class "simple-form"
		     (:h2 "Εισαγωγή επιταγής")
		     (with-form (insert-cheque)
		       (with-table (:style "formtable")
			 ((label 'issuer "Εκδότης")
			  (textbox 'issuer :value issuer))
			 ((label 'bank "Τράπεζα")
			  (textbox 'bank
				   :value (or bank bank*)
				   :style (if bank* "invalid" nil)))
			 ((label 'due-date "Ημερομηνία έκδοσης")
			  (textbox 'due-date
				   :value (or due-date due-date*)
				   :style (if due-date* "invalid" nil)))
			 ((label 'amount "Ποσό")
			  (textbox 'amount
				   :value (or amount amount*)
				   :style (if amount* "invalid" nil))))
		       (:ul :class "prompt hmenu"
			    (:li (submit "Δημιουργία"))
			    (:li (:a :href (cheques) "Ακύρωση"))))))
	 (footer))))))

(define-dynamic-page cheque/view ((cheque-id integer #'valid-cheque-id-p)) ("cheque/view")
  (no-cache)
  (if cheque-id*
      (see-other (cheque/notfound)) 
      (with-auth "root"
	(with-db
	  (let* ((cheque (get-dao 'cheque cheque-id))
		 (bank (query (:select 'title :from 'bank :where (:= 'id (bank-id cheque)))
			      :single)))
	    (with-page ()
	      (:head
	       (:title "Σκρουτζ: Προβολή επιταγής")
	       (css "reset.css" "scrooge.css"))
	      (:body
	       (:div :id "header"
		     (logo)
		     (navbar "Επιταγές"))
	       (:div :id "body" 
		     (:div :id "actions"
			   (cheque-menu 'view cheque-id))
		     (:div :id "content" :class "simple-form"
			   (:h2 "Στοιχεία επιταγής")
			   (with-table (:style "formtable")
			     ((label 'issuer "Εκδότης")
			      (textbox 'issuer :value (issuer cheque) :readonlyp t))
			     ((label 'bank "Τράπεζα")
			      (textbox 'bank :value bank :readonlyp t))
			     ((label 'due-date "Ημερομηνία έκδοσης")
			      (textbox 'due-date :value (due-date cheque)  :readonlyp t))
			     ((label 'amount "Ποσό")
			      (textbox 'amount :value (amount cheque) :readonlyp t))))))))))))

(define-dynamic-page cheque/edit ((cheque-id integer #'valid-cheque-id-p)
				  (bank string #'bank-exists-p)
				  issuer
				  (due-date date)
				  (amount integer #'positive-p))
    ("cheque/edit")
  (no-cache)
  (if cheque-id*
      (see-other (cheque/notfound))
      (with-auth "root"
	(with-db
	  (let ((cheque (get-dao 'cheque cheque-id)))
	    (with-page ()
	      (:head
	       (:title "Σκρουτζ: Επεξεργασία επιταγής")
	       (css "reset.css" "scrooge.css")
	       (js-headers))
	      (:body
	       (:div :id "header"
		     (logo)
		     (navbar "Επιταγές"))
	       (:div :id "body"
		     (:div :id "actions"
			   (cheque-menu 'edit cheque-id))
		     (when (or bank* due-date* amount*)
		       (cheque-errorbar bank* due-date* amount*))
		     (:div :id "content" :class "simple-form"
			   (:h2 "Επεξεργασία επιταγής")
			   (with-form (edit-cheque :cheque-id cheque-id)
			     (with-table (:style "formtable")
			       ((label 'issuer "Εκδότης")
				(textbox 'issuer :value (or issuer (issuer cheque))))
			       ((label 'bank "Τράπεζα")
				(textbox 'bank
					 :value (or bank
						    bank*
						    (query (:select 'title
								    :from 'bank
								    :where (:= 'id (bank-id cheque)))
							   :single))
					 :style (if bank* "invalid" nil))) 
			       ((label 'due-date "Ημερομηνία έκδοσης")
				(textbox 'due-date
					 :value (or due-date due-date* (due-date cheque))
					 :style (if due-date* "invalid" nil)))
			       ((label 'amount "Ποσό")
				(textbox 'amount
					 :value (or amount amount* (amount cheque))
					 :style (if amount* "invalid" nil))))
			     (:ul :class "prompt hmenu"
				  (:li (submit "Ενημέρωση"))
				  (:li (:a :href (cheque/view :cheque-id cheque-id)
					   "Ακύρωση")))))))))))))

(define-dynamic-page cheque/remove ((cheque-id integer #'valid-cheque-id-p))
    ("cheque/remove")
  (no-cache)
  (if cheque-id*
      (see-other (cheque/notfound))
      (with-auth "root"
	(with-db
	  (let* ((cheque (get-dao 'cheque cheque-id))
		 (bank (query (:select 'title :from 'bank :where (:= 'id (bank-id cheque)))
			      :single)))
	    (with-page ()
	      (:head
	       (:title "Σκρουτζ: Προβολή επιταγής")
	       (css "reset.css" "scrooge.css"))
	      (:body 
	       (:div :id "header"
		     (logo)
		     (navbar "Επιταγές"))
	       (:div :id "body"
		     (:div :id "actions"
			   (cheque-menu 'view cheque-id))
		     (:div :id "content" :class "simple-form"
			   (:h2 "Διαγραφή επιταγής")
			   (with-form (remove-cheque :cheque-id cheque-id)
			     (with-table (:style "formtable")
			       ((label 'issuer "Εκδότης")
				(textbox 'issuer :value (issuer cheque) :readonlyp t))
			       ((label 'bank "Τράπεζα")
				(textbox 'bank :value bank :readonlyp t))
			       ((label 'due-date "Ημερομηνία έκδοσης")
				(textbox 'due-date :value (due-date cheque) :readonlyp t))
			       ((label 'amount "Ποσό")
				(textbox 'amount :value (amount cheque) :readonlyp t)))
			     (:ul :class "prompt hmenu"
				  (:li (submit "Διαγραφή"))
				  (:li (:a :href (cheque/view :cheque-id cheque-id)
					   "Ακύρωση")))))))))))))


(define-dynamic-page cheque/notfound () ("cheque/notfound")
  (no-cache)
  (with-page ()
    (:head
     (:title "Σκρούτζ: Άγνωστη επιταγή")
     (css "reset.css" "scrooge.css"))
    (:body
     (:div :id "header"
	   (logo)
	   (navbar "Επιταγές"))
     (:div :id "body"
	   (:div :id "content" :class "summary"
		 (:p "Η επιταγή που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
		 (:p "Επιστρέψτε στο μενού των επιταγών και προσπαθήστε ξανά."))))))