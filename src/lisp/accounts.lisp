(in-package :scrooge)

;;; --- Snippets --------------------

(defun account-menu (state &optional id)
  (with-db
    (let ((children-exist-p (and id (query (:select 'id
						    :from 'account
						    :where (:= 'parent-id id)))))
	  (tx-types-exist-p (and id (query (:select 'id
						    :from 'tx-type
						    :where (:or (:= 'debit-acc-id id)
								(:= 'credit-acc-id id))))))) 
      (flet ((summary (&optional id)
	       (with-html
		 (:li (:a :href (accounts :account-id id)
			  (:img :src (url "img/table.png")) "Πίνακας λογαριασμών"))))
	     (insert ()
	       (with-html
		 (:li (:a :href (account/insert)
			  (:img :src (url "img/add.png")) "Δημιουργία"))))
	     (insert-child (id)
	       (if id
		   (with-html
		     (:li (:a :href (account/insert :parent-id id)
			      (:img :src (url "img/add.png")) "Δημιουργία υπολογαριασμού")))
		   nil))
	     (view (id)
	       (if id
		   (with-html
		     (:li (:a :href (account/view :account-id id)
			      (:img :src (url "img/magnifier.png")) "Προβολή")))
		   nil))
	     (edit (id)
	       (if id
		   (with-html
		     (:li (:a :href (account/edit :account-id id)
			      (:img :src (url "img/pencil.png")) "Επεξεργασία")))
		   nil))
	     (kill (id)
	       (if (or (null id) children-exist-p tx-types-exist-p)
		   nil
		   (with-html
		     (:li (:a :href (account/remove :account-id id)
			      (:img :src (url "img/delete.png")) "Διαγραφή"))))))
	(with-html
	  (:ul :class "hmenu" 
	       (ecase state
		 ;;
		 (summary (progn 
			    (insert)
			    (view id)
			    (edit id)
			    (insert-child id)
			    (kill id)))
		 ;;
		 (insert (summary))
		 ;;
		 (view (summary id)
		       (edit id)
		       (kill id))
		 ;; 
		 (edit (progn (summary id)
			      (view id)
			      (kill id)))
		 ;; 
		 (kill (progn (summary id)
			      (view id)
			      (edit id))))))))))

(defun account-errorbar (title)
  (with-html 
    (:div :id "msg"
	  (:ul :class "errorbar"
	       (when title
		 (htm (:li "Άκυρο όνομα λογαριασμού")))))))


;;; --- Actions --------------------

(define-dynamic-page insert-account ((parent-id integer (lambda (id)
							  (or (eq :null id)
							      (valid-account-id-p id)))) 
				     (title             string #'not-db-null-p)
				     (debit-p           boolean))
    ("actions/account/insert" :request-type :post)
  (no-cache)
  (with-auth "root"
    (with-error-plist (errors)
      (if errors
	  (see-other (account/insert :parent-id parent-id
				     :title (or title title*)
				     :debit-p debit-p))
	  (progn
	    (create-account parent-id title debit-p )
	    (see-other (accounts)))))))

(define-dynamic-page edit-account ((account-id integer #'valid-account-id-p)
				   (title      string  #'not-db-null-p))
    ("actions/account/edit" :request-type :post)
  (no-cache) 
  (with-auth "root"
    (with-error-plist (errors)
      (if errors
	  (see-other (account/edit :account-id account-id 
				   :title (or title title*)))
	  (with-db
	    (let ((account (get-dao 'account account-id)))
	      (update-account account-id
			      :parent-id (parent-id account) 
			      :title title
			      :debit-p (debit-p account))
	      (see-other (account/view :account-id account-id))))))))

(define-dynamic-page remove-account ((account-id integer))
    ("actions/accounts/remove" :request-type :post)
  (no-cache)
  (with-auth "root"
    (delete-account account-id)
    (see-other (accounts))))


;;; --- Pages --------------------

(define-dynamic-page accounts ((account-id integer #'valid-account-id-p))
    ("accounts/")
  (no-cache)
  (if account-id*
      (see-other (account/notfound))
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
		     (footer)))))))))

(define-dynamic-page account/insert ((parent-id integer (lambda (id)
							  (or (eq :null id)
							      (valid-account-id-p id))))
				     (title     string  #'not-db-null-p)
				     (debit-p   boolean))
    ("account/insert")
  (no-cache)
  (if parent-id*
      (see-other (account/notfound))
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
		     (account-menu 'insert parent-id))
	       (when title*
		 (account-errorbar title*))
	       (:div :id "content" :class "simple-form" 
		     (if (null parent-id)
			 ;; Root account, no parent
			 (htm
			  (:h2 "Εισαγωγή λογαριασμού")
			  (with-form (insert-account :parent-id :null)
			    (with-table (:style "formtable")
			      ((label 'title "Ονομασία λογαριασμού")
			       (textbox 'title
					:value (or title title*)
					:style (if title* "invalid" nil)))
			      ((label 'debit-p "Είδος λογαριασμού")
			       (radio 'debit-p '(("Χρεωστικός" t)
						 ("Πιστωτικός" nil))
				      :checked debit-p)))
			    (:ul :class "prompt hmenu"
				 (:li (submit "Δημιουργία"))
				 (:li (:a :href (accounts) "Ακύρωση")))))
			 ;; Non-root account, parent exists
			 (with-db 
			   (let ((parent-account (get-dao 'account parent-id)))
			     (htm
			      (:h2 "Εισαγωγή υπο-λογαριασμού")
			      (with-form (insert-account :parent-id (id parent-account)
							 :debit-p (debit-p parent-account))
				(with-table (:style "formtable")
				  ((label 'parent-title "Ονομασία λογαριασμού-γονέα")
				   (textbox 'parent-title
					    :value (title parent-account) 
					    :disabledp t))
				  ((label 'title "Ονομασία λογαριασμού")
				   (textbox 'title
					    :value (or title title*)
					    :style (if title* "invalid" nil))))
				(:ul :class "prompt hmenu"
				     (:li (submit "Δημιουργία"))
				     (:li (:a :href (accounts) "Ακύρωση")))))))))
	       (footer))))))

(define-dynamic-page account/view ((account-id integer #'valid-account-id-p)) ("account/view")
  (no-cache)
  (if account-id*
      (see-other (account/notfound))
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
		   (footer))))))))

(define-dynamic-page account/edit ((account-id integer #'valid-account-id-p)
				   (title      string  #'not-db-null-p))
    ("account/edit")
  (no-cache)
  (if account-id*
      (see-other (account/notfound))
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
		   (when title*
		     (account-errorbar title*))
		   (:div :id "content" :class "simple-form"
			 (:h2 "Επεξεργασία λογαριασμού")
			 (with-form (edit-account :account-id account-id 
						  :debit-p (debit-p account)) 
			   (with-table (:style "formtable")
			     ((label 'title "Ονομασία λογαριασμού")
			      (textbox 'title
				       :value (or title title* (title account))
				       :style (if title* "invalid" nil))))
			   (:ul :class "prompt hmenu"
				(:li (submit "Ενημέρωση"))
				(:li (:a :href (account/view :account-id account-id)
					 "Ακύρωση")))))
		   (footer))))))))

(define-dynamic-page account/remove ((account-id integer #'valid-account-id-p)) ("account/remove")
  (no-cache)
  (if account-id*
      (see-other (account/notfound))
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
					 "Ακύρωση"))))))))))))

(define-dynamic-page account/notfound () ("account/notfound")
  (with-page ()
    (:head
     (:title "Σκρούτζ: Άγνωστος λογαριασμός")
     (css "reset.css" "scrooge.css"))
    (:body
     (:div :id "header"
	   (logo)
	   (navbar nil))
     (:div :id "body"
	   (:div :id "content" :class "summary"
		 (:p "Ο λογαριασμός που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
		 (:p "Επιστρέψτε στο μενού των συναλλαγών και προσπαθήστε ξανά."))))))