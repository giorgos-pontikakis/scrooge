(in-package :scrooge)

(define-dynamic-page actions/account/create ((title       string)
					     (parent-id   integer #'valid-parent-acc-id-p)
					     (debit-acc-p boolean))
    ("actions/account/create" :request-type :post)
  (no-cache) 
  (with-parameter-list params 
    (if (every #'validp params)
	(with-parameter-rebinding #'val 
	  (with-db
	    (insert-dao (make-instance 'account
				       :title title
				       :parent-id parent-id
				       :debit-p debit-acc-p))
	    (redirect (accounts) :code +http-see-other+)))
	(redirect (notfound)
		  :code +http-see-other+))))

(define-dynamic-page actions/account/delete ((acc-id integer #'acc-id-exists-p))
    ("actions/account/delete" :request-type :post)
  (no-cache)
  (if (validp acc-id)
      (with-db
	(delete-dao (get-dao 'account (val acc-id))) 
	(redirect (accounts) :code +http-see-other+))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page actions/account/update ((acc-id    integer #'acc-id-exists-p) 
					     (title     string)
					     (parent-id integer #'valid-parent-acc-id-p))
    ("actions/account/update" :request-type :post)
  (no-cache) 
  (with-parameter-list params 
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (execute (:update 'account :set
			      :title title
			      :where (:= 'id acc-id)))
	    (redirect (accounts :acc-id acc-id) :code +http-see-other+)))
	(redirect (notfound) :code +http-see-other+))))

(define-dynamic-page accounts ((acc-id integer #'acc-id-exists-p))
    ("accounts/")
  (no-cache) 
  (if (validp acc-id)
      (with-parameter-rebinding #'val
	(let ((debit-p (and acc-id
			    (with-db (query (:select 'debit-p :from 'account :where (:= 'id acc-id))
					    :single)))))
	  (with-page ()
	    (:head
	     (:title "Λογαριασμοί")
	     (css "reset.css" "main.css"))
	    (:body
	     (:div :id "header"
		   (logo)
		   (primary-navbar 'config)
		   (config-navbar 'accounts)) 
	     (:div :id "body" 
		   (:div :id "debit-accounts" :class "window"
			 (apply #'account-menu
				acc-id t (if debit-p '(:create :edit :delete) '(:create)))
			 (:h2 "Χρεωστικοί Λογαριασμοί") 
			 (display-accounts t nil acc-id :view))
		   (:div :id "credit-accounts" :class "window" 
			 (apply #'account-menu
				acc-id nil (if debit-p '(:create) '(:create :edit :delete)))
			 (:h2 "Πιστωτικοί Λογαριασμοί") 
			 (display-accounts nil nil acc-id :view))
		   (footer))))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page account/create ((acc-id  integer #'acc-id-exists-p)
				     (debit-p boolean))
    ("account/create")
  (no-cache) 
  (if (every #'valid (list acc-id debit-p))
      (with-page ()
	(:head
	 (:title "Λογαριασμοί")
	 (css "reset.css" "main.css"))
	(:body
	 (:div :id "header"
	       (logo)
	       (primary-navbar 'config)
	       (config-navbar 'accounts)) 
	 (:div :id "body"
	       (:div :id "debit-accounts" :class "window"
		     (account-menu nil t)
		     (:h2 "Χρεωστικοί Λογαριασμοί") 
		     (display-accounts t nil (val acc-id) (if (val debit-p) :create :view)))
	       (:div :id "debit-accounts" :class "window"
		     (account-menu nil nil)
		     (:h2 "Πιστωτικοί Λογαριασμοί") 
		     (display-accounts nil nil (val acc-id) (if (val debit-p) :view :create)))
	       (footer))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page account/update ((acc-id integer #'acc-id-exists-p)
				     (debit-p boolean))
    ("account/update")
  (no-cache)
  (if (validp acc-id)
      (with-page ()
	(:head
	 (:title "Λογαριασμοί")
	 (css "reset.css" "main.css"))
	(:body
	 (:div :id "header"
	       (logo)
	       (primary-navbar 'config)
	       (config-navbar 'accounts)) 
	 (:div :id "body"
	       (:div :id "debit-accounts" :class "window"
		     (account-menu (val acc-id) t :view :delete)
		     (:h2 "Χρεωστικοί Λογαριασμοί")
		     (display-accounts t nil (val acc-id) (if (val debit-p) :edit :view))) 
	       (:div :id "credit-accounts" :class "window"
		     (account-menu (val acc-id) nil :view :delete)
		     (:h2 "Πιστωτικοί Λογαριασμοί") 
		     (display-accounts nil nil (val acc-id) (if (val debit-p) :view :edit)))
	       (footer))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page account/delete ((acc-id  integer #'acc-id-exists-p)
				     (debit-p boolean))
    ("account/delete")
  (no-cache)
  (if (validp acc-id)
      (with-page ()
	(:head
	 (:title "Λογαριασμοί")
	 (css "reset.css" "main.css"))
	(:body
	 (:div :id "header"
	       (logo)
	       (primary-navbar 'config)
	       (config-navbar 'accounts)) 
	 (:div :id "body" 
	       (:div :id "debit-accounts" :class "window"
		     (account-menu (val acc-id) t :view :edit)
		     (:h2 "Χρεωστικοί λογαριασμοί")
		     (display-accounts t nil (val acc-id) (if (val debit-p) :delete :view)))
	       (:div :id "credit-accounts" :class "window"
		     (account-menu (val acc-id) nil :view :edit)
		     (:h2 "Χρεωστικοί λογαριασμοί")
		     (display-accounts nil nil (val acc-id) (if (val debit-p) :view :delete)))
	       (footer))))
      (redirect (notfound) :code +http-see-other+)))

(defun display-accounts (debit-p parent-id active-id intent)
  (with-db
    (let ((accounts (if (null parent-id)
			(query (:select 'id 'title 'parent-id :from 'account
					:where (:and (:= 'debit-p debit-p)
						     (:is-null 'parent-id))))
			(query (:select 'id 'title 'parent-id :from 'account
					:where (:and (:= 'debit-p debit-p)
						     (:= 'parent-id parent-id)))))))
      (flet ((normal-row (acc-id title activep)
	       (with-html
		 (:li :class (if activep "active" nil)
		      (if activep
			  (htm (:a :href (accounts)
				   (:img :src (url "img/bullet_red.png"))))
			  (htm (:a :href (accounts :acc-id acc-id)
				   (:img :src (url "img/bullet_blue.png"))))) 
		      (str (lisp-to-html title)))))
	     (form-row-create (debit-acc-p parent-id)
	       (with-form (actions/account/create :parent-id (or parent-id :null)
						  :debit-acc-p debit-acc-p)
		 (:li :class "active"
		      (htm (:a :href (accounts)
			       (:img :src (url "img/bullet_red.png")))) 
		      (textbox 'title) 
		      (ok-button)
		      (cancel-button (accounts)))))
	     (form-row-update (acc-id title parent-id) 
	       (with-form (actions/account/update :acc-id acc-id
						  :parent-id (or parent-id :null))
		 (:li :class "active"
		      (:a :href (accounts)
			  (:img :src (url "img/bullet_red.png"))) 
		      (textbox 'title :value title) 
		      (ok-button)
		      (cancel-button (accounts :acc-id acc-id)))))
	     (form-row-delete (acc-id title)
	       (with-form (actions/account/delete :acc-id acc-id)
		 (:li :class "attention"
		      (:a :href (accounts)
			  (:img :src (url "img/bullet_red.png"))) 
		      (str (lisp-to-html title))
		      (ok-button)
		      (cancel-button (accounts :acc-id acc-id))))))
	(with-html
	  (:ul :class "indent forms-in-row"
	       (when (and (eql intent :create) (eql active-id parent-id))
		 (form-row-create debit-p parent-id))
	       (iter (for (acc-id title parent-id) in accounts)
		     (for activep = (and active-id (= active-id acc-id)))
		     (if activep
			 (case intent
			   ((or :create :view) (normal-row acc-id title activep))
			   (:edit (form-row-update acc-id title parent-id))
			   (:delete (form-row-delete acc-id title)))
			 (normal-row acc-id title activep)) 
		     (display-accounts debit-p acc-id active-id intent))))))))

(defun account-menu (acc-id debit-p &rest opt-list)
  (let ((options
	 (list :create (lambda () 
			 (with-html
			   (:li (:a :href (account/create :acc-id acc-id :debit-p debit-p)
				    (:img :src (url "img/add.png")) "Δημιουργία"))))
	       :view (lambda () 
		       (if acc-id
			   (with-html
			     (:li (:a :href (accounts :acc-id acc-id)
				      (:img :src (url "img/magnifier.png")) "Προβολή")))
			   nil))
	       :edit (lambda ()
		       (if acc-id
			   (with-html
			     (:li (:a :href (account/update :acc-id acc-id :debit-p debit-p)
				      (:img :src (url "img/pencil.png")) "Επεξεργασία")))
			   nil))
	       :delete (lambda ()
			 (if (and acc-id
				  (not (get-subaccounts acc-id))
				  (not (get-transactions acc-id)))
			     (with-html
			       (:li (:a :href (account/delete :acc-id acc-id :debit-p debit-p)
					(:img :src (url "img/delete.png")) "Διαγραφή")))
			     nil)))))
    (with-html
      (:div :class "actions"
	    (:ul :class "hmenu"
		 (iter (for opt in opt-list)
		       (funcall (getf options opt))))))))

(defun get-subaccounts (acc-id)
  (with-db
    (query (:select 'id :from 'account :where (:= 'parent-id acc-id))
	   :single)))

(defun get-transactions (acc-id)
  (with-db
    (query (:select 'id
		   :from 'tx
		   :where (:or (:= 'debit-acc-id acc-id)
			       (:= 'credit-acc-id acc-id))))))