(in-package :scrooge)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; ------------------------------------------------------------
;;; Accounts - tree widget
;;; ------------------------------------------------------------

(defclass account-table (table-ul-crud) 
  ((name :initform "banks-table")
   (header :initform '(:selector "" 
                       :title "Τράπεζα" 
                       :submit ""
                       :cancel ""))
   (styles :initform '(:active-row "active"
                       :inactive-row ""
                       :attention-row "attention"
                       :table "forms-in-row table-half")) 
   ;; page interface
   (id-keys :initform '(:id))
   (payload-keys :initform '(:title))
   (filter-keys :initform '())
   ;; crud mixin
   (main-page :initform 'banks)
   (submit-pages :initform '(:create actions/bank/create
                             :update actions/bank/update
                             :delete actions/bank/delete))
   
   (cells-fn :initform (config-cells-fn))
   (data-fn :initform (account-data-fn))
   (tbody-class :initform 'tbody-ul-crud)
   (row-class :initform 'account-row)))

(defclass account-row (row-ul-crud)
  ())

(defun make-account-table (&key
                           header styles                        ;; table
                           params id-keys data-keys filter-keys ;; page-interface
                           operation get-pages submit-pages)    ;; crud
  (make-instance 'account-table
                 ;; table
                 :header header
                 :styles styles  
                 ;; page-interface
                 :params params
                 :id-keys id-keys
                 :data-keys data-keys
                 :filter-keys filter-keys
                 ;; crud
                 :operation operation
                 :get-pages get-pages
                 :submit-pages submit-pages))

(defmethod render ((row account-row) &key)
  (let* ((table (table row))
         (cells-list (funcall (cells-fn table) row))
         (child-data (funcall (data-fn table)
                              :filters (plist-collect (id-keys table)
                                                      (data row)))))
    (with-html
      (:li :class (style row)
           (render cells-list)
           (render (make-instance 'tbody-ul-crud
                                  :table table
                                  :data child-data
                                  :style "indent"))))))



(defun account-data-fn ()
  (lambda (filters)
    (let ((parent-id (getf filters :parent-id)))
      (with-db
        (if (null parent-id)
            (query (:select 'id 'title 'parent-id :from 'account
                            :where (:and (:= 'debit-p debit-p)
                                         (:is-null 'parent-id)))
                   :plists)
            (query (:select 'id 'title 'parent-id :from 'account
                            :where (:and (:= 'debit-p debit-p)
                                         (:= 'parent-id parent-id)))))))))



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
			   ((:create :view) (normal-row acc-id title activep))
			   (:edit (form-row-update acc-id title parent-id))
			   (:delete (form-row-delete acc-id title)))
			 (normal-row acc-id title activep)) 
		     (display-accounts debit-p acc-id active-id intent))))))))



;;; ------------------------------------------------------------
;;; Accounts - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/account/create ((title       string  (complement #'account-exists-p))
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
	(with-parameter-rebinding #'raw
          (redirect (account/create :title title) :code +http-see-other+)))))

(define-dynamic-page actions/account/update ((acc-id    integer #'valid-account-id-p t) 
					     (title     string  (complement #'account-exists-p))
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
	(if (and (validp acc-id)
                 (validp parent-id))
            ;; user error, return back to update page
            (redirect (account/update :acc-id acc-id
                                      :title title
                                      :parent-id parent-id)
                      :code +http-see-other+)
            ;; URL fiddling - abort
            (redirect (notfound) :code +http-see-other+)))))

(define-dynamic-page actions/account/delete ((acc-id integer #'valid-acc-id-no-subaccounts-p t))
    ("actions/account/delete" :request-type :post)
  (no-cache)
  (if (validp acc-id)
      (with-db
	(delete-dao (get-dao 'account (val acc-id))) 
	(redirect (accounts) :code +http-see-other+))
      (redirect (notfound) :code +http-see-other+)))

;;; ------------------------------------------------------------
;;; Banks - Snippets
;;; ------------------------------------------------------------

(define-menu account-menu (acc-id debit-p) (:div-style "actions" :ul-style "hmenu")
  (:create (with-html
	     (:li (:a :href (account/create :acc-id acc-id :debit-p debit-p)
		      (:img :src (url "img/add.png")) "Δημιουργία"))))
  (:view (if acc-id
	     (with-html
	       (:li (:a :href (accounts :acc-id acc-id)
			(:img :src (url "img/magnifier.png")) "Προβολή")))
	     nil))
  (:edit (if acc-id
	     (with-html
	       (:li (:a :href (account/update :acc-id acc-id :debit-p debit-p)
			(:img :src (url "img/pencil.png")) "Επεξεργασία")))
	     nil))
  (:delete (if (and acc-id
		    (not (get-subaccounts acc-id))
		    (not (get-transactions acc-id)))
	       (with-html
		 (:li (:a :href (account/delete :acc-id acc-id :debit-p debit-p)
			  (:img :src (url "img/delete.png")) "Διαγραφή")))
	       nil)))

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


;;; ------------------------------------------------------------
;;; Banks - Pages
;;; ------------------------------------------------------------

(define-dynamic-page accounts ((acc-id integer #'valid-account-id-p))
    ("accounts/")
  (no-cache) 
  (if (validp acc-id)
      (with-parameter-rebinding #'val
	(let ((debit-p (if acc-id (debit-p acc-id) nil)))
	  (with-page ()
	    (:head
	     (:title "Λογαριασμοί")
	     (css-standard-headers))
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

(define-dynamic-page account/create ((acc-id  integer #'valid-account-id-p)
				     (debit-p boolean))
    ("account/create")
  (no-cache) 
  (if (every #'validp (list acc-id debit-p))
      (with-page ()
	(:head
	 (:title "Λογαριασμοί")
	 (css-standard-headers))
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

(define-dynamic-page account/update ((acc-id integer #'valid-account-id-p t)
				     (debit-p boolean))
    ("account/update")
  (no-cache)
  (if (validp acc-id)
      (with-page ()
	(:head
	 (:title "Λογαριασμοί")
	 (css-standard-headers))
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

(define-dynamic-page account/delete ((acc-id  integer #'valid-acc-id-no-subaccounts-p t)
				     (debit-p boolean))
    ("account/delete")
  (no-cache)
  (if (validp acc-id)
      (with-page ()
	(:head
	 (:title "Λογαριασμοί")
	 (css-standard-headers))
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



