(in-package :scrooge)

;;; Actions

(declaim (optimize (speed 0) (debug 3)))

(define-dynamic-page actions/autotx/create (description
					    (debit-acc string #'account-exists-p)
					    (credit-acc string #'account-exists-p))
    ("actions/autotx/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (let ((debit-acc-id (get-account-id debit-acc))
		(credit-acc-id (get-account-id credit-acc)))
	    (with-db
	      (insert-dao (make-instance 'autotx
					 :description description
					 :debit-acc-id debit-acc-id
					 :credit-acc-id credit-acc-id))
	      (redirect (autotx) :code +http-see-other+))))
	(with-parameter-rebinding #'raw 
	  (redirect (autotx/create :description description
				   :debit-acc debit-acc
				   :credit-acc credit-acc)
		    :code +http-see-other+)))))

(define-dynamic-page actions/autotx/delete ((autotx-id integer #'valid-autotx-id-p))
    ("actions/autotx/delete" :request-type :post)
  (no-cache)
  (if (validp autotx-id)
      (with-db
	(delete-dao (get-dao 'autotx (val autotx-id)))
	(redirect (autotx) :code +http-see-other+))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page actions/autotx/update ((autotx-id integer #'valid-autotx-id-p)
					    description
					    (debit-acc string #'account-exists-p)
					    (credit-acc string #'account-exists-p))
    ("actions/autotx/update" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (let ((debit-acc-id (get-account-id debit-acc))
		(credit-acc-id (get-account-id credit-acc)))
	    (with-db
	      (execute (:update 'autotx :set
				'description description
				'debit-acc-id debit-acc-id
				'credit-acc-id credit-acc-id
				:where (:= 'id autotx-id)))
	      (redirect (autotx) :code +http-see-other+))))
	(with-parameter-rebinding #'raw 
	  (redirect (autotx/update :autotx-id autotx-id
				   :description description
				   :debit-acc debit-acc
				   :credit-acc credit-acc)
		    :code +http-see-other+)))))

;;; Pages

(define-dynamic-page autotx ((autotx-id integer))
    ("config/autotx")
  (no-cache)
  (if (validp autotx-id)
      (with-parameter-rebinding #'val
	(with-page ()
	  (:head
	     (:title "Αυτόματες συναλλαγές")
	     (css "reset.css" "main.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'autotx))
	   (:div :id "body"
		 (:div :id "autotx" :class "window"
		       (autotx-menu autotx-id (if autotx-id
						  '(:create :update :delete)
						  '(:create)))
		       (display-autotx autotx-id :view))))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page autotx/create (description
				    (debit-acc string #'account-exists-p)
				    (credit-acc string #'account-exists-p))
    ("config/autotx/create")
  (no-cache)
  (with-parameter-list params
    (with-page ()
      (:head
       (:title "Αυτόματες συναλλαγές: Δημιουργία")
       (css "reset.css" "main.css"))
      (:body
       (:div :id "header"
	     (logo)
	     (primary-navbar 'config)
	     (config-navbar 'autotx))
       (:div :id "body"
	     (:div :id "autotx" :class "window"
		   (autotx-menu nil nil) 
		   (display-autotx nil :create params)))))))

(define-dynamic-page autotx/update ((autotx-id integer)
				    description 
				    (debit-acc string #'account-exists-p) 
				    (credit-acc string #'account-exists-p))
    ("config/autotx/update")
  (no-cache)
  (if (validp autotx-id)
      (with-parameter-list params
	(with-page ()
	  (:head
	   (:title "Αυτόματες συναλλαγές: Επεξεργασία")
	   (css "reset.css" "main.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'autotx))
	   (:div :id "body"
		 (:div :id "autotx" :class "window"
		       (autotx-menu (val autotx-id) '(:view :delete))
		       (display-autotx (val autotx-id) :update (rest params)))))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page autotx/delete ((autotx-id integer))
    ("config/autotx/delete")
  (no-cache)
  (if (validp autotx-id)
      (with-parameter-rebinding #'val
	(with-page ()
	  (:head
	   (:title "Αυτόματες συναλλαγές: Διαγραφή")
	   (css "reset.css" "main.css"))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'autotx))
	   (:div :id "body"
		 (:div :id "autotx" :class "window"
		       (autotx-menu autotx-id '(:view :update))
		       (display-autotx autotx-id :delete))))))
      (redirect (notfound) :code +http-see-other+)))


(defun display-autotx (active-id intent &optional params)
  (flet ((normal-row (id row  activep)
	   (bind (((description debit-acc credit-acc)  row))
	     (with-html
	       (:tr :class (if activep "active" nil)
		    (:td :class "select"
			 (if activep
			     (htm (:a :href (autotx)
				      (:img :src (url "img/bullet_red.png"))))
			     (htm (:a :href (autotx :autotx-id id)
				      (:img :src (url "img/bullet_blue.png"))))))
		    (:td :class "data" (str (lisp-to-html description)))
		    (:td :class "data" (str (lisp-to-html debit-acc)))
		    (:td :class "data" (str (lisp-to-html credit-acc)))
		    (:td :class "button" "")
		    (:td :class "button" "")))))
	 (form-row-create (row styles)
	   (bind (((description debit-acc credit-acc)  row)
		  ((description% debit-acc% credit-acc%) styles))
	     (with-form (actions/autotx/create)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (autotx)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (textbox 'description :value description :style description%))
		    (:td :class "data" (textbox 'debit-acc :value debit-acc :style debit-acc%))
		    (:td :class "data" (textbox 'credit-acc :value credit-acc :style credit-acc%))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (autotx)))))))
	 (form-row-update (id row styles)
	   (bind (((description debit-acc credit-acc)  row)
		  ((description% debit-acc% credit-acc%) styles))
	     (with-form (actions/autotx/update :autotx-id id)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (autotx)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (textbox 'description :value description :style description%))
		    (:td :class "data" (textbox 'debit-acc :value debit-acc :style debit-acc%))
		    (:td :class "data" (textbox 'credit-acc :value credit-acc :style credit-acc%))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (autotx :autotx-id id)))))))
	 (form-row-delete (id row)
	   (destructuring-bind (description debit-acc credit-acc) row
	     (with-form (actions/autotx/delete :autotx-id id)
	       (:tr :class "attention"
		    (:td :class "select"
			 (:a :href (autotx)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (str (lisp-to-html description)))
		    (:td :class "data" (str (lisp-to-html debit-acc)))
		    (:td :class "data" (str (lisp-to-html credit-acc)))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (autotx :autotx-id id))))))))
    (with-db
      (let ((auto-txs (query (:select 'autotx.id 'description 'debit-account.title 'credit-account.title
				      :from 'autotx
				      :left-join (:as 'account 'debit-account)
				      :on (:= 'debit-account.id 'autotx.debit-acc-id)
				      :left-join (:as 'account 'credit-account)
				      :on (:= 'credit-account.id 'autotx.credit-acc-id))))
	    (header '("" "Περιγραφή" "Λογαριασμός Χρέωσης" "Λογαριασμός Πίστωσης" "" ""))
	    (inputs (if params
			(mapcar #'val* params)
			(make-list 3)))
	    (styles (if params
			(mapcar (lambda (p) (if (validp p) nil "attention")) params)
			(make-list 3))))
	(with-html
	  (:table :id "autotx-table" :class "forms-in-row"
		  (:thead
		   (:tr (iter (for label in header) (htm (:th (str label))))))
		  (:tbody
		   (when (eql intent :create) 
		     (form-row-create inputs styles))
		   (iter (for (id . defaults) in auto-txs)
			 (for activep = (and active-id (= active-id id)))
			 (if activep
			     (let ((row (mapcar (lambda (p d) (or p d)) inputs defaults)))
			       (case intent
				 (:view (normal-row id row activep))
				 (:update (form-row-update id row styles))
				 (:delete (form-row-delete id row))))
			     (normal-row id defaults activep))))))))))

(defun autotx-menu (autotx-id opt-list)
  (let ((options
	 (list :create (lambda () 
			 (with-html
			   (:li (:a :href (autotx/create)
				    (:img :src (url "img/add.png")) "Δημιουργία"))))
	       :view (lambda () 
		       (if autotx-id
			   (with-html
			     (:li (:a :href (autotx :autotx-id autotx-id)
				      (:img :src (url "img/magnifier.png")) "Προβολή")))
			   nil))
	       :update (lambda ()
			 (if autotx-id
			     (with-html
			       (:li (:a :href (autotx/update :autotx-id autotx-id)
					(:img :src (url "img/pencil.png")) "Επεξεργασία")))
			     nil))
	       :delete (lambda ()
			 (if autotx-id
			     (with-html
			       (:li (:a :href (autotx/delete :autotx-id autotx-id)
					(:img :src (url "img/delete.png")) "Διαγραφή")))
			     nil)))))
    (with-html
      (:div :class "actions"
	    (:ul :class "hmenu"
		 (iter (for opt in opt-list)
		       (let ((fn (getf options opt)))
			 (if fn (funcall fn)))))))))