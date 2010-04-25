(in-package :scrooge)


;;; Config main page

(define-dynamic-page config () ("config/")
  (no-cache)
  (with-page ()
    (:head
     (:title "Ρυθμίσεις")
     (css-standard-headers))
    (:body
     (:div :id "header"
	   (logo)
	   (primary-navbar 'config)
	   (config-navbar))
     (:div :id "body" 
	   (:div :id "content" :class "window"
		 "Don't touch")
	   (footer)))))

(defun config-navbar (&optional active-item)
  (let ((options 
	 (list 'banks (lambda (class)
			(with-html
			  (:li (:a :class class :href (banks) "Τράπεζες"))))
	       'tofs (lambda (class)
		       (with-html
			 (:li (:a :class class :href (tofs) "Δ.Ο.Υ."))))
	       'accounts (lambda (class)
			   (with-html
			     (:li (:a :class class :href (accounts) "Λογαριασμοί"))))
	       'temtx (lambda (class)
			(with-html
			  (:li (:a :class class :href (temtx) "Πρότυπες Συναλλαγές"))))
	       'fsm (lambda (class)
		      (with-html
			(:li (:a :class class :href (fsm) " FSM transitions")))))))
    (with-html
      (:div :id "subnavbar"
	    (:ul :class "hmenu"
		 (iter (for item in options by #'cddr)
		       (for fn in (rest options) by #'cddr)
		       (funcall fn (if (eql item active-item) "active" nil))))))))


;;; ------------------------------------------------------------
;;; Banks
;;; ------------------------------------------------------------

;;; Banks - Actions

(define-dynamic-page actions/bank/create ((id string)
					  (title string))
    ("actions/bank/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (insert-dao (make-instance 'bank :id id :title title))
	    (redirect (banks :id id) :code +http-see-other+)))
	(with-parameter-rebinding #'raw
	  (redirect (bank/create :id id) :code +http-see-other+)))))

(define-dynamic-page actions/bank/update ((old-id string)
					  (id string)
					  (title string))
    ("actions/bank/update" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (execute (:update 'bank :set
			      'id id
			      'title title
			      :where (:= 'id old-id)))
	    (redirect (banks :id id) :code +http-see-other+)))
	(with-parameter-rebinding #'raw
	  ;; :TODO: The new id is lost in this phase 
	  (redirect (bank/update :id old-id :title title) :code +http-see-other+)))))

(define-dynamic-page actions/bank/delete ((id string))
    ("actions/bank/delete" :request-type :post)
  (if (validp id)
      (with-db
	(delete-dao (get-dao 'bank (val id)))
	(redirect (banks) :code +http-see-other+))
      (redirect (notfound) :code +http-see-other+)))

;;; Banks - Snippets

(defun bank-menu (id &rest opt-list)
  (let ((options
	 (list :create (lambda (id)
			 (declare (ignore id))
			 (with-html
			   (:li (:a :href (bank/create)
				    (:img :src (url "img/add.png")) "Δημιουργία"))))
	       :view (lambda (id) 
		       (with-html
			 (:li (:a :href (banks :id id)
				  (:img :src (url "img/magnifier.png")) "Προβολή"))))
	       :edit (lambda (id)
		       (if id
			   (with-html
			     (:li (:a :href (bank/update :id id)
				      (:img :src (url "img/pencil.png")) "Επεξεργασία")))
			   nil))
	       :delete (lambda (id)
			 (with-db
			   (let ((cheques-exist-p (and id
						       (query (:select 'id
								       :from 'cheque
								       :where (:= 'bank-id id))))))
			     (if (or (null id) cheques-exist-p)
				 nil
				 (with-html
				   (:li (:a :href (bank/delete :id id)
					    (:img :src (url "img/delete.png")) "Διαγραφή"))))))))))
    (with-html
      (:div :class "actions"
	    (:ul :class "hmenu"
		 (iter (for opt in opt-list)
		       (funcall (getf options opt) id)))))))

(defun banks-table (active-id intent)
  (flet ((normal-row (id title activep)
	   (with-html
	     (:tr :class (if activep "active" nil)
		  (:td :class "select"
		       (:a :href (banks :id id)
			   (:img :src (if activep
					  (url "img/bullet_red.png")
					  (url "img/bullet_blue.png")))))
		  (:td :class "id" (str (lisp-to-html id)))
		  (:td :class "data"  (str (lisp-to-html title)))
		  (:td :class "button" "")
		  (:td :class "button" ""))))
	 (form-row-create ()
	   (with-form (actions/bank/create)
	     (:tr :class "active"
		  (:td :class "select"
		       (:a :href (banks)
			   (:img :src (url "img/bullet_red.png"))))
		  (:td :class "id"  (textbox 'id))
		  (:td :class "data"  (textbox 'title))
		  (:td :class "button" (ok-button))
		  (:td :class "button" (cancel-button (banks))))))
	 (form-row-update (id title)
	   (with-form (actions/bank/update :old-id id)
	     (:tr :class "active"
		  (:td :class "select"
		       (:a :href (banks :id id)
			   (:img :src (url "img/bullet_red.png"))))
		  (:td :class "id"  (textbox 'id :value id))
		  (:td :class "data"  (textbox 'title :value title))
		  (:td :class "button" (ok-button))
		  (:td :class "button" (cancel-button (banks :id id))))))
	 (form-row-delete (id title)
	   (with-form (actions/bank/delete :id id)
	     (:tr :class "attention"
		  (:td :class "select"
		       (:a :href (banks :id id)
			   (:img :src (url "img/bullet_red.png"))))
		  (:td :class "id" (str (lisp-to-html id)))
		  (:td :class "data"  (str (lisp-to-html title)))
		  (:td :class "button" (ok-button))
		  (:td :class "button" (cancel-button (banks :id id)))))))
    (with-db
      (let ((banks (query (:select 'id 'title :from 'bank)))
	    (header '("" "ID" "Bank Name" "" "")))
	(with-html
	  (:table :id "banks-table" :class "forms-in-row"
		  (:thead
		   (:tr (iter (for label in header) (htm (:th (str label))))))
		  (:tbody
		   (when (eql intent :create)
		     (form-row-create))
		   (iter (for (id title) in banks)
			 (for activep = (and active-id (string-equal active-id id)))
			 (if activep 
			     (case intent
			       (:view (normal-row id title activep)) 
			       (:update (form-row-update id title))
			       (:delete (form-row-delete id title)))
			     (normal-row id title activep))))))))))

(defun bank-errorbar (id title)
  (unless (and (validp id) (validp title))
    (with-html
      (:div :id "msg"
	    (:ul :class "errorbar")
	    (unless (validp id)
	      (htm (:li "Άκυρο αναγνωριστικό (id) τράπεζας")))
	    (unless (validp title)
	      (htm (:li "Αυτό το όνομα τράπεζας υπάρχει ήδη")))))))

;;; Banks - Pages

(define-dynamic-page bank/create ((id string) (title string))
    ("config/bank/create")
  (no-cache)
  (with-page ()
    (:head
     (:title "Εισαγωγή τράπεζας")
     (css-standard-headers))
    (:body
     (:div :id "header"
	   (logo)
	   (primary-navbar 'config)
	   (config-navbar 'banks))
     (:div :id "body" 
	   (:div :id "banks" :class "window"
		 (bank-menu (val id))
		 (:h2 "Εισαγωγή τράπεζας")
		 (bank-errorbar id title)
		 (banks-table nil :create))
	   (footer)))))

(define-dynamic-page bank/update ((id string) (title string))
    ("config/bank/update")
  (if (validp id)
      (with-page ()
	(:head
	 (:title "Επεξεργασία τράπεζας")
	 (css-standard-headers))
	(:body
	 (:div :id "header"
	       (logo)
	       (primary-navbar 'config)
	       (config-navbar 'banks)
	       (bank-errorbar id title))
	 (:div :id "body" 
	       (:div :id "banks" :class "window"
		     (bank-menu (val id) :view :delete)
		     (:h2 "Επεξεργασία τράπεζας")
		     (banks-table (val id) :update))
	       (footer))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page bank/delete ((id string))
    ("config/bank/delete")
  (if (validp id)
      (with-parameter-rebinding #'val
	(with-page ()
	  (:head
	   (:title "Διαγραφή τράπεζας")
	   (css-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'banks))
	   (:div :id "body"
		 (:div :id "banks" :class "window"
		       (bank-menu id :view :edit)
		       (:h2 "Διαγραφή τράπεζας")
		       (banks-table id :delete)))
	   (footer))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page banks ((id string))
    ("config/banks")
  (if (validp id)
      (with-parameter-rebinding #'val
	(with-page ()
	  (:head
	   (:title "Τράπεζες")
	   (css-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'banks))
	   (:div :id "body" 
		 (:div :id "banks" :class "window"
		       (bank-menu id :create :edit :delete)
		       (:h2 "Κατάλογος τραπεζών")
		       (banks-table id :view))
		 (footer)))))
      (redirect (notfound) :code +http-see-other+)))


;;; ------------------------------------------------------------
;;; Taxation Offices
;;; ------------------------------------------------------------

;;; TOFs - Actions

(define-dynamic-page actions/tof/create ((id string)
					  (title string))
    ("actions/tof/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (insert-dao (make-instance 'tof :id id :title title))
	    (redirect (tofs :id id) :code +http-see-other+)))
	(with-parameter-rebinding #'raw
	  (redirect (tof/create :id id) :code +http-see-other+)))))

(define-dynamic-page actions/tof/update ((old-id string)
					  (id string)
					  (title string))
    ("actions/tof/update" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (execute (:update 'tof :set
			      'id id
			      'title title
			      :where (:= 'id old-id)))
	    (redirect (tofs :id id) :code +http-see-other+)))
	(with-parameter-rebinding #'raw
	  ;; :TODO: The new id is lost in this phase 
	  (redirect (tof/update :id old-id :title title) :code +http-see-other+)))))

(define-dynamic-page actions/tof/delete ((id string))
    ("actions/tof/delete" :request-type :post)
  (if (validp id)
      (with-db
	(delete-dao (get-dao 'tof (val id)))
	(redirect (tofs) :code +http-see-other+))
      (redirect (notfound) :code +http-see-other+)))


;;; TOFs - Snippets

(defun tof-menu (id &rest opt-list)
  (let ((options
	 (list :view (lambda (&optional id)
			  (with-html
			    (:li (:a :href (tofs :id id)
				     (:img :src (url "img/table.png")) "Προβολή"))))
	       :create (lambda (id)
			 (declare (ignore id))
			 (with-html
			   (:li (:a :href (tof/create)
				    (:img :src (url "img/add.png")) "Δημιουργία")))) 
	       :edit (lambda (id)
		       (if id
			   (with-html
			     (:li (:a :href (tof/update :id id)
				      (:img :src (url "img/pencil.png")) "Επεξεργασία")))
			   nil))
	       :delete (lambda (id)
			 (with-db
			   (let ((cheques-exist-p (and id
						       (query (:select 'id
								       :from 'company
								       :where (:= 'tof-id id))))))
			     (if (or (null id) cheques-exist-p)
				 nil
				 (with-html
				   (:li (:a :href (tof/delete :id id)
					    (:img :src (url "img/delete.png")) "Διαγραφή"))))))))))
    (with-html
      (:div :class "actions"
	    (:ul :class "hmenu"
		 (iter (for opt in opt-list)
		       (funcall (getf options opt) id)))))))

(defun tofs-table (active-id intent)
  (flet ((normal-row (id title activep)
	   (with-html
	     (:tr :class (if activep "active" nil)
		  (:td :class "select"
		       (:a :href (tofs :id id)
			   (:img :src (if activep
					  (url "img/bullet_red.png")
					  (url "img/bullet_blue.png")))))
		  (:td :class "id" (str (lisp-to-html id)))
		  (:td :class "data" (str (lisp-to-html title)))
		  (:td :class "button" "")
		  (:td :class "button" ""))))
	 (form-row-create ()
	   (with-form (actions/tof/create)
	     (:tr :class "active"
		  (:td :class "select"
		       (:a :href (tofs)
			   (:img :src (url "img/bullet_red.png"))))
		  (:td :class "id"  (textbox 'id))
		  (:td :class "data"  (textbox 'title))
		  (:td :class "button" (ok-button))
		  (:td :class "button" (cancel-button (tofs))))))
	 (form-row-update (id title)
	   (with-form (actions/tof/update :old-id id)
	     (:tr :class "active"
		  (:td :class "select"
		       (:a :href (tofs :id id)
			   (:img :src (url "img/bullet_red.png"))))
		  (:td :class "id"  (textbox 'id :value id))
		  (:td :class "data"  (textbox 'title :value title))
		  (:td :class "button" (ok-button))
		  (:td :class "button" (cancel-button (tofs :id id))))))
	 (form-row-delete (id title)
	   (with-form (actions/tof/delete :id id)
	     (:tr :class "attention"
		  (:td :class "select"
		       (:a :href (tofs :id id)
			   (:img :src (url "img/bullet_red.png"))))
		  (:td :class "id"  (str (lisp-to-html id)))
		  (:td :class "data"  (str (lisp-to-html title)))
		  (:td :class "button" (ok-button))
		  (:td :class "button" (cancel-button (tofs :id id)))))))
    (with-db
      (let ((tofs (query (:select 'id 'title :from 'tof)))
	    (header '("" "ID" "Tof Name" "" "")))
	(with-html
	  (:table :id "tofs-table" :class "forms-in-row"
		  (:thead
		   (:tr (iter (for label in header) (htm (:th (str label))))))
		  (:tbody
		   (if (eql intent :create) (form-row-create))
		   (iter (for (id title) in tofs)
			 (for activep = (and active-id (string-equal active-id id)))
			 (if activep 
			     (case intent
			       (:view (normal-row id title activep)) 
			       (:update (form-row-update id title))
			       (:delete (form-row-delete id title)))
			     (normal-row id title activep))))))))))

(defun tof-errorbar (id title)
  (unless (and (validp id) (validp title))
    (with-html
      (:div :id "msg"
	    (:ul :class "errorbar")
	    (unless (validp id)
	      (htm (:li "Άκυρο αναγνωριστικό (id) Δ.Ο.Υ.")))
	    (unless (validp title)
	      (htm (:li "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη")))))))


;;; TOFs - Pages

(define-dynamic-page tof/create ((id string) (title string))
    ("config/tof/create")
  (no-cache)
  (with-page ()
    (:head
     (:title "Εισαγωγή Δ.Ο.Υ.")
     (css-standard-headers))
    (:body
     (:div :id "header"
	   (logo)
	   (primary-navbar 'config)
	   (config-navbar 'tofs))
     (:div :id "body" 
	   (:div :id "tofs" :class "window"
		 (tof-menu nil :view)
		 (:h2 "Δημιουργία Δ.Ο.Υ.")
		 (tof-errorbar id title)
		 (tofs-table nil :create))
	   (footer)))))

(define-dynamic-page tof/update ((id string) (title string))
    ("config/tof/update")
  (if (validp id)
      (with-page ()
	(:head
	 (:title "Επεξεργασία Δ.Ο.Υ.")
	 (css-standard-headers))
	(:body
	 (:div :id "header"
	       (logo)
	       (primary-navbar 'config)
	       (config-navbar 'tofs))
	 (:div :id "body"
	       (:div :id "tofs" :class "window"
		     (tof-menu (val id) :view :delete) 
		     (tof-errorbar id title)
		     (:h2 "Επεξεργασία Δ.Ο.Υ.")
		     (tofs-table (val id) :update))
	       (footer))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page tof/delete ((id string))
    ("config/tof/delete")
  (if (validp id)
      (with-parameter-rebinding #'val
	(with-page ()
	  (:head
	   (:title "Διαγραφή Δ.Ο.Υ.")
	   (css-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'tofs))
	   (:div :id "body" 
		 (:div :id "tofs" :class "window"
		       (tof-menu id :view :edit)
		       (:h2 "Διαγραφή Δ.Ο.Υ. ")
		       (tofs-table id :delete))
		 (footer)))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page tofs ((id string))
    ("config/tofs")
  (if (validp id)
      (with-parameter-rebinding #'val
	(with-page ()
	  (:head
	   (:title "Δ.Ο.Υ.")
	   (css-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'tofs))
	   (:div :id "body" 
		 (:div :id "tofs" :class "window"
		       (tof-menu id :create :edit :delete)
		       (:h2 "Κατάλογος Δ.Ο.Υ.")
		       (tofs-table id :view))
		 (footer)))))
      (redirect (notfound) :code +http-see-other+)))





