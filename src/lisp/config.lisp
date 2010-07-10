(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

(define-navbar config-navbar () (:id "subnavbar" :ul-style "hmenu")
  (banks    (banks)    "Τράπεζες")
  (tofs     (tofs)     "Δ.Ο.Υ.")
  (accounts (accounts) "Λογαριασμοί")
  (temtx    (temtx)    "Πρότυπες Συναλλαγές")
  (fsm      (fsm)      "Καταστατικές Μεταβολές"))


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
	   (config-navbar nil))
     (:div :id "body" 
	   (:div :id "content" :class "window"
		 "Don't touch")
	   (footer)))))


;;; ------------------------------------------------------------
;;; Banks
;;; ------------------------------------------------------------

;;; Banks - Actions

(define-dynamic-page actions/bank/create ((id    string (complement #'bank-id-exists-p))
					  (title string (complement #'bank-exists-p)))
    ("actions/bank/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (insert-dao (make-instance 'bank :id id :title title))
	    (redirect (banks :id id) :code +http-see-other+)))
	(with-parameter-rebinding #'raw 
	  (redirect (bank/create :id id :title title) :code +http-see-other+)))))

(define-dynamic-page actions/bank/update
    ((id string #'bank-id-exists-p)
     (new-id string)
     (title  string))
    ("actions/bank/update"
     :request-type :post
     :validators ((new-id (valid-bank-id-p id new-id))
		  (title (valid-bank-title-p id title))))
  (no-cache) 
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (execute (:update 'bank :set
			      'id new-id
			      'title title
			      :where (:= 'id id)))
	    (redirect (banks :id new-id) :code +http-see-other+)))
	(with-parameter-rebinding #'raw
	  (redirect (bank/update :id id :new-id new-id :title title) :code +http-see-other+)))))

(define-dynamic-page actions/bank/delete ((id string #'bank-id-exists-p))
    ("actions/bank/delete" :request-type :post)
  (if (validp id)
      (with-db
	(delete-dao (get-dao 'bank (val id)))
	(redirect (banks) :code +http-see-other+))
      (redirect (notfound) :code +http-see-other+)))

;;; Banks - Snippets

(define-menu bank-menu (id) (:div-style "actions" :ul-style "hmenu")
  (:create (with-html
	     (:li (:a :href (bank/create)
		      (:img :src (url "img/add.png")) "Δημιουργία"))))
  (:view (with-html
	   (:li (:a :href (banks :id id)
		    (:img :src (url "img/magnifier.png")) "Προβολή"))))
  (:edit (if id
	     (with-html
	       (:li (:a :href (bank/update :id id)
			(:img :src (url "img/pencil.png")) "Επεξεργασία")))
	     nil))
  (:delete (with-db
	     (let ((cheques-exist-p (and id
					 (query (:select 'id
							 :from 'cheque
							 :where (:= 'bank-id id))))))
	       (if (or (null id) cheques-exist-p)
		   nil
		   (with-html
		     (:li (:a :href (bank/delete :id id)
			      (:img :src (url "img/delete.png")) "Διαγραφή"))))))))

(defun banks-table (active-id intent &optional params)
  (flet ((form-row-normal (row activep)
	   (bind (((id title) row))
	     (with-html
	       (:tr :class (if activep "active" nil)
		    (:td :class "select"
			 (if activep
			     (htm (:a :href (banks)
				      (:img :src (url "img/bullet_red.png"))))
			     (htm (:a :href (banks :id id)
				      (:img :src (url "img/bullet_blue.png"))))))
		    (:td :class "id" (str (lisp-to-html id)))
		    (:td :class "data"  (str (lisp-to-html title)))
		    (:td :class "button" "")
		    (:td :class "button" "")))))
	 (form-row-create (row styles)
	   (bind (((&optional id title) row)
		  ((&optional id% title%) styles))
	     (with-form (actions/bank/create)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (banks)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "id"  (textbox 'id :value id :style id%))
		    (:td :class "data"  (textbox 'title :value title :style title%))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (banks)))))))
	 (form-row-update (row styles)
	   (bind (((&optional new-id title) row)
		  ((&optional new-id% title%) styles)) 
	     (with-form (actions/bank/update :id active-id)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (banks :id active-id)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "id" (textbox 'new-id :value new-id :style new-id%))
		    (:td :class "data" (textbox 'title :value title :style title%))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (banks :id active-id)))))))
	 (form-row-delete (row)
	   (bind (((id title) row)) 
	     (with-form (actions/bank/delete :id id)
	       (:tr :class "attention"
		    (:td :class "select"
			 (:a :href (banks :id id)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "id" (str (lisp-to-html id)))
		    (:td :class "data" (str (lisp-to-html title)))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (banks :id id))))))))
    (with-db
      (let ((banks (query (:select 'id 'title :from 'bank)))
	    (header '("" "ID" "Ονομασία Τράπεζας" "" ""))
	    (input-row (mapcar #'val* params))
	    (styles (mapcar #'style-invalid params)))
	(with-html
	  (:table :id "banks-table" :class "table-half forms-in-row"
		  (:thead
		   (:tr (iter (for label in header) (htm (:th (str label))))))
		  (:tbody
		   (when (eql intent :create)
		     (form-row-create input-row styles))
		   (iter (for default-row in banks)
			 (for activep = (and active-id (string-equal active-id (first default-row))))
			 (if activep
			     (let ((row (merge-nonnull default-row input-row)))
			       (case intent
				 (:view (form-row-normal row activep)) 
				 (:update (form-row-update row styles))
				 (:delete (form-row-delete row))))
			     (form-row-normal default-row activep))))))))))

(define-errorbar bank-errorbar (:ul-style "error")
  (id "Αυτό το αναγνωριστικό (id) τράπεζας υπάρχει ήδη.") 
  (title "Αυτό το όνομα τράπεζας υπάρχει ήδη.")) 

;;; Banks - Pages

(define-dynamic-page bank/create ((id    string (complement #'bank-id-exists-p))
				  (title string (complement #'bank-exists-p)))
    ("config/bank/create") 
  (no-cache)
  (with-parameter-list params
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
	     (:div :class "message"
		   (:h2 :class "info" "Δημιουργία τράπεζας")
		   (bank-errorbar id title))
	     (:div :id "banks" :class "window"
		   (bank-menu nil :view) 
		   (banks-table nil :create params))
	     (footer))))))

(define-dynamic-page bank/update
    ((id string #'bank-id-exists-p)
     (new-id string)
     (title  string))
    ("config/bank/update"
     :validators ((new-id (valid-bank-id-p id new-id))
		  (title (valid-bank-title-p id title))))
  (if (validp id)
      (with-parameter-list params
	(with-page ()
	  (:head
	   (:title "Επεξεργασία τράπεζας")
	   (css-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'banks))
	   (:div :id "body"
		 (:div :class "message"
		       (:h2 :class "info" "Επεξεργασία τράπεζας")
		       (bank-errorbar new-id title))
		 (:div :id "banks" :class "window"
		       (bank-menu (val id) :view :delete) 
		       (banks-table (val id) :update (rest params)))
		 (footer)))))
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
		 (:div :class "message"
		       (:h2 :class "info" "Διαγραφή τράπεζας"))
		 (:div :id "banks" :class "window"
		       (bank-menu id :view :edit) 
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
	   (bank-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'banks))
	   (:div :id "body"
		 (:div :class "message"
		       (:h2 :class "info" "Κατάλογος τραπεζών"))
		 (:div :id "banks" :class "window"
		       (bank-menu id :create :edit :delete) 
		       (banks-table id :view))
		 (footer)))))
      (redirect (notfound) :code +http-see-other+)))


;;; ------------------------------------------------------------
;;; Taxation Offices
;;; ------------------------------------------------------------

;;; TOFs - Actions

(define-dynamic-page actions/tof/create ((id    string (complement #'tof-id-exists-p))
					 (title string (complement #'tof-exists-p)))
    ("actions/tof/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (insert-dao (make-instance 'tof :id id :title title))
	    (redirect (tofs :id id) :code +http-see-other+)))
	(with-parameter-rebinding #'raw
	  (redirect (tof/create :id id :title title) :code +http-see-other+)))))

(define-dynamic-page actions/tof/update ((id string #'tof-id-exists-p) new-id title)
    ("actions/tof/update"
     :request-type :post
     :validators ((new-id (valid-tof-id-p id new-id))
		  (title  (valid-tof-title-p id title))))
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (execute (:update 'tof :set
			      'id new-id
			      'title title
			      :where (:= 'id id)))
	    (redirect (tofs :id new-id) :code +http-see-other+)))
	(with-parameter-rebinding #'raw
	  (redirect (tof/update :id id :new-id new-id :title title) :code +http-see-other+)))))

(define-dynamic-page actions/tof/delete (id)
    ("actions/tof/delete" :request-type :post)
  (if (validp id)
      (with-db
	(delete-dao (get-dao 'tof (val id)))
	(redirect (tofs) :code +http-see-other+))
      (redirect (notfound) :code +http-see-other+)))


;;; TOFs - Snippets

(define-menu tof-menu (id) (:div-style "actions" :ul-style "hmenu")
  (:view (with-html
	   (:li (:a :href (tofs :id id)
		    (:img :src (url "img/table.png")) "Προβολή"))))
  (:create (with-html
	     (:li (:a :href (tof/create)
		      (:img :src (url "img/add.png")) "Δημιουργία")))) 
  (:edit (if id
	     (with-html
	       (:li (:a :href (tof/update :id id)
			(:img :src (url "img/pencil.png")) "Επεξεργασία")))
	     nil))
  (:delete (with-db
	     (let ((cheques-exist-p (and id
					 (query (:select 'id
							 :from 'company
							 :where (:= 'tof-id id))))))
	       (if (or (null id) cheques-exist-p)
		   nil
		   (with-html
		     (:li (:a :href (tof/delete :id id)
			      (:img :src (url "img/delete.png")) "Διαγραφή"))))))))

(defun tofs-table (active-id intent &optional params)
  (flet ((normal-row (row activep)
	   (bind (((id title) row))
	     (with-html
	       (:tr :class (if activep "active" nil)
		    (:td :class "select"
			 (if activep
			     (htm (:a :href (tofs)
				      (:img :src (url "img/bullet_red.png"))))
			     (htm (:a :href (tofs :id id)
				      (:img :src (url "img/bullet_blue.png"))))))
		    (:td :class "id" (str (lisp-to-html id)))
		    (:td :class "data" (str (lisp-to-html title)))
		    (:td :class "button" "")
		    (:td :class "button" "")))))
	 (form-row-create (row styles)
	   (bind (((&optional id title) row)
		  ((&optional id% title%) styles))
	     (with-form (actions/tof/create)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (tofs)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "id"  (textbox 'id :value id :style id%))
		    (:td :class "data"  (textbox 'title :value title :style title%))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (tofs)))))))
	 (form-row-update (row styles)
	   (bind (((&optional new-id title) row)
		  ((&optional new-id% title%) styles))
	     (with-form (actions/tof/update :id active-id)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (tofs :id active-id)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "id"  (textbox 'new-id :value new-id :style new-id%))
		    (:td :class "data"  (textbox 'title :value title :style title%))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (tofs :id active-id)))))))
	 (form-row-delete (row)
	   (bind (((id title) row))
	     (with-form (actions/tof/delete :id id)
	       (:tr :class "attention"
		    (:td :class "select"
			 (:a :href (tofs :id id)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "id"  (str (lisp-to-html id)))
		    (:td :class "data"  (str (lisp-to-html title)))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (tofs :id id))))))))
    (with-db
      (let ((tofs (query (:select 'id 'title :from 'tof)))
	    (header '("" "ID" "Ονομασία Δ.Ο.Υ." "" ""))
	    (input-row (mapcar #'val* params))
	    (styles (mapcar (lambda (p) (if (validp p) nil "attention")) params)))
	(with-html
	  (:table :id "tofs-table" :class "forms-in-row"
		  (:thead
		   (:tr (iter (for label in header) (htm (:th (str label))))))
		  (:tbody
		   (when (eql intent :create)
		     (form-row-create input-row styles))
		   (iter (for default-row in tofs)
			 (for activep = (and active-id (string-equal active-id (first default-row))))
			 (if activep
			     (let ((row (merge-nonnull default-row input-row)))
			       (case intent
				 (:view (normal-row row activep)) 
				 (:update (form-row-update row styles))
				 (:delete (form-row-delete row))))
			     (normal-row default-row activep)))))))))) 

(define-errorbar tof-errorbar (:ul-style "error")
  (id "Αυτό το αναγνωριστικό (id) Δ.Ο.Υ. υπάρχει ήδη")
  (title "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη"))


;;; TOFs - Pages

(define-dynamic-page tof/create ((id    string (complement #'tof-id-exists-p))
				 (title string (complement #'tof-exists-p)))
    ("config/tof/create")
  (no-cache)
  (with-parameter-list params
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
	    (:div :class "message"
		  (:h2 :class "info" "Δημιουργία Δ.Ο.Υ.")
		  (tof-errorbar id title))
	    (:div :id "tofs" :class "window"
		  (tof-menu nil :view) 
		  (tofs-table nil :create params))
	    (footer))))))

(define-dynamic-page tof/update ((id string #'tof-id-exists-p) new-id title)
    ("config/tof/update"
     :validators ((new-id (valid-tof-id-p id new-id))
		  (title  (valid-tof-title-p id title)))) 
  (if (validp id)
      (with-parameter-list params
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
		 (:div :class "message"
		       (:h2 :class "info" "Επεξεργασία Δ.Ο.Υ.")
		       (tof-errorbar new-id title))
		 (:div :id "tofs" :class "window"
		       (tof-menu (val id) :view :delete) 
		       (tofs-table (val id) :update (rest params)))
		 (footer)))))
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
		 (:div :class "message"
		       (:h2 :class "info" "Διαγραφή Δ.Ο.Υ."))
		 (:div :id "tofs" :class "window"
		       (tof-menu id :view :edit) 
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
		 (:div :class "message"
		       (:h2 :class "info" "Κατάλογος Δ.Ο.Υ."))
		 (:div :id "tofs" :class "window" 
		       (tof-menu id :create :edit :delete) 
		       (tofs-table id :view))
		 (footer)))))
      (redirect (notfound) :code +http-see-other+)))





