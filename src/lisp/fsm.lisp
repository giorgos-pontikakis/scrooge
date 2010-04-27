(in-package :scrooge)


;;; Actions

(declaim (optimize (speed 0) (debug 3)))

(define-dynamic-page actions/fsm/create ((tbl string)
					 description
					 (debit-acc string #'account-exists-p)
					 (credit-acc string #'account-exists-p) 
					 (old-status string)
					 (new-status string))
    ("actions/fsm/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (and (every #'validp params) (valid-combo (val tbl) (val old-status) (val new-status)))
	(with-parameter-rebinding #'val
	  (let ((debit-acc-id (get-account-id debit-acc))
		(credit-acc-id (get-account-id credit-acc))) 
	    (with-db
	      (insert-dao (make-instance (symbolicate (string-upcase tbl) "-FSM")
					 :description description
					 :debit-acc-id debit-acc-id
					 :credit-acc-id credit-acc-id
					 :old-status old-status
					 :new-status new-status))
	      (redirect (fsm) :code +http-see-other+))))
	(with-parameter-rebinding #'raw 
	  (redirect (fsm/create :tbl tbl
				:description description
				:debit-acc debit-acc
				:credit-acc credit-acc
				:old-status old-status
				:new-status new-status)
		    :code +http-see-other+)))))

(define-dynamic-page actions/fsm/delete ((fsm-id integer)
					 (tbl string))
    ("actions/fsm/delete" :request-type :post)
  (no-cache) 
  (with-parameter-rebinding #'val
    (if (valid-fsm-id-p fsm-id tbl) 
	(with-db 
	  (delete-dao (get-dao (symbolicate (string-upcase tbl) "-FSM") fsm-id))
	  (redirect (fsm) :code +http-see-other+))
	(redirect (notfound) :code +http-see-other+))))

(define-dynamic-page actions/fsm/update ((fsm-id integer)
					 (tbl string)
					 description
					 (debit-acc string #'account-exists-p)
					 (credit-acc string #'account-exists-p) 
					 (old-status string)
					 (new-status string))
    ("actions/fsm/update" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (and (every #'validp params)
	     (valid-combo (val tbl) (val old-status) (val new-status))
	     (valid-fsm-id-p (val fsm-id) (val tbl)))
	(with-parameter-rebinding #'val
	  (let ((debit-acc-id (get-account-id debit-acc))
		(credit-acc-id (get-account-id credit-acc)))
	    (with-db
	      (execute (:update (symbolicate tbl "-FSM") :set
				'description description
				'debit-acc-id debit-acc-id
				'credit-acc-id credit-acc-id 
				'old-status old-status
				'new-status new-status
				:where (:= 'id fsm-id)))
	      (redirect (fsm) :code +http-see-other+))))
	(with-parameter-rebinding #'raw 
	  (redirect (fsm/update :fsm-id fsm-id
				:description description
				:debit-acc debit-acc
				:credit-acc credit-acc
				:tbl tbl
				:old-status old-status
				:new-status new-status)
		    :code +http-see-other+)))))

;;; Pages

(define-dynamic-page fsm ((fsm-id integer) (tbl string))
    ("config/fsm")
  (no-cache)
  (if (validp fsm-id)
      (with-parameter-rebinding #'val
	(with-page ()
	  (:head
	   (:title "FSM transitions")
	   (css-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'fsm))
	   (:div :id "body"
		 (:div :id "fsm" :class "window"
		       (apply #'fsm-menu fsm-id tbl (if fsm-id
							'(:create :update :delete)
							'(:create)))
		       (display-fsm fsm-id tbl :view))))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page fsm/create ((tbl string)
				 description 
				 (debit-acc string #'account-exists-p)
				 (credit-acc string #'account-exists-p)
				 (old-status string)
				 (new-status string))
    ("config/fsm/create")
  (no-cache)
  (with-parameter-list params
    (with-page ()
      (:head
       (:title "FSM Transitions: Δημιουργία")
       (css-standard-headers))
      (:body
       (:div :id "header"
	     (logo)
	     (primary-navbar 'config)
	     (config-navbar 'fsm))
       (:div :id "body"
	     (:div :id "fsm" :class "window"
		   (fsm-menu nil nil) 
		   (display-fsm nil (val tbl) :create (nthcdr 1 params))))))))

(define-dynamic-page fsm/update ((fsm-id integer)
				 (tbl string)
				 description 
				 (debit-acc string #'account-exists-p) 
				 (credit-acc string #'account-exists-p) 
				 (old-status string)
				 (new-status string))
    ("config/fsm/update")
  (no-cache)
  (if (validp fsm-id)
      (with-parameter-list params
	(with-page ()
	  (:head
	   (:title "FSM Transitions: Επεξεργασία")
	   (css-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'fsm))
	   (:div :id "body"
		 (:div :id "fsm" :class "window"
		       (fsm-menu (val fsm-id) (val tbl) :view :delete)
		       (display-fsm (val fsm-id) (val tbl) :update (nthcdr 2 params)))))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page fsm/delete ((fsm-id integer) (tbl string))
    ("config/fsm/delete")
  (no-cache)
  (if (validp fsm-id)
      (with-parameter-rebinding #'val
	(with-page ()
	  (:head
	   (:title "FSM Transitions: Διαγραφή")
	   (css-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'fsm))
	   (:div :id "body"
		 (:div :id "fsm" :class "window"
		       (fsm-menu fsm-id tbl :view :update)
		       (display-fsm fsm-id tbl :delete))))))
      (redirect (notfound) :code +http-see-other+)))


(defun display-fsm (active-id active-tbl intent &optional params)
  (flet ((normal-row (id tbl row activep)
	   (bind (((description debit-acc credit-acc old-status new-status) row))
	     (with-html
	       (:tr :class (if activep "active" nil)
		    (:td :class "select"
			 (if activep
			     (htm (:a :href (fsm)
				      (:img :src (url "img/bullet_red.png"))))
			     (htm (:a :href (fsm :fsm-id id :tbl tbl)
				      (:img :src (url "img/bullet_blue.png"))))))
		    (:td :class "data" (str (lisp-to-html (fsm-table-label tbl))))
		    (:td :class "data" (str (lisp-to-html description)))
		    (:td :class "data" (str (lisp-to-html old-status)))
		    (:td :class "data" (str (lisp-to-html new-status)))
		    (:td :class "data" (str (lisp-to-html debit-acc)))
		    (:td :class "data" (str (lisp-to-html credit-acc))) 
		    (:td :class "button" "")
		    (:td :class "button" "")))))
	 (form-row-create (row styles)
	   (bind (((&optional description debit-acc credit-acc old-status new-status) row)
		  ((&optional description% debit-acc% credit-acc% old-status% new-status%) styles))
	     (with-form (actions/fsm/create)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (fsm)
			     (:img :src (url "img/bullet_red.png")))) 
		    (:td :class "data" (dropdown 'tbl *fsm-tables* :selected active-tbl))
		    (:td :class "data" (textbox 'description :value description :style description%))
		    (:td :class "data" (textbox 'old-status :value old-status :style old-status%))
		    (:td :class "data" (textbox 'new-status :value new-status :style new-status%))
		    (:td :class "data" (textbox 'debit-acc :value debit-acc :style debit-acc%))
		    (:td :class "data" (textbox 'credit-acc :value credit-acc :style credit-acc%)) 
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (fsm)))))))
	 (form-row-update (id tbl row styles)
	   (bind (((description debit-acc credit-acc old-status new-status) row)
		  ((&optional description% debit-acc% credit-acc% old-status% new-status%) styles))
	     (with-form (actions/fsm/update :fsm-id id :tbl tbl)
	       (:tr :class "active"
		    (:td :class "select"
			 (:a :href (fsm)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (dropdown 'tbl *fsm-tables* :selected tbl))
		    (:td :class "data" (textbox 'description :value description :style description%)) 
		    (:td :class "data" (textbox 'old-status :value old-status :style old-status%))
		    (:td :class "data" (textbox 'new-status :value new-status :style new-status%))
		    (:td :class "data" (textbox 'debit-acc :value debit-acc :style debit-acc%))
		    (:td :class "data" (textbox 'credit-acc :value credit-acc :style credit-acc%))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (fsm :fsm-id id :tbl tbl)))))))
	 (form-row-delete (id tbl row)
	   (destructuring-bind (description debit-acc credit-acc old-status new-status) row
	     (with-form (actions/fsm/delete :fsm-id id :tbl tbl)
	       (:tr :class "attention"
		    (:td :class "select"
			 (:a :href (fsm)
			     (:img :src (url "img/bullet_red.png"))))
		    (:td :class "data" (str (lisp-to-html (fsm-table-label tbl))))
		    (:td :class "data" (str (lisp-to-html description))) 
		    (:td :class "data" (str (lisp-to-html old-status)))
		    (:td :class "data" (str (lisp-to-html new-status)))
		    (:td :class "data" (str (lisp-to-html debit-acc)))
		    (:td :class "data" (str (lisp-to-html credit-acc)))
		    (:td :class "button" (ok-button))
		    (:td :class "button" (cancel-button (fsm :fsm-id id :tbl tbl))))))))
    (let ((header '("" "Πίνακας" "Περιγραφή" "Πριν" "Μετά" "Λογ. Χρέωσης" "Λογ. Πίστωσης" "" ""))
	  (header-styles '("select" "data" "data" "data" "data" "data" "data" "button" "button"))
	  (inputs (mapcar #'val* params))
	  (styles (mapcar (lambda (p) (if (validp p) nil "attention")) params)))
      (with-html
	(:table :id "fsm-table" :class "forms-in-row"
		(:thead
		 (:tr (iter (for label in header)
			    (for sty in header-styles)
			    (htm (:th :class sty (str label))))))
		(:tbody
		 (when (eql intent :create) 
		   (form-row-create inputs styles))
		 (iter (for tbl in (fsm-tables))
		       (let ((fsm-data (get-fsm-data tbl)))
			 (iter (for (id . defaults) in fsm-data)
			       (for activep = (and active-id
						   (= active-id id)
						   (string-equal active-tbl tbl)))
			       (if activep
				   (let ((row (merge-nonnull defaults inputs))) 
				     (case intent
				       (:view (normal-row id tbl row activep))
				       (:update (form-row-update id tbl row styles))
				       (:delete (form-row-delete id tbl row))))
				   (normal-row id tbl defaults activep)))))))))))

(defun get-fsm-data (table) 
  (with-db
    (query (:select (symbolicate table '-fsm.id) 'description
		    'debit-account.title 'credit-account.title
		    'old-status 'new-status
		    :from (symbolicate table '-fsm)
		    :left-join (:as 'account 'debit-account)
		    :on (:= 'debit-account.id (symbolicate table '-fsm.debit-acc-id))
		    :left-join (:as 'account 'credit-account)
		    :on (:= 'credit-account.id (symbolicate table '-fsm.credit-acc-id))))))

(define-menu fsm-menu (fsm-id tbl) ()
  (:create (lambda () 
	     (with-html
	       (:li (:a :href (fsm/create)
			(:img :src (url "img/add.png")) "Δημιουργία")))))
  (:view (lambda () 
	   (if fsm-id
	       (with-html
		 (:li (:a :href (fsm :fsm-id fsm-id :tbl tbl)
			  (:img :src (url "img/magnifier.png")) "Προβολή")))
	       nil)))
  (:update (lambda ()
	     (if fsm-id
		 (with-html
		   (:li (:a :href (fsm/update :fsm-id fsm-id :tbl tbl)
			    (:img :src (url "img/pencil.png")) "Επεξεργασία")))
		 nil)))
  (:delete (lambda ()
	     (if fsm-id
		 (with-html
		   (:li (:a :href (fsm/delete :fsm-id fsm-id :tbl tbl)
			    (:img :src (url "img/delete.png")) "Διαγραφή")))
		 nil))))