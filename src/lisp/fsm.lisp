(in-package :scrooge)


;;; Actions

(declaim (optimize (speed 0) (debug 3)))

(define-dynamic-page actions/fsm/create ((tbl string)
					 (description string #'not-db-null-p)
					 (debit-acc string #'account-exists-p)
					 (credit-acc string #'account-exists-p) 
					 (old-status string)
					 (new-status string))
    ("actions/fsm/create" :request-type :post
			  :validators ((old-status (valid-combo tbl old-status))
				       (new-status (valid-combo tbl new-status))))
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (let ((debit-acc-id (account-id debit-acc))
		(credit-acc-id (account-id credit-acc))) 
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
    ("actions/fsm/delete" :request-type :post
			  :validators (((fsm-id tbl) (valid-fsm-id-p fsm-id tbl))))
  (no-cache) 
  (with-parameter-list params
    (if (every #'validp params) 
	(with-parameter-rebinding #'val
	  (with-db 
	    (delete-dao (get-dao (symbolicate (string-upcase tbl) "-FSM") fsm-id))
	    (redirect (fsm) :code +http-see-other+)))
	(redirect (notfound) :code +http-see-other+))))

(define-dynamic-page actions/fsm/update ((fsm-id integer)
					 (tbl string)
					 (description string #'not-db-null-p)
					 (debit-acc string #'account-exists-p)
					 (credit-acc string #'account-exists-p) 
					 (old-status string)
					 (new-status string))
    ("actions/fsm/update"
     :request-type :post
     :validators ((old-status (valid-combo tbl old-status))
		  (new-status (valid-combo tbl new-status))
		  (fsm-id (valid-fsm-id-p fsm-id tbl))))
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (let ((debit-acc-id (account-id debit-acc))
		(credit-acc-id (account-id credit-acc)))
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

(define-dynamic-page fsm ((fsm-id integer)
			  (tbl string))
    ("config/fsm" :validators (((fsm-id tbl) (valid-fsm-id-p fsm-id tbl))))
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-page ()
	    (:head
	     (:title "Καταστατικές Μεταβολές")
	     (css-standard-headers))
	    (:body
	     (:div :id "header"
		   (logo)
		   (primary-navbar 'config)
		   (config-navbar 'fsm))
	     (:div :id "body"
		   (:div :class "message"
			 (:h2 :class "info" "Κατάλογος Καταστατικών Μεταβολών"))
		   (:div :id "fsm" :class "window"
			 (fsm-menu fsm-id tbl :create :update :delete)
			 (display-fsm fsm-id tbl :view))))))
	(redirect (notfound) :code +http-see-other+))))

(define-dynamic-page fsm/create ((tbl         string #'valid-tbl-p)
				 (description string #'not-db-null-p) 
				 (debit-acc   string #'account-exists-p)
				 (credit-acc  string #'account-exists-p)
				 (old-status  string)
				 (new-status  string))
    ("config/fsm/create" :validators ((old-status (valid-combo tbl old-status))
				      (new-status (valid-combo tbl new-status))))
  (no-cache)
  (with-parameter-list params
    (if (validp tbl)
	(with-page ()
	  (:head
	   (:title "Καταστατικές Μεταβολές: Δημιουργία")
	   (css-standard-headers)
	   (js-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'fsm))
	   (:div :id "body"
		 (:div :class "message"
		       (:h2 :class "info" "Δημιουργία μετάβασης")
		       (fsm-errorbar description debit-acc credit-acc old-status new-status)) 
		 (:div :id "fsm" :class "window"
		       (fsm-menu nil (val tbl) :view) 
		       (display-fsm nil (val tbl) :create (nthcdr 1 params))))))
	(redirect (notfound) :code +http-see-other+))))

(define-dynamic-page fsm/update ((fsm-id      integer)
				 (tbl         string #'valid-tbl-p)
				 (description string #'not-db-null-p)
				 (debit-acc   string #'account-exists-p) 
				 (credit-acc  string #'account-exists-p) 
				 (old-status  string)
				 (new-status  string))
    ("config/fsm/update" :validators ((old-status (valid-combo tbl old-status))
				      (new-status (valid-combo tbl new-status))
				      (fsm-id (valid-fsm-id-p fsm-id tbl))))
  (no-cache)
  (if (validp fsm-id)
      (with-parameter-list params
	(with-page ()
	  (:head
	   (:title "Καταστατικές Μεταβολές: Επεξεργασία")
	   (css-standard-headers)
	   (js-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'fsm))
	   (:div :id "body"
		 (:div :class "message"
		       (:h2 :class "info" "Επεξεργασία μετάβασης")
		       (fsm-errorbar description debit-acc credit-acc old-status new-status))
		 (:div :id "fsm" :class "window" 
		       (fsm-menu (val fsm-id) (val tbl) :view :delete) 
		       (display-fsm (val fsm-id) (val tbl) :update (nthcdr 2 params)))))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page fsm/delete ((fsm-id integer)
				 (tbl string #'valid-tbl-p))
    ("config/fsm/delete" :validators ((fsm-id (valid-fsm-id-p fsm-id tbl))))
  (no-cache) 
  (if (validp fsm-id)
      (with-parameter-rebinding #'val
	(with-page ()
	  (:head
	   (:title "Καταστατικές Μεταβολές: Διαγραφή")
	   (css-standard-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'fsm))
	   (:div :id "body"
		 (:div :class "message"
		       (:h2 :class "info" "Διαγραφή μετάβασης")) 
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
    (let ((header '("" "Πίνακας" "Περιγραφή" "Αρχική Κατάσταση" "Τελική Κατάσταση" "Λογ. Χρέωσης" "Λογ. Πίστωσης" "" ""))
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
		       (iter (for (id . defaults) in (get-fsm-data tbl))
			     (for activep = (and active-id
						 (= active-id id)
						 (string-equal active-tbl tbl)))
			     (if activep
				 (let ((row (merge-nonnull defaults inputs))) 
				   (case intent
				     (:view (normal-row id tbl row activep))
				     (:update (form-row-update id tbl row styles))
				     (:delete (form-row-delete id tbl row))))
				 (normal-row id tbl defaults activep))))))))))

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

(define-menu fsm-menu (fsm-id tbl) (:div-style "actions" :ul-style "hmenu")
  (:create (with-html
	     (:li (:a :href (fsm/create)
		      (:img :src (url "img/add.png")) "Δημιουργία"))))
  (:view (with-html
	   (:li (:a :href (fsm :fsm-id fsm-id :tbl tbl)
		    (:img :src (url "img/magnifier.png")) "Προβολή"))))
  (:update (if fsm-id
	       (with-html
		 (:li (:a :href (fsm/update :fsm-id fsm-id :tbl tbl)
			  (:img :src (url "img/pencil.png")) "Επεξεργασία")))
	       nil))
  (:delete (if fsm-id
	       (with-html
		 (:li (:a :href (fsm/delete :fsm-id fsm-id :tbl tbl)
			  (:img :src (url "img/delete.png")) "Διαγραφή")))
	       nil)))


(define-errorbar fsm-errorbar (:ul-style "error")
  (description "Η περιγραφή δεν πρέπει να είναι κενή")
  (debit-acc "Άκυρος λογαριασμός χρέωσης")
  (credit-acc "Άκυρος λογαριασμός πίστωσης")
  (old-status "Άκυρη αρχική κατάσταση")
  (new-status "Άκυρη τελική κατάσταση"))