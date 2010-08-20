(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Definition
;;; ------------------------------------------------------------

(defclass stran-table (table-normal-crud)
  ((name :initform "stran-table")
   (header :initform '(:selector    ""
                       :tbl         "Πίνακας"
                       :description "Περιγραφή"         
                       :old-status  "Αρχική Κατάσταση"  
                       :new-status  "Τελική Κατάσταση"
                       :debit-acc   "Λογ. Χρέωσης"      
                       :credit-acc  "Λογ. Πίστωσης"     
                       :submit      ""
                       :cancel      ""))
   (styles :initform '(:active-row "active"
                       :inactive-row ""
                       :attention-row "attention"
                       :table "forms-in-row table-half")) 
   ;; page interface
   (id-keys :initform '(:stran-id :tbl))
   (payload-keys :initform '(:description
                             :old-status
                             :new-status
                             :debit-acc
                             :credit-acc))
   (filter-keys :initform '())
   ;; crud mixin
   (main-page :initform 'stran)
   (submit-pages :initform '(:create actions/stran/create
                             :update actions/stran/update
                             :delete actions/stran/delete)) 
   (cells-fn :initform (stran-cells-fn))
   (data-fn :initform (stran-data-fn))))



(defun make-stran-table (&key operation params)
  (make-instance 'stran-table 
                 :operation operation
                 :params params))

(defun stran-cells-fn ()
  (lambda (row) 
    (destructuring-bind (&key stran-id tbl description
                              old-status new-status debit-acc credit-acc) (data row)
      (declare (ignore stran-id))
      (let ((pairs (with-db (query (:select 'description 'id :from 'stran)))))
        (list (make-cell-selector :row row
                                  :name :selector
                                  :style "select")
              (make-cell-dropdown :row row
                                  :name :tbl 
                                  :pairs pairs
                                  :value tbl
                                  :style "data"
                                  :operations '(:create))
              (make-cell-textbox :row row
                                 :name :description
                                 :value description
                                 :style "data"
                                 :operations '(:create :update))
              (make-cell-textbox :row row
                                 :name :old-status
                                 :value old-status
                                 :style "data"
                                 :operations '(:create :update))
              (make-cell-textbox :row row
                                 :name :new-status
                                 :value new-status
                                 :style "data"
                                 :operations '(:create :update))
              (make-cell-textbox :row row
                                 :name :debit-acc
                                 :value debit-acc
                                 :style "data"
                                 :operations '(:create :update))
              (make-cell-textbox :row row
                                 :name :credit-acc
                                 :value credit-acc
                                 :style "data"
                                 :operations '(:create :update))
              (make-cell-submit :row row
                                :name :submit
                                :style "button"
                                :operations '(:create :update :delete))
              (make-cell-cancel :row row
                                :name :cancel
                                :style "button"
                                :operations '(:create :update :delete)))))))

(defun stran-data-fn ()
  (lambda (filters)
    (declare (ignore filters))
    (flet ((select (table)
             `(:select (:as ,(symbolicate table '-stran.id) 'stran-id) ;; :stran-id
                       (:as 'stran.id 'tbl)                    ;; :tbl
                       ,(symbolicate table '-stran.description) ;; :description
                       'old-status ;; :old-status
                       'new-status ;; :new-status 
                       (:as 'debit-account.title 'debit-acc) ;; :debit-acc
                       (:as 'credit-account.title 'credit-acc) ;; :credit-acc

                       :from ,(symbolicate table '-stran)
                     
                       :left-join 'stran
                       :on (:= 'stran.id ,table)

                       :left-join (:as 'account 'debit-account)
                       :on (:= 'debit-account.id
                               ,(symbolicate table '-stran.debit-acc-id))

                       :left-join (:as 'account 'credit-account)
                       :on (:= 'credit-account.id
                               ,(symbolicate table '-stran.credit-acc-id)))))
      (with-db
        (query (sql-compile `(:union ,(select "project")
                                     ,(select "cheque")))
               :plists)))))



;;; ------------------------------------------------------------
;;; State transitions - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/stran/create ((tbl string)
                                           (description string #'not-db-null-p)
                                           (debit-acc string #'account-exists-p)
                                           (credit-acc string #'account-exists-p) 
                                           (old-status string)
                                           (new-status string))
    ("actions/stran/create" :request-type :post
                            :validators ((old-status (valid-combo tbl old-status))
                                         (new-status (valid-combo tbl new-status))))
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (let ((debit-acc-id (account-id debit-acc))
		(credit-acc-id (account-id credit-acc))) 
	    (with-db
	      (insert-dao (make-instance (symbolicate (string-upcase tbl) "-STRAN")
					 :description description
					 :debit-acc-id debit-acc-id
					 :credit-acc-id credit-acc-id
					 :old-status old-status
					 :new-status new-status))
	      (see-other (stran)))))
	(with-parameter-rebinding #'raw 
	  (see-other (stran/create :tbl tbl
                                  :description description
                                  :debit-acc debit-acc
                                  :credit-acc credit-acc
                                  :old-status old-status
                                  :new-status new-status))))))

(define-dynamic-page actions/stran/delete ((stran-id integer)
                                           (tbl string))
    ("actions/stran/delete" :request-type :post
                            :validators (((stran-id tbl) (valid-stran-id-p stran-id tbl))))
  (no-cache) 
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db 
	    (delete-dao (get-dao (symbolicate (string-upcase tbl) "-STRAN") stran-id))
	    (see-other (stran))))
	(see-other (notfound)))))

(define-dynamic-page actions/stran/update ((stran-id integer)
                                           (tbl string)
                                           (description string #'not-db-null-p)
                                           (debit-acc string #'account-exists-p)
                                           (credit-acc string #'account-exists-p) 
                                           (old-status string)
                                           (new-status string))
    ("actions/stran/update"
     :request-type :post
     :validators ((old-status (valid-combo tbl old-status))
		  (new-status (valid-combo tbl new-status))
		  ((stran-id tbl) (valid-stran-id-p stran-id tbl))))
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (let ((debit-acc-id (account-id debit-acc))
		(credit-acc-id (account-id credit-acc)))
	    (with-db
	      (execute (:update (symbolicate tbl "-STRAN") :set
				'description description
				'debit-acc-id debit-acc-id
				'credit-acc-id credit-acc-id 
				'old-status old-status
				'new-status new-status
				:where (:= 'id stran-id)))
	      (see-other (stran)))))
	(with-parameter-rebinding #'raw 
	  (see-other (stran/update :stran-id stran-id
                                  :description description
                                  :debit-acc debit-acc
                                  :credit-acc credit-acc
                                  :tbl tbl
                                  :old-status old-status
                                  :new-status new-status))))))


;;; ------------------------------------------------------------
;;; State transitions - Snippets
;;; ------------------------------------------------------------

(define-menu stran-menu (stran-id tbl) (:div-style "actions" :ul-style "hmenu")
  (:create (with-html
	     (:li (:a :href (stran/create)
		      (:img :src (url "img/add.png")) "Δημιουργία"))))
  (:view (with-html
	   (:li (:a :href (stran :stran-id stran-id :tbl tbl)
		    (:img :src (url "img/magnifier.png")) "Προβολή"))))
  (:update (if stran-id
	       (with-html
		 (:li (:a :href (stran/update :stran-id stran-id :tbl tbl)
			  (:img :src (url "img/pencil.png")) "Επεξεργασία")))
	       nil))
  (:delete (if stran-id
	       (with-html
		 (:li (:a :href (stran/delete :stran-id stran-id :tbl tbl)
			  (:img :src (url "img/delete.png")) "Διαγραφή")))
	       nil)))

(define-errorbar stran-errorbar (:ul-style "error")
  (description "Η περιγραφή δεν πρέπει να είναι κενή")
  (debit-acc "Άκυρος λογαριασμός χρέωσης")
  (credit-acc "Άκυρος λογαριασμός πίστωσης")
  (old-status "Άκυρη αρχική κατάσταση")
  (new-status "Άκυρη τελική κατάσταση"))


;;; ------------------------------------------------------------
;;; Pages
;;; ------------------------------------------------------------

(define-dynamic-page stran ((stran-id integer)
                            (tbl string))
    ("config/stran/" :validators (((stran-id tbl) (valid-stran-id-p stran-id tbl))))
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
        (with-page ()
          (:head
           (:title "Καταστατικές Μεταβολές")
           (config-headers))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'config)
                 (config-navbar 'stran))
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Κατάλογος Καταστατικών Μεταβολών"))
                 (:div :id "stran" :class "window"
                       (stran-menu (val stran-id) (val tbl) :create :update :delete) 
                       (render (make-stran-table :operation :view
                                                 :params params))))
           (footer)))
	(see-other (notfound)))))

(define-dynamic-page stran/create ((tbl         string #'valid-tbl-p)
                                   (description string #'not-db-null-p) 
                                   (debit-acc   string #'account-exists-p)
                                   (credit-acc  string #'account-exists-p)
                                   (old-status  string)
                                   (new-status  string))
    ("config/stran/create" :validators ((old-status (valid-combo tbl old-status))
                                        (new-status (valid-combo tbl new-status))))
  (no-cache)
  (if (validp tbl)
      (with-parameter-list params
        (with-page ()
          (:head
           (:title "Καταστατικές Μεταβολές: Δημιουργία")
           (config-headers))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'config)
                 (config-navbar 'stran))
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Δημιουργία μετάβασης")
                       (stran-errorbar description debit-acc credit-acc old-status new-status)) 
                 (:div :id "stran" :class "window"
                       (stran-menu nil (val tbl) :view)
                       (render (make-stran-table :operation :create
                                                 :params params)))))))
      (see-other (notfound))))

(define-dynamic-page stran/update ((stran-id      integer)
                                   (tbl         string #'valid-tbl-p)
                                   (description string #'not-db-null-p)
                                   (debit-acc   string #'account-exists-p) 
                                   (credit-acc  string #'account-exists-p) 
                                   (old-status  string)
                                   (new-status  string))
    ("config/stran/update" :validators ((old-status (valid-combo tbl old-status))
                                        (new-status (valid-combo tbl new-status))
                                        ((stran-id tbl) (valid-stran-id-p stran-id tbl))))
  (no-cache)
  (if (validp stran-id)
      (with-parameter-list params
        (with-page ()
          (:head
           (:title "Καταστατικές Μεταβολές: Επεξεργασία")
           (config-headers))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'config)
                 (config-navbar 'stran))
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Επεξεργασία μετάβασης")
                       (stran-errorbar description debit-acc credit-acc old-status new-status))
                 (:div :id "stran" :class "window" 
                       (stran-menu (val stran-id) (val tbl) :view :delete)
                       (render (make-stran-table :operation :update
                                                 :params params)))))))
      (see-other (notfound))))

(define-dynamic-page stran/delete ((stran-id integer)
                                   (tbl string #'valid-tbl-p))
    ("config/stran/delete" :validators (((stran-id tbl) (valid-stran-id-p stran-id tbl))))
  (no-cache) 
  (if (validp stran-id)
      (with-parameter-list params
	(with-page ()
	  (:head
	   (:title "Καταστατικές Μεταβολές: Διαγραφή")
	   (config-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'stran))
	   (:div :id "body"
		 (:div :class "message"
		       (:h2 :class "info" "Διαγραφή μετάβασης")) 
		 (:div :id "stran" :class "window"
		       (stran-menu (val stran-id) (val tbl) :view :update)
		       (render (make-stran-table :operation :delete
                                                 :params params)))))))
      (see-other (notfound))))



