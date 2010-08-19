(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Banks - Definitions
;;; ------------------------------------------------------------

(defclass city-table-crud (table-crud)
  ;; table
  ((name :initform "cities-table")
   (header :initform '(:selector "" 
                       :title "Πόλη" 
                       :submit ""
                       :cancel ""))
   (styles :initform '(:row "" :table "forms-in-row table-half")) 
   ;; page interface
   (id-keys :initform '(:id))
   (data-keys :initform '(:title))
   (main-page :initform 'city)
   (submit-pages :initform '(:create actions/city/create
                             :update actions/city/update
                             :delete actions/city/delete))
   (filter-keys :initform '())
   (cells :initform (simple-table-cells))))

(defun make-city-table-crud (&key operation params)
  (make-instance 'city-table-crud 
                 :operation operation
                 :params params))

(defmethod read-db ((obj city-table-crud) &key filters)
  (declare (ignore filters))
  (with-db
    (query (make-sql-config 'city) ;; rows keys are :id :title
           :plists)))



;;; ------------------------------------------------------------
;;; City - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/city/create ((title string (complement #'city-exists-p)))
    ("actions/city/create" :request-type :post) 
  (no-cache)
  (with-parameter-list params 
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (insert-dao (make-instance 'city :title title))
	    (redirect (city :id (city-id title)) :code +http-see-other+)))
	(with-parameter-rebinding #'raw 
	  (redirect (city/create :title title) :code +http-see-other+)))))

(define-dynamic-page actions/city/update ((id    integer #'city-id-exists-p)
                                          (title string  (complement #'city-exists-p)))
    ("actions/city/update" :request-type :post)
  (no-cache) 
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (execute (:update 'city :set 
			      'title title
			      :where (:= 'id id)))
	    (redirect (city :id id) :code +http-see-other+)))
	(with-parameter-rebinding #'raw
	  (redirect (city/update :id id :title title) :code +http-see-other+)))))

(define-dynamic-page actions/city/delete ((id integer #'city-id-exists-p))
    ("actions/city/delete" :request-type :post)
  (if (validp id)
      (with-db
	(delete-dao (get-dao 'city (val id)))
	(redirect (city) :code +http-see-other+))
      (redirect (notfound) :code +http-see-other+)))



;;; ------------------------------------------------------------
;;; City - Snippets
;;; ------------------------------------------------------------

(define-menu city-menu (id) (:div-style "actions" :ul-style "hmenu")
  (:view (with-html
	   (:li (:a :href (city :id id)
		    (:img :src (url "img/magnifier.png")) "Προβολή"))))
  (:create (with-html
	     (:li (:a :href (city/create)
		      (:img :src (url "img/add.png")) "Δημιουργία"))))
  (:edit (if id
	     (with-html
	       (:li (:a :href (city/update :id id)
			(:img :src (url "img/pencil.png")) "Επεξεργασία")))
	     nil))
  (:delete (with-db
	     (if id
                 (with-html
                   (:li (:a :href (city/delete :id id)
                            (:img :src (url "img/delete.png")) "Διαγραφή")))
                 nil))))

(define-errorbar city-errorbar (:ul-style "error") 
  (title "Αυτό το όνομα πόλης υπάρχει ήδη.")) 



;;; ------------------------------------------------------------
;;; City - Pages
;;; ------------------------------------------------------------

(define-dynamic-page city ((id integer #'city-id-exists-p))
    ("config/city")
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (render
         (make-config-page :name 'city
                           :title "Πόλεις"
                           :message "Κατάλογος πόλεων"
                           :body (html ()
                                   (city-menu (val id) :create :edit :delete) 
                                   (render (make-city-table-crud :operation :view
                                                                 :params params))))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page city/create ((title string (complement #'city-exists-p)))
    ("config/city/create") 
  (no-cache)
  (with-parameter-list params
    (render
     (make-config-page :name 'city
                       :title "Εισαγωγή πόλης"
                       :message "Εισαγωγή πόλης"
                       :body (html ()
                               (city-menu nil :view) 
                               (render (make-city-table-crud :operation :create
                                                             :params params)))))))

(define-dynamic-page city/update ((id    integer #'city-id-exists-p) 
                                  (title string  (complement #'city-exists-p)))
    ("config/city/update" )
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (render
         (make-config-page :name 'city
                           :title "Επεξεργασία πόλης"
                           :message "Επεξεργασία πόλης"
                           :body (html ()
                                   (city-menu (val id) :view :delete) 
                                   (render (make-city-table-crud :operation :update
                                                                 :params params))))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page city/delete ((id integer #'city-id-exists-p))
    ("config/city/delete")
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (render
         (make-config-page :name 'city
                           :title "Διαγραφή πόλης"
                           :message "Διαγραφή πόλης" 
                           :body (html ()
                                   (city-menu (val id) :view :edit) 
                                   (render (make-city-table-crud :operation :delete
                                                                 :params params))))))
      (redirect (notfound) :code +http-see-other+)))



