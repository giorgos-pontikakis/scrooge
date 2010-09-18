(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Banks - Definitions
;;; ------------------------------------------------------------

(defclass city-table (table-normal-crud)
  ;; table
  ((name :initform "cities-table")
   (header :initform '(:selector "" 
                       :title "Πόλη" 
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
   (main-page :initform 'city)
   (submit-pages :initform '(:create actions/city/create
                             :update actions/city/update
                             :delete actions/city/delete))
   (cells-fn :initform (config-cells-fn))
   (data-fn :initform (config-data-fn 'city))))

(defun make-city-table (&key operation params)
  (make-instance 'city-table 
                 :operation operation
                 :params params))



;;; ------------------------------------------------------------
;;; City - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/city/create ((title string (complement #'city-exists-p)))
    ("actions/city/create" :request-type :post) 
  (no-cache)
  (with-parameter-list params 
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db ()
	    (insert-dao (make-instance 'city :title title))
	    (see-other (city :id (city-id title)))))
	(with-parameter-rebinding #'raw 
	  (see-other (city/create :title title))))))

(define-dynamic-page actions/city/update ((id    integer #'city-id-exists-p)
                                          (title string  (complement #'city-exists-p)))
    ("actions/city/update" :request-type :post)
  (no-cache) 
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db ()
	    (execute (:update 'city :set 
			      'title title
			      :where (:= 'id id)))
	    (see-other (city :id id))))
	(with-parameter-rebinding #'raw
	  (see-other (city/update :id id :title title))))))

(define-dynamic-page actions/city/delete ((id integer #'city-id-exists-p))
    ("actions/city/delete" :request-type :post)
  (if (validp id)
      (with-db ()
	(delete-dao (get-dao 'city (val id)))
	(see-other (city)))
      (see-other (notfound))))



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
  (:delete (with-db ()
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
         (config-page :name 'city
                      :title "Πόλεις"
                      :message "Κατάλογος πόλεων"
                      :body (html ()
                              (city-menu (val id) :create :edit :delete) 
                              (render (make-city-table :operation :view
                                                            :params params))))))
      (see-other (notfound))))

(define-dynamic-page city/create ((title string (complement #'city-exists-p)))
    ("config/city/create") 
  (no-cache)
  (with-parameter-list params
    (render
     (config-page :name 'city
                  :title "Εισαγωγή πόλης"
                  :message "Εισαγωγή πόλης"
                  :body (html ()
                          (city-menu nil :view) 
                          (render (make-city-table :operation :create
                                                        :params params)))))))

(define-dynamic-page city/update ((id    integer #'city-id-exists-p) 
                                  (title string  (complement #'city-exists-p)))
    ("config/city/update")
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (render
         (config-page :name 'city
                      :title "Επεξεργασία πόλης"
                      :message "Επεξεργασία πόλης"
                      :body (html ()
                              (city-menu (val id) :view :delete) 
                              (render (make-city-table :operation :update
                                                            :params params))))))
      (see-other (notfound))))

(define-dynamic-page city/delete ((id integer #'city-id-exists-p))
    ("config/city/delete")
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (render
         (config-page :name 'city
                      :title "Διαγραφή πόλης"
                      :message "Διαγραφή πόλης" 
                      :body (html ()
                              (city-menu (val id) :view :edit) 
                              (render (make-city-table :operation :delete
                                                            :params params))))))
      (see-other (notfound))))



