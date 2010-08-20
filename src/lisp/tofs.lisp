(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Tof - Definitions
;;; ------------------------------------------------------------

(defclass tof-table (table-normal-crud)
  ((name :initform "tof-table")
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
   (main-page :initform 'tof)
   (submit-pages :initform '(:create actions/tof/create
                             :update actions/tof/update
                             :delete actions/tof/delete)) 
   (cells-fn :initform (config-cells-fn))
   (data-fn :initform (config-data-fn 'tof))))

(defun make-tof-table (&key operation params)
  (make-instance 'tof-table 
                 :operation operation
                 :params params))



;;; ------------------------------------------------------------
;;; Tof - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/tof/create ((title string (complement #'tof-exists-p)))
    ("actions/tof/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (insert-dao (make-instance 'tof :title title))
	    (see-other (tof :id (tof-id title)))))
	(with-parameter-rebinding #'raw
	  (see-other (tof/create :title title))))))

(define-dynamic-page actions/tof/update ((id    integer #'tof-id-exists-p) 
                                         (title string  (complement #'tof-exists-p)))
    ("actions/tof/update" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (execute (:update 'tof :set 
			      'title title
			      :where (:= 'id id)))
	    (see-other (tof :id id))))
	(with-parameter-rebinding #'raw
	  (see-other (tof/update :id id :title title))))))

(define-dynamic-page actions/tof/delete ((id integer #'tof-id-exists-p))
    ("actions/tof/delete" :request-type :post)
  (if (validp id)
      (with-db
	(delete-dao (get-dao 'tof (val id)))
	(see-other (tof)))
      (see-other (notfound))))


;;; ------------------------------------------------------------
;;; Tof - Snippets
;;; ------------------------------------------------------------

(define-menu tof-menu (id) (:div-style "actions" :ul-style "hmenu")
  (:view (with-html
	   (:li (:a :href (tof :id id)
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

(define-errorbar tof-errorbar (:ul-style "error") 
  (title "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη"))



;;; ------------------------------------------------------------
;;; Tof - Pages
;;; ------------------------------------------------------------

(define-dynamic-page tof ((id integer #'tof-id-exists-p))
    ("config/tof")
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (render
         (make-config-page :name 'tof
                           :title "Δ.Ο.Υ."
                           :message "Κατάλογος Δ.Ο.Υ."
                           :body (html ()
                                   (tof-menu (val id) :create :edit :delete) 
                                   (render (make-tof-table :operation :view
                                                           :params params))))))
      (see-other (notfound))))

(define-dynamic-page tof/create ((title string (complement #'tof-exists-p)))
    ("config/tof/create")
  (no-cache)
  (with-parameter-list params
    (render
     (make-config-page :name 'tof
                       :title "Εισαγωγή Δ.Ο.Υ."
                       :message "Εισαγωγή Δ.Ο.Υ."
                       :body (html ()
                               (tof-menu nil :view) 
                               (render (make-tof-table :operation :create
                                                       :params params)))))))

(define-dynamic-page tof/update ((id integer #'tof-id-exists-p)
                                 (title string (complement #'tof-exists-p)))
    ("config/tof/update")
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (render
         (make-config-page :name 'tof
                           :title "Επεξεργασία Δ.Ο.Υ."
                           :message "Επεξεργασία Δ.Ο.Υ."
                           :body (html ()
                                   (tof-menu (val id) :view :delete) 
                                   (render (make-tof-table :operation :update
                                                           :params params))))))
      (see-other (notfound))))

(define-dynamic-page tof/delete ((id integer #'tof-id-exists-p))
    ("config/tof/delete")
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (render
         (make-config-page :name 'tof
                           :title "Διαγραφή Δ.Ο.Υ."
                           :message "Διαγραφή Δ.Ο.Υ."
                           :body (html ()
                                   (tof-menu (val id) :view :edit) 
                                   (render (make-tof-table :operation :delete
                                                           :params params))))))
      (see-other (notfound))))







