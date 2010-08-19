(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; TOFs - Definitions
;;; ------------------------------------------------------------

(defclass tof-table-crud (table-crud)
  ;; table
  ((name :initform "tofs-table")
   (header :initform '(:selector    "" 
                       :title "Τράπεζα" 
                       :submit      ""
                       :cancel      ""))
   (styles :initform '(:row "" :table "table-half forms-in-row")) 
   ;; page interface
   (id-keys :initform '(:id))
   (data-keys :initform '(:title))
   (main-page :initform 'tofs)
   (submit-pages :initform '(:create actions/tof/create
                             :update actions/tof/update
                             :delete actions/tof/delete))
   (filter-keys :initform '())
   (cells :initform
          (lambda (row) 
            (list (make-cell-selector :row row
                                      :name :selector
                                      :style "select") 
                  (make-cell-textbox :row row
                                     :name :title
                                     :value (getf (data row) :title)
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

(defun make-tof-table-crud (&key operation params)
  (make-instance 'tof-table-crud 
                 :operation operation
                 :params params))

(defmethod read-db ((obj tof-table-crud) &key filters)
  (declare (ignore filters))
  (with-db
    (query (make-sql-config 'tof) ;; rows keys are :id :title
           :plists)))



;;; ------------------------------------------------------------
;;; TOFs - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/tof/create ((title string (complement #'tof-exists-p)))
    ("actions/tof/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (insert-dao (make-instance 'tof :title title))
	    (redirect (tofs :id (tof-id title)) :code +http-see-other+)))
	(with-parameter-rebinding #'raw
	  (redirect (tof/create :title title) :code +http-see-other+)))))

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
	    (redirect (tofs :id id) :code +http-see-other+)))
	(with-parameter-rebinding #'raw
	  (redirect (tof/update :id id :title title) :code +http-see-other+)))))

(define-dynamic-page actions/tof/delete ((id integer #'tof-id-exists-p))
    ("actions/tof/delete" :request-type :post)
  (if (validp id)
      (with-db
	(delete-dao (get-dao 'tof (val id)))
	(redirect (tofs) :code +http-see-other+))
      (redirect (notfound) :code +http-see-other+)))


;;; ------------------------------------------------------------
;;; TOFs - Snippets
;;; ------------------------------------------------------------

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

(define-errorbar tof-errorbar (:ul-style "error") 
  (title "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη"))



;;; ------------------------------------------------------------
;;; TOFs - Pages
;;; ------------------------------------------------------------

(define-dynamic-page tofs ((id integer #'tof-id-exists-p) 
                           (filter string))
    ("config/tofs")
  (if (validp id)
      (with-parameter-list params
        (with-page ()
          (:head
           (:title "Δ.Ο.Υ.")
           (config-headers))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'config)
                 (config-navbar 'tofs)) 
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Κατάλογος Δ.Ο.Υ."))
                 (:div :id "tofs" :class "window" 
                       (tof-menu (val id) :create :edit :delete)
                       (filter tofs (val id) (val filter) nil)
                       (render (make-tof-table-crud :operation :view
                                                    :params params)))
                 (footer)))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page tof/create ((title string (complement #'tof-exists-p)))
    ("config/tof/create")
  (no-cache)
  (with-parameter-list params
    (with-page ()
      (:head
       (:title "Εισαγωγή Δ.Ο.Υ.")
       (config-headers))
      (:body
       (:div :id "header"
             (logo)
             (primary-navbar 'config)
             (config-navbar 'tofs))
       (:div :id "body"
             (:div :class "message"
                   (:h2 :class "info" "Δημιουργία Δ.Ο.Υ.")
                   (tof-errorbar title))
             (:div :id "tofs" :class "window"
                   (tof-menu nil :view) 
                   (render (make-tof-table-crud :operation :create
                                                :params params)))
             (footer))))))

(define-dynamic-page tof/update ((id integer #'tof-id-exists-p)
                                 (title string (complement #'tof-exists-p)))
    ("config/tof/update") 
  (if (validp id)
      (with-parameter-list params
        (with-page ()
          (:head
           (:title "Επεξεργασία Δ.Ο.Υ.")
           (config-headers))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'config)
                 (config-navbar 'tofs))
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Επεξεργασία Δ.Ο.Υ.")
                       (tof-errorbar title))
                 (:div :id "tofs" :class "window"
                       (tof-menu (val id) :view :delete) 
                       (render (make-tof-table-crud :operation :update
                                                    :params params)))
                 (footer)))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page tof/delete ((id integer #'tof-id-exists-p))
    ("config/tof/delete")
  (if (validp id)
      (with-parameter-list params
        (with-page ()
          (:head
           (:title "Διαγραφή Δ.Ο.Υ.")
           (config-headers))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'config)
                 (config-navbar 'tofs))
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Διαγραφή Δ.Ο.Υ."))
                 (:div :id "tofs" :class "window"
                       (tof-menu (val id) :view :edit) 
                       (render (make-tof-table-crud :operation :delete
                                                    :params params)))
                 (footer)))))
      (redirect (notfound) :code +http-see-other+)))







