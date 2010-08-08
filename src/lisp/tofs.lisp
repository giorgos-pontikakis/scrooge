(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; TOFs - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/tof/create ((title string (complement #'tof-title-exists-p)))
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
                                         (title string))
    ("actions/tof/update" :request-type :post
                          :validators ((title  (valid-tof-title-p id title))))
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

(define-row-display tof-row-display tofs :id (:title) ("data"))

(define-row-create tof-row-create tofs actions/tof/create (:title) ("data"))

(define-row-update tof-row-update tofs actions/tof/update :id (:title) ("data"))

(define-row-delete tof-row-delete tofs actions/tof/delete :id (:title) ("data"))

(defun tofs-table (intent filter active-id title)
  (let ((params (list title)))
    (with-db
      (let ((db-data (query
                      (sql-compile
                       `(:select 'id 'id 'title :from 'tof
                                 :where (:or (:ilike 'title ,(ilike filter))
                                             ,(if active-id
                                                  `(:= 'id ,active-id)
                                                  nil))))
                      :plists)) 
            (header '("" "Ονομασία Δ.Ο.Υ." "" ""))
            (values (zip '(:title) (mapcar #'val* params)))
            (styles (zip '(:title) (mapcar #'style-invalid-p params))))
        (with-html
          (:table :id "tofs-table" :class "table-half forms-in-row"
                  (:thead
                   (:tr (iter (for label in header) (htm (:th (str label))))))
                  (:tbody
                   (when (eql intent :create)
                     (tof-row-create active-id values styles))
                   (iter (for row in db-data) 
                         (if (and active-id (eql active-id (getf row :id)))
                             (let ((merged (plist-union values row)))
                               (case intent
                                 (:view (tof-row-display active-id merged)) 
                                 (:update (tof-row-update active-id merged styles))
                                 (:delete (tof-row-delete active-id merged))))
                             (tof-row-display active-id row)))))))))) 

(define-errorbar tof-errorbar (:ul-style "error") 
  (title "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη"))


;;; ------------------------------------------------------------
;;; TOFs - Pages
;;; ------------------------------------------------------------

(define-dynamic-page tofs ((id integer #'tof-id-exists-p) 
                           (filter string))
    ("config/tofs")
  (if (validp id)
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
                     (tofs-table :view (val filter) (val id) nil))
               (footer))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page tof/create ((title string (complement #'tof-title-exists-p)))
    ("config/tof/create")
  (no-cache)
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
                 (tofs-table :create nil nil title))
           (footer)))))

(define-dynamic-page tof/update ((id integer #'tof-id-exists-p)
                                 (title string))
    ("config/tof/update" :validators ((title (valid-tof-title-p id title)))) 
  (if (validp id)
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
                     (tofs-table :update nil (val id) title))
               (footer))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page tof/delete ((id integer #'tof-id-exists-p))
    ("config/tof/delete")
  (if (validp id)
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
                     (tofs-table :delete nil (val id) nil))
               (footer))))
      (redirect (notfound) :code +http-see-other+)))







