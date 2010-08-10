(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Banks - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/bank/create ((title string (complement #'bank-exists-p)))
    ("actions/bank/create" :request-type :post) 
  (no-cache)
  (with-parameter-list params 
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (insert-dao (make-instance 'bank :title title))
	    (redirect (banks :id (bank-id title)) :code +http-see-other+)))
	(with-parameter-rebinding #'raw 
	  (redirect (bank/create :title title) :code +http-see-other+)))))

(define-dynamic-page actions/bank/update ((id integer #'bank-id-exists-p)
                                          (title string))
    ("actions/bank/update" :request-type :post
                           :validators ((title (valid-bank-p id title))))
  (no-cache) 
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (execute (:update 'bank :set 
			      'title title
			      :where (:= 'id id)))
	    (redirect (banks :id id) :code +http-see-other+)))
	(with-parameter-rebinding #'raw
	  (redirect (bank/update :id id :title title) :code +http-see-other+)))))

(define-dynamic-page actions/bank/delete ((id integer #'bank-id-exists-p))
    ("actions/bank/delete" :request-type :post)
  (if (validp id)
      (with-db
	(delete-dao (get-dao 'bank (val id)))
	(redirect (banks) :code +http-see-other+))
      (redirect (notfound) :code +http-see-other+)))



;;; ------------------------------------------------------------
;;; Banks - Snippets
;;; ------------------------------------------------------------

(define-menu bank-menu (id) (:div-style "actions" :ul-style "hmenu")
  (:view (with-html
	   (:li (:a :href (banks :id id)
		    (:img :src (url "img/magnifier.png")) "Προβολή"))))
  (:create (with-html
	     (:li (:a :href (bank/create)
		      (:img :src (url "img/add.png")) "Δημιουργία"))))
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

(define-row-display bank-row-display banks (:id) (:title) ("data"))

(define-row-create bank-row-create banks actions/bank/create (:id) (:title) ("data"))

(define-row-update bank-row-update banks actions/bank/update (:id) (:title) ("data"))

(define-row-delete bank-row-delete banks actions/bank/delete (:id) (:title) ("data"))

(defun banks-table (intent filter active-id title)
  (let ((params (list title)))
    (with-db
      (let ((db-data (query
                      (sql-compile
                       `(:select 'id 'title :from 'bank
                                 :where (:or (:ilike 'title ,(ilike filter))
                                             ,(if active-id
                                                  `(:= 'id ,active-id)
                                                  nil))))
                      :plists))
            (header '("" "Ονομασία Τράπεζας" "" ""))
            (values (zip '(:title) (mapcar #'val* params)))
            (styles (zip '(:title) (mapcar #'style-invalid-p params)))) 
        (with-html
          (:table :id "banks-table" :class "table-half forms-in-row"
                  (:thead
                   (:tr (iter (for label in header) (htm (:th (str label))))))
                  (:tbody  
                   (when (eql intent :create)
                     (bank-row-create active-id values styles))
                   (iter (for row in db-data) 
                         (if (and active-id (eql active-id (getf row :id)))
                             (let ((merged (plist-union values row))) 
                               (case intent 
                                 (:view (bank-row-display active-id merged))
                                 (:update (bank-row-update active-id merged styles))
                                 (:delete (bank-row-delete active-id merged))))
                             (bank-row-display active-id row))))))))))

(define-errorbar bank-errorbar (:ul-style "error") 
  (title "Αυτό το όνομα τράπεζας υπάρχει ήδη.")) 



;;; ------------------------------------------------------------
;;; Banks - Pages
;;; ------------------------------------------------------------

(define-dynamic-page banks ((id integer #'bank-id-exists-p) 
                            (filter string))
    ("config/banks")
  (if (validp id)
      (with-page ()
        (:head
         (:title "Τράπεζες")
         (config-headers))
        (:body
         (:div :id "header"
               (logo)
               (primary-navbar 'config)
               (config-navbar 'banks))
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Κατάλογος τραπεζών"))
               (:div :id "banks" :class "window"
                     (bank-menu (val id) :create :edit :delete)
                     (filter banks (val id) (val filter) nil)
                     (banks-table :view (val filter) (val id) nil))
               (footer))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page bank/create ((title string (complement #'bank-exists-p)))
    ("config/bank/create") 
  (no-cache)
  (with-page ()
    (:head
     (:title "Εισαγωγή τράπεζας")
     (config-headers))
    (:body
     (:div :id "header"
           (logo)
           (primary-navbar 'config)
           (config-navbar 'banks))
     (:div :id "body"
           (:div :class "message"
                 (:h2 :class "info" "Δημιουργία τράπεζας")
                 (bank-errorbar title))
           (:div :id "banks" :class "window"
                 (bank-menu nil :view) 
                 (banks-table :create nil nil title))
           (footer)))))

(define-dynamic-page bank/update ((id integer #'bank-id-exists-p) 
                                  (title string))
    ("config/bank/update" :validators ((title (valid-bank-p id title))))
  (if (validp id)
      (with-page ()
        (:head
         (:title "Επεξεργασία τράπεζας")
         (config-headers))
        (:body
         (:div :id "header"
               (logo)
               (primary-navbar 'config)
               (config-navbar 'banks))
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Επεξεργασία τράπεζας")
                     (bank-errorbar title))
               (:div :id "banks" :class "window"
                     (bank-menu (val id) :view :delete) 
                     (banks-table :update nil (val id) title))
               (footer))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page bank/delete ((id integer #'bank-id-exists-p))
    ("config/bank/delete")
  (if (validp id)
      (with-page ()
        (:head
         (:title "Διαγραφή τράπεζας")
         (config-headers))
        (:body
         (:div :id "header"
               (logo)
               (primary-navbar 'config)
               (config-navbar 'banks))
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Διαγραφή τράπεζας"))
               (:div :id "banks" :class "window"
                     (bank-menu (val id) :view :edit) 
                     (banks-table :delete nil (val id) nil)))
         (footer)))
      (redirect (notfound) :code +http-see-other+)))





