(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Bank - Definitions
;;; ------------------------------------------------------------

(defclass bank-table (table-normal-crud)
  ((name :initform "bank-table")
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
   (main-page :initform 'bank) 
   (submit-pages :initform '(:create actions/bank/create
                             :update actions/bank/update
                             :delete actions/bank/delete)) 
   (cells-fn :initform (config-cells-fn))
   (data-fn :initform (config-data-fn 'bank))))

(defun make-bank-table (&key operation params)
  (make-instance 'bank-table 
                 :operation operation
                 :params params))



;;; ------------------------------------------------------------
;;; Bank - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/bank/create ((title string (complement #'bank-exists-p)))
    ("actions/bank/create" :request-type :post) 
  (no-cache)
  (with-parameter-list params 
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (insert-dao (make-instance 'bank :title title))
	    (see-other (bank :id (bank-id title)))))
	(with-parameter-rebinding #'raw 
	  (see-other (bank/create :title title))))))

(define-dynamic-page actions/bank/update ((id    integer #'bank-id-exists-p)
                                          (title string  (complement #'bank-exists-p)))
    ("actions/bank/update" :request-type :post)
  (no-cache) 
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db
	    (execute (:update 'bank :set 
			      'title title
			      :where (:= 'id id)))
	    (see-other (bank :id id))))
	(with-parameter-rebinding #'raw
	  (see-other (bank/update :id id :title title))))))

(define-dynamic-page actions/bank/delete ((id integer #'bank-id-exists-p))
    ("actions/bank/delete" :request-type :post)
  (if (validp id)
      (with-db
	(delete-dao (get-dao 'bank (val id)))
	(see-other (bank)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; Bank - Snippets
;;; ------------------------------------------------------------

(define-menu bank-menu (id) (:div-style "actions" :ul-style "hmenu")
  (:view (with-html
	   (:li (:a :href (bank :id id)
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

(define-errorbar bank-errorbar (:ul-style "error") 
  (title "Αυτό το όνομα τράπεζας υπάρχει ήδη.")) 



;;; ------------------------------------------------------------
;;; Bank - Pages
;;; ------------------------------------------------------------

(define-dynamic-page bank ((id integer #'bank-id-exists-p))
    ("config/bank")
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (render
         (make-config-page :name 'bank
                           :title "Τράπεζες"
                           :message "Κατάλογος τραπεζών"
                           :body (html ()
                                   (bank-menu (val id) :create :edit :delete) 
                                   (render (make-bank-table :operation :view
                                                            :params params))))))
      (see-other (notfound))))

(define-dynamic-page bank/create ((title string (complement #'bank-exists-p)))
    ("config/bank/create") 
  (no-cache)
  (with-parameter-list params
    (render
     (make-config-page :name 'bank
                       :title "Εισαγωγή τράπεζας"
                       :message "Εισαγωγή τράπεζας"
                       :body (html ()
                               (bank-menu nil :view) 
                               (render (make-bank-table :operation :create
                                                        :params params)))))))

(define-dynamic-page bank/update ((id    integer #'bank-id-exists-p) 
                                  (title string  (complement #'bank-exists-p)))
    ("config/bank/update")
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (render
         (make-config-page :name 'bank
                           :title "Επεξεργασία τράπεζας"
                           :message "Επεξεργασία τράπεζας"
                           :body (html ()
                                   (bank-menu (val id) :view :delete) 
                                   (render (make-bank-table :operation :update
                                                            :params params))))))
      (see-other (notfound))))

(define-dynamic-page bank/delete ((id integer #'bank-id-exists-p))
    ("config/bank/delete")
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (render
         (make-config-page :name 'bank
                           :title "Διαγραφή τράπεζας"
                           :message "Διαγραφή τράπεζας" 
                           :body (html ()
                                   (bank-menu (val id) :view :edit) 
                                   (render (make-bank-table :operation :delete
                                                            :params params))))))
      (see-other (notfound))))



