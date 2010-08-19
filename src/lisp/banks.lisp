(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Banks - Definitions
;;; ------------------------------------------------------------

(defclass bank-table-crud (table-crud)
  ;; table
  ((name :initform "banks-table")
   (header :initform '(:selector    "" 
                       :title "Τράπεζα" 
                       :submit      ""
                       :cancel      ""))
   (styles :initform '(:row "" :table "forms-in-row table-half")) 
   ;; page interface
   (id-keys :initform '(:id))
   (data-keys :initform '(:title))
   (main-page :initform 'banks)
   (submit-pages :initform '(:create actions/bank/create
                             :update actions/bank/update
                             :delete actions/bank/delete))
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

(defun make-bank-table-crud (&key operation params)
  (make-instance 'bank-table-crud 
                 :operation operation
                 :params params))

(defmethod read-db ((obj bank-table-crud) &key filters)
  (declare (ignore filters))
  (with-db
    (query (make-sql-config 'bank) ;; rows keys are :id :title
           :plists)))



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

(define-errorbar bank-errorbar (:ul-style "error") 
  (title "Αυτό το όνομα τράπεζας υπάρχει ήδη.")) 



;;; ------------------------------------------------------------
;;; Banks - Pages
;;; ------------------------------------------------------------

(define-dynamic-page banks ((id integer #'bank-id-exists-p) 
                            (filter string))
    ("config/banks")
  (if (validp id)
      (with-parameter-list params
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
                       (render (make-bank-table-crud :operation :view
                                                     :params params)))
                 (footer)))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page bank/create ((title string (complement #'bank-exists-p)))
    ("config/bank/create") 
  (no-cache)
  (with-parameter-list params
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
                   (render (make-bank-table-crud :operation :create
                                                 :params params)))
             (footer))))))

(define-dynamic-page bank/update ((id    integer #'bank-id-exists-p) 
                                  (title string  (complement #'bank-exists-p)))
    ("config/bank/update" )
  (if (validp id)
      (with-parameter-list params
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
                       (render (make-bank-table-crud :operation :update
                                                    :params params)))
                 (footer)))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page bank/delete ((id integer #'bank-id-exists-p))
    ("config/bank/delete")
  (if (validp id)
      (with-parameter-list params
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
                       (render (make-bank-table-crud :operation :delete
                                                     :params params))))
           (footer))))
      (redirect (notfound) :code +http-see-other+)))





