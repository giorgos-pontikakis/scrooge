(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; City - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/city/create ((title string (complement #'city-exists-p)))
    ("actions/city/create" :request-type :post) 
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (insert-dao (make-instance 'city :title (val title)))
        (see-other (city :id (city-id (val title)))))
      (see-other (city/create :title (val title)))))

(define-dynamic-page actions/city/update ((id    integer #'city-id-exists-p)
                                          (title string  (complement #'city-exists-p)))
    ("actions/city/update" :request-type :post)
  (no-cache) 
  (if (every #'validp (parameters *page*))
      (with-db ()
        (execute (:update 'city :set 
                          'title (val title)
                          :where (:= 'id (val id))))
        (see-other (city :id id)))
      (see-other (city/update :id (val id) :title (val title)))))

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

(defun city-menu (id enabled-items)
  (funcall (actions-menu)
           :item-specs (standard-actions-spec (city :id id)
                                              (city/create)
                                              (city/update :id id)
                                              (if (or (null id)
                                                      (city-referenced-p id))
                                                  nil
                                                  (city/delete :id id)))
           :enabled-items enabled-items))

(defun city-referenced-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                         :from 'company
                         :where (:= 'city-id id))))))

(defun city-errorbar (params)
  (funcall (generic-errorbar)
           params
           '((title "Αυτό το όνομα πόλης υπάρχει ήδη."))))



;;; ------------------------------------------------------------
;;; City table
;;; ------------------------------------------------------------

(defun city-selector-cell (id)
  (selector-cell `((t   ,(city))
                   (nil ,(apply #'city id)))))



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



