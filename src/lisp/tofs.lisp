(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; Tof - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/tof/create ((title string (complement #'tof-exists-p)))
    ("actions/tof/create" :request-type :post)
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (insert-dao (make-instance 'tof :title (val title)))
        (see-other (tof :id (tof-id (val title)))))
      (see-other (tof/create :title (raw title)))))

(define-dynamic-page actions/tof/update ((id    integer #'tof-id-exists-p) 
                                         (title string  (complement #'tof-exists-p)))
    ("actions/tof/update" :request-type :post)
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (execute (:update 'tof :set 
                          'title (val title)
                          :where (:= 'id (val id))))
        (see-other (tof :id id)))
      (see-other (tof/update :id (raw id) :title (raw title)))))

(define-dynamic-page actions/tof/delete ((id integer #'tof-id-exists-p))
    ("actions/tof/delete" :request-type :post)
  (if (validp id)
      (with-db ()
	(delete-dao (get-dao 'tof (val id)))
	(see-other (tof)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; Tof - Snippets
;;; ------------------------------------------------------------

(defun tof-menu (id enabled-items)
  (funcall (actions-menu)
           :item-specs (standard-actions-spec (tof :id id)
                                              (tof/create)
                                              (tof/update :id id)
                                              (if (or (null id)
                                                      (tof-referenced-p id))
                                                  nil
                                                  (tof/delete :id id)))
           :enabled-items enabled-items))

(defun tof-referenced-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                         :from 'company
                         :where (:= 'tof-id id))))))

(defun tof-errorbar (params)
  (funcall (generic-errorbar)
           params
           '((title "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη"))))



;;; ------------------------------------------------------------
;;; Tof - Pages
;;; ------------------------------------------------------------

(define-dynamic-page tof ((id integer #'tof-id-exists-p))
    ("config/tof")
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (render
         (config-page :name 'tof
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
     (config-page :name 'tof
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
         (config-page :name 'tof
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
         (config-page :name 'tof
                      :title "Διαγραφή Δ.Ο.Υ."
                      :message "Διαγραφή Δ.Ο.Υ."
                      :body (html ()
                              (tof-menu (val id) :view :edit) 
                              (render (make-tof-table :operation :delete
                                                      :params params))))))
      (see-other (notfound))))







