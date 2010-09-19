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
        (see-other (city :id (val id))))
      (see-other (city/update :id (val id) :title (val title)))))

(define-dynamic-page actions/city/delete ((id integer #'city-id-exists-p))
    ("actions/city/delete" :request-type :post)
  (if (validp id)
      (with-db ()
	(delete-dao (get-dao 'city (val id)))
	(see-other (city)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; City menus
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
         nil #|(query (:select 'id
                         :from 'company
                         :where (:= 'city-id id)))|#)))

(defun city-errorbar (params)
  (funcall (generic-errorbar)
           params
           '((title "Αυτό το όνομα πόλης υπάρχει ήδη."))))



;;; ------------------------------------------------------------
;;; City table
;;; ------------------------------------------------------------

(defun mkfn-city-selector-states ()
  (lambda (id)
    `((t   ,(city))
      (nil ,(apply #'city id)))))

(defun city-table (op id)
  (let* ((id-keys '(:id))
         (payload-keys '(:title))
         (db-table (config-data 'city))
         (cancel-url (city :id (val* id)))
         (row-selected-p-fn (mkfn-row-selected-p id-keys))
         (selector-states-fn (mkfn-city-selector-states))
         ;; op-specific
         (row-controls-p-fn (mkfn-crud-row-controls-p op))
         (row-readonly-p-fn (mkfn-crud-row-readonly-p op))
         ;; id, payload and the row itself
         (row-id-fn (mkfn-row-id id-keys))
         (row-payload-fn (mkfn-row-payload op payload-keys)) 
         (row (mkfn-crud-row row-id-fn
                             row-payload-fn 
                             row-selected-p-fn
                             row-controls-p-fn
                             row-readonly-p-fn
                             selector-states-fn
                             cancel-url)))
    (html ()
      (:table :class "table-half forms-in-row"
              (thead "" "Ονομασία πόλης" "" "")
              (:tbody
               (when (eql op 'create)
                 (funcall row nil))
               (iter (for db-row in db-table)
                     (funcall row db-row)))))))



;;; ------------------------------------------------------------
;;; City - Pages
;;; ------------------------------------------------------------

(define-dynamic-page city ((id integer #'city-id-exists-p))
    ("config/city")
  (no-cache)
  (if (validp id) 
      (with-document ()
        (:head
         (:title "Πόλεις")
         (head-config))
        (:body
         (config-header 'city)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Κατάλογος πόλεων"))
               (:div :id "city" :class "window"
                     (city-menu (val id) (if (val id)
                                             '(create update delete)
                                             '(create)))
                     (render (city-table 'view id)))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page city/create ((title string (complement #'city-exists-p)))
    ("config/city/create") 
  (no-cache)
  (with-document ()
    (:head
     (:title "Δημιουργία πόλης")
     (head-config))
    (:body
     (config-header 'city)
     (:div :id "body"
           (:div :class "message"
                 (:h2 :class "info" "Δημιουργία πόλης")
                 (city-errorbar (list title)))
           (:div :id "city" :class "window"
                 (city-menu nil '(view))
                 (with-form (actions/city/create :title title)
                   (city-table 'create nil)))
           (footer)))))

(define-dynamic-page city/update ((id    integer #'city-id-exists-p) 
                                  (title string  (complement #'city-exists-p)))
    ("config/city/update")
  (no-cache)
  (if (validp id)
      (with-document ()
        (:head
         (:title "Επεξεργασία πόλης")
         (head-config))
        (:body
         (config-header 'city)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Επεξεργασία πόλης")
                     (city-errorbar (list title)))
               (:div :id "city" :class "window"
                     (city-menu (val id) '(view delete))
                     (with-form (actions/city/update :id (val id))
                       (city-table 'update id)))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page city/delete ((id integer #'city-id-exists-p))
    ("config/city/delete")
  (no-cache)
  (if (validp id)
      (with-document ()
        (:head
         (:title "Διαγραφή πόλης")
         (head-config))
        (:body
         (config-header 'city)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Διαγραφή πόλης"))
               (:div :id "city" :class "window"
                     (city-menu (val id) '(view update))
                     (with-form (actions/city/delete :id (val id))
                       (city-table 'delete id)))
               (footer))))
      (see-other (notfound))))



