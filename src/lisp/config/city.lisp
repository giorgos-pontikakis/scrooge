(in-package :scrooge)



;;; ------------------------------------------------------------
;;; City - Validation
;;; ------------------------------------------------------------

(defun city-referenced-p (id)
  (with-db ()
    (and id
         nil #|(query (:select 'id
         :from 'company
         :where (:= 'city-id id)))|#)))

(define-existence-predicate city-id-exists-p city id)
(define-existence-predicate city-title-exists-p city title)
(define-uniqueness-predicate city-title-unique-p city title id)

(defun chk-city-id (id)
  (if (city-id-exists-p id)
      nil
      :city-id-unknown))

(defun chk-city-id/ref (id)
  (if (and (null (chk-city-id id))
           (null (city-referenced-p id)))
      nil
      :city-referenced))

(defun chk-new-city-title (title &optional id)
  (cond ((eql :null title) :city-title-null)
        ((not (city-title-unique-p title id)) :city-title-exists)
        (t nil)))

(defun chk-city-title (title)
  (if (or (eql :null title)
          (city-title-exists-p title))
      nil
      :city-title-unknown))



;;; ------------------------------------------------------------
;;; City - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/config/city/create ("actions/config/city/create" :request-type :post)
    ((title  string chk-new-city-title t)
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (insert-dao (make-instance 'city :title (val title)))
          (see-other (city :id (city-id (val title)))))
        (see-other (city/create :title (raw title) :filter (raw filter))))))

(define-dynamic-page actions/config/city/update ("actions/config/city/update" :request-type :post)
    ((id     integer chk-city-id t)
     (title  string (chk-new-city-title title id) t)
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (execute (:update 'city :set
                            'title (val title)
                            :where (:= 'id (val id))))
          (see-other (city :id (val id) :filter (val filter))))
        (see-other (city/update :id (raw id) :title (raw title) :filter (raw filter))))))

(define-dynamic-page actions/config/city/delete ("actions/config/city/delete" :request-type :post)
    ((id     integer chk-city-id/ref t)
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'city (val id)))
          (see-other (city :filter (val filter))))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun city-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "city-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (crud-actions-spec (city :id id
                                                         :filter filter)
                                                   (city/create :filter filter)
                                                   (city/update :id id
                                                                :filter filter)
                                                   (if (or (null id)
                                                           (city-referenced-p id))
                                                       nil
                                                       (city/delete :id id
                                                                    :filter filter))))
           :disabled-items disabled-items))


(defun city-notifications ()
  (notifications '((title (:city-title-null "Το όνομα πόλης είναι κενό."
                           :city-title-exists "Αυτό το όνομα πόλης υπάρχει ήδη.")))))


;;; ------------------------------------------------------------
;;; City table
;;; ------------------------------------------------------------

;;; table

(defclass city-table (crud-table)
  ((header-labels :initform '("" "Ονομασία πόλης" "" ""))
   (paginator :initform (make-instance 'paginator
                                       :id "city-paginator"
                                       :style "paginator grid_9 alpha"
                                       :delta 10
                                       :urlfn (lambda (filter start)
                                                (city :filter filter
                                                      :start start)))))
  (:default-initargs :id "config-table" :item-class 'city-row))

(defmethod read-records ((table city-table))
  (config-data 'city (filter table)))


;;; rows

(defclass city-row (crud-row)
  ())

(defmethod cells ((row city-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (filter (filter (collection row))))
    (list :selector (make-instance 'selector-cell
                                   :states (list :on (city :filter filter
                                                           :start (page-start pg (index row) start))
                                                 :off (city :filter filter
                                                            :id id)))
          :payload (make-instance 'textbox-cell
                                  :name 'title
                                  :value (getf record :title))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (city :id id :filter filter))))))



;;; ------------------------------------------------------------
;;; City - Pages
;;; ------------------------------------------------------------

(define-dynamic-page city ("config/city")
    ((id     integer chk-city-id)
     (filter string)
     (start  integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((city-table (make-instance 'city-table
                                         :op 'catalogue
                                         :filter (val* filter))))
          (with-document ()
            (:head
             (:title "Πόλεις")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'city)
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (city) (val filter)))
                   (:div :id "city-window" :class "window grid_9"
                         (:div :class "title" "Κατάλογος πόλεων")
                         (city-menu (val id)
                                    (val filter)
                                    (if (val id)
                                        '(catalogue)
                                        '(catalogue update delete)))
                         (display city-table
                                  :selected-id (val* id)
                                  :start (val* start)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page city/create ("config/city/create")
    ((title  string chk-new-city-title)
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (let ((city-table (make-instance 'city-table
                                     :op 'create
                                     :filter (val* filter))))
      (with-document ()
        (:head
         (:title "Δημιουργία πόλης")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'city)
               (:div :id "sidebar" :class "sidebar grid_3"
                     (:p :class "title" "Φίλτρα")
                     (searchbox (city) (val filter))
                     (city-notifications))
               (:div :id "city-window" :class "window grid_9"
                     (:div :class "title" "Δημιουργία πόλης")
                     (city-menu nil
                                (val filter)
                                '(create update delete))
                     (with-form (actions/config/city/create :filter (val* filter))
                       (display city-table
                                :selected-id nil
                                :selected-data (list :title (val* title)))))
               (footer)))))))

(define-dynamic-page city/update ("config/city/update")
    ((id     integer chk-city-id                   t)
     (title  string  (chk-new-city-title title id))
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((city-table (make-instance 'city-table
                                         :op 'update
                                         :filter (val* filter))))
          (with-document ()
            (:head
             (:title "Επεξεργασία πόλης")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'city)
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (city) (val filter))
                         (city-notifications))
                   (:div :id "city-window" :class "window grid_9"
                         (:div :class "title" "Επεξεργασία πόλης")
                         (city-menu (val id)
                                    (val filter)
                                    '(create update))
                         (with-form (actions/config/city/update :id (val* id)
                                                                :filter (val* filter))
                           (display city-table
                                    :selected-id (val id)
                                    :selected-data (list :title (val* title)))))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page city/delete ("config/city/delete")
    ((id integer chk-city-id/ref t)
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((city-table (make-instance 'city-table
                                         :op 'delete
                                         :filter (val* filter))))
          (with-document ()
            (:head
             (:title "Διαγραφή πόλης")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'city)
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (city) (val filter)))
                   (:div :id "city-window" :class "window grid_9"
                         (:div :class "title" "Διαγραφή πόλης")
                         (city-menu (val id)
                                    (val filter)
                                    '(create delete))
                         (with-form (actions/config/city/delete :id (val id)
                                                                :filter (val* filter))
                           (display city-table
                                    :selected-id (val id))))
                   (footer)))))
        (see-other (notfound)))))
