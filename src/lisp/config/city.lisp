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

(define-dynamic-page actions/city/create ("actions/city/create" :request-type :post)
    ((title  string chk-new-city-title t)
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (insert-dao (make-instance 'city :title (val title)))
          (see-other (city :id (city-id (val title)))))
        (see-other (city/create :title (raw title) :filter (raw filter))))))

(define-dynamic-page actions/city/update ("actions/city/update" :request-type :post)
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

(define-dynamic-page actions/city/delete ("actions/city/delete" :request-type :post)
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
;;; City menu
;;; ------------------------------------------------------------

(defun city-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "city-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (standard-actions-spec (city :id id
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
                                                      :start start))))))

(defmethod read-items ((table city-table))
  (iter (for rec in (config-data 'city (filter table)))
        (for i from 0)
        (collect (make-instance 'city-row
                                :key (getf rec :id)
                                :record rec
                                :collection table
                                :index i))))

(defmethod insert-item ((table city-table) &key record index)
  (let* ((rows (rows table))
         (new-row (make-instance 'city-row
                                  :key (getf record :id)
                                  :record record
                                  :collection table
                                  :index index)))
    (setf (rows table)
          (ninsert-list index new-row rows))))


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
;;; Notifications
;;; ------------------------------------------------------------

(defun city-notifications (&rest params)
  (notifications '((title (:city-title-null "Το όνομα πόλης είναι κενό."
                           :city-title-exists "Αυτό το όνομα πόλης υπάρχει ήδη.")))
                 params))

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
                   (config-menu 'city)
                   (:div :id "controls" :class "controls grid_3"
                         (filters 'city (val filter)))
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
               (config-menu 'city)
               (:div :id "controls" :class "controls grid_3"
                     (filters 'city (val filter))
                     (city-notifications title))
               (:div :id "city-window" :class "window grid_9"
                     (:div :class "title" "Δημιουργία πόλης")
                     (city-menu nil
                                (val filter)
                                '(create update delete))
                     (with-form (actions/city/create :filter (val* filter))
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
                   (config-menu 'city)
                   (:div :id "controls" :class "controls grid_3"
                         (filters 'city (val filter))
                         (city-notifications title))
                   (:div :id "city-window" :class "window grid_9"
                         (:div :class "title" "Επεξεργασία πόλης")
                         (city-menu (val id)
                                    (val filter)
                                    '(create update))
                         (with-form (actions/city/update :id (val* id)
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
                   (config-menu 'city)
                   (:div :id "controls" :class "controls grid_3"
                         (filters 'city (val filter)))
                   (:div :id "city-window" :class "window grid_9"
                         (:div :class "title" "Διαγραφή πόλης")
                         (city-menu (val id)
                                    (val filter)
                                    '(create delete))
                         (with-form (actions/city/delete :id (val id)
                                                         :filter (val* filter))
                           (display city-table
                                    :selected-id (val id))))
                   (footer)))))
        (see-other (notfound)))))
