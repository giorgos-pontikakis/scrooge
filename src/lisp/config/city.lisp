(in-package :scrooge)



;;; ------------------------------------------------------------
;;; City - Validation
;;; ------------------------------------------------------------

(defun city-referenced-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                         :from 'company
                         :where (:= 'city-id id))
                :column))))

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
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (insert-dao (make-instance 'city :title (val title)))
          (see-other (city :id (city-id (val title)))))
        (see-other (city/create :title (raw title) :search (raw search))))))

(define-dynamic-page actions/config/city/update ("actions/config/city/update" :request-type :post)
    ((id     integer chk-city-id t)
     (title  string (chk-new-city-title title id) t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (execute (:update 'city :set
                            'title (val title)
                            :where (:= 'id (val id))))
          (see-other (city :id (val id) :search (val search))))
        (see-other (city/update :id (raw id) :title (raw title) :search (raw search))))))

(define-dynamic-page actions/config/city/delete ("actions/config/city/delete" :request-type :post)
    ((id     integer chk-city-id/ref t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'city (val id)))
          (see-other (city :search (val search))))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun city-menu (id filter &optional disabled)
  (menu (crud-actions-spec (apply #'city :id id filter)
                           (apply #'city/create filter)
                           (apply #'city/update :id id filter)
                           (if (or (null id)
                                   (city-referenced-p id))
                               nil
                               (apply #'city/delete :id id filter)))
        :id "city-actions"
        :disabled disabled))

(defun city-notifications ()
  (notifications '((title (:city-title-null "Το όνομα πόλης είναι κενό."
                           :city-title-exists "Αυτό το όνομα πόλης υπάρχει ήδη.")))))



;;; ------------------------------------------------------------
;;; City table
;;; ------------------------------------------------------------

;;; table

(defclass city-table (scrooge-crud-table)
  ((item-key-field :initform :id)
   (header-labels  :initform '("" "Ονομασία πόλης" "" ""))
   (paginator      :initform (make-instance 'scrooge-paginator
                                            :id "city-paginator"
                                            :style "paginator"
                                            :urlfn #'city)))
  (:default-initargs :id "config-table" :item-class 'city-row))

(defmethod read-records ((table city-table))
  (config-data 'city (getf (filter table) :search)))


;;; rows

(defclass city-row (config-row)
  ())

(define-selector city-row city)
(define-controls city-row city)



;;; ------------------------------------------------------------
;;; City - Pages
;;; ------------------------------------------------------------

(define-dynamic-page city ("config/city")
    ((id     integer chk-city-id)
     (search string)
     (start  integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search))
               (city-table (make-instance 'city-table
                                          :op :read
                                          :filter filter)))
          (with-document ()
            (:head
             (:title "Πόλεις")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'city)
                   (:div :id "city-window" :class "window grid_10"
                         (:div :class "title" "Πόλεις » Κατάλογος")
                         (city-menu (val id)
                                    filter
                                    (if (val id)
                                        '(:read)
                                        '(:read :update :delete)))
                         (display city-table
                                  :selected-id (val* id)
                                  :start (val* start)))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (city) (val search)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page city/create ("config/city/create")
    ((title  string chk-new-city-title)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (let* ((filter (parameters->plist search))
           (city-table (make-instance 'city-table
                                      :op :create
                                      :filter filter)))
      (with-document ()
        (:head
         (:title "Πόλη » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'city)
               (:div :id "city-window" :class "window grid_10"
                     (:div :class "title" "Πόλη » Δημιουργία")
                     (city-menu nil
                                filter
                                '(:create :update :delete))
                     (with-form (actions/config/city/create :search (val* search))
                       (display city-table
                                :selected-id nil
                                :selected-data (list :title (val* title)))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (city) (val search))
                     (city-notifications))
               (footer)))))))

(define-dynamic-page city/update ("config/city/update")
    ((id     integer chk-city-id                   t)
     (title  string  (chk-new-city-title title id))
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search))
               (city-table (make-instance 'city-table
                                          :op :update
                                          :filter filter)))
          (with-document ()
            (:head
             (:title "Πόλη » Επεξεργασία")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'city)
                   (:div :id "city-window" :class "window grid_10"
                         (:div :class "title" "Πόλη » Επεξεργασία")
                         (city-menu (val id)
                                    filter
                                    '(:create :update))
                         (with-form (actions/config/city/update :id (val* id)
                                                                :search (val* search))
                           (display city-table
                                    :selected-id (val id)
                                    :selected-data (list :title (val* title)))))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (city) (val search))
                         (city-notifications))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page city/delete ("config/city/delete")
    ((id integer chk-city-id/ref t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search))
               (city-table (make-instance 'city-table
                                          :op :delete
                                          :filter filter)))
          (with-document ()
            (:head
             (:title "Πόλη » Διαγραφή")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'city)
                   (:div :id "city-window" :class "window grid_10"
                         (:div :class "title" "Πόλη » Διαγραφή")
                         (city-menu (val id)
                                    filter
                                    '(:create :delete))
                         (with-form (actions/config/city/delete :id (val id)
                                                                :search (val* search))
                           (display city-table
                                    :selected-id (val id))))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (city) (val search)))
                   (footer)))))
        (see-other (notfound)))))
