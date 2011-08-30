(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass city-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(city-id))
   (user-parameter-names
    :allocation :class
    :initform '(title))
   (filter-parameter-names
    :allocation :class
    :initform '(search))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))))



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
(define-existence-predicate* city-title-exists-p city title id)

(defun chk-city-id (id)
  (if (city-id-exists-p id)
      nil
      :city-id-unknown))

(defun chk-city-id/ref (id)
  (if (and (null (chk-city-id id))
           (null (city-referenced-p id)))
      nil
      :city-referenced))

(defun chk-city-title/create (title)
  (cond ((eql :null title) :city-title-null)
        ((city-title-exists-p title) :city-title-exists)
        (t nil)))

(defun chk-city-title/update (title id)
  (cond ((eql :null title) :city-title-null)
        ((city-title-exists-p title id) :city-title-exists)
        (t nil)))

(defun chk-city-title (title)
  (if (or (eql :null title)
          (city-title-exists-p title))
      nil
      :city-title-unknown))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun city-menu (id filter &optional disabled)
  (anchor-menu (crud-actions-spec (apply #'city :id id filter)
                                  (apply #'city/create filter)
                                  (apply #'city/update :id id filter)
                                  (if (or (null id)
                                          (city-referenced-p id))
                                      nil
                                      (apply #'city/delete :id id filter)))
               :id "city-actions"
               :css-class "hmenu actions"
               :disabled disabled))

(defun city-notifications ()
  (notifications '((title (:city-title-null "Το όνομα πόλης είναι κενό."
                           :city-title-exists "Αυτό το όνομα πόλης υπάρχει ήδη.")))))



;;; ------------------------------------------------------------
;;; City table
;;; ------------------------------------------------------------

;;; table

(defclass city-table (config-table)
  ((header-labels  :initform '("" "Ονομασία πόλης" "" ""))
   (paginator      :initform (make-instance 'city-paginator
                                            :id "city-paginator"
                                            :css-class "paginator")))
  (:default-initargs :id "config-table"
                     :item-class 'city-row
                     :record-class 'city))

(defmethod read-records ((table city-table))
  (config-data 'city (getf (filter table) :search)))


;;; rows

(defclass city-row (config-row)
  ())

(defmethod selector ((row city-row) enabled-p)
  (simple-selector row enabled-p #'city))

(defmethod controls ((row city-row) enabled-p)
  (simple-controls row enabled-p #'city))


;;; paginator

(defclass city-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg city-paginator) start)
  (apply #'city :start start (filter (table pg))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage city-page city ("config/city")
    ((id     integer chk-city-id)
     (search string)
     (start  integer))
  (with-view-page
    (let* ((filter (params->plist (filter-parameters)))
           (city-table (make-instance 'city-table
                                      :op :read
                                      :filter filter
                                      :start-index (val start))))
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
                     (display city-table :key (val id)))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (city) (val search)))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage city-page city/create ("config/city/create")
    ((title  string chk-city-title/create)
     (search string))
  (with-view-page
    (let* ((filter (params->plist (filter-parameters)))
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
                     (with-form (actions/config/city/create :search (val search))
                       (display city-table
                                :payload (params->plist (user-parameters)))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (city) (val search))
                     (city-notifications))
               (footer)))))))

(defpage city-page actions/config/city/create ("actions/config/city/create" :request-type :post)
    ((title  string chk-city-title/create t)
     (search string))
  (with-controller-page (city/create)
    (let ((new-city (make-instance 'city :title (val title))))
      (insert-dao new-city)
      (see-other (city :id (city-id new-city))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage city-page city/update ("config/city/update")
    ((id     integer chk-city-id                   t)
     (title  string  (chk-city-title/update title id))
     (search string))
  (with-view-page
    (let* ((filter (params->plist (filter-parameters)))
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
                     (with-form (actions/config/city/update :id (val id)
                                                            :search (val search))
                       (display city-table
                                :key (val id)
                                :payload (params->plist (user-parameters)))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (city) (val search))
                     (city-notifications))
               (footer)))))))

(defpage city-page actions/config/city/update ("actions/config/city/update" :request-type :post)
    ((id     integer chk-city-id t)
     (title  string (chk-city-title/update title id) t)
     (search string))
  (with-controller-page (city/update)
    (execute (:update 'city :set
                      'title (val title)
                      :where (:= 'id (val id))))
    (see-other (city :id (val id) :search (val search)))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage city-page city/delete ("config/city/delete")
    ((id integer chk-city-id/ref t)
     (search string))
  (with-view-page
    (let* ((filter (params->plist (filter-parameters)))
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
                                                            :search (val search))
                       (display city-table
                                :key (val id))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (city) (val search)))
               (footer)))))))

(defpage city-page actions/config/city/delete ("actions/config/city/delete" :request-type :post)
    ((id     integer chk-city-id/ref t)
     (search string))
  (with-controller-page (city/delete)
    (delete-dao (get-dao 'city (val id)))
    (see-other (city :search (val search)))))