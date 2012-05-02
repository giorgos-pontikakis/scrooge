(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass city-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(city-id))
   (payload-parameter-names
    :allocation :class
    :initform '(title))
   (filter-parameter-names
    :allocation :class
    :initform '(search))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))
   (messages
    :allocation :class
    :reader messages
    :initform '((title (:city-title-null "Το όνομα πόλης είναι κενό."
                        :city-title-exists "Αυτό το όνομα πόλης υπάρχει ήδη."))))))



;;; ------------------------------------------------------------
;;; Validation
;;; ------------------------------------------------------------

(defun city-referenced-p (city-id)
  (with-db ()
    (and city-id
         (query (:select 'id
                 :from 'company
                 :where (:= 'city-id city-id))
                :column))))

(define-existence-predicate city-id-exists-p city id)
(define-existence-predicate* city-title-exists-p city title id)

(defun chk-city-id (city-id)
  (if (city-id-exists-p city-id)
      nil
      :city-id-unknown))

(defun chk-city-id/ref (city-id)
  (if (and (null (chk-city-id city-id))
           (null (city-referenced-p city-id)))
      nil
      :city-referenced))

(defun chk-city-title/create (title)
  (cond ((eql :null title) :city-title-null)
        ((city-title-exists-p title) :city-title-exists)
        (t nil)))

(defun chk-city-title/update (title city-id)
  (cond ((eql :null title) :city-title-null)
        ((city-title-exists-p title city-id) :city-title-exists)
        (t nil)))

(defun chk-city-title (title)
  (if (or (eql :null title)
          (city-title-exists-p title))
      nil
      :city-title-unknown))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun city-top-actions (city-id filter)
  (top-actions
   (make-instance 'menu
                  :spec `((create ,(html ()
                                     (:a :href (apply #'config/city/create filter)
                                         (:img :src "/scrooge/img/add.png")
                                         (str "Νέα πόλη")))))
                  :css-class "hmenu")
   (searchbox #'config/city
              #'(lambda (&rest args)
                  (apply #'config/city :city-id city-id args))
              filter
              "ac-city")))

(defun city-actions (op city-id filter)
  (actions-menu (make-menu-spec
                 (action-anchors/crud (apply #'config/city/update :city-id city-id filter)
                                      (if (or (null city-id)
                                              (city-referenced-p city-id))
                                          nil
                                          (apply #'config/city/delete :city-id city-id filter))))
                (enabled-actions/crud op city-id)))



;;; ------------------------------------------------------------
;;; City table
;;; ------------------------------------------------------------

;;; table

(defclass city-table (config-table)
  ((header-labels :initform '("" "Ονομασία πόλης" "" ""))
   (paginator     :initform (make-instance 'city-paginator
                                            :id "city-paginator"
                                            :css-class "paginator"))
   (key-name      :initform :city-id))
  (:default-initargs :id "config-table"
                     :item-class 'city-row))

(defmethod get-records ((table city-table))
  (config-data 'city (getf (filter table) :search)))


;;; rows

(defclass city-row (config-row)
  ((record-class :allocation :class :initform 'city)))

(defmethod selector ((row city-row) enabled-p)
  (simple-selector row enabled-p #'config/city))

(defmethod controls ((row city-row) enabled-p)
  (simple-controls row enabled-p #'config/city))


;;; paginator

(defclass city-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg city-paginator) start)
  (apply #'config/city :start start (filter (table pg))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage city-page config/city ("config/city")
  ((city-id integer chk-city-id)
   (search  string)
   (start   integer))
  (with-view-page
      (let* ((op :catalogue)
             (filter (params->filter))
             (city-table (make-instance 'city-table
                                        :op op
                                        :filter filter
                                        :start-index (val start))))
        (with-document ()
          (:head
           (:title "Πόλεις » Κατάλογος")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'city)
                 (city-top-actions (val city-id) filter)
                 (:div :class "grid_12"
                       (:div :id "city-window" :class "window"
                             (:div :class "title" "Κατάλογος")
                             (city-actions op (val city-id) filter)
                             (display city-table :key (val city-id))))
                 (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage city-page config/city/create ("config/city/create")
    ((title  string chk-city-title/create)
     (search string))
  (with-view-page
    (let* ((op :create)
           (filter (params->filter))
           (city-table (make-instance 'city-table
                                      :op op
                                      :filter filter)))
      (with-document ()
        (:head
         (:title "Πόλη » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'city)
               (city-top-actions nil filter)
               (:div :class "grid_12"
                     (:div :id "city-window" :class "window"
                           (:div :class "title" "Δημιουργία")
                           (city-actions op nil filter)
                           (notifications)
                           (with-form (actions/config/city/create :search (val search))
                             (display city-table
                                      :payload (params->payload)))))
               (footer)))))))

(defpage city-page actions/config/city/create ("actions/config/city/create" :request-type :post)
  ((title  string chk-city-title/create t)
   (search string))
  (with-controller-page (config/city/create)
    (let ((new-city (make-instance 'city :title (val title))))
      (insert-dao new-city)
      (see-other (config/city :city-id (city-id new-city))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage city-page config/city/update ("config/city/update")
  ((city-id integer chk-city-id                           t)
   (title   string  (chk-city-title/update title city-id))
   (search  string))
  (with-view-page
      (let* ((op :update)
             (filter (params->filter))
             (city-table (make-instance 'city-table
                                        :op op
                                        :filter filter)))
        (with-document ()
          (:head
           (:title "Πόλη » Επεξεργασία")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'city)
                 (city-top-actions (val city-id) filter)
                 (:div :class "grid_12"
                       (:div :id "city-window" :class "window"
                             (:div :class "title" "Επεξεργασία")
                             (city-actions op (val city-id) filter)
                             (notifications)
                             (with-form (actions/config/city/update :city-id (val city-id)
                                                                    :search (val search))
                               (display city-table
                                        :key (val city-id)
                                        :payload (params->payload)))))
                 (footer)))))))

(defpage city-page actions/config/city/update ("actions/config/city/update" :request-type :post)
  ((city-id integer chk-city-id                           t)
   (title   string  (chk-city-title/update title city-id) t)
   (search  string))
  (with-controller-page (config/city/update)
    (execute (:update 'city :set
                      'title (val title)
                      :where (:= 'id (val city-id))))
    (see-other (config/city :city-id (val city-id) :search (val search)))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage city-page config/city/delete ("config/city/delete")
  ((city-id integer chk-city-id/ref t)
   (search  string))
  (with-view-page
      (let* ((op :delete)
             (filter (params->filter))
             (city-table (make-instance 'city-table
                                        :op op
                                        :filter filter)))
        (with-document ()
          (:head
           (:title "Πόλη » Διαγραφή")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'city)
                 (city-top-actions (val city-id) filter)
                 (:div :class "grid_12"
                       (:div :id "city-window" :class "window"
                             (:div :class "title" "Διαγραφή")
                             (city-actions op (val city-id) filter)
                             (with-form (actions/config/city/delete :city-id (val city-id)
                                                                    :search (val search))
                               (display city-table
                                        :key (val city-id)))))
                 (footer)))))))

(defpage city-page actions/config/city/delete ("actions/config/city/delete" :request-type :post)
  ((city-id integer chk-city-id/ref t)
   (search  string))
  (with-controller-page (config/city/delete)
    (delete-dao (get-dao 'city (val city-id)))
    (see-other (config/city :search (val search)))))