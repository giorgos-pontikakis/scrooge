(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass city-family (config-family)
  ((ac-class :accessor ac-class :initform "ac-city"))
  (:default-initargs :parameter-groups '(:system (city-id)
                                         :payload (title)
                                         :filter (search))))

(defclass city-page (auth-dynamic-page city-family)
  ((messages
    :allocation :class
    :reader messages
    :initform '((title (:city-title-null "Το όνομα πόλης είναι κενό."
                        :city-title-exists "Αυτό το όνομα πόλης υπάρχει ήδη."))))))

(defun city-top-actions (op)
  (top-actions-area
   (make-instance 'scrooge-menu
                  :spec (make-menu-spec `(:create  (,(family-url 'config/city/create) "Νέα πόλη")))
                  :css-class "hmenu"
                  :disabled (list op))
   (searchbox (family-url-fn 'config/city)
              (family-url-fn 'config/city :system)
              (family-params 'config/city :filter)
              "ac-city")))



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
  (cond ((chk-city-id city-id))
        ((city-referenced-p city-id) :city-referenced)))

(defun chk-city-title/create (title)
  (cond ((eql :null title) :city-title-null)
        ((city-title-exists-p title) :city-title-exists)))

(defun chk-city-title/update (title city-id)
  (cond ((eql :null title) :city-title-null)
        ((city-title-exists-p title city-id) :city-title-exists)))

(defun chk-city-title (title)
  (if (or (eql :null title)
          (city-title-exists-p title))
      nil
      :city-title-unknown))



;;; ------------------------------------------------------------
;;; City table
;;; ------------------------------------------------------------

;;; table

(defclass city-table (config-table city-family)
  ((header-labels :initform '("" "Ονομασία πόλης" "" ""))
   (paginator     :initform (make-instance 'city-paginator
                                           :id "city-paginator"
                                           :css-class "paginator")))
  (:default-initargs :id "config-table"
                     :item-class 'city-row))

(defmethod get-records ((table city-table))
  (config-data 'city (getf (filter table) :search)))

(defmethod actions ((tbl city-table) &key)
  (let* ((city-id (selected-key tbl))
         (filter (filter tbl))
         (hrefs (if city-id
                    (list :update (apply #'config/city/update :city-id city-id filter)
                          :delete (if (chk-city-id/ref city-id)
                                      nil
                                      (apply #'config/city/delete :city-id city-id filter)))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))


;;; rows

(defclass city-row (config-row)
  ((record-class :allocation :class :initform 'city)))

(defmethod selector ((row city-row) enabled-p)
  (simple-selector row enabled-p #'config/city :city-id))

(defmethod controls ((row city-row) enabled-p)
  (simple-controls row enabled-p #'config/city :city-id))


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
    (let* ((filter (params->filter))
           (city-table (make-instance 'city-table
                                      :op :catalogue
                                      :selected-key (val city-id)
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
            (city-top-actions :catalogue)
            (:div :class "grid_12"
              (:div :id "city-window" :class "window"
                (:div :class "title" "Κατάλογος")
                (actions city-table)
                (display city-table)))
            (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage city-page config/city/create ("config/city/create")
    ((title  string chk-city-title/create)
     (search string))
  (with-view-page
    (let* ((filter (params->filter))
           (city-table (make-instance 'city-table
                                      :op :create
                                      :filter filter))
           (payload (params->payload)))
      (with-document ()
        (:head
          (:title "Πόλη » Δημιουργία")
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'city)
            (city-top-actions :create)
            (:div :class "grid_12"
              (:div :id "city-window" :class "window"
                (:div :class "title" "Δημιουργία")
                (actions city-table)
                (notifications)
                (with-form (actions/config/city/create :search (val search))
                  (display city-table
                           :payload payload))))
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
    (let* ((filter (params->filter))
           (city-table (make-instance 'city-table
                                      :op :update
                                      :selected-key (val city-id)
                                      :filter filter)))
      (with-document ()
        (:head
          (:title "Πόλη » Επεξεργασία")
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'city)
            (city-top-actions :update)
            (:div :class "grid_12"
              (:div :id "city-window" :class "window"
                (:div :class "title" "Επεξεργασία")
                (actions city-table)
                (notifications)
                (with-form (actions/config/city/update :city-id (val city-id)
                                                       :search (val search))
                  (display city-table
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
    (let* ((filter (params->filter))
           (city-table (make-instance 'city-table
                                      :op :delete
                                      :selected-key (val city-id)
                                      :filter filter)))
      (with-document ()
        (:head
          (:title "Πόλη » Διαγραφή")
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'city)
            (city-top-actions :delete)
            (:div :class "grid_12"
              (:div :id "city-window" :class "window"
                (:div :class "title" "Διαγραφή")
                (actions city-table)
                (with-form (actions/config/city/delete :city-id (val city-id)
                                                       :search (val search))
                  (display city-table))))
            (footer)))))))

(defpage city-page actions/config/city/delete ("actions/config/city/delete" :request-type :post)
    ((city-id integer chk-city-id/ref t)
     (search  string))
  (with-controller-page (config/city/delete)
    (delete-dao (get-dao 'city (val city-id)))
    (see-other (config/city :search (val search)))))
