(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass bank-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
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
;;; Validation
;;; ------------------------------------------------------------

(defun bank-referenced-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                         :from 'cheque
                         :where (:= 'bank-id id))
                :column))))

(define-existence-predicate bank-id-exists-p bank id)
(define-existence-predicate bank-title-exists-p bank title)
(define-uniqueness-predicate bank-title-unique-p bank title id)

(defun chk-bank-id (id)
  (if (bank-id-exists-p id)
      nil
      :bank-id-unknown))

(defun chk-bank-id/ref (id)
  (if (and (null (chk-bank-id id))
           (null (bank-referenced-p id)))
      nil
      :bank-referenced))

(defun chk-new-bank-title (title &optional id)
  (cond ((eql :null title) :bank-title-null)
        ((not (bank-title-unique-p title id)) :bank-title-exists)
        (t nil)))

(defun chk-bank-title (title)
  (if (or (eql :null title)
          (bank-title-exists-p title))
      nil
      :bank-title-unknown))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun bank-menu (id filter &optional disabled)
  (anchor-menu (crud-actions-spec (apply #'bank :id id filter)
                                  (apply #'bank/create filter)
                                  (apply #'bank/update :id id filter)
                                  (if (or (null id)
                                          (bank-referenced-p id))
                                      nil
                                      (apply #'bank/delete :id id filter)))
               :id "bank-actions"
               :css-class "hmenu actions"
               :disabled disabled))

(defun bank-notifications ()
  (notifications '((title (:bank-title-null "Το όνομα τράπεζας είναι κενό."
                           :bank-title-exists "Αυτό το όνομα τράπεζας υπάρχει ήδη.")))))



;;; ------------------------------------------------------------
;;; Bank table
;;; ------------------------------------------------------------

;;; table

(defclass bank-table (crud-table/dao)
  ((header-labels  :initform '("" "Ονομασία τράπεζας" "" ""))
   (paginator      :initform (make-instance 'bank-paginator
                                            :id "bank-paginator"
                                            :css-class "paginator")))
  (:default-initargs :id "config-table" :item-class 'bank-row))

(defmethod read-records ((table bank-table))
  (config-data 'bank (getf (filter table) :search)))


;;; rows

(defclass bank-row (config-row)
  ())

(defmethod selector ((row bank-row) enabled-p)
  (simple-selector row enabled-p #'bank))

(defmethod controls ((row bank-row) enabled-p)
  (simple-controls row enabled-p #'bank))


;;; paginator

(defclass bank-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg bank-paginator) start)
  (apply #'bank :start start (filter (table pg))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage bank-page bank ("config/bank")
    ((id     integer chk-bank-id)
     (search string)
     (start  integer))
  (with-view-page
    (let* ((filter (params->plist (list search)))
           (bank-table (make-instance 'bank-table
                                      :op :read
                                      :filter filter
                                      :start-index (val start))))
      (with-document ()
        (:head
         (:title "Τράπεζες")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'bank)
               (:div :id "bank-window" :class "window grid_10"
                     (:div :class "title" "Τράπεζες » Κατάλογος")
                     (bank-menu (val id)
                                filter
                                (if (val id)
                                    '(:read)
                                    '(:read :update :delete)))
                     (display bank-table
                              :selected-id (val id)))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (bank) (val search)))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage bank-page bank/create ("config/bank/create")
    ((title  string chk-new-bank-title)
     (search string))
  (with-view-page
    (let* ((filter (params->plist (list search)))
           (bank-table (make-instance 'bank-table
                                      :op :create
                                      :filter filter)))
      (with-document ()
        (:head
         (:title "Τράπεζα » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'bank)
               (:div :id "bank-window" :class "window grid_10"
                     (:div :class "title" "Τράπεζα » Δημιουργία")
                     (bank-menu nil
                                filter
                                '(:create :update :delete))
                     (with-form (actions/config/bank/create :search (val search))
                       (display bank-table
                                :selected-id nil
                                :selected-data (list :title (val title)))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (bank) (val search))
                     (bank-notifications))
               (footer)))))))

(defpage bank-page actions/config/bank/create ("actions/config/bank/create" :request-type :post)
    ((title  string chk-new-bank-title t)
     (search string))
  (with-controller-page (bank/create)
    (insert-dao (make-instance 'bank :title (val title)))
    (see-other (bank :id (bank-id (val title))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage bank-page bank/update ("config/bank/update")
    ((id     integer chk-bank-id t)
     (title  string  (chk-new-bank-title title id))
     (search string))
  (with-view-page
    (let* ((filter (params->plist (list search)))
           (bank-table (make-instance 'bank-table
                                      :op :update
                                      :filter filter)))
      (with-document ()
        (:head
         (:title "Τράπεζα » Επεξεργασία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'bank)
               (:div :id "bank-window" :class "window grid_10"
                     (:div :class "title" "Τράπεζα » Επεξεργασία")
                     (bank-menu (val id)
                                filter
                                '(:create :update))
                     (with-form (actions/config/bank/update :id (val id)
                                                            :search (val search))
                       (display bank-table
                                :selected-id (val id)
                                :selected-data (list :title (val title)))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (bank) (val search))
                     (bank-notifications))
               (footer)))))))

(defpage bank-page actions/config/bank/update ("actions/config/bank/update" :request-type :post)
    ((id     integer chk-bank-id t)
     (title  string  (chk-new-bank-title title id) t)
     (search string))
  (with-controller-page (bank/update)
    (execute (:update 'bank :set
                      'title (val title)
                      :where (:= 'id (val id))))
    (see-other (bank :id (val id) :search (val search)))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage bank-page bank/delete ("config/bank/delete")
    ((id     integer chk-bank-id/ref t)
     (search string))
  (with-view-page
    (let* ((filter (params->plist (list search)))
           (bank-table (make-instance 'bank-table
                                      :op :delete
                                      :filter filter)))
      (with-document ()
        (:head
         (:title "Τράπεζα » Διαγραφή")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'bank)
               (:div :id "bank-window" :class "window grid_10"
                     (:div :class "title" "Τράπεζα » Διαγραφή")
                     (bank-menu (val id)
                                filter
                                '(:create :delete))
                     (with-form (actions/config/bank/delete :id (val id)
                                                            :search (val search))
                       (display bank-table
                                :selected-id (val id))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (bank) (val search)))
               (footer)))))))

(defpage bank-page actions/config/bank/delete ("actions/config/bank/delete" :request-type :post)
    ((id     integer chk-bank-id/ref t)
     (search string))
  (with-controller-page (bank/delete)
    (delete-dao (get-dao 'bank (val id)))
    (see-other (bank :search (val search)))))
