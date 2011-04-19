(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Bank - Validation
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
;;; Bank - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/config/bank/create ("actions/config/bank/create" :request-type :post)
    ((title  string chk-new-bank-title t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (insert-dao (make-instance 'bank :title (val title)))
          (see-other (bank :id (bank-id (val title)))))
        (see-other (bank/create :title (raw title) :search (raw search))))))

(define-dynamic-page actions/config/bank/update ("actions/config/bank/update" :request-type :post)
    ((id     integer chk-bank-id t)
     (title  string  (chk-new-bank-title title id) t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (execute (:update 'bank :set
                            'title (val title)
                            :where (:= 'id (val id))))
          (see-other (bank :id (val id) :search (val search))))
        (see-other (bank/update :id (raw id) :title (raw title) :search (raw search))))))

(define-dynamic-page actions/config/bank/delete ("actions/config/bank/delete" :request-type :post)
    ((id     integer chk-bank-id/ref t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'bank (val id)))
          (see-other (bank :search (val search))))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun bank-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "bank-actions"
                          :spec (crud-actions-spec (apply #'bank :id id filter)
                                                   (apply #'bank/create filter)
                                                   (apply #'bank/update :id id filter)
                                                   (if (or (null id)
                                                           (bank-referenced-p id))
                                                       nil
                                                       (apply #'bank/delete :id id filter)))
                          :disabled disabled-items)))

(defun bank-notifications ()
  (notifications '((title (:bank-title-null "Το όνομα τράπεζας είναι κενό."
                           :bank-title-exists "Αυτό το όνομα τράπεζας υπάρχει ήδη.")))))



;;; ------------------------------------------------------------
;;; Bank table
;;; ------------------------------------------------------------

;;; table

(defclass bank-table (scrooge-crud-table)
  ((item-key-field :initform :id)
   (header-labels  :initform '("" "Ονομασία τράπεζας" "" ""))
   (paginator      :initform (make-instance 'default-paginator
                                           :id "bank-paginator"
                                           :style "paginator"
                                           :urlfn #'bank)))
  (:default-initargs :id "config-table" :item-class 'bank-row))

(defmethod read-records ((table bank-table))
  (config-data 'bank (getf (filter table) :search)))


;;; rows

(defclass bank-row (config-row)
  ())

(define-selector bank-row bank)
(define-controls bank-row bank)



;;; ------------------------------------------------------------
;;; Bank - Pages
;;; ------------------------------------------------------------

(define-dynamic-page bank ("config/bank")
    ((id     integer chk-bank-id)
     (search string)
     (start  integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search))
               (bank-table (make-instance 'bank-table
                                          :op :catalogue
                                          :filter filter
                                          :start (val* start))))
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
                                        '(:catalogue)
                                        '(:catalogue :update :delete)))
                         (display bank-table
                                  :selected-id (val* id)))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (bank) (val search)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page bank/create ("config/bank/create")
    ((title  string chk-new-bank-title)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (let* ((filter (parameters->plist search))
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
                     (with-form (actions/config/bank/create :search (val* search))
                       (display bank-table
                                :selected-id nil
                                :selected-data (list :title (val* title)))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (bank) (val search))
                     (bank-notifications))
               (footer)))))))

(define-dynamic-page bank/update ("config/bank/update")
    ((id     integer chk-bank-id t)
     (title  string  (chk-new-bank-title title id))
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search))
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
                         (with-form (actions/config/bank/update :id (val* id)
                                                                :search (val* search))
                           (display bank-table
                                    :selected-id (val id)
                                    :selected-data (list :title (val* title)))))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (bank) (val search))
                         (bank-notifications))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page bank/delete ("config/bank/delete")
    ((id     integer chk-bank-id/ref t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search))
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
                                                                :search (val* search))
                           (display bank-table
                                    :selected-id (val id))))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (bank) (val search)))
                   (footer)))))
        (see-other (notfound)))))
