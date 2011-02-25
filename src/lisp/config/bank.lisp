(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Bank - Validation
;;; ------------------------------------------------------------

(defun bank-referenced-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                         :from 'cheque
                         :where (:= 'bank-id id))))))

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
  (cond ((eql :null title) :bank-title-null)
        ((not (bank-title-exists-p title)) :bank-title-unknown)
        (t nil)))



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

(defun bank-menu (id search &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "bank-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (crud-actions-spec (bank :id id
                                                         :search search)
                                                   (bank/create :search search)
                                                   (bank/update :id id
                                                                :search search)
                                                   (if (or (null id)
                                                           (chk-bank-id/ref id))
                                                       nil
                                                       (bank/delete :id id
                                                                    :search search))))
           :disabled-items disabled-items))

(defun bank-notifications ()
  (notifications '((title (:bank-title-null "Το όνομα τράπεζας είναι κενό."
                           :bank-title-exists "Αυτό το όνομα τράπεζας υπάρχει ήδη.")))))



;;; ------------------------------------------------------------
;;; Bank table
;;; ------------------------------------------------------------

;;; table

(defclass bank-table (crud-table)
  ((item-key-field :initform :id)
   (header-labels  :initform '("" "Ονομασία τράπεζας" "" ""))
   (paginator      :initform (make-instance 'paginator
                                           :id "bank-paginator"
                                           :style "paginator grid_9 alpha"
                                           :delta 10
                                           :urlfn (lambda (search start)
                                                    (bank :search search
                                                          :start start)))))
  (:default-initargs :id "config-table" :item-class 'bank-row))

(defmethod read-records ((table bank-table))
  (config-data 'bank (filter table)))


;;; rows

(defclass bank-row (crud-row)
  ())

(defmethod cells ((row bank-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (search (filter (collection row))))
    (list :selector (make-instance 'selector-cell
                                   :states (list :on (bank :search search
                                                           :start (page-start pg (index row) start))
                                                 :off (bank :search search
                                                            :id id)))
          :payload (make-instance 'textbox-cell
                                  :name 'title
                                  :value (getf record :title))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (bank :id id :search search))))))



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
        (let ((bank-table (make-instance 'bank-table
                                         :op 'catalogue
                                         :filter (val* search))))
          (with-document ()
            (:head
             (:title "Τράπεζες")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'bank)
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (bank) (val search)))
                   (:div :id "bank-window" :class "window grid_9"
                         (:div :class "title" "Κατάλογος τραπεζών")
                         (bank-menu (val id)
                                    (val search)
                                    (if (val id)
                                        '(catalogue)
                                        '(catalogue update delete)))
                         (display bank-table
                                  :selected-id (val* id)
                                  :start (val* start)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page bank/create ("config/bank/create")
    ((title  string chk-new-bank-title)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (let ((bank-table (make-instance 'bank-table
                                     :op 'create
                                     :filter (val* search))))
      (with-document ()
        (:head
         (:title "Δημιουργία τράπεζας")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'bank)
               (:div :id "sidebar" :class "sidebar grid_3"
                     (:p :class "title" "Φίλτρα")
                     (searchbox (bank) (val search))
                     (bank-notifications))
               (:div :id "bank-window" :class "window grid_9"
                     (:div :class "title" "Δημιουργία τράπεζας")
                     (bank-menu nil
                                (val search)
                                '(create update delete))
                     (with-form (actions/config/bank/create :search (val* search))
                       (display bank-table
                                :selected-id nil
                                :selected-data (list :title (val* title)))))
               (footer)))))))

(define-dynamic-page bank/update ("config/bank/update")
    ((id     integer chk-bank-id t)
     (title  string  (chk-new-bank-title title id))
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((bank-table (make-instance 'bank-table
                                         :op 'update
                                         :filter (val* search))))
          (with-document ()
            (:head
             (:title "Επεξεργασία τράπεζας")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'bank)
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (bank) (val search))
                         (bank-notifications))
                   (:div :id "bank-window" :class "window grid_9"
                         (:div :class "title" "Επεξεργασία τράπεζας")
                         (bank-menu (val id)
                                    (val search)
                                    '(create update))
                         (with-form (actions/config/bank/update :id (val* id)
                                                                :search (val* search))
                           (display bank-table
                                    :selected-id (val id)
                                    :selected-data (list :title (val* title)))))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page bank/delete ("config/bank/delete")
    ((id     integer chk-bank-id/ref t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((bank-table (make-instance 'bank-table
                                         :op 'delete
                                         :filter (val* search))))
          (with-document ()
            (:head
             (:title "Διαγραφή τράπεζας")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'bank)
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (bank) (val search)))
                   (:div :id "bank-window" :class "window grid_9"
                         (:div :class "title" "Διαγραφή τράπεζας")
                         (bank-menu (val id)
                                    (val search)
                                    '(create delete))
                         (with-form (actions/config/bank/delete :id (val id)
                                                                :search (val* search))
                           (display bank-table
                                    :selected-id (val id))))
                   (footer)))))
        (see-other (notfound)))))
