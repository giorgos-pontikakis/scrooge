(in-package :scrooge)



;;; ------------------------------------------------------------
;;; TOF - Validation
;;; ------------------------------------------------------------

(defun tof-referenced-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                         :from 'company
                         :where (:= 'tof-id id))))))

(define-existence-predicate tof-id-exists-p tof id)
(define-existence-predicate tof-title-exists-p tof title)
(define-uniqueness-predicate tof-title-unique-p tof title id)

(defun chk-tof-id (id)
  (if (tof-id-exists-p id)
      nil
      :tof-id-unknown))

(defun chk-tof-id/ref (id)
  (if (and (null (chk-tof-id id))
           (null (tof-referenced-p id)))
      nil
      :tof-referenced))

(defun chk-new-tof-title (title &optional id)
  (cond ((eql :null title) :tof-title-null)
        ((not (tof-title-unique-p title id)) :tof-title-exists)
        (t nil)))

(defun chk-tof-title (title)
  (if (or (eql :null title)
          (tof-title-exists-p title))
      nil
      :tof-title-unknown))



;;; ------------------------------------------------------------
;;; TOF - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/config/tof/create ("actions/config/tof/create" :request-type :post)
    ((title  string chk-new-tof-title t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (insert-dao (make-instance 'tof :title (val title)))
          (see-other (tof :id (tof-id (val title)))))
        (see-other (tof/create :title (raw title) :search (raw search))))))

(define-dynamic-page actions/config/tof/update ("actions/config/tof/update" :request-type :post)
    ((id     integer chk-tof-id t)
     (title  string (chk-new-tof-title title id) t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (execute (:update 'tof :set
                            'title (val title)
                            :where (:= 'id (val id))))
          (see-other (tof :id (val id) :search (val search))))
        (see-other (tof/update :id (raw id) :title (raw title) :search (raw search))))))

(define-dynamic-page actions/config/tof/delete ("actions/config/tof/delete" :request-type :post)
    ((id     integer chk-tof-id/ref t)
     (search string))
  (with-auth ("configuration")
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'tof (val id)))
          (see-other (tof :search (val search))))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun tof-menu (id search &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "tof-actions"
                          :style "hnavbar actions"
                          :spec (crud-actions-spec (tof :id id
                                                        :search search)
                                                   (tof/create :search search)
                                                   (tof/update :id id
                                                               :search search)
                                                   (if (or (null id)
                                                           (tof-referenced-p id))
                                                       nil
                                                       (tof/delete :id id
                                                                   :search search))))
           :disabled-items disabled-items))

(defun tof-notifications ()
  (notifications '((title (:tof-title-null "Το όνομα της Δ.Ο.Υ. είναι κενό."
                           :tof-title-exists "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη.")))))



;;; ------------------------------------------------------------
;;; TOF table
;;; ------------------------------------------------------------

;;; table

(defclass tof-table (crud-table)
  ((item-key-field :initform :id)
   (header-labels  :initform '("" "Ονομασία Δ.Ο.Υ." "" ""))
   (paginator      :initform (make-instance 'paginator
                                           :id "tof-paginator"
                                           :style "paginator"
                                           :delta 10
                                           :urlfn (lambda (search start)
                                                    (tof :search search
                                                         :start start)))))
  (:default-initargs :id "config-table" :item-class 'tof-row))


(defmethod read-records ((table tof-table))
  (config-data 'tof (filter table)))


;; rows

(defclass tof-row (crud-row)
  ())

(defmethod cells ((row tof-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (search (filter (collection row))))
    (list :selector (make-instance 'selector-cell
                                   :states (list :on (tof :search search
                                                          :start (page-start pg (index row) start))
                                                 :off (tof :search search
                                                           :id id)))
          :payload (make-instance 'textbox-cell
                                  :name 'title
                                  :value (getf record :title))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (tof :id id :search search))))))



;;; ------------------------------------------------------------
;;; TOF - Pages
;;; ------------------------------------------------------------

(define-dynamic-page tof ("config/tof")
    ((id     integer chk-tof-id)
     (search string)
     (start  integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((tof-table (make-instance 'tof-table
                                        :op 'catalogue
                                        :filter (val* search))))
          (with-document ()
            (:head
             (:title "Δ.Ο.Υ.")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'tof)
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (tof) (val search)))
                   (:div :id "tof-window" :class "window grid_9"
                         (:div :class "title" "Δ.Ο.Υ. » Κατάλογος")
                         (tof-menu (val id)
                                   (val search)
                                   (if (val id)
                                       '(catalogue)
                                       '(catalogue update delete)))
                         (display tof-table
                                  :selected-id (val* id)
                                  :start (val* start)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page tof/create ("config/tof/create")
    ((title  string chk-new-tof-title)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (let ((tof-table (make-instance 'tof-table
                                    :op 'create
                                    :filter (val* search))))
      (with-document ()
        (:head
         (:title "Δ.Ο.Υ. » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'tof)
               (:div :id "sidebar" :class "sidebar grid_3"
                     (:p :class "title" "Φίλτρα")
                     (searchbox (tof) (val search))
                     (tof-notifications))
               (:div :id "tof-window" :class "window grid_9"
                     (:div :class "title" "Δ.Ο.Υ. » Δημιουργία")
                     (tof-menu nil
                               (val search)
                               '(create update delete))
                     (with-form (actions/config/tof/create :search (val* search))
                       (display tof-table
                                :selected-id nil
                                :selected-data (list :title (val* title)))))
               (footer)))))))

(define-dynamic-page tof/update ("config/tof/update")
    ((id     integer chk-tof-id                   t)
     (title  string  (chk-new-tof-title title id))
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((tof-table (make-instance 'tof-table
                                        :op 'update
                                        :filter (val* search))))
          (with-document ()
            (:head
             (:title "Δ.Ο.Υ. » Επεξεργασία")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'tof)
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (tof) (val search))
                         (tof-notifications))
                   (:div :id "tof-window" :class "window grid_9"
                         (:div :class "title" "Δ.Ο.Υ. » Επεξεργασία")
                         (tof-menu (val id)
                                   (val search)
                                   '(create update))
                         (with-form (actions/config/tof/update :id (val* id)
                                                               :filter (val* search))
                           (display tof-table
                                    :selected-id (val id)
                                    :selected-data (list :title (val* title)))))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page tof/delete ("config/tof/delete")
    ((id     integer chk-tof-id/ref t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((tof-table (make-instance 'tof-table
                                        :op 'delete
                                        :filter (val* search))))
          (with-document ()
            (:head
             (:title "Δ.Ο.Υ. » Διαγραφή")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'tof)
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (tof) (val search)))
                   (:div :id "tof-window" :class "window grid_9"
                         (:div :class "title" "Δ.Ο.Υ. » Διαγραφή")
                         (tof-menu (val id)
                                   (val search)
                                   '(create delete))
                         (with-form (actions/config/tof/delete :id (val id)
                                                               :search (val* search))
                           (display tof-table
                                    :selected-id (val id))))
                   (footer)))))
        (see-other (notfound)))))
