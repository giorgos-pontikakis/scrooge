(in-package :scrooge)



;;; ------------------------------------------------------------
;;; TOF - Validation
;;; ------------------------------------------------------------

(defun tof-referenced-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                         :from 'company
                         :where (:= 'tof-id id))
                :column))))

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
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'tof (val id)))
          (see-other (tof :search (val search))))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun tof-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "tof-actions"
                          :spec (crud-actions-spec (apply #'tof :id id filter)
                                                   (apply #'tof/create filter)
                                                   (apply #'tof/update :id id filter)
                                                   (if (or (null id)
                                                           (tof-referenced-p id))
                                                       nil
                                                       (apply #'tof/delete :id id filter)))
                          :disabled-items disabled-items)))

(defun tof-notifications ()
  (notifications '((title (:tof-title-null "Το όνομα της Δ.Ο.Υ. είναι κενό."
                           :tof-title-exists "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη.")))))



;;; ------------------------------------------------------------
;;; TOF table
;;; ------------------------------------------------------------

;;; table

(defclass tof-table (scrooge-crud-table)
  ((item-key-field :initform :id)
   (header-labels  :initform '("" "Ονομασία Δ.Ο.Υ." "" ""))
   (paginator      :initform (make-instance 'default-paginator
                                           :id "tof-paginator"
                                           :style "paginator"
                                           :urlfn #'tof)))
  (:default-initargs :id "config-table" :item-class 'tof-row))

(defmethod read-records ((table tof-table))
  (config-data 'tof (getf (filter table) :search)))


;; rows

(defclass tof-row (config-row)
  ())

(define-selector tof-row tof)
(define-controls tof-row tof)



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
        (let* ((filter (parameters->plist search))
               (tof-table (make-instance 'tof-table
                                         :op :catalogue
                                         :filter filter)))
          (with-document ()
            (:head
             (:title "Δ.Ο.Υ.")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'tof)
                   (:div :id "tof-window" :class "window grid_10"
                         (:div :class "title" "Δ.Ο.Υ. » Κατάλογος")
                         (tof-menu (val id)
                                   filter
                                   (if (val id)
                                       '(:catalogue)
                                       '(:catalogue :update :delete)))
                         (display tof-table
                                  :selected-id (val* id)
                                  :start (val* start)))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (tof) (val search)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page tof/create ("config/tof/create")
    ((title  string chk-new-tof-title)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (let* ((filter (parameters->plist search))
           (tof-table (make-instance 'tof-table
                                     :op :create
                                     :filter filter)))
      (with-document ()
        (:head
         (:title "Δ.Ο.Υ. » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'tof)
               (:div :id "tof-window" :class "window grid_10"
                     (:div :class "title" "Δ.Ο.Υ. » Δημιουργία")
                     (tof-menu nil
                               filter
                               '(:create :update :delete))
                     (with-form (actions/config/tof/create :search (val* search))
                       (display tof-table
                                :selected-id nil
                                :selected-data (list :title (val* title)))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (tof) (val search))
                     (tof-notifications))
               (footer)))))))

(define-dynamic-page tof/update ("config/tof/update")
    ((id     integer chk-tof-id                   t)
     (title  string  (chk-new-tof-title title id))
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search))
               (tof-table (make-instance 'tof-table
                                         :op :update
                                         :filter filter)))
          (with-document ()
            (:head
             (:title "Δ.Ο.Υ. » Επεξεργασία")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'tof)
                   (:div :id "tof-window" :class "window grid_10"
                         (:div :class "title" "Δ.Ο.Υ. » Επεξεργασία")
                         (tof-menu (val id)
                                   filter
                                   '(:create :update))
                         (with-form (actions/config/tof/update :id (val* id)
                                                               :filter (val* search))
                           (display tof-table
                                    :selected-id (val id)
                                    :selected-data (list :title (val* title)))))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (tof) (val search))
                         (tof-notifications))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page tof/delete ("config/tof/delete")
    ((id     integer chk-tof-id/ref t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search))
               (tof-table (make-instance 'tof-table
                                         :op :delete
                                         :filter filter)))
          (with-document ()
            (:head
             (:title "Δ.Ο.Υ. » Διαγραφή")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'tof)
                   (:div :id "tof-window" :class "window grid_10"
                         (:div :class "title" "Δ.Ο.Υ. » Διαγραφή")
                         (tof-menu (val id)
                                   filter
                                   '(:create :delete))
                         (with-form (actions/config/tof/delete :id (val id)
                                                               :search (val* search))
                           (display tof-table
                                    :selected-id (val id))))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (tof) (val search)))
                   (footer)))))
        (see-other (notfound)))))
