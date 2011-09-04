(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass tof-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(tof-id))
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
    :initform '((title (:tof-title-null "Το όνομα της Δ.Ο.Υ. είναι κενό."
                        :tof-title-exists "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη."))))))



;;; ------------------------------------------------------------
;;; Validation
;;; ------------------------------------------------------------

(defun tof-referenced-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                 :from 'company
                 :where (:= 'tof-id id))
                :column))))

(define-existence-predicate tof-id-exists-p tof id)
(define-existence-predicate* tof-title-exists-p tof title id)

(defun chk-tof-id (id)
  (if (tof-id-exists-p id)
      nil
      :tof-id-unknown))

(defun chk-tof-id/ref (id)
  (if (and (null (chk-tof-id id))
           (null (tof-referenced-p id)))
      nil
      :tof-referenced))

(defun chk-tof-title/create (title)
  (cond ((eql :null title) :tof-title-null)
        ((tof-title-exists-p title) :tof-title-exists)
        (t nil)))

(defun chk-tof-title/update (title id)
  (cond ((eql :null title) :tof-title-null)
        ((tof-title-exists-p title id) :tof-title-exists)
        (t nil)))

(defun chk-tof-title (title)
  (if (or (eql :null title)
          (tof-title-exists-p title))
      nil
      :tof-title-unknown))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun tof-menu (id filter &optional disabled)
  (with-html
    (anchor-menu (crud-actions-spec (apply #'tof :id id filter)
                                    (apply #'tof/create filter)
                                    (apply #'tof/update :id id filter)
                                    (if (or (null id)
                                            (tof-referenced-p id))
                                        nil
                                        (apply #'tof/delete :id id filter)))
                 :id "tof-actions"
                 :css-class "hmenu actions"
                 :disabled disabled)))



;;; ------------------------------------------------------------
;;; TOF table
;;; ------------------------------------------------------------

;;; table

(defclass tof-table (config-table)
  ((header-labels  :initform '("" "Ονομασία Δ.Ο.Υ." "" ""))
   (paginator      :initform (make-instance 'tof-paginator
                                            :id "tof-paginator"
                                            :css-class "paginator")))
  (:default-initargs :id "config-table"
                     :item-class 'tof-row))

(defmethod get-records ((table tof-table))
  (config-data 'tof (getf (filter table) :search)))


;; rows

(defclass tof-row (config-row)
  ((record-class :allocation :class :initform 'tof)))

(defmethod selector ((row tof-row) enabled-p)
  (simple-selector row enabled-p #'tof))

(defmethod controls ((row tof-row) enabled-p)
  (simple-controls row enabled-p #'tof))


;;; paginator

(defclass tof-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg tof-paginator) start)
  (apply #'tof :start start (filter (table pg))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage tof-page tof ("config/tof")
    ((id     integer chk-tof-id)
     (search string)
     (start  integer))
  (with-view-page
    (let* ((filter (params->filter))
           (tof-table (make-instance 'tof-table
                                     :op :read
                                     :filter filter
                                     :start-index (val start))))
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
                                   '(:read)
                                   '(:read :update :delete)))
                     (display tof-table :key (val id)))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (tof) (val search)))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage tof-page tof/create ("config/tof/create")
    ((title  string chk-tof-title/create)
     (search string))
  (with-view-page
    (let* ((filter (params->filter))
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
                     (with-form (actions/tof/create :search (val search))
                       (display tof-table
                                :key nil
                                :payload (params->payload))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (tof) (val search))
                     (notifications))
               (footer)))))))

(defpage tof-page actions/tof/create ("actions/tof/create" :request-type :post)
    ((title  string chk-tof-title/create t)
     (search string))
  (with-controller-page (tof/create)
    (let ((new-tof (make-instance 'tof :title (val title))))
      (insert-dao new-tof)
      (see-other (tof :id (tof-id new-tof))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage tof-page tof/update ("config/tof/update")
    ((id     integer chk-tof-id                   t)
     (title  string  (chk-tof-title/update title id))
     (search string))
  (with-view-page
    (let* ((filter (params->filter))
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
                     (with-form (actions/tof/update :id (val id)
                                                    :filter (val search))
                       (display tof-table
                                :key (val id)
                                :payload (params->payload))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (tof) (val search))
                     (notifications))
               (footer)))))))

(defpage tof-page actions/tof/update ("actions/tof/update" :request-type :post)
    ((id     integer chk-tof-id t)
     (title  string (chk-tof-title/update title id) t)
     (search string))
  (with-controller-page (tof/update)
    (execute (:update 'tof :set
                      'title (val title)
                      :where (:= 'id (val id))))
    (see-other (tof :id (val id) :search (val search)))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage tof-page tof/delete ("config/tof/delete")
    ((id     integer chk-tof-id/ref t)
     (search string))
  (with-view-page
    (let* ((filter (params->filter))
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
                     (with-form (actions/tof/delete :id (val id)
                                                    :search (val search))
                       (display tof-table
                                :key (val id))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (tof) (val search)))
               (footer)))))))

(defpage tof-page actions/tof/delete ("actions/tof/delete" :request-type :post)
    ((id     integer chk-tof-id/ref t)
     (search string))
  (with-controller-page (tof/delete)
    (delete-dao (get-dao 'tof (val id)))
    (see-other (tof :search (val search)))))
