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

(define-dynamic-page actions/tof/create ("actions/tof/create" :request-type :post)
    ((title string chk-new-tof-title t))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (insert-dao (make-instance 'tof :title (val title)))
        (see-other (tof :id (tof-id (val title)))))
      (see-other (tof/create :title (raw title)))))

(define-dynamic-page actions/tof/update ("actions/tof/update" :request-type :post)
    ((id    integer chk-tof-id t)
     (title string (chk-new-tof-title title id) t)
     (filter string))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (execute (:update 'tof :set
                          'title (val title)
                          :where (:= 'id (val id))))
        (see-other (tof :id (val id) :filter (val filter))))
      (see-other (tof/update :id (raw id) :title (raw title) :filter (raw filter)))))

(define-dynamic-page actions/tof/delete ("actions/tof/delete" :request-type :post)
    ((id integer chk-tof-id/ref t)
     (filter string))
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'tof (val id)))
        (see-other (tof :filter (val filter))))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; TOF menu
;;; ------------------------------------------------------------

(defun tof-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "tof-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (standard-actions-spec (tof :id id
                                                            :filter filter)
                                                       (tof/create :filter filter)
                                                       (tof/update :id id
                                                                   :filter filter)
                                                       (if (or (null id)
                                                               (tof-referenced-p id))
                                                           nil
                                                           (tof/delete :id id
                                                                       :filter filter))))
           :disabled-items disabled-items))



;;; ------------------------------------------------------------
;;; TOF table
;;; ------------------------------------------------------------

;;; table

(defclass tof-table (crud-table)
  ((header-labels :initform '("" "Ονομασία Δ.Ο.Υ." "" ""))
   (paginator     :initform (make-instance 'paginator
                                           :id "tof-paginator"
                                           :style "paginator grid_9 alpha"
                                           :delta 10
                                           :urlfn (lambda (filter start)
                                                    (tof :filter filter
                                                         :start start))))))


(defmethod read-items ((table tof-table))
  (iter (for rec in (config-data 'tof (filter table)))
        (for i from 0)
        (collect (make-instance 'tof-row
                                :key (getf rec :id)
                                :record rec
                                :collection table
                                :index i))))

(defmethod insert-item ((table tof-table) &key record index)
  (let* ((rows (rows table))
         (new-row (make-instance 'tof-row
                                 :key (getf record :id)
                                 :record record
                                 :collection table
                                 :index index)))
    (setf (rows table)
          (ninsert-list index new-row rows))))


;; rows

(defclass tof-row (crud-row)
  ())

(defmethod cells ((row tof-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (filter (filter (collection row))))
    (list :selector (make-instance 'selector-cell
                                   :states (list :on (tof :filter filter
                                                          :start (page-start pg (index row) start))
                                                 :off (tof :filter filter
                                                           :id id)))
          :payload (make-instance 'textbox-cell
                                  :name 'title
                                  :value (getf record :title))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (tof :id id :filter filter))))))



;;; ------------------------------------------------------------
;;; Notifications
;;; ------------------------------------------------------------

(defun tof-notifications (&rest params)
  (notifications '((title (:tof-title-null "Το όνομα της Δ.Ο.Υ. είναι κενό."
                           :tof-title-exists "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη.")))
                 params))



;;; ------------------------------------------------------------
;;; TOF - Pages
;;; ------------------------------------------------------------

(define-dynamic-page tof ("config/tof")
    ((id integer chk-tof-id)
     (filter string)
     (start integer))
  (no-cache)
  (if (validp id)
      (let ((tof-table (make-instance 'tof-table
                                      :op 'catalogue
                                      :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Τράπεζες")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-menu 'tof)
                 (:div :id "controls" :class "controls grid_3"
                       (filters 'tof (val filter)))
                 (:div :id "tof-window" :class "window grid_9"
                       (:div :class "title" "Κατάλογος Δ.Ο.Υ.")
                       (tof-menu (val id)
                                  (val filter)
                                  (if (val id)
                                      '(catalogue)
                                      '(catalogue update delete)))
                       (display tof-table
                                :selected-id (val* id)
                                :start (val* start)))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page tof/create ("config/tof/create")
    ((title string chk-new-tof-title)
     (filter string))
  (no-cache)
  (let ((tof-table (make-instance 'tof-table
                                  :op 'create
                                  :filter (val* filter))))
    (with-document ()
      (:head
       (:title "Δημιουργία Δ.Ο.Υ.")
       (config-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'config)
             (config-menu 'tof)
             (:div :id "controls" :class "controls grid_3"
                   (filters 'tof (val filter))
                   (tof-notifications title))
             (:div :id "tof-window" :class "window grid_9"
                   (:div :class "title" "Δημιουργία Δ.Ο.Υ.")
                   (tof-menu nil
                              (val filter)
                              '(create update delete))
                   (with-form (actions/tof/create :title (val* title))
                     (display tof-table
                              :selected-id nil
                              :selected-data (list :title (val* title)))))
             (footer))))))

(define-dynamic-page tof/update ("config/tof/update")
    ((id    integer chk-tof-id t)
     (title string  (chk-new-tof-title title id))
     (filter string))
  (no-cache)
  (if (validp id)
      (let ((tof-table (make-instance 'tof-table
                                      :op 'update
                                      :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Επεξεργασία Δ.Ο.Υ.")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-menu 'tof)
                 (:div :id "controls" :class "controls grid_3"
                       (filters 'tof (val filter))
                       (tof-notifications title))
                 (:div :id "tof-window" :class "window grid_9"
                       (:div :class "title" "Επεξεργασία Δ.Ο.Υ.")
                       (tof-menu (val id)
                                  (val filter)
                                  '(create update))
                       (with-form (actions/tof/update :id (val* id)
                                                      :title (val* title)
                                                      :filter (val* filter))
                         (display tof-table
                                  :selected-id (val id)
                                  :selected-data (list :title (val* title)))))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page tof/delete ("config/tof/delete")
    ((id integer chk-tof-id/ref t)
     (filter string))
  (no-cache)
  (if (validp id)
      (let ((tof-table (make-instance 'tof-table
                                      :op 'delete
                                      :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Διαγραφή Δ.Ο.Υ.")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-menu 'tof)
                 (:div :id "controls" :class "controls grid_3"
                       (filters 'tof (val filter)))
                 (:div :id "tof-window" :class "window grid_9"
                       (:div :class "title" "Διαγραφή Δ.Ο.Υ.")
                       (tof-menu (val id)
                                  (val filter)
                                  '(create delete))
                       (with-form (actions/tof/delete :id (val id)
                                                      :filter (val* filter))
                         (display tof-table
                                  :selected-id (val id))))
                 (footer)))))
      (see-other (notfound))))
