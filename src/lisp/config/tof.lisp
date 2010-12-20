(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



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
(define-uniqueness-predicate tof-title-unique-p tof title id)

(defun chk-tof-id (id)
  (if (tof-id-exists-p id)
      nil
      'tof-id-unknown))

(defun chk-tof-id/ref (id)
  (if (and (null (chk-tof-id id))
           (null (tof-referenced-p id)))
      nil
      'tof-referenced))

(defun chk-tof-title (title &optional id)
  (cond ((eql :null title) 'tof-title-null)
        ((not (tof-title-unique-p title id)) 'tof-title-exists)
        (t nil)))



;;; ------------------------------------------------------------
;;; TOF - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/tof/create ("actions/tof/create" :request-type :post)
    ((title string chk-tof-title t))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (insert-dao (make-instance 'tof :title (val title)))
        (see-other (tof :id (tof-id (val title)))))
      (see-other (tof/create :title (raw title)))))

(define-dynamic-page actions/tof/update ("actions/tof/update" :request-type :post)
    ((id    integer chk-tof-id t)
     (title string (chk-tof-title title id) t))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (execute (:update 'tof :set
                          'title (val title)
                          :where (:= 'id (val id))))
        (see-other (tof :id (val id))))
      (see-other (tof/update :id (raw id) :title (raw title)))))

(define-dynamic-page actions/tof/delete ("actions/tof/delete" :request-type :post)
    ((id integer chk-tof-id/ref t))
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'tof (val id)))
        (see-other (tof)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; TOF menu
;;; ------------------------------------------------------------

(defun tof-menu (id enabled-items)
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
  ((header-labels :initform '("" "Ονομασία Δ.Ο.Υ." "" ""))))

(defmethod read-data ((table tof-table))
  (config-data 'tof (filter table)))

(defmethod make-row ((table tof-table) data)
  (make-instance 'tof-row
                 :table table
                 :data data))

(defmethod paginator ((table tof-table))
  (make-instance 'paginator
                 :id "tof-paginator"
                 :style "paginator"
                 :delta 10
                 :urlfn (lambda (start)
                          (tof :filter (filter table) :start start))
                 :len (length (db-data table))))

;; rows

(defclass tof-row (crud-row)
  ())

(defmethod get-id ((row tof-row))
  (getf (data row) :id))

(defmethod cells ((row tof-row))
  (let* ((id (get-id row))
         (data (data row))
         (table (table row))
         (pg (paginator table ))
         (filter (filter table)))
    (list :selector (make-instance 'selector-cell
                                   :style "selector"
                                   :states (list :on (tof :filter filter
                                                          :start (pg-start table pg id))
                                                 :off (tof :filter filter
                                                           :id id)))
          :payload (make-instance 'textbox-cell
                                  :name 'title
                                  :style "payload"
                                  :value (getf data :title))
          :controls (list
                     (make-instance 'ok-cell
                                    :style "control")
                     (make-instance 'cancel-cell
                                    :style "control"
                                    :href (tof :id id))))))

(defmethod display ((row tof-row) &key selected-id)
  (with-html
    (:tr (display (getf (cells row) :selector)
                  :state (if (selected-p row selected-id) :on :off))
         (display (getf (cells row) :payload)
                  :readonlyp (readonly-p row selected-id))
         (mapc (lambda (cell)
                 (htm (display cell :activep (controls-p row selected-id))))
               (getf (cells row) :controls)))))



;;; ------------------------------------------------------------
;;; Other areas
;;; ------------------------------------------------------------

(defun tof-filters (filter)
  (with-html
    (:div :id "filters"
          (:p :class "title" "Φίλτρα")
          (with-form (tof)
            (htm
             (:p (textbox 'filter :value filter) (submit (html ()
                                                           (img "magnifier.png")))))))))

(defun notifications (&rest params)
  (let ((messenger (messenger '(title ((tof-title-null "Το όνομα της Δ.Ο.Υ. είναι κενό.")
                                     (tof-title-exists "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη.")))
                              "msg-error")))
    (with-html
      (:div :id "notifications"
            (:p :class "title" "Μηνύματα")
            (display messenger :params params)))))



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
                                        :op 'view
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
                       (tof-filters (val filter)))
                 (:div :id "tof-window" :class "window grid_9"
                       (:div :class "title" "Κατάλογος Δ.Ο.Υ.")
                       (tof-menu (val id)
                                  (val filter)
                                  (if (val id)
                                      '(view)
                                      '(view update delete)))
                       (display tof-table
                                :start (val* start)
                                :selected-id (val* id)))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page tof/create ("config/tof/create")
    ((title string chk-tof-title)
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
                   (tof-filters (val filter))
                   (notifications title))
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
     (title string  (chk-tof-title title id))
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
                       (tof-filters (val filter))
                       (notifications title))
                 (:div :id "tof-window" :class "window grid_9"
                       (:div :class "title" "Επεξεργασία Δ.Ο.Υ.")
                       (tof-menu (val id)
                                  (val filter)
                                  '(create update))
                       (with-form (actions/tof/update :id (val* id)
                                                       :title (val* title))
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
                       (tof-filters (val filter)))
                 (:div :id "tof-window" :class "window grid_9"
                       (:div :class "title" "Διαγραφή Δ.Ο.Υ.")
                       (tof-menu (val id)
                                  (val filter)
                                  '(create delete))
                       (with-form (actions/tof/delete :id (val id))
                         (display tof-table
                                  :selected-id (val id))))
                 (footer)))))
      (see-other (notfound))))
