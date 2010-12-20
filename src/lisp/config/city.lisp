(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; City - Validation
;;; ------------------------------------------------------------

(defun city-referenced-p (id)
  (with-db ()
    (and id
         nil #|(query (:select 'id
                         :from 'company
                         :where (:= 'city-id id)))|#)))

(define-existence-predicate city-id-exists-p city id)
(define-uniqueness-predicate city-title-unique-p city title id)

(defun chk-city-id (id)
  (if (city-id-exists-p id)
      nil
      'city-id-unknown))

(defun chk-city-id/ref (id)
  (if (and (null (chk-city-id id))
           (null (city-referenced-p id)))
      nil
      'city-referenced))

(defun chk-city-title (title &optional id)
  (cond ((eql :null title) 'city-title-null)
        ((not (city-title-unique-p title id)) 'city-title-exists)
        (t nil)))



;;; ------------------------------------------------------------
;;; City - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/city/create ("actions/city/create" :request-type :post)
    ((title string chk-city-title t))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (insert-dao (make-instance 'city :title (val title)))
        (see-other (city :id (city-id (val title)))))
      (see-other (city/create :title (raw title)))))

(define-dynamic-page actions/city/update ("actions/city/update" :request-type :post)
    ((id    integer chk-city-id t)
     (title string (chk-city-title title id) t))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (execute (:update 'city :set
                          'title (val title)
                          :where (:= 'id (val id))))
        (see-other (city :id (val id))))
      (see-other (city/update :id (raw id) :title (raw title)))))

(define-dynamic-page actions/city/delete ("actions/city/delete" :request-type :post)
    ((id integer chk-city-id/ref t))
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'city (val id)))
        (see-other (city)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; City menu
;;; ------------------------------------------------------------

(defun city-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "city-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (standard-actions-spec (city :id id
                                                             :filter filter)
                                                       (city/create :filter filter)
                                                       (city/update :id id
                                                                    :filter filter)
                                                       (if (or (null id)
                                                               (city-referenced-p id))
                                                           nil
                                                           (city/delete :id id
                                                                        :filter filter))))
           :disabled-items disabled-items))



;;; ------------------------------------------------------------
;;; City table
;;; ------------------------------------------------------------

;;; table

(defclass city-table (crud-table)
  ((header-labels :initform '("" "Ονομασία πόλης" "" ""))))

(defmethod read-data ((table city-table))
  (config-data 'city (filter table)))

(defmethod make-row ((table city-table) data)
  (make-instance 'city-row
                 :table table
                 :data data))

(defmethod paginator ((table city-table))
  (make-instance 'paginator
                 :id "city-paginator"
                 :style "paginator grid_9 alpha"
                 :delta 10
                 :urlfn (lambda (start)
                          (city :filter (filter table) :start start))
                 :len (length (db-data table))))

;;; rows

(defclass city-row (crud-row)
  ())

(defmethod get-id ((row city-row))
  (getf (data row) :id))

(defmethod cells ((row city-row))
  (let* ((id (get-id row))
         (data (data row))
         (table (table row))
         (pg (paginator table ))
         (filter (filter table)))
    (list :selector (make-instance 'selector-cell
                                   :style "selector"
                                   :states (list :on (city :filter filter
                                                           :start (pg-start table pg id))
                                                 :off (city :filter filter
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
                                    :href (city :id id))))))

(defmethod display ((row city-row) &key selected-id)
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

(defun city-filters (filter)
  (with-html
    (:div :id "filters"
          (:p :class "title" "Φίλτρα")
          (with-form (city)
            (htm
             (:p :class "search"
                 (textbox 'filter :value filter)
                 (submit (html ()
                           (img "magnifier.png")))))))))

(defun city-notifications (&rest params)
  (notifications '(title ((city-title-null "Το όνομα πόλης είναι κενό.")
                          (city-title-exists "Αυτό το όνομα πόλης υπάρχει ήδη.")))
                 params))

;;; ------------------------------------------------------------
;;; City - Pages
;;; ------------------------------------------------------------

(define-dynamic-page city ("config/city")
    ((id integer chk-city-id)
     (filter string)
     (start integer))
  (no-cache)
  (if (validp id)
      (let ((city-table (make-instance 'city-table
                                        :op 'view
                                        :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Πόλεις")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-menu 'city)
                 (:div :id "controls" :class "controls grid_3"
                       (city-filters (val filter)))
                 (:div :id "city-window" :class "window grid_9"
                       (:div :class "title" "Κατάλογος πόλεων")
                       (city-menu (val id)
                                  (val filter)
                                  (if (val id)
                                      '(view)
                                      '(view update delete)))
                       (display city-table
                                :start (val* start)
                                :selected-id (val* id)))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page city/create ("config/city/create")
    ((title string chk-city-title)
     (filter string))
  (no-cache)
  (let ((city-table (make-instance 'city-table
                                   :op 'create
                                   :filter (val* filter))))
    (with-document ()
      (:head
       (:title "Δημιουργία πόλης")
       (config-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'config)
             (config-menu 'city)
             (:div :id "controls" :class "controls grid_3"
                   (city-filters (val filter))
                   (city-notifications title))
             (:div :id "city-window" :class "window grid_9"
                   (:div :class "title" "Δημιουργία πόλης")
                   (city-menu nil
                              (val filter)
                              '(create update delete))
                   (with-form (actions/city/create :title (val* title))
                     (display city-table
                              :selected-id nil
                              :selected-data (list :title (val* title)))))
             (footer))))))

(define-dynamic-page city/update ("config/city/update")
    ((id    integer chk-city-id t)
     (title string  (chk-city-title title id))
     (filter string))
  (no-cache)
  (if (validp id)
      (let ((city-table (make-instance 'city-table
                                       :op 'update
                                       :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Επεξεργασία πόλης")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-menu 'city)
                 (:div :id "controls" :class "controls grid_3"
                       (city-filters (val filter))
                       (city-notifications title))
                 (:div :id "city-window" :class "window grid_9"
                       (:div :class "title" "Επεξεργασία πόλης")
                       (city-menu (val id)
                                  (val filter)
                                  '(create update))
                       (with-form (actions/city/update :id (val* id)
                                                       :title (val* title))
                         (display city-table
                                  :selected-id (val id)
                                  :selected-data (list :title (val* title)))))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page city/delete ("config/city/delete")
    ((id integer chk-city-id/ref t)
     (filter string))
  (no-cache)
  (if (validp id)
      (let ((city-table (make-instance 'city-table
                                       :op 'delete
                                       :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Διαγραφή πόλης")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-menu 'city)
                 (:div :id "controls" :class "controls grid_3"
                       (city-filters (val filter)))
                 (:div :id "city-window" :class "window grid_9"
                       (:div :class "title" "Διαγραφή πόλης")
                       (city-menu (val id)
                                  (val filter)
                                  '(create delete))
                       (with-form (actions/city/delete :id (val id))
                         (display city-table
                                  :selected-id (val id))))
                 (footer)))))
      (see-other (notfound))))
