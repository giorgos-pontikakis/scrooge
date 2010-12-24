(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



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
(define-uniqueness-predicate bank-title-unique-p bank title id)

(defun chk-bank-id (id)
  (if (bank-id-exists-p id)
      nil
      'bank-id-unknown))

(defun chk-bank-id/ref (id)
  (if (and (null (chk-bank-id id))
           (null (bank-referenced-p id)))
      nil
      'bank-referenced))

(defun chk-bank-title (title &optional id)
  (cond ((eql :null title) 'bank-title-null)
        ((not (bank-title-unique-p title id)) 'bank-title-exists)
        (t nil)))



;;; ------------------------------------------------------------
;;; Bank - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/bank/create ("actions/bank/create" :request-type :post)
    ((title string chk-bank-title t))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (insert-dao (make-instance 'bank :title (val title)))
        (see-other (bank :id (bank-id (val title)))))
      (see-other (bank/create :title (raw title)))))

(define-dynamic-page actions/bank/update ("actions/bank/update" :request-type :post)
    ((id    integer chk-bank-id t)
     (title string  (chk-bank-title title id) t))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (execute (:update 'bank :set
                          'title (val title)
                          :where (:= 'id (val id))))
        (see-other (bank :id (val id))))
      (see-other (bank/update :id (raw id) :title (raw title)))))

(define-dynamic-page actions/bank/delete ("actions/bank/delete" :request-type :post)
    ((id integer chk-bank-id/ref t))
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'bank (val id)))
        (see-other (bank)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; Bank menu
;;; ------------------------------------------------------------

(defun bank-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "bank-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (standard-actions-spec (bank :id id
                                                             :filter filter)
                                                       (bank/create :filter filter)
                                                       (bank/update :id id
                                                                    :filter filter)
                                                       (if (or (null id)
                                                               (bank-referenced-p id))
                                                           nil
                                                           (bank/delete :id id
                                                                        :filter filter))))
           :disabled-items disabled-items))



;;; ------------------------------------------------------------
;;; Bank table
;;; ------------------------------------------------------------

;;; table

(defclass bank-table (crud-table)
  ((header-labels :initform '("" "Ονομασία τράπεζας" "" ""))))

(defmethod read-data ((table bank-table))
  (config-data 'bank (filter table)))

(defmethod make-row ((table bank-table) data)
  (make-instance 'bank-row
                 :collection table
                 :data data))

(defmethod paginator ((table bank-table))
  (make-instance 'paginator
                 :id "bank-paginator"
                 :style "paginator grid_9 alpha"
                 :delta 10
                 :urlfn (lambda (start)
                          (bank :filter (filter table) :start start))
                 :len (length (db-data table))))

;;; rows

(defclass bank-row (crud-row)
  ())

(defmethod get-id ((row bank-row))
  (getf (data row) :id))

(defmethod cells ((row bank-row))
  (let* ((id (get-id row))
         (data (data row))
         (table (collection row))
         (pg (paginator table))
         (filter (filter table)))
    (list :selector (make-instance 'selector-cell
                                   :style "selector"
                                   :states (list :on (bank :filter filter
                                                           :start (pg-start table pg id))
                                                 :off (bank :filter filter
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
                                    :href (bank :id id))))))

(defmethod display ((row bank-row) &key selected-id)
  (let ((selected-p (selected-p row selected-id)))
    (with-html
      (:tr :class (if selected-p
                      (if (eq (op (collection row)) 'delete)
                          "attention"
                          "selected")
                      nil)
           (:td :class "selector"
                (display (getf (cells row) :selector)
                         :state (if selected-p :on :off)))
           (:td :class "payload"
                (display (getf (cells row) :payload)
                         :readonlyp (readonly-p row selected-id)))
           (mapc (lambda (cell)
                   (htm (:td :class "control"
                             (display cell :activep (controls-p row selected-id)))))
                 (getf (cells row) :controls))))))



;;; ------------------------------------------------------------
;;; Other areas
;;; ------------------------------------------------------------

(defun bank-filters (filter)
  (with-html
    (:div :id "filters"
          (:p :class "title" "Φίλτρα")
          (with-form (bank)
            (htm
             (:p :class "search"
                 (textbox 'filter :value filter)
                 (submit (html ()
                           (img "magnifier.png")))))))))

(defun bank-notifications (&rest params)
  (notifications '(title ((bank-title-null "Το όνομα τράπεζας είναι κενό.")
                          (bank-title-exists "Αυτό το όνομα τράπεζας υπάρχει ήδη.")))
                 params))

;;; ------------------------------------------------------------
;;; Bank - Pages
;;; ------------------------------------------------------------

(define-dynamic-page bank ("config/bank")
    ((id integer chk-bank-id)
     (filter string)
     (start integer))
  (no-cache)
  (if (validp id)
      (let ((bank-table (make-instance 'bank-table
                                        :op 'view
                                        :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Τράπεζες")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-menu 'bank)
                 (:div :id "controls" :class "controls grid_3"
                       (bank-filters (val filter)))
                 (:div :id "bank-window" :class "window grid_9"
                       (:div :class "title" "Κατάλογος τραπεζών")
                       (bank-menu (val id)
                                  (val filter)
                                  (if (val id)
                                      '(view)
                                      '(view update delete)))
                       (display bank-table
                                :start (val* start)
                                :selected-id (val* id)))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page bank/create ("config/bank/create")
    ((title string chk-bank-title)
     (filter string))
  (no-cache)
  (let ((bank-table (make-instance 'bank-table
                                   :op 'create
                                   :filter (val* filter))))
    (with-document ()
      (:head
       (:title "Δημιουργία τράπεζας")
       (config-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'config)
             (config-menu 'bank)
             (:div :id "controls" :class "controls grid_3"
                   (bank-filters (val filter))
                   (bank-notifications title))
             (:div :id "bank-window" :class "window grid_9"
                   (:div :class "title" "Δημιουργία τράπεζας")
                   (bank-menu nil
                              (val filter)
                              '(create update delete))
                   (with-form (actions/bank/create :title (val* title))
                     (display bank-table
                              :selected-id nil
                              :selected-data (list :title (val* title)))))
             (footer))))))

(define-dynamic-page bank/update ("config/bank/update")
    ((id    integer chk-bank-id t)
     (title string  (chk-bank-title title id))
     (filter string))
  (no-cache)
  (if (validp id)
      (let ((bank-table (make-instance 'bank-table
                                       :op 'update
                                       :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Επεξεργασία τράπεζας")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-menu 'bank)
                 (:div :id "controls" :class "controls grid_3"
                       (bank-filters (val filter))
                       (bank-notifications title))
                 (:div :id "bank-window" :class "window grid_9"
                       (:div :class "title" "Επεξεργασία τράπεζας")
                       (bank-menu (val id)
                                  (val filter)
                                  '(create update))
                       (with-form (actions/bank/update :id (val* id)
                                                       :title (val* title))
                         (display bank-table
                                  :selected-id (val id)
                                  :selected-data (list :title (val* title)))))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page bank/delete ("config/bank/delete")
    ((id integer chk-bank-id/ref t)
     (filter string))
  (no-cache)
  (if (validp id)
      (let ((bank-table (make-instance 'bank-table
                                       :op 'delete
                                       :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Διαγραφή τράπεζας")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-menu 'bank)
                 (:div :id "controls" :class "controls grid_3"
                       (bank-filters (val filter)))
                 (:div :id "bank-window" :class "window grid_9"
                       (:div :class "title" "Διαγραφή τράπεζας")
                       (bank-menu (val id)
                                  (val filter)
                                  '(create delete))
                       (with-form (actions/bank/delete :id (val id))
                         (display bank-table
                                  :selected-id (val id))))
                 (footer)))))
      (see-other (notfound))))
