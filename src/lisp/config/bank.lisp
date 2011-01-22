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
  (if (bank-title-exists-p title)
      nil
      :bank-title-unknown))



;;; ------------------------------------------------------------
;;; Bank - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/bank/create ("actions/bank/create" :request-type :post)
    ((title string chk-new-bank-title t))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (insert-dao (make-instance 'bank :title (val title)))
          (see-other (bank :id (bank-id (val title)))))
        (see-other (bank/create :title (raw title))))))

(define-dynamic-page actions/bank/update ("actions/bank/update" :request-type :post)
    ((id    integer chk-bank-id t)
     (title string  (chk-new-bank-title title id) t)
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (execute (:update 'bank :set
                            'title (val title)
                            :where (:= 'id (val id))))
          (see-other (bank :id (val id) :filter (val filter))))
        (see-other (bank/update :id (raw id) :title (raw title) :filter (raw filter))))))

(define-dynamic-page actions/bank/delete ("actions/bank/delete" :request-type :post)
    ((id integer chk-bank-id/ref t)
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'bank (val id)))
          (see-other (bank :filter (val filter))))
        (see-other (notfound)))))



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
                                                               (chk-bank-id/ref id))
                                                           nil
                                                           (bank/delete :id id
                                                                        :filter filter))))
           :disabled-items disabled-items))



;;; ------------------------------------------------------------
;;; Bank table
;;; ------------------------------------------------------------

;;; table

(defclass bank-table (crud-table)
  ((header-labels :initform '("" "Ονομασία τράπεζας" "" ""))
   (paginator     :initform (make-instance 'paginator
                                           :id "bank-paginator"
                                           :style "paginator grid_9 alpha"
                                           :delta 10
                                           :urlfn (lambda (filter start)
                                                    (bank :filter filter
                                                          :start start))))))

(defmethod read-items ((table bank-table))
  (iter (for rec in (config-data 'bank (filter table)))
        (for i from 0)
        (collect (make-instance 'bank-row
                                :key (getf rec :id)
                                :record rec
                                :collection table
                                :index i))))

(defmethod insert-item ((table bank-table) &key record index)
  (let* ((rows (rows table))
         (new-row (make-instance 'bank-row
                                  :key (getf record :id)
                                  :record record
                                  :collection table
                                  :index index)))
    (setf (rows table)
          (ninsert-list index new-row rows))))


;;; rows

(defclass bank-row (crud-row)
  ())

(defmethod cells ((row bank-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (filter (filter (collection row))))
    (list :selector (make-instance 'selector-cell
                                   :states (list :on (bank :filter filter
                                                           :start (page-start pg (index row) start))
                                                 :off (bank :filter filter
                                                            :id id)))
          :payload (make-instance 'textbox-cell
                                  :name 'title
                                  :value (getf record :title))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (bank :id id :filter filter))))))



;;; ------------------------------------------------------------
;;; Notifications
;;; ------------------------------------------------------------

(defun bank-notifications (&rest params)
  (notifications '((title (:bank-title-null "Το όνομα τράπεζας είναι κενό."
                           :bank-title-exists "Αυτό το όνομα τράπεζας υπάρχει ήδη.")))
                 params))



;;; ------------------------------------------------------------
;;; Bank - Pages
;;; ------------------------------------------------------------

(define-dynamic-page bank ("config/bank")
    ((id integer chk-bank-id)
     (filter string)
     (start integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((bank-table (make-instance 'bank-table
                                         :op 'catalogue
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
                         (filters 'bank (val filter)))
                   (:div :id "bank-window" :class "window grid_9"
                         (:div :class "title" "Κατάλογος τραπεζών")
                         (bank-menu (val id)
                                    (val filter)
                                    (if (val id)
                                        '(catalogue)
                                        '(catalogue update delete)))
                         (display bank-table
                                  :selected-id (val* id)
                                  :start (val* start)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page bank/create ("config/bank/create")
    ((title string chk-new-bank-title)
     (filter string))
  (with-auth ("configuration")
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
                     (filters 'bank (val filter))
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
               (footer)))))))

(define-dynamic-page bank/update ("config/bank/update")
    ((id    integer chk-bank-id t)
     (title string  (chk-new-bank-title title id))
     (filter string))
  (with-auth ("configuration")
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
                         (filters 'bank (val filter))
                         (bank-notifications title))
                   (:div :id "bank-window" :class "window grid_9"
                         (:div :class "title" "Επεξεργασία τράπεζας")
                         (bank-menu (val id)
                                    (val filter)
                                    '(create update))
                         (with-form (actions/bank/update :id (val* id)
                                                         :title (val* title)
                                                         :filter (val* filter))
                           (display bank-table
                                    :selected-id (val id)
                                    :selected-data (list :title (val* title)))))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page bank/delete ("config/bank/delete")
    ((id integer chk-bank-id/ref t)
     (filter string))
  (with-auth ("configuration")
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
                         (filters 'bank (val filter)))
                   (:div :id "bank-window" :class "window grid_9"
                         (:div :class "title" "Διαγραφή τράπεζας")
                         (bank-menu (val id)
                                    (val filter)
                                    '(create delete))
                         (with-form (actions/bank/delete :id (val id)
                                                         :filter (val* filter))
                           (display bank-table
                                    :selected-id (val id))))
                   (footer)))))
        (see-other (notfound)))))
