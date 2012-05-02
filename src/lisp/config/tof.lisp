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

(defun tof-referenced-p (tof-id)
  (with-db ()
    (and tof-id
         (query (:select 'id
                 :from 'company
                 :where (:= 'tof-id tof-id))
                :column))))

(define-existence-predicate tof-id-exists-p tof id)
(define-existence-predicate* tof-title-exists-p tof title id)

(defun chk-tof-id (tof-id)
  (if (tof-id-exists-p tof-id)
      nil
      :tof-id-unknown))

(defun chk-tof-id/ref (tof-id)
  (if (and (null (chk-tof-id tof-id))
           (null (tof-referenced-p tof-id)))
      nil
      :tof-referenced))

(defun chk-tof-title/create (title)
  (cond ((eql :null title) :tof-title-null)
        ((tof-title-exists-p title) :tof-title-exists)
        (t nil)))

(defun chk-tof-title/update (title tof-id)
  (cond ((eql :null title) :tof-title-null)
        ((tof-title-exists-p title tof-id) :tof-title-exists)
        (t nil)))

(defun chk-tof-title (title)
  (if (or (eql :null title)
          (tof-title-exists-p title))
      nil
      :tof-title-unknown))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun tof-top-actions (tof-id filter)
  (top-actions
   (make-instance 'menu
                  :spec `((create ,(html ()
                                         (:a :href (apply #'config/tof/create filter)
                                             (:img :src "/scrooge/img/add.png")
                                             (str "Νέα Δ.Ο.Υ.")))))
                  :css-class "hmenu")
   (searchbox #'config/tof
              #'(lambda (&rest args)
                  (apply #'config/tof :tof-id tof-id args))
              filter
              "ac-tof")))

(defun tof-actions (op tof-id filter)
  (with-html
      (actions-menu (make-menu-spec
                     (action-anchors/crud (apply #'config/tof/update :tof-id tof-id filter)
                                          (if (or (null tof-id)
                                                  (tof-referenced-p tof-id))
                                              nil
                                              (apply #'config/tof/delete :tof-id tof-id filter))))
                    (enabled-actions/crud op tof-id))))



;;; ------------------------------------------------------------
;;; TOF table
;;; ------------------------------------------------------------

;;; table

(defclass tof-table (config-table)
  ((header-labels :initform '("" "Ονομασία Δ.Ο.Υ." "" ""))
   (paginator     :initform (make-instance 'tof-paginator
                                           :id "tof-paginator"
                                           :css-class "paginator"))
   (key-name      :initform :tof-id))
  (:default-initargs :id "config-table"
                     :item-class 'tof-row))

(defmethod get-records ((table tof-table))
  (config-data 'tof (getf (filter table) :search)))


;; rows

(defclass tof-row (config-row)
  ((record-class :allocation :class :initform 'tof)))

(defmethod selector ((row tof-row) enabled-p)
  (simple-selector row enabled-p #'config/tof))

(defmethod controls ((row tof-row) enabled-p)
  (simple-controls row enabled-p #'config/tof))


;;; paginator

(defclass tof-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg tof-paginator) start)
  (apply #'config/tof :start start (filter (table pg))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage tof-page config/tof ("config/tof")
  ((tof-id integer chk-tof-id)
   (search string)
   (start  integer))
  (with-view-page
      (let* ((op :catalogue)
             (filter (params->filter))
             (tof-table (make-instance 'tof-table
                                       :op op
                                       :filter filter
                                       :start-index (val start))))
        (with-document ()
          (:head
           (:title "Δ.Ο.Υ. » Κατάλογος")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'tof)
                 (tof-top-actions (val tof-id) filter)
                 (:div :class "grid_12"
                       (:div :id "tof-window" :class "window"
                             (:div :class "title" "Κατάλογος")
                             (tof-actions op (val tof-id) filter)
                             (display tof-table :key (val tof-id))))
                 (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage tof-page config/tof/create ("config/tof/create")
  ((title  string chk-tof-title/create)
   (search string))
  (with-view-page
      (let* ((op :create)
             (filter (params->filter))
             (tof-table (make-instance 'tof-table
                                       :op op
                                       :filter filter)))
        (with-document ()
          (:head
           (:title "Δ.Ο.Υ. » Δημιουργία")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'tof)
                 (tof-top-actions nil filter)
                 (:div :class "grid_12"
                       (:div :id "tof-window" :class "window"
                             (:div :class "title" "Δημιουργία")
                             (tof-actions op nil filter)
                             (notifications)
                             (with-form (actions/config/tof/create :search (val search))
                               (display tof-table
                                        :key nil
                                        :payload (params->payload)))))
                 (footer)))))))

(defpage tof-page actions/config/tof/create ("actions/config/tof/create" :request-type :post)
  ((title  string chk-tof-title/create t)
   (search string))
  (with-controller-page (config/tof/create)
    (let ((new-tof (make-instance 'tof :title (val title))))
      (insert-dao new-tof)
      (see-other (config/tof :tof-id (tof-id new-tof))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage tof-page config/tof/update ("config/tof/update")
  ((tof-id integer chk-tof-id                          t)
   (title  string  (chk-tof-title/update title tof-id))
   (search string))
  (with-view-page
      (let* ((op :update)
             (filter (params->filter))
             (tof-table (make-instance 'tof-table
                                       :op op
                                       :filter filter)))
        (with-document ()
          (:head
           (:title "Δ.Ο.Υ. » Επεξεργασία")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'tof)
                 (tof-top-actions (val tof-id) filter)
                 (:div :class "grid_12"
                       (:div :id "tof-window" :class "window"
                             (:div :class "title" "Επεξεργασία")
                             (tof-actions op (val tof-id) filter)
                             (notifications)
                             (with-form (actions/config/tof/update :tof-id (val tof-id)
                                                                   :filter (val search))
                               (display tof-table
                                        :key (val tof-id)
                                        :payload (params->payload)))))
                 (footer)))))))

(defpage tof-page actions/config/tof/update ("actions/config/tof/update" :request-type :post)
  ((tof-id integer chk-tof-id                          t)
   (title  string  (chk-tof-title/update title tof-id) t)
   (search string))
  (with-controller-page (config/tof/update)
    (execute (:update 'tof :set
                      'title (val title)
                      :where (:= 'id (val tof-id))))
    (see-other (config/tof :tof-id (val tof-id) :search (val search)))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage tof-page config/tof/delete ("config/tof/delete")
  ((tof-id integer chk-tof-id/ref t)
   (search string))
  (with-view-page
      (let* ((op :delete)
             (filter (params->filter))
             (tof-table (make-instance 'tof-table
                                       :op op
                                       :filter filter)))
        (with-document ()
          (:head
           (:title "Δ.Ο.Υ. » Διαγραφή")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'tof)
                 (tof-top-actions (val tof-id) filter)
                 (:div :class "grid_12"
                       (:div :id "tof-window" :class "window"
                             (:div :class "title" "Διαγραφή")
                             (tof-actions op (val tof-id) filter)
                             (with-form (actions/config/tof/delete :tof-id (val tof-id)
                                                                   :search (val search))
                               (display tof-table
                                        :key (val tof-id)))))
                 (footer)))))))

(defpage tof-page actions/config/tof/delete ("actions/config/tof/delete" :request-type :post)
  ((tof-id integer chk-tof-id/ref t)
   (search string))
  (with-controller-page (config/tof/delete)
    (delete-dao (get-dao 'tof (val tof-id)))
    (see-other (config/tof :search (val search)))))
