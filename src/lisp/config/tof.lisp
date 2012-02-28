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

(defun tof-top-actions (id filter)
  (top-actions
   (make-instance 'menu
                  :spec `((create ,(html ()
                                     (:a :href (apply #'config/tof/create filter)
                                         (:img :src "/scrooge/img/add.png")
                                         (str "Νέα Δ.Ο.Υ.")))))
                  :css-class "hmenu")
   (searchbox #'config/tof
              #'(lambda (&rest args)
                  (apply #'config/tof :id id args))
              filter
              "ac-tof")))

(defun tof-actions (op id filter)
  (with-html
    (actions-menu (make-menu-spec
                   (action-anchors/crud (apply #'config/tof/update :id id filter)
                                        (if (or (null id)
                                                (tof-referenced-p id))
                                            nil
                                            (apply #'config/tof/delete :id id filter))))
                  (enabled-actions/crud op id))))



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
    ((id     integer chk-tof-id)
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
               (tof-top-actions (val id) filter)
               (:div :class "grid_12"
                     (:div :id "tof-window" :class "window"
                           (:div :class "title" "Κατάλογος")
                           (tof-actions op (val id) filter)
                           (display tof-table :key (val id))))
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
      (see-other (config/tof :id (tof-id new-tof))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage tof-page config/tof/update ("config/tof/update")
    ((id     integer chk-tof-id                   t)
     (title  string  (chk-tof-title/update title id))
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
               (tof-top-actions (val id) filter)
               (:div :class "grid_12"
                     (:div :id "tof-window" :class "window"
                           (:div :class "title" "Επεξεργασία")
                           (tof-actions op (val id) filter)
                           (with-form (actions/config/tof/update :id (val id)
                                                                 :filter (val search))
                             (display tof-table
                                      :key (val id)
                                      :payload (params->payload)))))
               (footer)))))))

(defpage tof-page actions/config/tof/update ("actions/config/tof/update" :request-type :post)
    ((id     integer chk-tof-id t)
     (title  string (chk-tof-title/update title id) t)
     (search string))
  (with-controller-page (config/tof/update)
    (execute (:update 'tof :set
                      'title (val title)
                      :where (:= 'id (val id))))
    (see-other (config/tof :id (val id) :search (val search)))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage tof-page config/tof/delete ("config/tof/delete")
    ((id     integer chk-tof-id/ref t)
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
               (tof-top-actions (val id) filter)
               (:div :class "grid_12"
                     (:div :id "tof-window" :class "window"
                           (:div :class "title" "Διαγραφή")
                           (tof-actions op (val id) filter)
                           (with-form (actions/config/tof/delete :id (val id)
                                                                 :search (val search))
                             (display tof-table
                                      :key (val id)))))
               (footer)))))))

(defpage tof-page actions/config/tof/delete ("actions/config/tof/delete" :request-type :post)
    ((id     integer chk-tof-id/ref t)
     (search string))
  (with-controller-page (config/tof/delete)
    (delete-dao (get-dao 'tof (val id)))
    (see-other (config/tof :search (val search)))))
