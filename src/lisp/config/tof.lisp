(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass tof-family (config-family)
  ()
  (:default-initargs :parameter-groups '(:system (tof-id)
                                         :payload (title)
                                         :filter (search))))

(defclass tof-page (auth-dynamic-page tof-family)
  ((messages
    :allocation :class
    :reader messages
    :initform '((title (:tof-title-null "Το όνομα της Δ.Ο.Υ. είναι κενό."
                        :tof-title-exists "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη."))))))

(defun tof-top-actions (op)
  (top-actions-area
   (make-instance 'scrooge-menu
                  :spec (make-menu-spec `(:create (,(family-url 'config/tof/create) "Νέα Δ.Ο.Υ.")))
                  :css-class "hmenu"
                  :disabled (list op))
   (searchbox (family-url-fn 'config/tof)
              (family-url-fn 'config/tof :system)
              (family-params 'config/tof :filter)
              "ac-tof")))



;;; ------------------------------------------------------------
;;; Validation
;;; ------------------------------------------------------------

(defun tof-referenced-p (tof-id)
  (referenced-by tof-id 'company 'tof-id))

(define-existence-predicate tof-id-exists-p tof id)
(define-existence-predicate* tof-title-exists-p tof title id)

(defun chk-tof-id (tof-id)
  (if (tof-id-exists-p tof-id)
      nil
      :tof-id-unknown))

(defun chk-tof-id/ref (tof-id)
  (cond ((chk-tof-id tof-id))
        ((tof-referenced-p tof-id) :tof-referenced)))

(defun chk-tof-title/create (title)
  (cond ((eql :null title) :tof-title-null)
        ((tof-title-exists-p title) :tof-title-exists)))

(defun chk-tof-title/update (title tof-id)
  (cond ((eql :null title) :tof-title-null)
        ((tof-title-exists-p title tof-id) :tof-title-exists)))

(defun chk-tof-title (title)
  (if (or (eql :null title)
          (tof-title-exists-p title))
      nil
      :tof-title-unknown))



;;; ------------------------------------------------------------
;;; TOF table
;;; ------------------------------------------------------------

;;; table

(defclass tof-table (config-table tof-family)
  ()
  (:default-initargs :record-class 'tof
                     :item-class 'tof-row
                     :paginator (make-instance 'tof-paginator)
                     :id "config-table"
                     :header-labels '("" "Ονομασία Δ.Ο.Υ." "" "")))

(defmethod get-records ((table tof-table))
  (config-data 'tof (getf (filter table) :search)))

(defmethod actions ((tbl tof-table) &key)
  (let* ((tof-id (selected-key tbl))
         (filter (filter tbl))
         (hrefs (if tof-id
                    (list :update (apply #'config/tof/update :tof-id tof-id filter)
                          :delete (if (tof-referenced-p tof-id)
                                      nil
                                      (apply #'config/tof/delete :tof-id tof-id filter)))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))


;; rows

(defclass tof-row (config-row)
  ())

(defmethod selector ((row tof-row) enabled-p)
  (simple-selector row enabled-p #'config/tof :tof-id))

(defmethod controls ((row tof-row) enabled-p)
  (simple-controls row enabled-p #'config/tof :tof-id))


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
    (let* ((filter (params->filter))
           (tof-table (make-instance 'tof-table
                                     :op :catalogue
                                     :selected-key (val tof-id)
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
            (tof-top-actions :catalogue)
            (:div :class "grid_12"
              (:div :id "tof-window" :class "window"
                (:div :class "title" "Κατάλογος")
                (actions tof-table)
                (display tof-table)))
            (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage tof-page config/tof/create ("config/tof/create")
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
            (tof-top-actions :create)
            (:div :class "grid_12"
              (:div :id "tof-window" :class "window"
                (:div :class "title" "Δημιουργία")
                (actions tof-table)
                (notifications)
                (with-form (actions/config/tof/create :search (val search))
                  (display tof-table
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
    (let* ((filter (params->filter))
           (tof-table (make-instance 'tof-table
                                     :op :update
                                     :selected-key (val tof-id)
                                     :filter filter)))
      (with-document ()
        (:head
          (:title "Δ.Ο.Υ. » Επεξεργασία")
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'tof)
            (tof-top-actions :update)
            (:div :class "grid_12"
              (:div :id "tof-window" :class "window"
                (:div :class "title" "Επεξεργασία")
                (actions tof-table)
                (notifications)
                (with-form (actions/config/tof/update :tof-id (val tof-id)
                                                      :filter (val search))
                  (display tof-table
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
    (let* ((filter (params->filter))
           (tof-table (make-instance 'tof-table
                                     :op :delete
                                     :selected-key (val tof-id)
                                     :filter filter)))
      (with-document ()
        (:head
          (:title "Δ.Ο.Υ. » Διαγραφή")
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'tof)
            (tof-top-actions :delete)
            (:div :class "grid_12"
              (:div :id "tof-window" :class "window"
                (:div :class "title" "Διαγραφή")
                (actions tof-table)
                (with-form (actions/config/tof/delete :tof-id (val tof-id)
                                                      :search (val search))
                  (display tof-table))))
            (footer)))))))

(defpage tof-page actions/config/tof/delete ("actions/config/tof/delete" :request-type :post)
    ((tof-id integer chk-tof-id/ref t)
     (search string))
  (with-controller-page (config/tof/delete)
    (delete-dao (get-dao 'tof (val tof-id)))
    (see-other (config/tof :search (val search)))))
