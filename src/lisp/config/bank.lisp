(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass bank-family (family-mixin)
  ()
  (:default-initargs :parameter-groups '(:system (bank-id)
                                         :payload (title)
                                         :filter (search))))

(defclass bank-page (auth-dynamic-page bank-family)
  ((messages
    :allocation :class
    :reader messages
    :initform '((title (:bank-title-null "Το όνομα τράπεζας είναι κενό."
                        :bank-title-exists "Αυτό το όνομα τράπεζας υπάρχει ήδη."))))))



;;; ------------------------------------------------------------
;;; Validation
;;; ------------------------------------------------------------

(defun bank-referenced-p (bank-id)
  (with-db ()
    (and bank-id
         (query (:select 'id
                 :from 'cheque
                 :where (:= 'bank-id bank-id))
                :column))))

(define-existence-predicate  bank-id-exists-p bank id)
(define-existence-predicate* bank-title-exists-p bank title id)

(defun chk-bank-id (bank-id)
  (if (bank-id-exists-p bank-id)
      nil
      :bank-id-unknown))

(defun chk-bank-id/ref (bank-id)
  (cond ((chk-bank-id bank-id))
        ((bank-referenced-p bank-id) :bank-referenced)))

(defun chk-bank-title/create (title)
  (cond ((eql :null title) :bank-title-null)
        ((bank-title-exists-p title) :bank-title-exists)))

(defun chk-bank-title/update (title bank-id)
  (cond ((eql :null title) :bank-title-null)
        ((bank-title-exists-p title bank-id) :bank-title-exists)))

(defun chk-bank-title (title)
  (if (or (eql :null title)
          (bank-title-exists-p title))
      nil
      :bank-title-unknown))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun bank-top-actions (bank-id filter)
  (top-actions
   (make-instance 'menu
                  :spec `((create ,(html ()
                                     (:a :href (apply #'config/bank/create filter)
                                         (:img :src "/scrooge/img/add.png")
                                         (str "Νέα Τράπεζα")))))
                  :css-class "hmenu")
   (searchbox #'config/bank
              #'(lambda (&rest args)
                  (apply #'config/bank :bank-id bank-id args))
              filter
              "ac-bank")))



;;; ------------------------------------------------------------
;;; Bank table
;;; ------------------------------------------------------------

;;; table

(defclass bank-table (config-table)
  ((header-labels :initform '("" "Ονομασία τράπεζας" "" ""))
   (paginator     :initform (make-instance 'bank-paginator
                                           :id "bank-paginator"
                                           :css-class "paginator")))
  (:default-initargs :id "config-table"
                     :item-class 'bank-row))

(defmethod get-records ((table bank-table))
  (config-data 'bank (getf (filter table) :search)))

(defmethod actions ((tbl bank-table) &key key)
  (let* ((bank-id key)
         (filter (filter tbl))
         (hrefs (if bank-id
                    (list :update (apply #'config/bank/update :bank-id bank-id filter)
                          :delete (if (chk-bank-id/ref bank-id)
                                      nil
                                      (apply #'config/bank/delete :bank-id bank-id filter)))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))


;;; rows

(defclass bank-row (config-row)
  ((record-class :allocation :class :initform 'bank)))

(defmethod selector ((row bank-row) selected-p)
  (simple-selector row selected-p #'config/bank :bank-id))

(defmethod controls ((row bank-row) controls-p)
  (simple-controls row controls-p #'config/bank :bank-id))


;;; paginator

(defclass bank-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg bank-paginator) start)
  (apply #'config/bank :start start (filter (table pg))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage bank-page config/bank ("config/bank")
    ((bank-id integer chk-bank-id)
     (search  string)
     (start   integer))
  (with-view-page
    (let* ((filter (params->values :filter))
           (bank-table (make-instance 'bank-table
                                      :op :catalogue
                                      :filter filter
                                      :start-index (val start))))
      (with-document ()
        (:head
         (:title "Τράπεζες » Κατάλογος")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'bank)
               (bank-top-actions (val bank-id) filter)
               (:div :class "grid_12"
                     (:div :id "bank-window" :class "window"
                           (:div :class "title" "Κατάλογος")
                           (actions bank-table :key (val bank-id))
                           (display bank-table :key (val bank-id))))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage bank-page config/bank/create ("config/bank/create")
    ((title  string chk-bank-title/create)
     (search string))
  (with-view-page
    (let* ((filter (params->values :filter))
           (bank-table (make-instance 'bank-table
                                      :op :create
                                      :filter filter)))
      (with-document ()
        (:head
         (:title "Τράπεζα » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'bank)
               (bank-top-actions nil filter)
               (:div :class "grid_12"
                     (:div :id "bank-window" :class "window"
                           (:div :class "title" "Δημιουργία")
                           (actions bank-table)
                           (notifications)
                           (with-form (actions/config/bank/create :search (val search))
                             (display bank-table
                                      :key nil
                                      :payload (params->values :payload)))))
               (footer)))))))

(defpage bank-page actions/config/bank/create ("actions/config/bank/create" :request-type :post)
    ((title  string chk-bank-title/create t)
     (search string))
  (with-controller-page (config/bank/create)
    (let ((new-bank (make-instance 'bank :title (val title))))
      (insert-dao new-bank)
      (see-other (config/bank :bank-id (bank-id new-bank))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage bank-page config/bank/update ("config/bank/update")
    ((bank-id integer chk-bank-id                           t)
     (title   string  (chk-bank-title/update title bank-id))
     (search  string))
  (with-view-page
    (let* ((filter (params->values :filter))
           (bank-table (make-instance 'bank-table
                                      :op :update
                                      :filter filter)))
      (with-document ()
        (:head
         (:title "Τράπεζα » Επεξεργασία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'bank)
               (bank-top-actions (val bank-id) filter)
               (:div :class "grid_12"
                     (:div :id "bank-window" :class "window"
                           (:div :class "title" "Επεξεργασία")
                           (actions bank-table :key (val bank-id))
                           (notifications)
                           (with-form (actions/config/bank/update :bank-id (val bank-id)
                                                                  :search (val search))
                             (display bank-table
                                      :key (val bank-id)
                                      :payload (params->values :payload)))))
               (footer)))))))

(defpage bank-page actions/config/bank/update ("actions/config/bank/update" :request-type :post)
    ((bank-id integer chk-bank-id                           t)
     (title   string  (chk-bank-title/update title bank-id) t)
     (search  string))
  (with-controller-page (config/bank/update)
    (execute (:update 'bank :set
                      'title (val title)
                      :where (:= 'id (val bank-id))))
    (see-other (config/bank :bank-id (val bank-id) :search (val search)))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage bank-page config/bank/delete ("config/bank/delete")
    ((bank-id integer chk-bank-id/ref t)
     (search  string))
  (with-view-page
    (let* ((filter (params->values :filter))
           (bank-table (make-instance 'bank-table
                                      :op :delete
                                      :filter filter)))
      (with-document ()
        (:head
         (:title "Τράπεζα » Διαγραφή")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'bank)
               (bank-top-actions (val bank-id) filter)
               (:div :class "grid_12"
                     (:div :id "bank-window" :class "window"
                           (:div :class "title" "Διαγραφή")
                           (actions bank-table :key (val bank-id))
                           (with-form (actions/config/bank/delete :bank-id (val bank-id)
                                                                  :search (val search))
                             (display bank-table
                                      :key (val bank-id)))))
               (footer)))))))

(defpage bank-page actions/config/bank/delete ("actions/config/bank/delete" :request-type :post)
    ((bank-id integer chk-bank-id/ref t)
     (search  string))
  (with-controller-page (config/bank/delete)
    (delete-dao (get-dao 'bank (val bank-id)))
    (see-other (config/bank :search (val search)))))
