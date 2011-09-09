(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass bill-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id bill-id))
   (payload-parameter-names
    :allocation :class
    :initform '(tag amount))
   (filter-parameter-names
    :allocation :class
    :initform '(search status))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))))



;;; ----------------------------------------------------------------------
;;; Bill Table
;;; ----------------------------------------------------------------------

;;; table

(defclass bill-table (scrooge-table)
  ((header-labels  :initform nil)
   (paginator      :initform nil)
   (project-id     :accessor project-id :initarg :project-id))
  (:default-initargs :item-class 'bill-row :id "bill-table"))

(defmethod get-records ((table bill-table))
  (with-db ()
    (query (:order-by (:select (:as 'id 'bill-id) 'tag 'amount
                       :from 'bill
                       :where (:= 'project-id (project-id table)))
                      'tag)
           :plists)))


;;; rows

(defclass bill-row (scrooge-row/plist)
  ())

(defmethod selector ((row bill-row) enabled-p)
  (let ((table (collection row))
        (bill-id (getf (record row) :bill-id)))
    (html ()
      (:a :href (if enabled-p
                    (project/details :id (project-id table))
                    (project/details :id (project-id table) :bill-id bill-id))
          (selector-img enabled-p)))))

(defmethod payload ((row bill-row) enabled-p)
  (let ((record (record row))
        (disabled (not enabled-p)))
    (mapcar (lambda (name)
              (make-instance 'textbox
                             :name name
                             :value (getf record (make-keyword name))
                             :disabled disabled))
            '(tag amount))))

(defmethod controls ((row bill-row) enabled-p)
  (let ((table (collection row))
        (bill-id (getf (record row) :bill-id)))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (project/details :id (project-id table)
                                                    :bill-id bill-id)))
        (list nil nil))))

(defmethod key ((item bill-row))
  (getf (record item) :bill-id))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun bill-menu (id bill-id filter &optional disabled)
  (anchor-menu (crud-actions-spec (apply #'project/details
                                         :id id
                                         :bill-id bill-id
                                         filter)
                                  (apply #'bill/create
                                         :id id
                                         filter)
                                  (apply #'bill/update
                                         :id id
                                         :bill-id bill-id
                                         filter)
                                  (apply #'bill/delete
                                         :id id
                                         :bill-id bill-id
                                         filter))
               :id "bill-actions"
               :css-class "hmenu actions"
               :disabled disabled))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage bill-page bill/create ("project/details/bill/create")
    ((search string)
     (status string)
     (id     integer chk-project-id t))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :read
                                        :record (get-record 'project (val id))
                                        :cancel-url (apply #'project :id (val id) filter)))
           (bill-table (make-instance 'bill-table
                                      :op :create
                                      :project-id (val id))))
      (with-document ()
        (:head
         (:title "Επαφές » Δημιουργία")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'project)
               (:div :id "project-window" :class "window grid_6"
                     (:div :class "title" "Έργο » Λεπτομέρειες")
                     (project-menu (val id)
                                   filter
                                   '(:details :create))
                     (display project-form))
               (:div :id "bill-window" :class "window grid_6"
                     (:div :class "title" "Κοστολόγηση » Δημιουργία κόστους")
                     (bill-menu (val id)
                                nil
                                filter
                                '(:details :create :update :delete))
                     (with-form (actions/bill/create :id (val id))
                       (display bill-table)))
               (footer)))))))

(defpage bill-page actions/bill/create ("actions/bill/create"
                                        :request-type :post)
    ((search string)
     (status string)
     (id     integer chk-project-id)
     (tag    string)
     (amount string))
  (with-controller-page (bill/create)
    (let ((new-bill (make-instance 'bill
                                   :project-id (val id)
                                   :tag (val tag)
                                   :amount (val amount))))
      (insert-dao new-bill)
      (see-other (apply #'project/details :id (val id) :bill-id (bill-id new-bill)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage bill-page bill/update ("project/details/bill/update")
    ((search  string)
     (status  string)
     (id      integer chk-project-id t)
     (bill-id integer))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :read
                                        :record (get-record 'project (val id))
                                        :cancel-url (apply #'project :id (val id) filter)))
           (bill-table (make-instance 'bill-table
                                      :op :update
                                      :project-id (val id))))
      (with-document ()
        (:head
         (:title "Επεξεργασία Έργος > Επεξεργασία Επαφής")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'project)
               (:div :id "project-window" :class "window grid_6"
                     (:div :class "title" "Έργο » Λεπτομέρειες")
                     (project-menu (val id)
                                   filter
                                   '(:details :create))
                     (display project-form))
               (:div :id "bill-window" :class "window grid_6"
                     (:div :class "title" "Κοστολόγηση » Επεξεργασία κόστους")
                     (bill-menu (val id)
                                (val bill-id)
                                filter
                                '(:create :update))
                     (with-form (actions/bill/update :id (val id)
                                                     :bill-id (val bill-id))
                       (display bill-table :key (val bill-id))))
               (footer)))))))

(defpage bill-page actions/bill/update ("actions/bill/update"
                                        :request-type :post)
    ((search  string)
     (status  string)
     (id      integer chk-project-id)
     (bill-id integer (chk-bill-id id bill-id))
     (tag     string)
     (amount  string))
  (with-controller-page (bill/update)
    (execute (:update 'bill :set
                      'tag (val tag)
                      'amount (val amount)
                      :where (:= 'id (val bill-id))))
    (see-other (apply #'project/details :id (val id) :bill-id (val bill-id)
                      (params->filter)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage bill-page bill/delete ("project/details/bill/delete")
    ((search  string)
     (status  string)
     (id      integer chk-project-id t)
     (bill-id integer))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :read
                                        :record (get-record 'project (val id))
                                        :cancel-url (apply #'project :id (val id) filter)))
           (bill-table (make-instance 'bill-table
                                      :op :delete
                                      :project-id (val id))))
      (with-document ()
        (:head
         (:title "Επεξεργασία Επαφής")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'project)
               (:div :id "project-window" :class "window grid_6"
                     (:div :class "title" "Έργο » Λεπτομέρειες")
                     (project-menu (val id)
                                   filter
                                   '(:details :create))
                     (display project-form))
               (:div :id "bill-window" :class "window grid_6"
                     (:div :class "title" "Κοστολόγηση » Διαγραφή κόστους")
                     (bill-menu (val id)
                                (val bill-id)
                                filter
                                (if (val bill-id)
                                    '(:details)
                                    '(:details :update :delete)))
                     (with-form (actions/bill/delete :id (val id)
                                                     :bill-id (val bill-id))
                       (display bill-table :key (val bill-id))))
               (footer)))))))

(defpage bill-page actions/bill/delete ("actions/bill/delete"
                                        :request-type :post)
    ((search string)
     (status string)
     (id      integer chk-project-id)
     (bill-id integer (chk-bill-id id bill-id)))
  (with-controller-page (bill/delete)
    (delete-dao (get-dao 'bill (val bill-id)))
    (see-other (apply #'project/details :id (val id)
                      (params->filter)))))