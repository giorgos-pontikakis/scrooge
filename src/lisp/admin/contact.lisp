(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass contact-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id contact-id))
   (payload-parameter-names
    :allocation :class
    :initform '(tag phone))
   (filter-parameter-names
    :allocation :class
    :initform '(search))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))))



;;; ----------------------------------------------------------------------
;;; Contact Table
;;; ----------------------------------------------------------------------

;;; table

(defclass contact-table (scrooge-table)
  ((header-labels  :initform nil)
   (paginator      :initform nil)
   (company-id     :accessor company-id :initarg :company-id))
  (:default-initargs :item-class 'contact-row :id "contact-table"))

(defmethod get-records ((table contact-table))
  (with-db ()
    (query (:order-by (:select (:as 'id 'contact-id) 'tag 'phone
                       :from 'contact
                       :where (:= 'company-id (company-id table)))
                      'tag)
           :plists)))


;;; rows

(defclass contact-row (scrooge-row/plist)
  ())

(defmethod selector ((row contact-row) enabled-p)
  (let ((table (collection row))
        (contact-id (getf (record row) :contact-id)))
    (html ()
      (:a :href (if enabled-p
                    (company/details :id (company-id table))
                    (company/details :id (company-id table) :contact-id contact-id))
          (selector-img enabled-p)))))

(defmethod payload ((row contact-row) enabled-p)
  (let ((record (record row))
        (disabled (not enabled-p)))
    (mapcar (lambda (name)
              (make-instance 'textbox
                             :name name
                             :value (getf record (make-keyword name))
                             :disabled disabled))
            '(tag phone))))

(defmethod controls ((row contact-row) enabled-p)
  (let ((table (collection row))
        (contact-id (getf (record row) :contact-id)))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (company/update :id (company-id table)
                                                   :contact-id contact-id)))
        (list nil nil))))

(defmethod key ((item contact-row))
  (getf (record item) :contact-id))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun contact-menu (id contact-id filter &optional disabled)
  (anchor-menu (crud-actions-spec (apply #'company/details
                                         :id id
                                         :contact-id contact-id
                                         filter)
                                  (apply #'contact/create
                                         :id id
                                         filter)
                                  (apply #'contact/update
                                         :id id
                                         :contact-id contact-id
                                         filter)
                                  (apply #'contact/delete
                                         :id id
                                         :contact-id contact-id
                                         filter))
               :id "contact-actions"
               :css-class "hmenu actions"
               :disabled disabled))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage contact-page contact/create ("company/details/contact/create")
    ((search string)
     (id     integer chk-company-id t)
     (tag    string)
     (phone  string))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :read
                                        :record (get-record 'company (val id))
                                        :cancel-url (apply #'company :id (val id) filter)))
           (contact-table (make-instance 'contact-table
                                         :op :create
                                         :company-id (val id))))
      (with-document ()
        (:head
         (:title "Επαφές » Δημιουργία")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'company)
               (:div :id "company-window" :class "window grid_6"
                     (:div :class "title" "Εταιρία » Λεπτομέρειες")
                     (company-menu (val id)
                                   filter
                                   '(:details :create))
                     (display company-form))
               (:div :id "contact-window" :class "window grid_6"
                     (:div :class "title" "Επαφές » Δημιουργία")
                     (contact-menu (val id)
                                   nil
                                   filter
                                   '(:create :update :delete))
                     (with-form (actions/contact/create :id (val id))
                       (display contact-table)))
               (footer)))))))

(defpage contact-page actions/contact/create ("actions/contact/create"
                                              :request-type :post)
    ((search string)
     (id     integer chk-company-id)
     (tag    string)
     (phone  string))
  (with-controller-page (contact/create)
    (let ((new-contact (make-instance 'contact
                                      :company-id (val id)
                                      :tag (val tag)
                                      :phone (val phone))))
      (insert-dao new-contact)
      (see-other (apply #'company/details :id (val id) :contact-id (contact-id new-contact)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage contact-page contact/update ("company/details/contact/update")
    ((search     string)
     (id         integer chk-company-id                 t)
     (contact-id integer (chk-contact-id id contact-id) t)
     (tag        string)
     (phone      string))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :read
                                        :record (get-record 'company (val id))
                                        :cancel-url (apply #'company :id (val id) filter)))
           (contact-table (make-instance 'contact-table
                                         :op :update
                                         :company-id (val id))))
      (with-document ()
        (:head
         (:title "Επεξεργασία Εταιρίας > Επεξεργασία Επαφής")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'company)
               (:div :id "company-window" :class "window grid_6"
                     (:div :class "title" "Εταιρία » Λεπτομέρειες")
                     (company-menu (val id)
                                   filter
                                   '(:details :create))
                     (display company-form))
               (:div :id "contact-window" :class "window grid_6"
                     (:div :class "title" "Επαφές » Επεξεργασία")
                     (contact-menu (val id)
                                   (val contact-id)
                                   filter
                                   '(:read :update))
                     (with-form (actions/contact/update :id (val id)
                                                        :contact-id (val contact-id))
                       (display contact-table :key (val contact-id))))
               (footer)))))))

(defpage contact-page actions/contact/update ("actions/contact/update"
                                              :request-type :post)
    ((search     string)
     (id         integer chk-company-id                 t)
     (contact-id integer (chk-contact-id id contact-id) t)
     (tag        string)
     (phone      string))
  (with-controller-page (contact/update)
    (execute (:update 'contact :set
                      'tag (val tag)
                      'phone (val phone)
                      :where (:= 'id (val contact-id))))
    (see-other (apply #'company/details :id (val id) :contact-id (val contact-id)
                      (params->filter)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage contact-page contact/delete ("company/details/contact/delete")
    ((search     string)
     (id         integer chk-company-id                 t)
     (contact-id integer (chk-contact-id id contact-id) t))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :read
                                        :record (get-record 'company (val id))
                                        :cancel-url (apply #'company :id (val id) filter)))
           (contact-table (make-instance 'contact-table
                                         :op :delete
                                         :company-id (val id))))
      (with-document ()
        (:head
         (:title "Επεξεργασία Εταιρίας > Επεξεργασία Επαφής")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'company)
               (:div :id "company-window" :class "window grid_6"
                     (:div :class "title" "Εταιρία » Λεπτομέρειες")
                     (company-menu (val id)
                                   filter
                                   '(:details :create))
                     (display company-form))
               (:div :id "contact-window" :class "window grid_6"
                     (:div :class "title" "Επαφές » Διαγραφή")
                     (contact-menu (val id)
                                   (val contact-id)
                                   filter
                                   '(:read :delete))
                     (with-form (actions/contact/delete :id (val id)
                                                        :contact-id (val contact-id))
                       (display contact-table :key (val contact-id))))
               (footer)))))))

(defpage contact-page actions/contact/delete ("actions/contact/delete"
                                              :request-type :post)
    ((search     string)
     (id         integer chk-company-id                 t)
     (contact-id integer (chk-contact-id id contact-id) t))
  (with-controller-page (contact/delete)
    (delete-dao (get-dao 'contact (val contact-id)))
    (see-other (apply #'company/details :id (val id)
                      (params->filter)))))