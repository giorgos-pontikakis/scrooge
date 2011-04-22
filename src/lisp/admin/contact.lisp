(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; Actions
;;; ----------------------------------------------------------------------

(define-dynamic-page actions/admin/contact/create ("actions/admin/contact/create"
                                                   :request-type :post)
    ((id integer chk-company-id)
     (tag string)
     (phone string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (let ((new-contact (make-instance 'contact
                                            :company-id (val id)
                                            :tag (val tag)
                                            :phone (val phone))))
            (insert-dao new-contact)
            (see-other (company/details :id (val id) :contact-id (id new-contact)))))
        (see-other (notfound)))))

(define-dynamic-page actions/admin/contact/update ("actions/admin/contact/update"
                                                   :request-type :post)
    ((id integer chk-company-id)
     (contact-id integer (chk-contact-id id contact-id))
     (tag string)
     (phone string))
  (with-auth ("configuration")
    (no-cache)
    (if (and (validp id) (validp contact-id))
        (with-db ()
          (execute (:update 'contact :set
                            'tag (val tag)
                            'phone (val phone)
                            :where (:= 'id (val contact-id))))
          (see-other (company/details :id (val id) :contact-id (val contact-id))))
        (see-other (notfound)))))

(define-dynamic-page actions/admin/contact/delete ("actions/admin/contact/delete"
                                                   :request-type :post)
    ((id integer chk-company-id)
     (contact-id integer (chk-contact-id id contact-id)))
  (with-auth ("configuration")
    (no-cache)
    (if (and (validp id) (validp contact-id))
        (with-db ()
          (delete-dao (get-dao 'contact (val contact-id)))
          (see-other (company/details :id (val id))))
        (see-other (notfound)))))



;;; ----------------------------------------------------------------------
;;; Contact Table
;;; ----------------------------------------------------------------------

;;; table

(defclass contact-table (scrooge-crud-table)
  ((item-key-field :initform :contact-id)
   (header-labels  :initform '("" ""))
   (paginator      :initform nil)
   (company-id     :accessor company-id :initarg :company-id))
  (:default-initargs :item-class 'contact-row :id "contact-table"))

(defmethod read-records ((table contact-table))
  (with-db ()
    (query (:order-by (:select (:as 'id 'contact-id) 'tag 'phone
                               :from 'contact
                               :where (:= 'company-id (company-id table)))
                      'tag)
           :plists)))


;;; rows

(defclass contact-row (scrooge-crud-row)
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



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun contact-menu (id contact-id filter &optional disabled)
  (menu (crud-actions-spec (apply #'company/update
                                  :id id
                                  :contact-id contact-id
                                  filter)
                           (apply #'company/details/contact/create
                                  :id id
                                  filter)
                           (apply #'company/details/contact/update
                                  :id id
                                  :contact-id contact-id
                                  filter)
                           (apply #'company/details/contact/delete
                                  :id id
                                  :contact-id contact-id filter))
        :id "contact-actions"
        :css-class "hmenu actions"
        :disabled disabled))



;;; ----------------------------------------------------------------------
;;; Contact pages
;;; ----------------------------------------------------------------------

(define-dynamic-page company/details/contact/create ("admin/company/details/contact/create")
    ((search     string)
     (id         integer chk-company-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let ((filter (parameters->plist search))
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
                         (company-data-form :details
                                            :filter filter
                                            :data (company-record (val id))))
                   (:div :id "contact-window" :class "window grid_6"
                         (:div :class "title" "Επαφές » Δημιουργία")
                         (contact-menu (val id)
                                       nil
                                       filter
                                       '(:details :create :update :delete))
                         (with-form (actions/admin/contact/create :id (val id))
                           (display contact-table)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page company/details/contact/update ("admin/company/details/contact/update")
    ((search     string)
     (id         integer chk-company-id t)
     (contact-id integer))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let ((filter (parameters->plist search))
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
                         (company-data-form :details
                                            :filter filter
                                            :data (company-record (val id))))
                   (:div :id "contact-window" :class "window grid_6"
                         (:div :class "title" "Επαφές » Επεξεργασία")
                         (contact-menu (val id)
                                       (val contact-id)
                                       filter
                                       '(:create :update))
                         (with-form (actions/admin/contact/update :id (val id)
                                                                  :contact-id (val contact-id))
                           (display contact-table :selected-id (val contact-id))))
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (company-notifications))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page company/details/contact/delete ("admin/company/details/contact/delete")
    ((search     string)
     (id         integer chk-company-id t)
     (contact-id integer))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let ((filter (parameters->plist search))
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
                         (company-data-form :details
                                            :filter filter
                                            :data (company-record (val id))))
                   (:div :id "contact-window" :class "window grid_6"
                         (:div :class "title" "Επαφές » Διαγραφή")
                         (contact-menu (val id)
                                       (val contact-id)
                                       filter
                                       (if (val contact-id)
                                           '(:details)
                                           '(:details :update :delete)))
                         (with-form (actions/admin/contact/delete :id (val id)
                                                                  :contact-id (val contact-id))
                           (display contact-table :selected-id (val contact-id))))
                   (footer)))))
        (see-other (notfound)))))
