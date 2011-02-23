(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; Actions
;;; ----------------------------------------------------------------------

(define-dynamic-page actions/admin/contact/create ("actions/admin/contact/create"
                                                   :request-type :post)
    ((company-id integer chk-company-id)
     (tag string)
     (phone string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp company-id)
        (with-db ()
          (let ((new-contact (make-instance 'contact
                                            :company-id (val company-id)
                                            :tag (val tag)
                                            :phone (val phone))))
            (insert-dao new-contact)
            (see-other (company/update :id (val company-id) :contact-id (id new-contact)))))
        (see-other (notfound)))))

(define-dynamic-page actions/admin/contact/update ("actions/admin/contact/update"
                                                   :request-type :post)
    ((company-id integer chk-company-id)
     (contact-id integer (chk-contact-id company-id contact-id))
     (tag string)
     (phone string))
  (with-auth ("configuration")
    (no-cache)
    (if (and (validp company-id) (validp contact-id))
        (with-db ()
          (execute (:update 'contact :set
                            'tag (val tag)
                            'phone (val phone)
                            :where (:= 'id (val contact-id))))
          (see-other (company/update :id (val company-id) :contact-id (val contact-id))))
        (see-other (notfound)))))

(define-dynamic-page actions/admin/contact/delete ("actions/admin/contact/delete"
                                                   :request-type :post)
    ((company-id integer chk-company-id)
     (contact-id integer (chk-contact-id company-id contact-id)))
  (with-auth ("configuration")
    (no-cache)
    (if (and (validp company-id) (validp contact-id))
        (with-db ()
          (delete-dao (get-dao 'contact (val contact-id)))
          (see-other (company/update :id (val company-id))))
        (see-other (notfound)))))



;;; ----------------------------------------------------------------------
;;; Contact Table
;;; ----------------------------------------------------------------------

;;; table

(defclass contact-table (crud-table)
  ((header-labels :initform '("" ""))
   (paginator     :initform nil)
   (company-id    :accessor company-id :initarg :company-id))
  (:default-initargs :item-class 'contact-row))

(defmethod read-records ((table contact-table))
  (with-db ()
    (query (:select (:as 'id 'contact-id) 'tag 'phone
                    :from 'contact
                    :where (:= 'company-id (company-id table)))
           :plists)))


;;; rows

(defclass contact-row (crud-row)
  ())

(defmethod cells ((row contact-row) &key)
  (let ((table (collection row))
        (contact-id (getf (record row) :contact-id)))
    (list :selector (make-instance 'selector-cell
                                   :states (list
                                            :on (company/update :id (company-id table))
                                            :off (company/update :id (company-id table)
                                                                 :contact-id contact-id)))
          :payload (mapcar (lambda (name)
                             (make-instance 'textbox-cell
                                            :name name
                                            :value (getf (record row) (make-keyword name))))
                           '(tag phone))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (company/update :id (company-id table)
                                                          :contact-id contact-id))))))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defun get-contact-plist (contact-id)
  (with-db ()
    (query (:select (:as 'id 'contact-id) 'tag 'phone
                    :from 'contact
                    :where (:= 'id contact-id))
           :plists)))


;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun contact-menu (id contact-id search &optional disabled-items)
  (display
   (make-instance 'actions-menu
                  :id "contact-actions"
                  :style "hnavbar actions grid_8 alpha"
                  :spec (crud-actions-spec (company/update :id id
                                                           :contact-id contact-id
                                                           :search search)
                                           (company/update/contact/create :id id
                                                                          :search search)
                                           (company/update/contact/update :id id
                                                                          :contact-id contact-id
                                                                          :search search)
                                           (company/update/contact/delete :id id
                                                                          :contact-id contact-id
                                                                          :search search)))
   :disabled-items disabled-items))



;;; ----------------------------------------------------------------------
;;; Contact pages
;;; ----------------------------------------------------------------------

(define-dynamic-page company/update/contact/create ("admin/company/update/contact/create")
    ((search     string)
     (id         integer chk-company-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let ((contact-table (make-instance 'contact-table
                                            :op 'create
                                            :company-id (val id))))
          (with-document ()
            (:head
             (:title "Επεξεργασία Εταιρίας > Δημιουργία Επαφής")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-navbar 'company)
                   (:div :id "company-window" :class "window grid_9"
                         (:div :class "title" "Επεξεργασία Εταιρίας > Δημιουργία Επαφής")
                         (company-menu (val id)
                                       (val search)
                                       '(details create))
                         (company-data-form 'details
                                            :search (val search)
                                            :data (get-company-plist (val id))))
                   (:div :id "contact-window" :class "window grid_6"
                         (:div :class "title" "Επαφές > Δημιουργία")
                         (contact-menu (val id)
                                       nil
                                       (val search)
                                       '(details create update delete))
                         (:div :class "grid_4 omega company-data-form-contact-data"
                               (with-form (actions/admin/contact/create :id (val id))
                                 (display contact-table))))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page company/update/contact/update ("admin/company/update/contact/update")
    ((search     string)
     (id         integer chk-company-id t)
     (contact-id integer))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let ((contact-table (make-instance 'contact-table
                                            :op 'update
                                            :company-id (val id))))
          (with-document ()
            (:head
             (:title "Επεξεργασία Εταιρίας > Επεξεργασία Επαφής")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-navbar 'company)
                   (:div :id "company-window" :class "window grid_9"
                         (:div :class "title" "Επεξεργασία Εταιρίας > Επεξεργασία Επαφής")
                         (company-menu (val id)
                                       (val search)
                                       '(details create))
                         (company-data-form 'details
                                            :search (val search)
                                            :data (get-company-plist (val id))))
                   (:div :id "contact-window" :class "window grid_6"
                         (:div :class "title" "Επαφές > Επεξεργασία")
                         (contact-menu (val id)
                                       (val contact-id)
                                       (val search)
                                       (if (val contact-id)
                                           '(details)
                                           '(details update delete)))
                         (:div :class "grid_4 omega company-data-form-contact-data"
                               (with-form (actions/admin/contact/update :id (val id)
                                                                        :contact-id (val contact-id))
                                 (display contact-table :selected-id (val contact-id)))))
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (company-notifications))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page company/update/contact/delete ("admin/company/update/contact/delete")
    ((search     string)
     (id         integer chk-company-id t)
     (contact-id integer))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let ((contact-table (make-instance 'contact-table
                                            :op 'delete
                                            :company-id (val id))))
          (with-document ()
            (:head
             (:title "Επεξεργασία Εταιρίας > Επεξεργασία Επαφής")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-navbar 'company)
                   (:div :id "company-window" :class "window grid_9"
                         (:div :class "title" "Επεξεργασία Εταιρίας > Διαγραφή Επαφής")
                         (company-menu (val id)
                                       (val search)
                                       '(details create))
                         (company-data-form 'details
                                            :search (val search)
                                            :data (get-company-plist (val id))))
                   (:div :id "contact-window" :class "window grid_6"
                         (:div :class "title" "Επαφές > Διαγραφή")
                         (contact-menu (val id)
                                       (val contact-id)
                                       (val search)
                                       (if (val contact-id)
                                           '(details)
                                           '(details update delete)))
                         (:div :class "grid_4 omega company-data-form-contact-data"
                               (with-form (actions/admin/contact/delete :id (val id)
                                                                        :contact-id (val contact-id))
                                 (display contact-table :selected-id (val contact-id)))))
                   (footer)))))
        (see-other (notfound)))))
