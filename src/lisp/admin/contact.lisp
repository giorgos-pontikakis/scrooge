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
;;; Contact pages
;;; ----------------------------------------------------------------------

(define-dynamic-page company/update/contact/create ("admin/company/update/contact/create")
    ((search     string)
     (id         integer chk-company-id t)
     (tag        string)
     (phone      string))
  (with-auth ("configuration")
    (no-cache)
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
             (:div :id "sidebar" :class "sidebar grid_3"
                   (company-notifications))
             (footer))))))

(define-dynamic-page company/update/contact/update ("admin/company/update/contact/update")
    ((search     string)
     (id         integer chk-company-id t)
     (contact-id integer)
     (tag        string)
     (phone      string))
  (with-auth ("configuration")
    (no-cache)
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
             (:div :id "sidebar" :class "sidebar grid_3"
                   (company-notifications))
             (footer))))))

(define-dynamic-page company/update/contact/delete ("admin/company/update/contact/delete")
    ((search     string)
     (id         integer chk-company-id t)
     (contact-id integer))
  (with-auth ("configuration")
    (no-cache)
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
             (:div :id "sidebar" :class "sidebar grid_3"
                   (company-notifications))
             (footer))))))

(defun company-contact-form (op company-id contact-id)
  (let ((contact-table (make-instance 'contact-table
                                      :op op
                                      :id contact-id
                                      :company-id company-id)))
    (with-html
      (:div :class "grid_5 omega company-data-form-contact-data"
            (display contact-table
                     :selected-id contact-id
                     :selected-data (get-contact-plist contact-id))))))