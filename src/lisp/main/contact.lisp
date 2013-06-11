(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass contact-family (family-mixin)
  ()
  (:default-initargs
   :parameter-groups '(:system (company-id contact-id)
                       :payload (tag phone)
                       :filter (search subset))))

(defclass contact-page (auth-dynamic-page contact-family)
  ())



;;; ----------------------------------------------------------------------
;;; Contact Table
;;; ----------------------------------------------------------------------

;;; table

(defclass contact-table (ranked-table-mixin scrooge-crud-table)
  ((company-id :accessor company-id :initarg :company-id))
  (:default-initargs :record-class 'cons
                     :item-class 'contact-row
                     :create-pos :last
                     :id "contact-table"
                     :css-class "crud-table crud-table-half"
                     :header-labels nil))

(defmethod get-records ((table contact-table))
  (query (:order-by (:select 'id 'tag 'phone 'rank
                             :from 'contact
                             :where (:= 'company-id (company-id table)))
                    'rank)
         :plists))

(defmethod actions ((tbl contact-table) &key)
  (let* ((contact-id (selected-key tbl))
         (company-id (company-id tbl))
         (filter (filter tbl))
         (spec `((:create
                  ,(html ()
                     (:a :class "create"
                         :href (apply #'contact/create
                                      :company-id company-id filter)
                         "Νέα Επαφή")))
                 (:update
                  ,(html ()
                     (:a :class "update"
                         :href (apply #'contact/update
                                      :company-id company-id
                                      :contact-id contact-id
                                      filter)
                         "Επεξεργασία")))
                 (:delete
                  ,(html ()
                     (:a :class "delete"
                         :href (apply #'contact/delete
                                      :company-id company-id
                                      :contact-id contact-id
                                      filter)
                         "Διαγραφή")))
                 (:rank-up
                  ,(make-instance 'form
                                  :action (action/contact/rank-dec)
                                  :reqtype :post
                                  :hidden `(:company-id ,company-id :contact-id ,contact-id ,@filter)
                                  :body (make-instance 'submit
                                                       :body "Πάνω" :css-class "up")))
                 (:rank-down
                  ,(make-instance 'form
                                  :action (action/contact/rank-inc)
                                  :reqtype :post
                                  :hidden `(:company-id ,company-id :contact-id ,contact-id ,@filter)
                                  :body (make-instance 'submit
                                                       :body "Κάτω" :css-class "down"))))))
    (actions-menu spec
                  (disabled-actions tbl :key contact-id))))



;;; rows

(defclass contact-row (scrooge-row)
  ())

(defmethod selector ((row contact-row) selected-p)
  (let ((company-id (company-id (collection row)))
        (contact-id (key row)))
    (html ()
      (:a :href (if selected-p
                    (company/details :company-id company-id)
                    (company/details :company-id company-id :contact-id contact-id))
          (selector-img selected-p)))))

(defmethod payload ((row contact-row) enabled-p)
  (mapcar (textbox-maker (record row) enabled-p)
          '((tag :css-class "tag")
            (phone :css-class "phone"))))

(defmethod controls ((row contact-row) enabled-p)
  (let ((table (collection row))
        (contact-id (key row)))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (company/details :company-id (company-id table)
                                                    :contact-id contact-id)))
        (list nil nil))))



;;; ----------------------------------------------------------------------
;;; RANKS
;;; ----------------------------------------------------------------------

(defpage contact-page action/contact/rank-inc ("action/contact/rank-inc" :request-type :post)
    ((search     string)
     (subset     string  chk-subset)
     (cstate     string)
     (company-id         integer chk-company-id                 t)
     (contact-id integer (chk-contact-id company-id contact-id) t))
  (with-controller-page nil
    (swap-ranks (get-dao 'contact (val contact-id)) +1)
    (see-other (apply #'company/details :company-id (val company-id)
                                        :contact-id (val contact-id)
                                        (params->filter)))))

(defpage contact-page action/contact/rank-dec ("action/contact/rank-dec" :request-type :post)
    ((search     string)
     (subset     string  chk-subset)
     (cstate     string)
     (company-id         integer chk-company-id                 t)
     (contact-id integer (chk-contact-id company-id contact-id) t))
  (with-controller-page nil
    (swap-ranks (get-dao 'contact (val contact-id)) -1)
    (see-other (apply #'company/details :company-id (val company-id)
                                        :contact-id (val contact-id)
                                        (params->filter)))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage contact-page contact/create ("company/details/contact/create")
    ((search     string)
     (subset     string  chk-subset)
     (company-id integer chk-company-id t)
     (tag        string)
     (phone      string))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :details
                                        :key (val company-id)
                                        :cancel-url (apply #'company
                                                           :company-id (val company-id)
                                                           filter)))
           (contact-table (make-instance 'contact-table
                                         :op :create
                                         :company-id (val company-id))))
      (with-document ()
        (:head
         (:title "Επαφές » Δημιουργία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'main)
               (navbar 'main 'company)
               (company-top-actions :details)
               (company-tabs (val company-id) filter 'details
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Λεπτομέρειες")
                                           (actions company-form :filter filter)
                                           (display company-form)))
                               (:div :class "grid_6 omega"
                                     (:div :company-id "contact-window" :class "window"
                                           (:div :class "title" "Δημιουργία")
                                           (actions contact-table)
                                           (with-form
                                               (actions/contact/create :company-id (val company-id))
                                             (display contact-table))))))
               (footer)))))))

(defpage contact-page actions/contact/create ("actions/contact/create" :request-type :post)
    ((search     string)
     (subset     string  chk-subset)
     (company-id integer chk-company-id)
     (tag        string)
     (phone      string))
  (with-controller-page (contact/create)
    (let ((new-contact (make-instance 'contact
                                      :company-id (val company-id)
                                      :tag (val tag)
                                      :phone (val phone))))
      (setf (rank new-contact) (1+ (max-rank new-contact)))
      (insert-dao new-contact)
      (see-other (apply #'company/details :company-id (val company-id)
                                          :contact-id (contact-id new-contact)
                                          (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage contact-page contact/update ("company/details/contact/update")
    ((search     string)
     (subset     string  chk-subset)
     (company-id integer chk-company-id                         t)
     (contact-id integer (chk-contact-id company-id contact-id) t)
     (tag        string)
     (phone      string))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :details
                                        :key (val company-id)
                                        :cancel-url (apply #'company
                                                           :company-id (val company-id)
                                                           filter)))
           (contact-table (make-instance 'contact-table
                                         :op :update
                                         :selected-key (val contact-id)
                                         :company-id (val company-id))))
      (with-document ()
        (:head
         (:title "Επαφές » Επεξεργασία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'main)
               (navbar 'main 'company)
               (company-top-actions :details)
               (company-tabs (val company-id) filter 'details
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Λεπτομέρειες")
                                           (actions company-form :filter filter)
                                           (display company-form)))
                               (:div :class "grid_6 omega"
                                     (:div :id "contact-window" :class "window"
                                           (:div :class "title" "Επεξεργασία")
                                           (actions contact-table)
                                           (with-form
                                               (actions/contact/update :company-id (val company-id)
                                                                       :contact-id (val contact-id))
                                             (display contact-table))))))
               (footer)))))))

(defpage contact-page actions/contact/update ("actions/contact/update"
                                              :request-type :post)
    ((search     string)
     (subset     string  chk-subset)
     (company-id integer chk-company-id                         t)
     (contact-id integer (chk-contact-id company-id contact-id) t)
     (tag        string)
     (phone      string))
  (with-controller-page (contact/update)
    (execute (:update 'contact :set
                      'tag (val tag)
                      'phone (val phone)
                      :where (:= 'id (val contact-id))))
    (see-other (apply #'company/details :company-id (val company-id) :contact-id (val contact-id)
                      (params->filter)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage contact-page contact/delete ("company/details/contact/delete")
    ((search     string)
     (subset     string  chk-subset)
     (company-id integer chk-company-id                         t)
     (contact-id integer (chk-contact-id company-id contact-id) t))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :details
                                        :key (val company-id)
                                        :cancel-url (apply #'company
                                                           :company-id (val company-id)
                                                           filter)))
           (contact-table (make-instance 'contact-table
                                         :op :delete
                                         :selected-key (val contact-id)
                                         :company-id (val company-id))))
      (with-document ()
        (:head
         (:title "Επαφές » Διαγραφή")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'main)
               (navbar 'main 'company)
               (company-top-actions :details)
               (company-tabs (val company-id) filter 'details
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Λεπτομέρειες")
                                           (actions company-form :filter filter)
                                           (display company-form)))
                               (:div :class "grid_6 omega"
                                     (:div :id "contact-window" :class "window"
                                           (:div :class "title" "Διαγραφή")
                                           (actions contact-table)
                                           (with-form
                                               (actions/contact/delete :company-id (val company-id)
                                                                       :contact-id (val contact-id))
                                             (display contact-table))))))
               (footer)))))))

(defpage contact-page actions/contact/delete ("actions/contact/delete"
                                              :request-type :post)
    ((search     string)
     (subset     string  chk-subset)
     (company-id integer chk-company-id                         t)
     (contact-id integer (chk-contact-id company-id contact-id) t))
  (with-controller-page (contact/delete)
    (with-transaction ()
      (let ((dao (get-dao 'contact (val contact-id))))
        (shift-higher-rank-daos dao -1)
        (delete-dao dao)))
    (see-other (apply #'company/details :company-id (val company-id)
                      (params->filter)))))
