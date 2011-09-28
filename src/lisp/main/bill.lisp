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
    :initform '(search cstate))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))
   (messages
    :allocation :class
    :reader messages
    :initform '((amount (:non-positive-amount  "Το κόστος πρέπει να είναι θετικός αριθμός"
                         :parse-error  "Το κόστος περιέχει άκυρους χαρακτήρες"))))))



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
    (query (:order-by (:select (:as 'id 'bill-id) 'tag 'amount 'rank
                               :from 'bill
                               :where (:= 'project-id (project-id table)))
                      'rank)
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
                             :css-class (string-downcase name)
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

(defun bill-actions (op id bill-id filter)
  (actions-menu
   `((:create ,(html ()
                 (:a :class "create"
                     :href (apply #'bill/create :id id filter)
                     "Δημιουργία")))
     (:update ,(html ()
                 (:a :class "update"
                     :href (apply #'bill/update :id id :bill-id bill-id filter)
                     "Επεξεργασία")))
     (:delete ,(html ()
                 (:a :class "delete"
                     :href (apply #'bill/delete :id id :bill-id bill-id filter)
                     "Διαγραφή")))
     (:rank-up ,(make-instance 'form
                               :action (action/bill/rank-dec)
                               :reqtype :post
                               :hidden `(:id ,id :bill-id ,bill-id ,@filter)
                               :body (make-instance 'submit
                                                    :body "Πάνω" :css-class "up")))
     (:rank-down ,(make-instance 'form
                                 :action (action/bill/rank-inc)
                                 :reqtype :post
                                 :hidden `(:id ,id :bill-id ,bill-id ,@filter)
                                 :body (make-instance 'submit
                                                      :body "Κάτω" :css-class "down"))))
   (crud+ranks-actions-enabled/disabled op bill-id)
   #'menu))



;;; ----------------------------------------------------------------------
;;; RANKS
;;; ----------------------------------------------------------------------

(defpage bill-page action/bill/rank-inc ("action/bill/rank-inc" :request-type :post)
    ((search  string)
     (cstate  string)
     (id      integer chk-project-id           t)
     (bill-id integer (chk-bill-id id bill-id) t))
  (with-controller-page nil
    (swap-ranks (get-dao 'bill (val bill-id)) +1)
    (see-other (apply #'project/details :id (val id)
                                        :bill-id (val bill-id)
                                        (params->filter)))))

(defpage bill-page action/bill/rank-dec ("action/bill/rank-dec" :request-type :post)
    ((search  string)
     (cstate  string)
     (id      integer chk-project-id           t)
     (bill-id integer (chk-bill-id id bill-id) t))
  (with-controller-page nil
    (swap-ranks (get-dao 'bill (val bill-id)) -1)
    (see-other (apply #'project/details :id (val id)
                                        :bill-id (val bill-id)
                                        (params->filter)))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage bill-page bill/create ("project/details/bill/create")
    ((search string)
     (cstate string)
     (id     integer chk-project-id t)
     (tag    string)
     (amount float   chk-amount*))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :details
                                        :record (get-record 'project (val id))
                                        :cancel-url (apply #'project :id (val id) filter)))
           (bill-table (make-instance 'bill-table
                                      :op :create
                                      :project-id (val id))))
      (with-document ()
        (:head
         (:title "Κοστολόγηση » Δημιουργία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'project)
               (project-subnavbar :details filter)
               (:div :id "project-window" :class "window grid_6"
                     (:div :class "title" "Λεπτομέρειες")
                     (project-actions :details (val id) filter)
                     (display project-form))
               (:div :id "bill-window" :class "window grid_6"
                     (:div :class "title" "Δημιουργία")
                     (bill-actions :create (val id) nil filter)
                     (notifications)
                     (with-form (actions/bill/create :id (val id))
                       (display bill-table
                                :payload (params->payload))))
               (footer)))))))

(defpage bill-page actions/bill/create ("actions/bill/create" :request-type :post)
    ((search string)
     (cstate string)
     (id     integer chk-project-id)
     (tag    string)
     (amount float   chk-amount*))
  (with-controller-page (bill/create)
    (let ((new-bill (make-instance 'bill
                                   :project-id (val id)
                                   :tag (val tag)
                                   :amount (val amount))))
      (setf (rank new-bill) (1+ (max-rank new-bill)))
      (insert-dao new-bill)
      (see-other (apply #'project/details :id (val id) :bill-id (bill-id new-bill)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage bill-page bill/update ("project/details/bill/update")
    ((search  string)
     (cstate  string)
     (id      integer chk-project-id           t)
     (bill-id integer (chk-bill-id id bill-id) t)
     (tag     string)
     (amount  float   chk-amount*))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :details
                                        :record (get-record 'project (val id))
                                        :cancel-url (apply #'project :id (val id) filter)))
           (bill-table (make-instance 'bill-table
                                      :op :update
                                      :project-id (val id))))
      (with-document ()
        (:head
         (:title "Κοστολόγηση » Επεξεργασία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'project)
               (project-subnavbar :details filter)
               (:div :id "project-window" :class "window grid_6"
                     (:div :class "title" "Λεπτομέρειες")
                     (project-actions :details (val id) filter)
                     (display project-form))
               (:div :id "bill-window" :class "window grid_6"
                     (:div :class "title" "Επεξεργασία")
                     (bill-actions :update (val id) (val bill-id) filter)
                     (with-form (actions/bill/update :id (val id)
                                                     :bill-id (val bill-id))
                       (display bill-table :key (val bill-id)
                                           :payload (params->payload))))
               (footer)))))))

(defpage bill-page actions/bill/update ("actions/bill/update"
                                        :request-type :post)
    ((search  string)
     (cstate  string)
     (id      integer chk-project-id           t)
     (bill-id integer (chk-bill-id id bill-id) t)
     (tag     string)
     (amount  float   chk-amount*))
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
     (cstate  string)
     (id      integer chk-project-id           t)
     (bill-id integer (chk-bill-id id bill-id) t))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :details
                                        :record (get-record 'project (val id))
                                        :cancel-url (apply #'project :id (val id) filter)))
           (bill-table (make-instance 'bill-table
                                      :op :delete
                                      :project-id (val id))))
      (with-document ()
        (:head
         (:title "Κοστολόγηση » Διαγραφή")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'project)
               (project-subnavbar :details filter)
               (:div :id "project-window" :class "window grid_6"
                     (:div :class "title" "Λεπτομέρειες")
                     (project-actions :details (val id) filter)
                     (display project-form))
               (:div :id "bill-window" :class "window grid_6"
                     (:div :class "title" "Διαγραφή")
                     (bill-actions :delete (val id) (val bill-id) filter)
                     (with-form (actions/bill/delete :id (val id)
                                                     :bill-id (val bill-id))
                       (display bill-table :key (val bill-id))))
               (footer)))))))

(defpage bill-page actions/bill/delete ("actions/bill/delete"
                                        :request-type :post)
    ((search  string)
     (cstate  string)
     (id      integer chk-project-id           t)
     (bill-id integer (chk-bill-id id bill-id) t))
  (with-controller-page (bill/delete)
    (with-transaction ()
      (let ((dao  (get-dao 'bill (val bill-id))))
        (shift-higher-rank-daos dao -1)
        (delete-dao dao)))
    (see-other (apply #'project/details :id (val id)
                      (params->filter)))))