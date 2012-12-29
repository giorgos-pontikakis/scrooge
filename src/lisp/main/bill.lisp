(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass bill-family (family-mixin)
  ()
  (:default-initargs :parameter-groups '(:system (project-id bill-id)
                                         :payload (tag amount)
                                         :filter (search cstate))))

(defclass bill-page (auth-dynamic-page bill-family)
  ((messages
    :allocation :class
    :reader messages
    :initform '((amount (:non-positive-amount  "Το κόστος πρέπει να είναι θετικός αριθμός"
                         :parse-error  "Το κόστος περιέχει άκυρους χαρακτήρες"))))))



;;; ----------------------------------------------------------------------
;;; Bill Table
;;; ----------------------------------------------------------------------

;;; table

(defclass bill-table (ranked-table-mixin scrooge-table)
  ((header-labels  :initform nil)
   (paginator      :initform nil)
   (project-id     :accessor project-id :initarg :project-id))
  (:default-initargs :item-class 'bill-row
                     :id "bill-table"
                     :css-class "crud-table crud-table-half"
                     :create-pos :last))

(defmethod get-records ((table bill-table))
  (query (:order-by (:select 'id 'tag 'amount 'rank
                             :from 'bill
                             :where (:= 'project-id (project-id table)))
                    'rank)
         :plists))

(defmethod actions ((tbl bill-table) &key)
  (let* ((bill-id (selected-key tbl))
         (project-id (project-id tbl))
         (filter (filter tbl))
         (spec `((:create
                  ,(html ()
                     (:a :class "create"
                       :href (apply #'bill/create :project-id project-id filter)
                       "Νέα Χρέωση")))
                 (:update
                  ,(html ()
                     (:a :class "update"
                       :href (apply #'bill/update :project-id project-id :bill-id bill-id filter)
                       "Επεξεργασία")))
                 (:delete
                  ,(html ()
                     (:a :class "delete"
                       :href (apply #'bill/delete :project-id project-id :bill-id bill-id filter)
                       "Διαγραφή")))
                 (:rank-up
                  ,(make-instance 'form
                                  :action (action/bill/rank-dec)
                                  :reqtype :post
                                  :hidden `(:project-id ,project-id :bill-id ,bill-id ,@filter)
                                  :body (make-instance 'submit
                                                       :body "Πάνω" :css-class "up")))
                 (:rank-down
                  ,(make-instance 'form
                                  :action (action/bill/rank-inc)
                                  :reqtype :post
                                  :hidden `(:project-id ,project-id :bill-id ,bill-id ,@filter)
                                  :body (make-instance 'submit
                                                       :body "Κάτω" :css-class "down"))))))
    (actions-menu spec
                  (disabled-actions tbl :key bill-id))))

;;; rows

(defclass bill-row (scrooge-row/plist)
  ())

(defmethod selector ((row bill-row) selected-p)
  (let ((project-id (project-id (collection row)))
        (bill-id (key row)))
    (html ()
      (:a :href (if selected-p
                    (project/details :project-id project-id)
                    (project/details :project-id project-id :bill-id bill-id))
        (selector-img selected-p)))))

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
        (bill-id (key row)))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (project/details :project-id (project-id table)
                                                    :bill-id bill-id)))
        (list nil nil))))



;;; ----------------------------------------------------------------------
;;; RANKS
;;; ----------------------------------------------------------------------

(defpage bill-page action/bill/rank-inc ("action/bill/rank-inc" :request-type :post)
    ((search     string)
     (cstate     string)
     (project-id integer chk-project-id                   t)
     (bill-id    integer (chk-bill-id project-id bill-id) t))
  (with-controller-page nil
    (swap-ranks (get-dao 'bill (val bill-id)) +1)
    (see-other (apply #'project/details :project-id (val project-id)
                                        :bill-id (val bill-id)
                                        (params->filter)))))

(defpage bill-page action/bill/rank-dec ("action/bill/rank-dec" :request-type :post)
    ((search  string)
     (cstate  string)
     (project-id      integer chk-project-id           t)
     (bill-id integer (chk-bill-id project-id bill-id) t))
  (with-controller-page nil
    (swap-ranks (get-dao 'bill (val bill-id)) -1)
    (see-other (apply #'project/details :project-id (val project-id)
                                        :bill-id (val bill-id)
                                        (params->filter)))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage bill-page bill/create ("project/details/bill/create")
    ((project-id integer chk-project-id t)
     (tag        string)
     (amount     float   chk-amount*)
     (search     string)
     (cstate     string))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :details
                                        :key (val project-id)
                                        :cancel-url (apply #'project
                                                           :project-id (val project-id)
                                                           filter)))
           (bill-table (make-instance 'bill-table
                                      :op :create
                                      :project-id (val project-id))))
      (with-document ()
        (:head
          (:title "Κοστολόγηση » Δημιουργία")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header)
            (main-navbar 'project)
            (project-top-actions :details)
            (project-tabs (val project-id)
                          (html ()
                            (:div :class "grid_6 alpha"
                              (:div :id "project-window" :class "window"
                                (:div :class "title" "Λεπτομέρειες")
                                (actions project-form :filter filter)
                                (display project-form)))
                            (:div :class "grid_6 omega"
                              (:div :id "bill-window" :class "window"
                                (:div :class "title" "Δημιουργία")
                                (actions bill-table)
                                (notifications)
                                (with-form
                                    (actions/bill/create :project-id (val project-id))
                                  (display bill-table
                                           :payload (params->payload)))))))
            (footer)))))))

(defpage bill-page actions/bill/create ("actions/bill/create" :request-type :post)
    ((project-id integer chk-project-id)
     (tag        string)
     (amount     float   chk-amount*)
     (search     string)
     (cstate     string))
  (with-controller-page (bill/create)
    (let ((new-bill (make-instance 'bill
                                   :project-id (val project-id)
                                   :tag (val tag)
                                   :amount (val amount))))
      (setf (rank new-bill) (1+ (max-rank new-bill)))
      (insert-dao new-bill)
      (see-other (apply #'project/details :project-id (val project-id)
                                          :bill-id (bill-id new-bill)
                                          (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage bill-page bill/update ("project/details/bill/update")
    ((project-id integer chk-project-id                   t)
     (bill-id    integer (chk-bill-id project-id bill-id) t)
     (tag        string)
     (amount     float   chk-amount*)
     (search     string)
     (cstate     string))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :details
                                        :key (val project-id)
                                        :cancel-url (apply #'project
                                                           :project-id (val project-id)
                                                           filter)))
           (bill-table (make-instance 'bill-table
                                      :op :update
                                      :selected-key (val bill-id)
                                      :project-id (val project-id))))
      (with-document ()
        (:head
          (:title "Κοστολόγηση » Επεξεργασία")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header)
            (main-navbar 'project)
            (project-top-actions :details)
            (project-tabs (val project-id)
                          (html ()
                            (:div :class "grid_6 alpha"
                              (:div :id "project-window" :class "window"
                                (:div :class "title" "Λεπτομέρειες")
                                (actions project-form :filter filter)
                                (display project-form)))
                            (:div :class "grid_6 omega"
                              (:div :id "bill-window" :class "window"
                                (:div :class "title" "Επεξεργασία")
                                (actions bill-table)
                                (with-form (actions/bill/update :project-id (val project-id)
                                                                :bill-id (val bill-id))
                                  (display bill-table :payload (params->payload)))))))
            (footer)))))))

(defpage bill-page actions/bill/update ("actions/bill/update"
                                        :request-type :post)
    ((project-id integer chk-project-id                   t)
     (bill-id    integer (chk-bill-id project-id bill-id) t)
     (tag        string)
     (amount     float   chk-amount*)
     (search     string)
     (cstate     string))
  (with-controller-page (bill/update)
    (execute (:update 'bill :set
                      'tag (val tag)
                      'amount (val amount)
                      :where (:= 'id (val bill-id))))
    (see-other (apply #'project/details :project-id (val project-id) :bill-id (val bill-id)
                      (params->filter)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage bill-page bill/delete ("project/details/bill/delete")
    ((project-id integer chk-project-id                   t)
     (bill-id    integer (chk-bill-id project-id bill-id) t)
     (search     string)
     (cstate     string))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :details
                                        :key (val project-id)
                                        :cancel-url (apply #'project
                                                           :project-id (val project-id)
                                                           filter)))
           (bill-table (make-instance 'bill-table
                                      :op :delete
                                      :selected-key (val bill-id)
                                      :project-id (val project-id))))
      (with-document ()
        (:head
          (:title "Κοστολόγηση » Διαγραφή")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header)
            (main-navbar 'project)
            (project-top-actions :details)
            (project-tabs (val project-id)
                          (html ()
                            (:div :class "grid_6 alpha"
                              (:div :id "project-window" :class "window"
                                (:div :class "title" "Λεπτομέρειες")
                                (actions project-form :filter filter)
                                (display project-form)))
                            (:div :class "grid_6 omega"
                              (:div :id "bill-window" :class "window"
                                (:div :class "title" "Διαγραφή")
                                (actions bill-table)
                                (with-form (actions/bill/delete :project-id (val project-id)
                                                                :bill-id (val bill-id))
                                  (display bill-table))))))
            (footer)))))))

(defpage bill-page actions/bill/delete ("actions/bill/delete"
                                        :request-type :post)
    ((project-id integer chk-project-id                   t)
     (bill-id    integer (chk-bill-id project-id bill-id) t)
     (search     string)
     (cstate     string))
  (with-controller-page (bill/delete)
    (with-transaction ()
      (let ((dao  (get-dao 'bill (val bill-id))))
        (shift-higher-rank-daos dao -1)
        (delete-dao dao)))
    (see-other (apply #'project/details :project-id (val project-id)
                      (params->filter)))))
