(in-package :scrooge)


;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass company-tx-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id tx-id start))
   (payload-parameter-names
    :allocation :class
    :initform '())
   (filter-parameter-names
    :allocation :class
    :initform '(search subset role since until))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun company-tx-filters (filter)
  (let* ((url-fn #'company/details/tx)
         (filter* (remove-from-plist filter :role))
         (filter-spec `((nil      ,(apply url-fn filter*)
                                  "Μικτός ρόλος")
                        (customer  ,(apply url-fn :role "customer" filter*)
                                   "Πελάτης")
                        (supplier ,(apply url-fn :role "supplier" filter*)
                                  "Προμηθευτής"))))
    (display (filter-navbar filter-spec
                            :active (getf filter :role)))
    (display (datebox (lambda (&rest args)
                        (apply url-fn args))
                      filter))))



;;; ------------------------------------------------------------
;;; Company tx table
;;; ------------------------------------------------------------

(defun cheque-row-p (row)
  (let ((due-date (getf row :due-date)))
    (if (eql due-date :null)
        nil
        due-date)))

;; (defun pending-cheque-row-p (row)
;;   (let ((due-date (cheque-row-p row)))
;;     (if (and due-date
;;              (string= (getf row :state-id) "pending"))
;;         due-date
;;         nil)))

;; (defun due-or-tx-date (row)
;;   (if-let (due-date (pending-cheque-row-p row))
;;     due-date
;;     (getf row :tx-date)))


;;;  Customer

(defun customer-debits ()
  `(:in tx.credit-acc-id (:set ,@*revenues-accounts*)))

(defun customer-cash-credits ()
  `(:= tx.debit-acc-id ,*cash-acc-id*))

(defun customer-contra-credits ()
  `(:in tx.debit-acc-id (:set ,@*revenues-accounts*)))

(defun customer-cheque-credits ()
  ;; subquery receives cheque-event.cheque-id from main query
  `(:and (:= tx.debit-acc-id ,*cheque-receivable-acc-id*)
         (:not (:exists (:select 1
                         :from (:as tx tx2)
                         :inner-join (:as cheque-event cheque-event2)
                         :on (:= cheque-event2.tx-id tx2.id)
                         :where (:and
                                 (:= cheque-event2.cheque-id
                                     cheque-event.cheque-id)
                                 (:= tx2.credit-acc-id
                                     ,*cheque-receivable-acc-id*)))))))

;;;  Supplier

(defun supplier-credits ()
  `(:in tx.debit-acc-id (:set ,@*expense-accounts*)))

(defun supplier-cash-debits ()
  `(:= tx.credit-acc-id ,*cash-acc-id*))

(defun supplier-contra-debits ()
  `(:in tx.credit-acc-id (:set ,@*expense-accounts*)))

(defun supplier-cheque-debits ()
  ;; subquery receives cheque-event.cheque-id from main query
  `(:and (:= tx.credit-acc-id ,*cheque-payable-acc-id*)
         (:not (:exists (:select 1
                         :from (:as tx tx2)
                         :inner-join (:as cheque-event cheque-event2)
                         :on (:= cheque-event2.tx-id tx2.id)
                         :where (:and
                                 (:= cheque-event2.cheque-id
                                     cheque-event.cheque-id)
                                 (:= tx2.debit-acc-id
                                     ,*cheque-payable-acc-id*)))))))




(defun company-debits (company-id roles &optional since until)
  (let ((base-query '(:select tx-date (:as tx.id tx-id) tx.description
                      (:as tx.amount debit-amount) cheque.due-date cheque.state-id
                      :from tx
                      :left-join cheque-event
                      :on (:= cheque-event.tx-id tx.id)
                      :left-join cheque
                      :on (:= cheque.id cheque-event.cheque-id)))
        (where-base `(:= tx.company-id ,company-id))
        (where-tx '())
        (where-dates nil))
    (when (member :customer roles)
      (push (customer-debits) where-tx))
    (when (member :supplier roles)
      (push (supplier-cash-debits) where-tx)
      (push (supplier-contra-debits) where-tx)
      (push (supplier-cheque-debits) where-tx))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since tx-date) where-dates))
    (when (and until (not (eql until :null)))
      (push `(:<= tx-date ,until) where-dates))
    (let ((sql `(:order-by (,@base-query
                            :where (:and ,where-base
                                         (:or ,@where-tx)
                                         ,@where-dates))
                           'tx-date)))
      (query (sql-compile sql)
             :plists))))

(defun company-credits (company-id roles &optional since until)
  (let ((base-query '(:select tx-date (:as tx.id tx-id) tx.description
                      (:as tx.amount credit-amount) cheque.due-date cheque.state-id
                      :from tx
                      :left-join cheque-event
                      :on (:= cheque-event.tx-id tx.id)
                      :left-join cheque
                      :on (:= cheque.id cheque-event.cheque-id)))
        (where-base `(:= tx.company-id ,company-id))
        (where-tx `())
        (where-dates nil))
    (when (member :customer roles)
      (push (customer-cash-credits) where-tx)
      (push (customer-contra-credits) where-tx)
      (push (customer-cheque-credits) where-tx))
    (when (member :supplier roles)
      (push (supplier-credits) where-tx))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since tx-date) where-dates))
    (when (and until (not (eql until :null)))
      (push `(:<= tx-date ,until) where-dates))
    (let ((sql `(:order-by (,@base-query
                            :where (:and ,where-base
                                         (:or ,@where-tx)
                                         ,@where-dates))
                           'tx-date)))
      (query (sql-compile sql)
             :plists))))

(defun company-debits/credits (company-id roles since until)
  (flet ((get-tx-date (row)
           (getf row :tx-date)))
    (let* ((sorted (stable-sort (nconc (company-debits company-id roles)
                                       (company-credits company-id roles))
                                #'local-time:timestamp<
                                :key #'get-tx-date))
           (truncated (remove-if (lambda (row)
                                   (or (and since
                                            (not (eql since :null))
                                            (timestamp< (get-tx-date row) since))
                                       (and until
                                            (not (eql until :null))
                                            (timestamp> (get-tx-date row) until))))
                                 sorted)))
      (let ((total 0)
            (debit-sum 0)
            (credit-sum 0))
        (dolist (row sorted)
          (let* ((debit (getf row :debit-amount 0))
                 (credit (getf row :credit-amount 0))
                 (delta (- debit credit)))
            (setf total (+ total delta))
            (setf debit-sum (+ debit-sum debit))
            (setf credit-sum (+ credit-sum credit))
            (when (cheque-row-p row)
              (setf (getf row :description)
                    (concatenate 'string
                                 (getf row :description)
                                 " (Λήξη: "
                                 (lisp->html (getf row :due-date))
                                 ")")))
            (nconc row (list :total total))))
        (values (nreverse truncated) debit-sum credit-sum total)))))


;;; table

(defclass company-tx-table (scrooge-table)
  ((header-labels :initform '("" "Ημερομηνία" "Περιγραφή" "Χρέωση" "Πίστωση" "Υπόλοιπο" ""))
   (paginator     :initform (make-instance 'company-tx-paginator
                                            :id "company-tx-paginator"
                                            :css-class "paginator")
                  :initarg :paginator)
   (company-id    :accessor company-id :initarg :company-id))
  (:default-initargs :item-class 'company-tx-row :id "company-tx-table"))


;;; rows

(defclass company-tx-row (scrooge-row/plist)
  ())

(defmethod key ((row company-tx-row))
  (getf (record row) :tx-id))

(defmethod selector ((row company-tx-row) selected-p)
  (let* ((table (collection row))
         (tx-id (key row))
         (company-id (company-id table))
         (filter (filter table))
         (start (page-start (paginator table) (index row) (start-index table))))
    (html ()
      (:a :href (if selected-p
                    (apply #'company/details/tx :id company-id
                                                :start start filter)
                    (apply #'company/details/tx :id company-id
                                                :tx-id tx-id filter))
          (selector-img selected-p)))))

(defmethod controls ((row company-tx-row) controls-p)
  (simple-controls row controls-p #'company/details/tx))

(defmethod payload ((row company-tx-row) enabled-p)
  (let ((record (record row)))
    (mapcar (lambda (name)
              (make-instance 'textbox
                             :name name
                             :value (getf record (make-keyword name))
                             :disabled (not enabled-p)))
            '(tx-date description debit-amount credit-amount total))))


;;; paginator

(defclass company-tx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg company-tx-paginator) start)
  (apply #'company/details/tx :start start :id (company-id (table pg)) (filter (table pg))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage company-tx-page company/details/tx ("company/details/tx")
    ((search string)
     (subset string)
     (id     integer chk-company-id t)
     (tx-id  integer chk-tx-id)
     (since  date)
     (until  date)
     (start  integer)
     (role   string))
  (with-view-page
    (let ((filter (params->filter))
          (company-filter (params->company-filter))
          (roles (if (val role)
                     (list (make-keyword (string-upcase (val role))))
                     (list :customer :supplier))))
      (multiple-value-bind (records debit-sum credit-sum total)
          (company-debits/credits (val id) roles (val since) (val until))
        (with-document ()
          (:head
           (:title "Εταιρία » Λεπτομέρειες » Συναλλαγές")
           (main-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header)
                 (main-navbar 'company)
                 (company-top-actions :tx (val id)
                                      company-filter)
                 (company-tabs (val id) company-filter 'tx
                               (html ()
                                 (:div :class "secondary-filter-area"
                                       (company-tx-filters (list* :id (val id) filter)))
                                 (:div :id "company-tx-window" :class "window"
                                       (:div :class "title" "Συναλλαγές")
                                       (display (make-instance 'company-tx-table
                                                               :records records
                                                               :company-id (val id)
                                                               :op :details
                                                               :filter filter
                                                               :start-index (val start))
                                                :key (val tx-id))
                                       (:h4 "Σύνολο Χρεώσεων: " (fmt "~9,2F" debit-sum))
                                       (:h4 "Σύνολο Πιστώσεων: " (fmt "~9,2F" credit-sum))
                                       (:h4 "Γενικό Σύνολο: " (fmt "~9,2F" total)))))
                 (footer))))))))

(defpage company-tx-page company/details/tx/print ("company/details/tx/print")
    ((search string)
     (subset string)
     (id     integer chk-company-id t)
     (tx-id  integer chk-tx-id)
     (since  date)
     (until  date)
     (role   string))
  (with-view-page
    (let ((filter (params->filter))
          (system (params->plist #'val-or-raw (list id tx-id)))
          (misc  (params->plist #'val-or-raw (list since until)))
          (roles (if (val role)
                     (list (make-keyword (string-upcase (val role))))
                     (list :customer :supplier))))
      (multiple-value-bind (records debit-sum credit-sum)
          (company-debits/credits (val id) roles (val since) (val until))
        (with-document ()
          (:head
           (:title "Εταιρία » Λεπτομέρειες » Συναλλαγές » Εκτύπωση")
           (print-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (:div :class "grid_12"
                       (:a :id "back"
                           :href (apply #'company/details/tx (append system filter misc))
                           "« Επιστροφή")
                       (:div :id "company-tx-window" :class "window"
                             (:div :class "title"
                                   (:h3 (str (title (get-dao 'company (val id)))))
                                   (display (datebox #'company/details/tx/print
                                                     (append system filter misc))))
                             (display (make-instance 'company-tx-table
                                                     :records records
                                                     :company-id (val id)
                                                     :op :details
                                                     :filter (append system filter misc)
                                                     :paginator nil))
                             (:h4 "Σύνολο χρεώσεων: " (fmt "~9,2F" debit-sum))
                             (:h4 "Σύνολο πιστώσεων: " (fmt "~9,2F" credit-sum))
                             (:h4 "Γενικό Σύνολο: " (fmt "~9,2F" (- debit-sum
                                                                    credit-sum))))))))))))
