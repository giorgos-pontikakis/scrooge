(in-package :scrooge)


;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass company-tx-family (family-mixin)
  ()
  (:default-initargs :parameter-groups '(:system (company-id tx-id start
                                                  subset role)   ;; checked filter parameters
                                         :payload (tx-date description debit-amount credit-amount)
                                         :filter (search subset role since until))))

(defclass company-tx-page (auth-dynamic-page company-tx-family)
  ((messages
    :allocation :class
    :reader messages
    :initform
    '(((debit-amount credit-amount)
       (:empty-amount
        "Το ποσό της συναλλαγής είναι κενό"
        :non-positive-amount
        "Το ποσό της συναλλαγής δεν είναι θετικός αριθμός"
        :amount-overflow
        "Το ποσό της συναλλαγής είναι δεν πρέπει να ξεπερνά το 9,999,999.99"
        :parse-error
        "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
      (tx-date
       (:date-null
        "Η ημερομηνία της συναλλαγής είναι κενή"
        :parse-error
        "Η ημερομηνία της συναλλαγής είναι άκυρη"))))))

(defun chk-role (role)
  (if (or (eql role :null)
          (member role '("customer" "supplier") :test #'string-equal))
      nil
      :invalid-role))



;;; ------------------------------------------------------------
;;; Utilities
;;; ------------------------------------------------------------

(defun tx-roles (&optional role)
  (if role
      (list (make-keyword (string-upcase role)))
      (list :customer :supplier)))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun company-tx-top-actions (op)
  (top-actions-area
   (make-instance 'scrooge-menu
                  :spec (make-menu-spec
                         `(:catalogue ,(family-url 'company :system :filter)
                           :create (,(family-url 'company/create :filter) "Νέα Εταιρία")
                           :print ,(family-url 'company/tx/print :system :filter)))
                  :css-class "hmenu"
                  :disabled (case op
                              (:catalogue '())
                              ((:create :update :delete) '(:print))))
   (searchbox (family-url-fn 'actions/company/search)
              (family-url-fn 'company :system)
              (family-params 'company :filter)
              "ac-company")))



;;; ------------------------------------------------------------
;;; Company tx table
;;; ------------------------------------------------------------

;;; table

(defclass company-tx-table (tx-table)
  ((company-id    :accessor company-id :initarg :company-id)
   (debit-sum     :reader   debit-sum  :initarg :debit-sum)
   (credit-sum    :reader   credit-sum :initarg :credit-sum)
   (total         :reader   total      :initarg :total))
  (:default-initargs :item-class 'company-tx-row
                     :id "company-tx-table"
                     :paginator (make-instance 'company-tx-paginator)
                     :header-labels '("" "Ημερομηνία" "Περιγραφή" "Χρέωση" "Πίστωση" "Υπόλοιπο" "")))

(defmethod actions ((tbl company-tx-table) &key)
  (let* ((company-id (company-id tbl))
         (filter (filter tbl))
         (tx-id (selected-key tbl))
         (hrefs (if (and tx-id (not (tx-referenced-p tx-id)))
                    (list :update (apply #'company/tx/update :company-id company-id
                                                             :tx-id tx-id
                                                             filter)
                          :journal (list (tx :tx-id tx-id) "Καθολικό" "journal"))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod filters ((tbl company-tx-table))
  (let* ((company-id (company-id tbl))
         (filter (filter tbl))
         (filter* (remove-from-plist filter :role))
         (filter-spec `((nil      ,(apply #'company/tx :company-id company-id filter*)
                                  "Μικτός ρόλος")
                        (customer  ,(apply #'company/tx :company-id company-id
                                                        :role "customer" filter*)
                                   "Πελάτης")
                        (supplier ,(apply #'company/tx :company-id company-id
                                                       :role "supplier" filter*)
                                  "Προμηθευτής"))))
    (secondary-filter-area (filter-navbar filter-spec
                                          :active (getf filter :role))
                           (datebox #'company/tx
                                    (list* :company-id company-id filter)))))

(defmethod extra-info ((table company-tx-table))
  (with-html
      (:div :class "window-footer"
            (:h4 "Σύνολο χρεώσεων: " (str (fmt-amount (debit-sum table))))
            (:h4 "Σύνολο πιστώσεων: " (str (fmt-amount (credit-sum table))))
            (:h4 "Γενικό Σύνολο: " (str (fmt-amount (total table)))))))


;;; rows

(defclass company-tx-row (scrooge-row)
  ())

(defmethod selector ((row company-tx-row) selected-p)
  (let* ((table (collection row))
         (tx-id (key row))
         (company-id (company-id table))
         (filter (filter table))
         (start (page-start (paginator table) (index row) (start-index table))))
    (html ()
          (:a :href (if selected-p
                        (apply #'company/tx :company-id company-id
                                            :start start filter)
                        (apply #'company/tx :company-id company-id
                                            :tx-id tx-id filter))
              (selector-img selected-p)))))

(defmethod controls ((row company-tx-row) controls-p)
  (let ((tx-id (key row))
        (table (collection row)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply #'company/tx
                                          :company-id (company-id table)
                                          :tx-id tx-id
                                          (filter table))))
        (list nil nil))))

(defmethod payload ((row company-tx-row) enabled-p)
  (let* ((record (record row))
         (tx-id (key row))
         (role (tx-role record))
         (href (case (app-section (getf record :temtx-id))
                 (:libtx (libtx/details role :tx-id tx-id))
                 (:cash (cash/details role :tx-id tx-id))
                 (:cheque (cheque role :cheque-id (getf record :cheque-id)))
                 (:invoice (invoice/details role (invoice-kind record)
                                            :tx-id tx-id)))))
    (mapcar (textbox-maker record enabled-p)
            `((tx-date :css-class ,(if enabled-p "datepicker" nil))
              (description :href ,href)
              (debit-amount :format-fn ,#'fmt-amount
                            :disabled ,(or (not (getf record :debit-amount))
                                           (not enabled-p)))
              (credit-amount :format-fn ,#'fmt-amount
                             :disabled ,(or (not (getf record :credit-amount))
                                            (not enabled-p)))
              (total :disabled t :format-fn ,#'fmt-amount)))))



;;; paginator

(defclass company-tx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg company-tx-paginator) start)
  (let ((table (table pg)))
    (apply #'company/tx :start start
                        :company-id (company-id table)
                        (filter table))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage company-tx-page company/tx ("company/tx")
  ((company-id integer chk-company-id t)
   (tx-id      integer chk-tx-id)
   (start      integer)
   (search     string)
   (subset     string  chk-subset)
   (role       string  chk-role)
   (since      date)
   (until      date))
  (with-view-page
      (let ((filter (params->filter))
            (roles (tx-roles (val role))))
        (multiple-value-bind (records debit-sum credit-sum total)
            (company-debits/credits (val company-id) roles (val since) (val until) :reverse-p t)
          (let ((company-tx-table (make-instance 'company-tx-table
                                                 :records records
                                                 :company-id (val company-id)
                                                 :op :catalogue
                                                 :selected-key (val tx-id)
                                                 :filter filter
                                                 :start-index (val start)
                                                 :debit-sum debit-sum
                                                 :credit-sum credit-sum
                                                 :total total)))
            (with-document ()
              (:head
               (:title "Συναλλαγές » Κατάλογος")
               (main-headers))
              (:body
               (:div :id "container" :class "container_12"
                     (header 'main)
                     (navbar 'main 'company)
                     (company-tx-top-actions :catalogue)
                     (company-tabs
                      (val company-id) filter 'tx
                      (html ()
                            (filters company-tx-table)
                            (:div :id "company-tx-window" :class "window"
                                  (:div :class "title" "Συναλλαγές » Κατάλογος")
                                  (actions company-tx-table)
                                  (display company-tx-table)
                                  (extra-info company-tx-table))))
                     (footer)))))))))

(defpage company-tx-page company/tx/print ("company/tx/print")
  ((company-id integer chk-company-id t)
   (tx-id      integer chk-tx-id)
   (search     string)
   (subset     string  chk-subset)
   (role       string  chk-role)
   (since      date)
   (until      date))
  (with-view-page
      (let ((filter (params->filter))
            (roles (tx-roles (val role))))
        (multiple-value-bind (records debit-sum credit-sum total)
            (company-debits/credits (val company-id) roles (val since) (val until))
          (let ((company-tx-table (make-instance 'company-tx-table
                                                 :records records
                                                 :company-id (val company-id)
                                                 :op :catalogue
                                                 :filter filter
                                                 :paginator nil
                                                 :debit-sum debit-sum
                                                 :credit-sum credit-sum
                                                 :total total)))
            (with-document ()
              (:head
               (:title "Εταιρία » Λεπτομέρειες » Συναλλαγές » Εκτύπωση")
               (print-headers))
              (:body
               (:div :id "container" :class "container_12"
                     (:div :class "grid_12"
                           (back (family-url 'company/tx :system :filter))
                           (:div :id "company-tx-window" :class "window"
                                 (:div :class "title"
                                       (:h1 (str (string-upcase-gr
                                                  (title (get-dao 'company (val company-id))))))
                                       (:h2 :class "grid_7 alpha" (str (conc "Συναλλαγές ως "
                                                                             (cond ((not (suppliedp role))
                                                                                    "προμηθευτής και πελάτης")
                                                                                   ((customer-p (val role))
                                                                                    "πελάτης")
                                                                                   (t
                                                                                    "προμηθευτής")))))
                                       (:div :class "grid_4 omega"
                                             (display (datebox (family-url-fn 'company/tx/print)
                                                               (family-params 'company/tx/print
                                                                              :system
                                                                              :filter))))
                                       (clear))
                                 (display company-tx-table)
                                 (extra-info company-tx-table))
                           (print-pages-footer))))))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage company-tx-page company/tx/update ("company/tx/update")
  ((company-id    integer chk-company-id t)
   (tx-id         integer chk-tx-id)
   (tx-date       date)
   (description   string)
   (debit-amount  float   chk-amount)
   (credit-amount float   chk-amount)
   (search        string)
   (subset        string  chk-subset)
   (role          string  chk-role)
   (since         date)
   (until         date))
  (with-view-page
      (let ((filter (params->filter))
            (roles (tx-roles (val role))))
        (multiple-value-bind (records debit-sum credit-sum total)
            (company-debits/credits (val company-id) roles (val since) (val until) :reverse-p t)
          (let ((company-tx-table (make-instance 'company-tx-table
                                                 :records records
                                                 :company-id (val company-id)
                                                 :op :update
                                                 :selected-key (val tx-id)
                                                 :filter filter
                                                 :debit-sum debit-sum
                                                 :credit-sum credit-sum
                                                 :total total)))
            (with-document ()
              (:head
               (:title "Συναλλαγές » Επεξεργασία")
               (main-headers))
              (:body
               (:div :id "container" :class "container_12"
                     (header 'main)
                     (navbar 'main 'company)
                     (company-tx-top-actions :update)
                     (company-tabs
                      (val company-id) filter 'tx
                      (html ()
                            (filters company-tx-table)
                            (:div :id "company-tx-window" :class "window"
                                  (:div :class "title" "Συναλλαγές » Επεξεργασία")
                                  (actions company-tx-table)
                                  (notifications)
                                  (with-form (actions/company/tx/update :company-id (val company-id)
                                                                        :tx-id (val tx-id)
                                                                        :search (val search)
                                                                        :subset (val subset)
                                                                        :since (val since)
                                                                        :until (val until)
                                                                        :role (val role))
                                    (display company-tx-table :payload (params->payload)))
                                  (extra-info company-tx-table))))
                     (footer)))))))))

(defpage company-tx-page actions/company/tx/update
  ("actions/company/tx/update" :request-type :post)
  ((company-id    integer chk-company-id t)
   (tx-id         integer chk-tx-id)
   (tx-date       date)
   (description   string)
   (debit-amount  float   chk-amount)
   (credit-amount float   chk-amount)
   (search        string)
   (subset        string  chk-subset)
   (role          string  chk-role)
   (since         date)
   (until         date))
  (with-controller-page (company/tx/update)
    (execute (:update 'tx :set
                      'tx-date (val tx-date)
                      'description (val description)
                      'company-id (val company-id)
                      'amount (or (val debit-amount)
                                  (val credit-amount))
                      :where (:= 'id (val tx-id))))
    (see-other (apply #'company/tx :company-id (val company-id)
                                   :tx-id (val tx-id)
                      (params->filter)))))
