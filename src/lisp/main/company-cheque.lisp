(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass company-cheque-page (cheque-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(company-id cheque-id start))
   (payload-parameter-names
    :allocation :class
    :initform '(bank due-date amount tstamp serial))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))))



;;; ------------------------------------------------------------
;;; Company cheque table
;;; ------------------------------------------------------------

(defclass company-cheque-table (cheque-table)
  ((header-labels :initform '("" "Σειριακός<br />Αριθμός" "<br />Τράπεζα"
                              "Ημερομηνία<br />λήξης" "<br />Ποσό")))
  (:default-initargs :item-class 'company-cheque-row
                     :paginator (make-instance 'company-cheque-paginator
                                               :id "cheque-paginator"
                                               :css-class "paginator")))

(defclass company-cheque-row (cheque-row)
  ())

(defmethod selector ((row company-cheque-row) selected-p)
  (let* ((table (collection row))
         (cheque-id (key row))
         (filter (filter table))
         (pg (paginator table))
         (kind (kind table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'company/cheque
                           kind
                           :start (page-start pg (index row) start)
                           filter)
                    (apply #'company/cheque
                           kind
                           :cheque-id cheque-id
                           filter))
          (selector-img selected-p)))))

(defmethod payload ((row company-cheque-row) enabled-p)
  (let ((record (record row)))
    (list (make-instance 'textbox
                         :name 'serial
                         :value (getf record :serial)
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'bank
                         :value (getf record :bank)
                         :css-class "ac-bank"
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'due-date
                         :value (getf record :due-date)
                         :css-class (if enabled-p "datepicker" nil)
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'amount
                         :value (getf record :amount)
                         :disabled (not enabled-p)))))

(defmethod controls ((row company-cheque-row) controls-p)
  (let* ((cheque-id (key row))
         (table (collection row))
         (filter (filter table))
         (kind (kind table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply #'company/cheque
                                          kind
                                          :cheque-id cheque-id
                                          filter)))
        (list nil nil))))


;;; paginator

(defclass company-cheque-paginator (cheque-paginator)
  ())

(defmethod target-url ((pg company-cheque-paginator) start)
  (apply #'company/cheque (kind table) :company-id company-id
                                       :start start (filter (table pg))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun company-cheque-top-actions (op kind company-id company-filter cheque-filter filter)
  (top-actions (make-instance 'menu
                              :spec `((catalogue ,(company-catalogue-link company-id
                                                                          company-filter))
                                      (company-create ,(company-create-link company-filter))
                                      (cheque-create ,(cheque-create-link #'company/cheque
                                                                          kind
                                                                          cheque-filter))
                                      (print
                                       ,(html ()
                                          (:a :href (apply #'company/cheque/print
                                                           kind :company-id company-id filter)
                                              (:img :src "/scrooge/img/printer.png")
                                              "Εκτύπωση"))))
                              :css-class "hmenu"
                              :disabled (company-disabled-actions op))
               (searchbox #'actions/company/search
                          #'(lambda (&rest args)
                              (apply #'company :company-id company-id args))
                          company-filter
                          "ac-company")))

(defun company-cheque-actions (op kind company-id cheque-id filter)
  (actions-menu
   (make-menu-spec
    (action-anchors/crud (apply #'company/cheque/update kind :company-id company-id
                                                             :cheque-id cheque-id
                                                             filter)
                         (apply #'company/cheque/delete kind :company-id company-id
                                                             :cheque-id cheque-id
                                                             filter)
                         (apply #'company/cheque/create kind :company-id company-id filter)))
   (enabled-actions/crud op cheque-id)))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage company-cheque-page company/cheque
    (("company/cheque/" (kind "(receivable|payable)")))
    ((company-id integer chk-company-id t)
     (cheque-id  integer chk-cheque-id)
     (search     string)
     (subset     string)
     (since      date)
     (until      date)
     (start      integer)
     (cstate     string))
  (with-view-page
    (check-cheque-accounts)
    (let* ((op :catalogue)
           (filter (params->filter))
           (cheque-filter (params->cheque-filter))
           (company-filter (params->company-filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op :details
                                        :filter (list* :company-id (val company-id)
                                                       cheque-filter)
                                        :start-index (val start)
                                        :kind kind))
           (page-title (conc "Εταιρία » Λεπτομέρειες » Επιταγές "
                             (cheque-page-title kind))))
      (with-document ()
        (:head
         (:title "Εταιρία » Λεπτομέρειες » Επιταγές")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-cheque-top-actions :tx-cheque kind (val company-id)
                                           company-filter cheque-filter filter)
               (company-tabs (val company-id) company-filter 'cheque
                             (html ()
                               (:div :class "secondary-filter-area"
                                     (display (cheque-filters kind
                                                              (list* :company-id (val company-id)
                                                                     filter)
                                                              #'company/cheque)))
                               (:div :id "company-tx-window"
                                     (:div :class "window"
                                           (:div :class "title" (str page-title))
                                           (company-cheque-actions op kind
                                                                   (val company-id) (val cheque-id)
                                                                   filter)
                                           (display cheque-table
                                                    :key (val cheque-id))))))
               (footer)))))))

(defpage company-cheque-page company/cheque/print (("company/cheque/"
                                                    (kind "(receivable|payable)")
                                                    "/print"))
    ((search     string)
     (subset     string)
     (company-id integer chk-company-id t)
     (cheque-id  integer)
     (since      date)
     (until      date)
     (start      integer)
     (cstate     string))
  (with-view-page
    (let* ((filter (params->filter))
           (payable-table (make-instance 'company-cheque-table
                                         :op :details
                                         :filter filter
                                         :start-index (val start)
                                         :kind "payable"))
           (receivable-table (make-instance 'company-cheque-table
                                            :op :details
                                            :filter filter
                                            :start-index (val start)
                                            :kind "receivable")))
      (with-document ()
        (:head
         (:title "Εταιρία » Λεπτομέρειες » Επιταγές » Εκτύπωση")
         (print-headers))
        (:body
         (:div :id "container" :class "container_12"
               (:div :class "grid_12"
                     (:a :id "back"
                         :href (apply #'company/cheque kind filter)
                         "« Επιστροφή"))
               (:div :id "company-tx-window"
                     (when (records payable-table)
                       (htm (:div :class "window"
                                  (:div :class "title" "Προς είσπραξη")
                                  (display payable-table
                                           :key (val cheque-id)))))
                     (when (records receivable-table)
                       (htm (:div :class "window"
                                  (:div :class "title" "Προς πληρωμή")
                                  (display receivable-table
                                           :key (val cheque-id))))))))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage company-cheque-page company/cheque/create
    (("company/cheque/" (kind "(receivable|payable)") "/create"))
    ((company-id integer chk-company-id    t)
     (search     string)
     (cstate     string)
     (subset     string)
     (start      integer)
     (since      date    chk-date)
     (until      date    chk-date)
     (serial     string  chk-cheque-serial)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date)
     (amount     float   chk-amount))
  (with-view-page
    (check-cheque-accounts)
    (let* ((op :create)
           (filter (params->filter))
           (cheque-filter (params->cheque-filter))
           (company-filter (params->company-filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op op
                                        :filter (list* :company-id (val company-id) cheque-filter)
                                        :start-index (val start)
                                        :kind kind))
           (page-title (conc "Εταιρία » Λεπτομέρειες » Επιταγές "
                             (cheque-page-title kind)
                             " » Δημιουργία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-cheque-top-actions :tx-cheque kind (val company-id)
                                           company-filter cheque-filter filter)
               (company-tabs
                (val company-id) company-filter 'cheque
                (html ()
                  (:div :class "secondary-filter-area"
                        (display (cheque-filters kind
                                                 (list* :company-id (val company-id)
                                                        filter)
                                                 #'company/cheque)))
                  (:div :id "company-tx-window"
                        (:div :class "window"
                              (:div :class "title" (str page-title))
                              (company-cheque-actions op kind
                                                      (val company-id) nil
                                                      filter)
                              (notifications)
                              (with-form (actions/company/cheque/create kind
                                                                        :company-id (val company-id)
                                                                        :search (val search)
                                                                        :cstate (val cstate)
                                                                        :subset (val subset)
                                                                        :start (val start)
                                                                        :since (val since)
                                                                        :until (val until))
                                (display cheque-table :payload (params->payload)))))))
               (footer)))))))

(defpage company-cheque-page actions/company/cheque/create
    (("actions/company/cheque/" (kind "(receivable|payable)") "/create") :request-type :post)
    ((company-id integer chk-company-id      t)
     (search     string)
     (cstate     string  chk-cheque-state-id)
     (subset     string)
     (start      integer)
     (since      date    chk-date)
     (until      date    chk-date)
     (bank       string  chk-bank-title)
     (serial     string  chk-cheque-serial)
     (due-date   date    chk-date            t)
     (amount     float   chk-amount          t))
  (with-controller-page (company/cheque/create kind)
    (check-cheque-accounts)
    (let ((new-cheque (make-instance 'cheque
                                     :serial (val serial)
                                     :bank-id (bank-id (val bank))
                                     :company-id (val company-id)
                                     :due-date (val due-date)
                                     :amount (val amount)
                                     :payable-p (string= kind "payable")
                                     :state-id "pending")))
      (insert-dao new-cheque)
      (see-other (apply #'company/cheque kind :company-id (val company-id)
                                              :cheque-id (cheque-id new-cheque)
                                              (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage company-cheque-page company/cheque/update
    (("company/cheque/" (kind "(receivable|payable)") "/update"))
    ((company-id integer chk-company-id    t)
     (cheque-id  integer chk-cheque-id     t)
     (search     string)
     (cstate     string)
     (subset     string)
     (start      integer)
     (since      date    chk-date)
     (until      date    chk-date)
     (serial     string  chk-cheque-serial)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date)
     (amount     float   chk-amount))
  (with-view-page
    (check-cheque-accounts)
    (let* ((op :update)
           (filter (params->filter))
           (cheque-filter (params->cheque-filter))
           (company-filter (params->company-filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op op
                                        :filter (list* :company-id (val company-id) cheque-filter)
                                        :start-index (val start)
                                        :kind kind))
           (page-title (conc "Εταιρία » Λεπτομέρειες » Επιταγές "
                             (cheque-page-title kind)
                             " » Επεξεργασία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-cheque-top-actions :tx kind (val company-id)
                                           company-filter cheque-filter filter)
               (company-tabs
                (val company-id) company-filter 'cheque
                (html ()
                  (:div :class "secondary-filter-area"
                        (display (cheque-filters kind
                                                 (list* :company-id (val company-id) filter)
                                                 #'company/cheque)))
                  (:div :id "company-tx-window"
                        (:div :class "window"
                              (:div :class "title" (str page-title))
                              (company-cheque-actions op kind
                                                      (val company-id) (val cheque-id)
                                                      filter)
                              (notifications)
                              (with-form (actions/company/cheque/update kind
                                                                        :company-id (val company-id)
                                                                        :cheque-id (val cheque-id)
                                                                        :search (val search)
                                                                        :cstate (val cstate)
                                                                        :subset (val subset)
                                                                        :start (val start)
                                                                        :since (val since)
                                                                        :until (val until))
                                (display cheque-table :key (val cheque-id)
                                                      :payload (params->payload)))))))
               (footer)))))))

(defpage company-cheque-page actions/company/cheque/update
    (("actions/company/cheque/" (kind "(receivable|payable)") "/update") :request-type :post)
    ((company-id integer chk-company-id      t)
     (cheque-id  integer chk-cheque-id       t)
     (search     string)
     (cstate     string  chk-cheque-state-id)
     (subset     string)
     (start      integer)
     (since      date    chk-date)
     (until      date    chk-date)
     (bank       string  chk-bank-title)
     (serial     string  chk-cheque-serial)
     (due-date   date    chk-date            t)
     (amount     float   chk-amount          t))
  (with-controller-page (company/cheque/update kind)
    (check-cheque-accounts)
    (let ((cheque-dao (get-dao 'cheque (val cheque-id))))
      ;; Don't touch company-id, state-id and payable-p
      ;; HACK: Pass plist of states
      (setf (bank-id cheque-dao) (bank-id (val bank))
            (due-date cheque-dao) (val due-date)
            (amount cheque-dao) (val amount)
            (serial cheque-dao) (val serial)
            (state-id cheque-dao) (list :from-state-id (state-id cheque-dao)
                                        :to-state-id (state-id cheque-dao))) ; unchanged
      (update-dao cheque-dao)
      (see-other (apply #'company/cheque kind :company-id (val company-id)
                                              :cheque-id (val cheque-id)
                                              (params->filter))))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage company-cheque-page company/cheque/delete
    (("company/cheque/" (kind "(receivable|payable)") "/delete"))
    ((company-id integer chk-company-id    t)
     (cheque-id  integer chk-cheque-id     t)
     (search     string)
     (cstate     string)
     (subset     string)
     (start      integer)
     (since      date    chk-date)
     (until      date    chk-date)
     (serial     string  chk-cheque-serial)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date)
     (company    string  chk-company-title)
     (amount     float   chk-amount))
  (with-view-page
    (check-cheque-accounts)
    (let* ((op :delete)
           (filter (params->filter))
           (cheque-filter (params->cheque-filter))
           (company-filter (params->company-filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op op
                                        :filter (list* :company-id (val company-id) cheque-filter)
                                        :start-index (val start)
                                        :kind kind))
           (page-title (conc "Εταιρία » Λεπτομέρειες » Επιταγές "
                             (cheque-page-title kind)
                             " » Διαγραφή")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-cheque-top-actions :tx kind (val company-id)
                                           company-filter cheque-filter filter)
               (company-tabs
                (val company-id) company-filter 'cheque
                (html ()
                  (:div :class "secondary-filter-area"
                        (display (cheque-filters kind
                                                 (list* :company-id (val company-id) filter)
                                                 #'company/cheque)))
                  (:div :id "company-tx-window"
                        (:div :class "window"
                              (:div :class "title" (str page-title))
                              (company-cheque-actions op kind
                                                      (val company-id) (val cheque-id)
                                                      filter)
                              (notifications)
                              (with-form (actions/company/cheque/delete kind
                                                                        :company-id (val company-id)
                                                                        :cheque-id (val cheque-id)
                                                                        :search (val search)
                                                                        :cstate (val cstate)
                                                                        :subset (val subset)
                                                                        :start (val start)
                                                                        :since (val since)
                                                                        :until (val until))
                                (display cheque-table :key (val cheque-id)))))))
               (footer)))))))

(defpage company-cheque-page actions/company/cheque/delete
    (("actions/company/cheque/" (kind "(receivable|payable)") "/delete") :request-type :post)
    ((company-id integer chk-company-id      t)
     (cheque-id  integer chk-cheque-id       t)
     (search     string)
     (cstate     string  chk-cheque-state-id)
     (subset     string)
     (start      integer)
     (since      date    chk-date)
     (until      date    chk-date))
  (with-controller-page (company/cheque/delete kind)
    (check-cheque-accounts)
    (delete-dao (get-dao 'cheque (val cheque-id)))
    (see-other (apply #'company/cheque kind :company-id (val company-id) (params->filter)))))