(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass company-cheque-family (family-mixin)
  ()
  (:default-initargs
   :parameter-groups '(:system (company-id cheque-id start)
                       :payload (bank due-date amount serial tstamp)
                       :filter (search subset since until cstate))))

(defclass company-cheque-page (auth-regex-page company-cheque-family)
  ())



;;; ------------------------------------------------------------
;;; Company cheque table
;;; ------------------------------------------------------------

;;; table

(defclass company-cheque-table (cheque-table)
  ((header-labels :initform '("" "Σειριακός<br />Αριθμός" "<br />Τράπεζα"
                              "Ημερομηνία<br />λήξης" "<br />Ποσό"))
   (company-id    :accessor company-id
                  :initarg :company-id))
  (:default-initargs :item-class 'company-cheque-row
                     :id "company-cheque-table"
                     :paginator (make-instance 'company-cheque-paginator
                                               :id "cheque-paginator"
                                               :css-class "paginator")))

(defmethod get-records ((table company-cheque-table))
  (get-cheque-records table (company-id table)))

(defmethod actions ((table company-cheque-table) &key)
  (let* ((cheque-id (selected-key table))
         (kind (kind table))
         (filter (filter table))
         (company-id (company-id table))
         (hrefs (if cheque-id
                    (list :update (apply #'company/cheque/update kind :company-id company-id
                                                                      :cheque-id cheque-id
                                                                      filter)
                          :delete (apply #'company/cheque/delete kind :company-id company-id
                                                                      :cheque-id cheque-id
                                                                      filter))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions table))))

(defmethod filters ((tbl company-cheque-table))
  (let* ((kind (kind tbl))
         (filter (filter tbl))
         (company-id (company-id tbl))
         (filter* (remove-from-plist filter kind :cstate))
         (filter-spec `((nil      ,(apply #'company/cheque kind filter*)
                                  "Όλες")
                        (pending  ,(apply #'company/cheque kind :cstate "pending"
                                                                :company-id company-id
                                                                filter*)
                                  "Σε εκκρεμότητα")
                        (paid     ,(apply #'company/cheque kind :cstate "paid"
                                                                :company-id company-id
                                                                filter*)
                                  "Πληρωμένες")
                        (bounced  ,(apply #'company/cheque kind :cstate "bounced"
                                                                :company-id company-id
                                                                filter*)
                                  "Ακάλυπτες")
                        (returned ,(apply #'company/cheque kind :cstate "returned"
                                                                :company-id company-id
                                                                filter*)
                                  "Επιστραμμένες")
                        (stamped  ,(apply #'company/cheque kind :cstate "stamped"
                                                                :company-id company-id
                                                                filter*)
                                  "Σφραγισμένες"))))
    (secondary-filter-area (filter-navbar `((receivable ,(apply #'company/cheque "receivable"
                                                                :company-id company-id
                                                                filter)
                                                        "Προς είσπραξη")
                                            (payable ,(apply #'company/cheque "payable"
                                                             :company-id company-id
                                                             filter)
                                                     "Προς πληρωμή"))
                                          :active kind
                                          :id "cheque-kind-navbar")
                           (filter-navbar filter-spec
                                          :active (getf filter :cstate))
                           (datebox (family-url-fn 'company/cheque)
                                    (family-params 'company/cheque :system :filter)))))


;;; row

(defclass company-cheque-row (cheque-row)
  ())

(defmethod selector ((row company-cheque-row) selected-p)
  (let* ((table (collection row))
         (cheque-id (key row))
         (company-id (company-id table))
         (filter (filter table))
         (pg (paginator table))
         (kind (kind table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'company/cheque
                           kind
                           :start (page-start pg (index row) start)
                           :company-id company-id
                           filter)
                    (apply #'company/cheque
                           kind
                           :cheque-id cheque-id
                           :company-id company-id
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
                         :value (fmt-amount (getf record :amount))
                         :disabled (not enabled-p)))))

(defmethod controls ((row company-cheque-row) controls-p)
  (let* ((cheque-id (key row))
         (table (collection row))
         (company-id (company-id table))
         (filter (filter table))
         (kind (kind table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply #'company/cheque
                                          kind
                                          :cheque-id cheque-id
                                          :company-id company-id
                                          filter)))
        (list nil nil))))


;;; paginator

(defclass company-cheque-paginator (cheque-paginator)
  ())

(defmethod target-url ((pg company-cheque-paginator) start)
  (let ((table (table pg)))
   (apply #'company/cheque (kind table) :company-id (company-id table)
                                        :start start
                                        (filter table))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun company-cheque-top-actions (op)
  (top-actions
   (make-instance 'scrooge-menu
                  :spec (make-menu-spec
                         `(:catalogue ,(family-url 'company :system :filter)
                           :create-company (,(family-url 'company/create :filter)
                                            "Νέα εταιρία" "create")
                           :create-cheque (,(family-url 'company/cheque/create :filter)
                                           "Νέα επιταγή" "create")
                           :print ,(family-url 'company/cheque/print :filter)))
                  :css-class "hmenu"
                  :disabled (case op
                              (:create '(:create-company :create-cheque :print))
                              ((:update :delete) '(:print))))
   (searchbox (family-url-fn 'actions/company/search)
              (family-url-fn 'company :system)
              (family-params 'company :filter)
              "ac-company")))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage company-cheque-page company/cheque (("company/cheque/"
                                              (kind "(receivable|payable)")))
    ((company-id integer chk-company-id t)
     (start      integer)
     (cheque-id  integer chk-cheque-id)
     (search     string)
     (subset     string)
     (since      date)
     (until      date)
     (cstate     string))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op :catalogue
                                        :selected-key (val cheque-id)
                                        :filter filter
                                        :start-index (val start)
                                        :kind kind
                                        :company-id (val company-id)))
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
               (company-cheque-top-actions :catalogue)
               (company-tabs (val company-id) filter 'cheque
                             (html ()
                               (filters cheque-table)
                               (:div :id "company-tx-window"
                                     (:div :class "window"
                                           (:div :class "title" (str page-title))
                                           (actions cheque-table)
                                           (display cheque-table)))))
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
                                         :selected-key (val cheque-id)
                                         :filter filter
                                         :start-index (val start)
                                         :kind "payable"
                                         :company-id (val company-id)))
           (receivable-table (make-instance 'company-cheque-table
                                            :op :details
                                            :selected-key (val cheque-id)
                                            :filter filter
                                            :start-index (val start)
                                            :kind "receivable"
                                            :company-id (val company-id))))
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
                                  (display payable-table))))
                     (when (records receivable-table)
                       (htm (:div :class "window"
                                  (:div :class "title" "Προς πληρωμή")
                                  (display receivable-table)))))))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage company-cheque-page company/cheque/create
    (("company/cheque/" (kind "(receivable|payable)") "/create"))
    ((company-id integer chk-company-id    t)
     (start      integer)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date)
     (amount     float   chk-amount)
     (serial     string  chk-cheque-serial)
     (search     string)
     (subset     string)
     (since      date    chk-date)
     (until      date    chk-date)
     (cstate     string))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op :create
                                        :filter filter
                                        :start-index (val start)
                                        :kind kind
                                        :company-id (val company-id)))
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
               (company-cheque-top-actions :create)
               (company-tabs
                (val company-id) filter 'cheque
                (html ()
                  (filters cheque-table)
                  (:div :id "company-tx-window"
                        (:div :class "window"
                              (:div :class "title" (str page-title))
                              (actions cheque-table)
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
     (start      integer)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date            t)
     (amount     float   chk-amount          t)
     (serial     string  chk-cheque-serial)
     (search     string)
     (subset     string)
     (since      date    chk-date)
     (until      date    chk-date)
     (cstate     string  chk-cheque-state-id))
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
     (start      integer)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date)
     (amount     float   chk-amount)
     (serial     string  chk-cheque-serial)
     (search     string)
     (subset     string)
     (since      date    chk-date)
     (until      date    chk-date)
     (cstate     string))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op :update
                                        :selected-key (val cheque-id)
                                        :kind kind
                                        :filter filter
                                        :start-index (val start)
                                        :company-id (val company-id)))
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
               (company-cheque-top-actions :update)
               (company-tabs
                (val company-id) filter 'cheque
                (html ()
                  (filters cheque-table)
                  (:div :id "company-tx-window"
                        (:div :class "window"
                              (:div :class "title" (str page-title))
                              (actions cheque-table)
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
                                (display cheque-table :payload (params->payload)))))))
               (footer)))))))

(defpage company-cheque-page actions/company/cheque/update
    (("actions/company/cheque/" (kind "(receivable|payable)") "/update") :request-type :post)
    ((company-id integer chk-company-id      t)
     (cheque-id  integer chk-cheque-id       t)
     (start      integer)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date            t)
     (amount     float   chk-amount          t)
     (serial     string  chk-cheque-serial)
     (search     string)
     (subset     string)
     (since      date    chk-date)
     (until      date    chk-date)
     (cstate     string  chk-cheque-state-id))
  (with-controller-page (company/cheque/update kind)
    (check-cheque-accounts)
    (let ((cheque-dao (get-dao 'cheque (val cheque-id))))
      ;; Don't touch company-id, state-id and payable-p
      (setf (bank-id cheque-dao) (bank-id (val bank))
            (due-date cheque-dao) (val due-date)
            (amount cheque-dao) (val amount)
            (serial cheque-dao) (val serial)
            (old-state-id cheque-dao) (state-id cheque-dao)
            (state-id cheque-dao) (state-id cheque-dao)) ; unchanged
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
     (start      integer)
     (search     string)
     (subset     string)
     (since      date    chk-date)
     (until      date    chk-date)
     (cstate     string))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op :delete
                                        :selected-key (val cheque-id)
                                        :filter filter
                                        :start-index (val start)
                                        :kind kind
                                        :company-id (val company-id)))
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
               (company-cheque-top-actions :delete)
               (company-tabs
                (val company-id) filter 'cheque
                (html ()
                  (:div :class "secondary-filter-area"
                        (filters cheque-table))
                  (:div :id "company-tx-window"
                        (:div :class "window"
                              (:div :class "title" (str page-title))
                              (actions cheque-table)
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
                                (display cheque-table))))))
               (footer)))))))

(defpage company-cheque-page actions/company/cheque/delete
    (("actions/company/cheque/" (kind "(receivable|payable)") "/delete") :request-type :post)
    ((company-id integer chk-company-id      t)
     (cheque-id  integer chk-cheque-id       t)
     (start      integer)
     (search     string)
     (subset     string)
     (since      date    chk-date)
     (until      date    chk-date)
     (cstate     string  chk-cheque-state-id))
  (with-controller-page (company/cheque/delete kind)
    (check-cheque-accounts)
    (delete-dao (get-dao 'cheque (val cheque-id)))
    (see-other (apply #'company/cheque kind :company-id (val company-id) (params->filter)))))