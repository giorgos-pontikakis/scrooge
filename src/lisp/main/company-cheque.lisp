(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass company-cheque-family (family-mixin)
  ()
  (:default-initargs
   :parameter-groups '(:system (company-id cheque-id start
                                subset cstate) ;; checked filter parameters
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
         (direction (direction table))
         (filter (filter table))
         (company-id (company-id table))
         (hrefs (if cheque-id
                    (list :update (apply #'company/cheque/update direction :company-id company-id
                                                                           :cheque-id cheque-id
                                                                           filter)
                          :delete (apply #'company/cheque/delete direction :company-id company-id
                                                                           :cheque-id cheque-id
                                                                           filter))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions table))))

(defmethod filters ((tbl company-cheque-table))
  (let* ((direction (direction tbl))
         (filter (filter tbl))
         (company-id (company-id tbl))
         (filter* (remove-from-plist filter direction :cstate))
         (filter-spec `((nil      ,(apply #'company/cheque direction :company-id company-id filter*)
                                  "Όλες")
                        (pending  ,(apply #'company/cheque direction :cstate "pending"
                                                                     :company-id company-id
                                                                     filter*)
                                  "Σε εκκρεμότητα")
                        (paid     ,(apply #'company/cheque direction :cstate "paid"
                                                                     :company-id company-id
                                                                     filter*)
                                  "Πληρωμένες")
                        (bounced  ,(apply #'company/cheque direction :cstate "bounced"
                                                                     :company-id company-id
                                                                     filter*)
                                  "Ακάλυπτες")
                        (returned ,(apply #'company/cheque direction :cstate "returned"
                                                                     :company-id company-id
                                                                     filter*)
                                  "Επιστραμμένες")
                        (stamped  ,(apply #'company/cheque direction :cstate "stamped"
                                                                     :company-id company-id
                                                                     filter*)
                                  "Σφραγισμένες"))))
    (secondary-filter-area
     (filter-navbar `((incoming ,(apply #'company/cheque "incoming" :company-id company-id filter)
                                "Προς είσπραξη")
                      (outgoing ,(apply #'company/cheque "outgoing" :company-id company-id filter)
                                "Προς πληρωμή"))
                    :active direction
                    :id "cheque-direction-navbar")
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
         (direction (direction table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'company/cheque direction :start (page-start pg (index row) start)
                                                      :company-id company-id
                                                      filter)
                    (apply #'company/cheque direction :cheque-id cheque-id
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
         (direction (direction table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply #'company/cheque direction :cheque-id cheque-id
                                                                     :company-id company-id
                                                                     filter)))
        (list nil nil))))


;;; paginator

(defclass company-cheque-paginator (cheque-paginator)
  ())

(defmethod target-url ((pg company-cheque-paginator) start)
  (let ((table (table pg)))
    (apply #'company/cheque (direction table) :company-id (company-id table)
                                              :start start
                                              (filter table))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun company-cheque-top-actions (op)
  (top-actions-area
   (make-instance 'scrooge-menu
                  :spec (make-menu-spec
                         `(:catalogue ,(family-url 'company :system :filter)
                           :create-company (,(family-url 'company/create :system :filter)
                                            "Νέα εταιρία" "create")
                           :create-cheque (,(family-url 'company/cheque/create :system :filter)
                                           "Νέα επιταγή" "create")
                           :print ,(family-url 'company/cheque/print :system :filter)))
                  :css-class "hmenu"
                  :disabled (case op
                              (:catalogue '())
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
                                              (direction "(incoming|outgoing)")))
    ((company-id integer chk-company-id t)
     (cheque-id  integer chk-cheque-id)
     (start      integer)
     (search     string)
     (subset     string  chk-subset)
     (since      date)
     (until      date)
     (cstate     string  chk-cheque-state-id))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op :catalogue
                                        :selected-key (val cheque-id)
                                        :filter filter
                                        :start-index (val start)
                                        :direction direction
                                        :company-id (val company-id)))
           (page-title (conc "Εταιρία » Λεπτομέρειες » Επιταγές "
                             (cheque-page-title direction))))
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
                                                    (direction "(incoming|outgoing)")
                                                    "/print"))
    ((company-id integer chk-company-id t)
     (cheque-id  integer chk-cheque-id)
     (start      integer)
     (search     string)
     (subset     string  chk-subset)
     (since      date)
     (until      date)
     (cstate     string  chk-cheque-state-id))
  (flet ((cheque-state-label (cheque-state)
           (with-hashed-identity (:test #'equal)
             (case cheque-state
               ((nil) "Όλες")
               ("pending" "Σε εκκρεμότητα")
               ("paid" "Πληρωμένες")
               ("bounced" "Ακάλυπτες")
               ("returned" "Επιστραμμένες")
               ("stamped" "Σφραγισμένες")
               (t (error "CHEQUE-STATE-LABEL: Unknown cheque-state"))))))
    (with-view-page
      (let* ((filter (params->filter))
             (receivable-table (make-instance 'company-cheque-table
                                              :op :details
                                              :selected-key (val cheque-id)
                                              :filter filter
                                              :start-index (val start)
                                              :direction "incoming"
                                              :company-id (val company-id)))
             (payable-table (make-instance 'company-cheque-table
                                           :op :details
                                           :selected-key (val cheque-id)
                                           :filter filter
                                           :start-index (val start)
                                           :direction "outgoing"
                                           :company-id (val company-id))))
        (with-document ()
          (:head
            (:title "Εταιρία » Λεπτομέρειες » Επιταγές » Εκτύπωση")
            (print-headers))
          (:body
            (:div :id "container" :class "container_12"
              (:div :class "grid_12"
                (:a :id "back"
                  :href (family-url 'company/cheque :system :filter)
                  "« Επιστροφή")
                (:div :class "window"
                  (:div :class "title"
                    (:h1 (str (string-upcase-gr
                               (title (get-dao 'company (val company-id))))))
                    (:h2 :class "grid_7 alpha"
                      (str (conc "Επιταγές: "
                                 (cheque-state-label (val cstate)))))
                    (:div :class "grid_4 omega"
                      (display (datebox (family-url-fn 'company/cheque/print)
                                        (family-params 'company/cheque/print
                                                       :system
                                                       :filter))))
                    (clear)))

                (when (records receivable-table)
                  (htm (:div :class "window"
                         (:div (:div :class "title" "Επιταγές προς είσπραξη")
                           (display receivable-table)))))
                (when (records payable-table)
                  (htm (:div :class "window"
                         (:div (:div :class "title" "Επιταγές προς πληρωμή")
                           (display payable-table)))))
                (print-pages-footer)))))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage company-cheque-page company/cheque/create
    (("company/cheque/" (direction "(incoming|outgoing)") "/create"))
    ((company-id integer chk-company-id    t)
     (start      integer)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date)
     (amount     float   chk-amount)
     (serial     string  chk-cheque-serial)
     (search     string)
     (subset     string  chk-subset)
     (since      date    chk-date)
     (until      date    chk-date)
     (cstate     string  chk-cheque-state-id))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op :create
                                        :filter filter
                                        :start-index (val start)
                                        :direction direction
                                        :company-id (val company-id)))
           (page-title (conc "Εταιρία » Λεπτομέρειες » Επιταγές "
                             (cheque-page-title direction)
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
                   (with-form (actions/company/cheque/create direction
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
    (("actions/company/cheque/" (direction "(incoming|outgoing)") "/create") :request-type :post)
    ((company-id integer chk-company-id      t)
     (start      integer)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date            t)
     (amount     float   chk-amount          t)
     (serial     string  chk-cheque-serial)
     (search     string)
     (subset     string  chk-subset)
     (since      date    chk-date)
     (until      date    chk-date)
     (cstate     string  chk-cheque-state-id))
  (with-controller-page (company/cheque/create direction)
    (check-cheque-accounts)
    (let ((new-cheque (make-instance 'cheque
                                     :serial (val serial)
                                     :bank-id (bank-id (val bank))
                                     :company-id (val company-id)
                                     :due-date (val due-date)
                                     :amount (val amount)
                                     :receivable-p (incoming-p direction)
                                     :state-id "pending")))
      (insert-dao new-cheque)
      (see-other (apply #'company/cheque direction :company-id (val company-id)
                                                   :cheque-id (cheque-id new-cheque)
                                                   (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage company-cheque-page company/cheque/update
    (("company/cheque/" (direction "(incoming|outgoing)") "/update"))
    ((company-id integer chk-company-id    t)
     (cheque-id  integer chk-cheque-id     t)
     (start      integer)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date)
     (amount     float   chk-amount)
     (serial     string  chk-cheque-serial)
     (search     string)
     (subset     string  chk-subset)
     (since      date    chk-date)
     (until      date    chk-date)
     (cstate     string  chk-cheque-state-id))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op :update
                                        :selected-key (val cheque-id)
                                        :direction direction
                                        :filter filter
                                        :start-index (val start)
                                        :company-id (val company-id)))
           (page-title (conc "Εταιρία » Λεπτομέρειες » Επιταγές "
                             (cheque-page-title direction)
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
                   (with-form (actions/company/cheque/update direction
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
    (("actions/company/cheque/" (direction "(incoming|outgoing)") "/update") :request-type :post)
    ((company-id integer chk-company-id      t)
     (cheque-id  integer chk-cheque-id       t)
     (start      integer)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date            t)
     (amount     float   chk-amount          t)
     (serial     string  chk-cheque-serial)
     (search     string)
     (subset     string  chk-subset)
     (since      date    chk-date)
     (until      date    chk-date)
     (cstate     string  chk-cheque-state-id))
  (with-controller-page (company/cheque/update direction)
    (check-cheque-accounts)
    (let ((cheque-dao (get-dao 'cheque (val cheque-id))))
      ;; Don't touch company-id, state-id and receivable-p
      (setf (bank-id cheque-dao) (bank-id (val bank))
            (due-date cheque-dao) (val due-date)
            (amount cheque-dao) (val amount)
            (serial cheque-dao) (val serial)
            (old-state-id cheque-dao) (state-id cheque-dao)
            (state-id cheque-dao) (state-id cheque-dao)) ; unchanged
      (update-dao cheque-dao)
      (see-other (apply #'company/cheque direction :company-id (val company-id)
                                                   :cheque-id (val cheque-id)
                                                   (params->filter))))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage company-cheque-page company/cheque/delete
    (("company/cheque/" (direction "(incoming|outgoing)") "/delete"))
    ((company-id integer chk-company-id    t)
     (cheque-id  integer chk-cheque-id     t)
     (start      integer)
     (search     string)
     (subset     string  chk-subset)
     (since      date    chk-date)
     (until      date    chk-date)
     (cstate     string  chk-cheque-state-id))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op :delete
                                        :selected-key (val cheque-id)
                                        :filter filter
                                        :start-index (val start)
                                        :direction direction
                                        :company-id (val company-id)))
           (page-title (conc "Εταιρία » Λεπτομέρειες » Επιταγές "
                             (cheque-page-title direction)
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
                   (with-form (actions/company/cheque/delete direction
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
    (("actions/company/cheque/" (direction "(incoming|outgoing)") "/delete") :request-type :post)
    ((company-id integer chk-company-id      t)
     (cheque-id  integer chk-cheque-id       t)
     (start      integer)
     (search     string)
     (subset     string  chk-subset)
     (since      date    chk-date)
     (until      date    chk-date)
     (cstate     string  chk-cheque-state-id))
  (with-controller-page (company/cheque/delete direction)
    (check-cheque-accounts)
    (delete-dao (get-dao 'cheque (val cheque-id)))
    (see-other (apply #'company/cheque direction :company-id (val company-id) (params->filter)))))
