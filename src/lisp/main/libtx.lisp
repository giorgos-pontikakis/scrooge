(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass libtx-family (tx-family)
  ()
  (:default-initargs
   :parameter-groups '(:system (tx-id)
                       :payload (tx-date description company amount temtx-id)
                       :filter (search since until))))

(defclass libtx-page (auth-regex-page libtx-family)
  ((messages
    :allocation :class
    :reader messages
    :initform
    '((company
       (:company-title-unknown
        "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"
        :company-title-null
        "Η επωνυμία της εταιρίας είναι κενή"
        :company-supplier-only
        "Αυτή η εταιρία δεν μπορεί να εμφανίζει έσοδα."
        :company-customer-only
        "Αυτή η εταιρία δεν μπορεί να εμφανίζει έξοδα."))
      (amount
       (:empty-amount
        "Το ποσό της συναλλαγής είναι κενό"
        :non-positive-amount
        "Το ποσό της συναλλαγής δεν είναι θετικός αριθμός"
        :parse-error
        "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
      (temtx-id
       (:temtx-id-null
        "Δεν έχετε επιλέξει πρότυπο συναλλαγής από τη βιβλιοθήκη"))
      (tx-date
       (:parse-error
        "Η ημερομηνία της συναλλαγής είναι άκυρη"))))))



;;; ----------------------------------------------------------------------
;;; Libtx transactions table
;;; ----------------------------------------------------------------------

;;; table

(defclass libtx-table (tx-table)
  ((role :accessor role :initarg :role))
  (:default-initargs :item-class 'libtx-row
                     :id "libtx-table"
                     :paginator (make-instance 'libtx-paginator)
                     :header-labels '("" "Ημερομηνία" "Εταιρία" "Περιγραφή" "Πρότυπο" "Ποσό" "" "")))

(defmethod get-records ((table libtx-table))
  (let* ((search (getf (filter table) :search))
         (since (getf (filter table) :since))
         (until (getf (filter table) :until))
         (role (role table))
         (base-query `(:select tx.id tx-date
                               (:as company.title company)
                               (:as company.id 'company-id)
                               (:as temtx.title temtx-title)
                               description amount
                       :from tx
                       :inner-join temtx
                       :on (:= tx.temtx-id temtx.id)
                       :inner-join company
                       :on (:= tx.company-id company.id)))
         (where nil))
    (when search
      (push `(:or (:ilike description ,(ilike search))
                  (:ilike company.title ,(ilike search))
                  (:ilike temtx.title ,(ilike search)))
            where))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since tx-date)
            where))
    (when (and until (not (eql until :null)))
      (push `(:<= tx-date ,until)
            where))
    (let ((sql `(:order-by (,@base-query :where (:and (:= temtx.customer-p ,(customer-p role))
                                                      (:= temtx.lib-p t)
                                                      ,@where))
                           (:desc tx-date) temtx-title company description)))
      (query (sql-compile sql)
             :plists))))

(defmethod actions ((tbl libtx-table) &key)
  (let* ((tx-id (selected-key tbl))
         (role (role tbl))
         (filter (filter tbl))
         (hrefs (if tx-id
                    (list :details (apply #'libtx/details role :tx-id tx-id filter)
                          :journal (list (tx :tx-id tx-id) "Καθολικό" "journal")
                          :delete (apply #'libtx/delete role :tx-id tx-id filter))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod filters ((tbl libtx-table))
  (let ((role (role tbl))
        (filter (filter tbl)))
    (filter-area (filter-navbar `((customer ,(apply #'libtx "customer" filter) "Πελάτες")
                                  (supplier ,(apply #'libtx "supplier" filter) "Προμηθευτές"))
                                :active role
                                :id "role-navbar")
                 (datebox (lambda (&rest args)
                            (apply #'libtx role args))
                          filter))))



;;; rows

(defclass libtx-row (tx-row)
  ())

(defmethod selector ((row libtx-row) selected-p)
  (let* ((tx-id (key row))
         (table (collection row))
         (pg (paginator table))
         (filter (filter table))
         (role (role table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'libtx role :start (page-start pg (index row) start) filter)
                    (apply #'libtx role :tx-id tx-id filter))
        (selector-img selected-p)))))

(defmethod payload ((row libtx-row) enabled-p)
  (let ((record (record row)))
    (mapcar (textbox-maker record enabled-p)
            `((tx-date :css-class ,(if enabled-p "datepicker" nil))
              (company :href ,(company/tx :company-id (getf record :company-id)
                                          :tx-id (key row)))
              (description :href ,(libtx/details (role (collection row))
                                                 :tx-id (key row)))
              temtx-title
              (amount :format-fn ,#'fmt-amount)))))

(defmethod controls ((row libtx-row) controls-p)
  (let* ((tx-id (key row))
         (table (collection row))
         (filter (filter table))
         (role (role table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button :href (apply #'libtx role :tx-id tx-id filter)))
        (list nil nil))))


;;; paginator

(defclass libtx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg libtx-paginator) start)
  (let ((table (table pg)))
    (apply #'libtx (role table) :start start (filter table))))



;;; ----------------------------------------------------------------------
;;; Utilities
;;; ----------------------------------------------------------------------

(defun libtx-page-title (role op-label)
  (conc "Βιβλιοθήκη » " (if (customer-p role) "Πελάτες" "Προμηθευτές") " » " op-label))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun libtx-top-actions (op)
  (let* ((role (first *registers*))
         (new-libtx-label (conc "Νέα Συναλλαγή " (if (customer-p role) "Πελάτη" "Προμηθευτή"))))
    (top-actions-area
     (make-instance 'scrooge-menu
                    :spec (make-menu-spec
                           `(:catalogue ,(family-url 'libtx :system :filter)
                             :create (,(family-url 'libtx/create :filter) ,new-libtx-label)))
                    :css-class "hmenu"
                    :disabled (list op))
     (searchbox (family-url-fn 'actions/libtx/search)
                (family-url-fn 'libtx :system)
                (family-params 'libtx :filter)
                "ac-company"))))



;;; ------------------------------------------------------------
;;; Libtx form
;;; ------------------------------------------------------------

(defclass libtx-form (tx-form)
  ((role :accessor role :initarg :role)))

(defmethod display ((form libtx-form) &key styles)
  (let* ((customer-p (customer-p (role form)))
         (disabled (eql (op form) :details))
         (record (record form))
         (lib-temtx (query (:select 'id 'title
                                    :from 'temtx
                                    :where (:and (:= 'lib-p t)
                                                 (:= 'customer-p customer-p)))
                           :plists)))
    (with-html
      (:div :id "split-data-form" :class "data-form"
        (:div :class "grid_6 alpha"
          (left-column form styles disabled))
        (:div :class "grid_5 omega"
          (:h3 "Βιβλιοθήκη προτύπων")
          (:ul (loop for i in lib-temtx
                     do (htm (:li (:input :type "radio"
                                    :name 'temtx-id
                                    :checked (eql (getf i :id)
                                                  (getf record :temtx-id))
                                    :disabled disabled
                                    :value (getf i :id)
                                    (str (getf i :title))))))))
        (clear)))))

(defmethod actions ((form libtx-form) &key filter)
  (let* ((tx-id (key form))
         (role (role form))
         (hrefs (list :update (apply #'libtx/update role :tx-id tx-id filter)
                      :journal (list (tx :tx-id tx-id) "Καθολικό" "journal")
                      :delete (apply #'libtx/delete role :tx-id tx-id filter))))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions form))))



;;; ----------------------------------------------------------------------
;;; SEARCH
;;; ----------------------------------------------------------------------

(defpage libtx-page actions/libtx/search
    (("actions/libtx/" (role "(customer|supplier)") "/search") :request-type :get)
    ((search string)
     (since date)
     (until date))
  (with-db ()
    (let* ((filter (params->filter))
           (records (records (make-instance 'libtx-table :op :catalogue
                                                   :role role
                                                   :filter filter))))
      (if (single-item-list-p records)
          (see-other (apply #'libtx/details role
                            :tx-id (get-key (first records))
                            filter))
          (see-other (apply #'libtx role filter))))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage libtx-page libtx (("libtx/" (role "(customer|supplier)")))
    ((tx-id  integer chk-tx-id)
     (start  integer)
     (search string)
     (since  date)
     (until  date))
  (with-view-page
    (let* ((filter (params->filter))
           (page-title (libtx-page-title role "Κατάλογος"))
           (libtx-table (make-instance 'libtx-table
                                       :role role
                                       :op :catalogue
                                       :selected-key (val tx-id)
                                       :filter filter
                                       :start-index (val start))))
      (when-let (id (val tx-id))
        (let ((tx (get-dao-plist 'tx id)))
          (maybe-abort-on-incompatible-id libtx-table
                                          id
                                          (libtx (tx-role tx)
                                                 :tx-id id))))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'libtx)
            (libtx-top-actions :catalogue)
            (filters libtx-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" (str page-title))
                (actions libtx-table)
                (display libtx-table)))
            (footer)))))))

(defpage libtx-page libtx/details (("libtx/" (role "(customer|supplier)") "/details"))
    ((tx-id  integer chk-tx-id t)
     (search string)
     (since  date)
     (until  date))
  (with-view-page
    (let* ((filter (params->filter))
           (libtx-form (make-instance 'libtx-form
                                      :role role
                                      :op :details
                                      :key (val tx-id)
                                      :cancel-url (apply #'libtx role :tx-id (val tx-id) filter)))
           (page-title (libtx-page-title role "Λεπτομέρειες")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'libtx)
            (libtx-top-actions :details)
            (:div :class "grid_12"
              (:div :id "libtx-window" :class "window"
                (:div :class "title" "Λεπτομέρειες")
                (actions libtx-form :filter filter)
                (display libtx-form)))
            (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage libtx-page libtx/create
    (("libtx/" (role "(customer|supplier)") "/create"))
    ((tx-date     date)
     (description string)
     (company     string  chk-company-title)
     (amount      float   chk-amount)
     (temtx-id    integer chk-temtx-id)
     (search      string)
     (since       date)
     (until       date))
  (validate-parameters (tx-company-constraints-chker role t) company)
  (with-view-page
    (let* ((filter (params->filter))
           (libtx-form (make-instance 'libtx-form
                                      :role role
                                      :op :create
                                      :cancel-url (apply #'libtx role filter)))
           (page-title (libtx-page-title role "Δημιουργία")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'libtx)
            (libtx-top-actions :create)
            (:div :class "grid_12"
              (:div :id "libtx-window" :class "window"
                (:div :class "title" (str page-title))
                (actions libtx-form :filter filter)
                (notifications)
                (with-form (actions/libtx/create role
                                                 :search (val search)
                                                 :since (val since)
                                                 :until (val until))
                  (display libtx-form :payload (params->payload)
                                      :styles (params->styles)))))
            (footer)))))))

(defpage libtx-page actions/libtx/create
    (("actions/libtx/" (role "(customer|supplier)") "/create") :request-type :post)
    ((tx-date     date)
     (description string)
     (company     string  chk-company-title t)
     (amount      float   chk-amount t)
     (temtx-id    integer chk-temtx-id t)
     (search      string)
     (since       date)
     (until       date))
  (validate-parameters (tx-company-constraints-chker role t) company)
  (with-controller-page (libtx/create role :temtx-id (if (suppliedp temtx-id) (val temtx-id) :null))
    (let* ((company-id (company-id (val company)))
           (temtx (get-dao 'temtx (val temtx-id)))
           (new-tx (make-instance 'tx
                                  :tx-date (val tx-date)
                                  :description (val description)
                                  :company-id company-id
                                  :amount (val amount)
                                  :credit-account-id (credit-account-id temtx)
                                  :debit-account-id (debit-account-id temtx))))
      (insert-dao new-tx)
      (see-other (apply #'libtx/details role :tx-id (tx-id new-tx)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage libtx-page libtx/update
    (("libtx/" (role "(customer|supplier)") "/update"))
    ((search      string)
     (since       date)
     (until       date)
     (tx-id       integer chk-tx-id         t)
     (tx-date     date)
     (description string)
     (company     string  chk-company-title)
     (amount      float   chk-amount)
     (temtx-id    integer chk-temtx-id))
  (validate-parameters (tx-company-constraints-chker role t) company)
  (with-view-page
    (let* ((filter (params->filter))
           (libtx-form (make-instance 'libtx-form
                                      :role role
                                      :op :update
                                      :key (val tx-id)
                                      :cancel-url (apply #'libtx/details role
                                                         :tx-id (val tx-id)
                                                         filter)))
           (page-title (libtx-page-title role "Επεξεργασία")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'libtx)
            (libtx-top-actions :update)
            (:div :class "grid_12"
              (:div :id "libtx-window" :class "window"
                (:div :class "title" (str page-title))
                (actions libtx-form :filter filter)
                (notifications)
                (with-form (actions/libtx/update role
                                                 :tx-id (val tx-id)
                                                 :search (val search)
                                                 :since (val since)
                                                 :until (val until))
                  (display libtx-form :payload (params->payload)
                                      :styles (params->styles)))))
            (footer)))))))

(defpage libtx-page actions/libtx/update
    (("actions/libtx/" (role "(customer|supplier)") "/update") :request-type :post)
    ((search      string)
     (since       date)
     (until       date)
     (tx-id       integer chk-tx-id         t)
     (tx-date     date)
     (description string)
     (company     string  chk-company-title)
     (amount      float   chk-amount)
     (temtx-id    integer chk-temtx-id t))
  (validate-parameters (tx-company-constraints-chker role t) company)
  (with-controller-page (libtx/update role :temtx-id (if (suppliedp temtx-id) (val temtx-id) :null))
    (let ((company-id (company-id (val company)))
          (temtx (get-dao 'temtx (val temtx-id))))
      (execute (:update 'tx :set
                        'tx-date (val tx-date)
                        'description (val description)
                        'company-id company-id
                        'amount (val amount)
                        'debit-account-id (debit-account-id temtx)
                        'credit-account-id (credit-account-id temtx)
                        :where (:= 'id (val tx-id))))
      (see-other (apply #'libtx/details role :tx-id (val tx-id) (params->filter))))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage libtx-page libtx/delete
    (("libtx/" (role "(customer|supplier)") "/delete"))
    ((tx-id  integer chk-tx-id/ref t)
     (search string)
     (since  date)
     (until  date))
  (with-view-page
    (let* ((filter (params->filter))
           (page-title (libtx-page-title role "Διαγραφή"))
           (libtx-table (make-instance 'libtx-table
                                       :op :delete
                                       :role role
                                       :selected-key (val tx-id)
                                       :filter filter)))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'libtx)
            (libtx-top-actions :delete)
            (filters libtx-table)
            (:div :class "grid_12"
              (:div :id "libtx-window" :class "window"
                (:div :class "title" (str page-title))
                (actions libtx-table)
                (with-form (actions/libtx/delete role
                                                 :tx-id (val tx-id)
                                                 :search (val search)
                                                 :since (val since)
                                                 :until (val until))
                  (display libtx-table))))
            (footer)))))))

(defpage libtx-page actions/libtx/delete
    (("actions/libtx/" (role "(customer|supplier)") "/delete") :request-type :post)
    ((tx-id  integer chk-tx-id/ref t)
     (search string)
     (since  date)
     (until  date))
  (with-controller-page (libtx/delete)
    (delete-dao (get-dao 'tx (val tx-id)))
    (see-other (apply #'libtx role (params->filter)))))
