(in-package :scrooge)

(defparameter *cash-account* 11)

(defparameter *expenses-root-account* 20)
(defparameter *revenues-root-account* 05)


;;; ----------------------------------------------------------------------
;;; Utilities
;;; ----------------------------------------------------------------------

(defun cash-kind-label (cash-kind)
  (cond ((string-equal cash-kind "revenue") "Έσοδα")
        ((string-equal cash-kind "expense") "Έξοδα")
        (t (error "internal error in cash-kind-label"))))



;;; ----------------------------------------------------------------------
;;; Actions
;;; ----------------------------------------------------------------------

(define-regex-page actions/financial/cash/create (("actions/financial/cash/" cash-kind "/create")
                                                  :registers (cash-kind "(expense|revenue)")
                                                  :request-type :post)
    ((search      string)
     (date        date)
     (company     string  chk-company-title*)
     (description string)
     (amount      string  chk-amount)
     (account-id  integer chk-acc-id t))
  (if (every #'validp (parameters *page*))
      (let* ((company-id (company-id (val* company))) ;; using val* (accept null values)
             (debit-acc-id (if (string-equal cash-kind "revenue")
                               *cash-account*
                               (val account-id)))
             (credit-acc-id (if (string-equal cash-kind "expense")
                                *cash-account*
                                (val account-id)))
             (new-tx (make-instance 'tx
                                    :tx-date (val date)
                                    :description (val description)
                                    :company-id company-id
                                    :amount (val amount)
                                    :credit-acc-id credit-acc-id
                                    :debit-acc-id debit-acc-id)))
        (with-db ()
          (insert-dao new-tx)
          (see-other (cash cash-kind :id (id new-tx) :search (val search)))))
      (see-other (cash/create cash-kind
                              :date (raw date)
                              :description (raw description)
                              :company (raw company)
                              :amount (raw amount)
                              :account-id (raw account-id)
                              :search (raw search)))))

(define-regex-page actions/financial/cash/update (("actions/financial/cash/" cash-kind "/update")
                                                  :registers (cash-kind "(expense|revenue)")
                                                  :request-type :post)
    ((search      string)
     (id          integer chk-tx-id t)
     (date        date)
     (description string)
     (company     string  chk-company-title*)
     (amount      string  chk-amount)
     (account-id  integer chk-acc-id))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let ((company-id (company-id (val* company))) ;; using val* (accept null values)
              (debit-acc-id (if (string-equal cash-kind "revenue")
                                *cash-account*
                                (val account-id)))
              (credit-acc-id (if (string-equal cash-kind "expense")
                                 *cash-account*
                                 (val account-id))))
          (with-db ()
            (execute (:update 'tx :set
                              'tx-date (val date)
                              'description (val description)
                              'company-id company-id
                              'amount (val amount)
                              'debit-acc-id debit-acc-id
                              'credit-acc-id credit-acc-id
                              :where (:= 'id (val id))))
            (see-other (cash cash-kind :id (val id)))))
        (see-other (cash/update cash-kind
                                :search (raw search)
                                :id (raw id)
                                :date (raw date)
                                :description (raw description)
                                :company (raw company)
                                :amount (raw amount)
                                :account-id (raw account-id))))))

(define-regex-page actions/financial/cash/delete (("actions/financial/cash/" cash-kind "/delete")
                                                  :registers (cash-kind "(expense|revenue)")
                                                  :request-type :post)
    ((id     integer chk-tx-id t)
     (search string))
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'tx (val id)))
        (see-other (cash cash-kind :search (val search))))
      (see-other (notfound))))



;;; ----------------------------------------------------------------------
;;; account-ro (read only) tree
;;; ----------------------------------------------------------------------

(defclass account-radio-tree (account-crud-tree)
  ((op :initform 'catalogue))
  (:default-initargs :item-class 'account-radio-node))

(defclass account-radio-node (account-crud-node)
  ())

(defmethod cells ((node account-radio-node) &key)
  (let* ((id (key node))
         (record (record node)))
    (list :selector (make-instance 'radio-cell
                                   :name 'account-id
                                   :value id
                                   :content (getf record :title)))))



;;; ----------------------------------------------------------------------
;;; Cash transactions table
;;; ----------------------------------------------------------------------

(defclass cash-tx-table (tx-table)
  ((item-key-field :initform :id)
   (subpage :accessor subpage :initarg :subpage))
  (:default-initargs :item-class 'cash-tx-row))

(defmethod read-records ((table cash-tx-table))
  (flet ((cash-kind-account (cash-kind)
           (cond ((string-equal cash-kind "revenue") 'debit-acc-id)
                 ((string-equal cash-kind "expense") 'credit-acc-id)
                 (t (error "internal error in cash-kind-account")))))
    (let* ((search (getf (filter table) :search))
           (cash-kind (subpage table))
           (base-query `(:select tx.id
                                 (:as tx-date date)
                                 (:as company.title company)
                                 description amount
                                 :from tx
                                 :left-join company
                                 :on (:= tx.company-id company.id)))
           (composite-query
            (if search
                (append base-query
                        `(:where (:and (:= ,(cash-kind-account cash-kind) ,*cash-account*)
                                       (:or (:ilike description ,(ilike search))
                                            (:ilike company.title ,(ilike search))))))
                (append base-query
                        `(:where (:or (:= ,(cash-kind-account cash-kind) ,*cash-account*))))))
           (final-query `(:order-by ,composite-query date)))
      (with-db ()
        (query (sql-compile final-query)
               :plists)))))


;;; rows

(defclass cash-tx-row (tx-row)
  ())

(defmethod cells ((row cash-tx-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (filter (filter (collection row)))
         (cash-kind (subpage (collection row))))
    (list :selector
          (make-instance 'selector-cell
                         :states (list :on (apply #'cash cash-kind
                                                  :start (page-start pg (index row) start)
                                                  filter)
                                       :off (apply #'cash cash-kind :id id filter)))
          :payload
          (mapcar (lambda (name)
                    (make-instance 'textbox-cell
                                   :name name
                                   :value (getf record (make-keyword name))))
                  '(date company description amount))
          :controls
          (list
           (make-instance 'ok-cell)
           (make-instance 'cancel-cell
                          :href (apply #'cash cash-kind :id id filter))))))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun cash-filters (cash-kind search)
  (let ((spec `((revenue ,(cash "revenue" :search search) "Έσοδα")
                (expense ,(cash "expense" :search search) "Έξοδα"))))
    (with-html
      (:div :id "filters" :class "filters"
            (:p :class "title" "Κατάσταση")
            (display
             (make-instance 'vertical-navbar
                            :id "cash-filters"
                            :style "vnavbar"
                            :spec spec)
             :active-page-name (intern (string-upcase cash-kind)))))))



(defun cash-menu (cash-kind id filter disabled-items)
  (display
   (make-instance 'actions-menu
                  :id "cash-actions"
                  :style "hnavbar actions"
                  :spec (crud-actions-spec (apply #'cash        cash-kind :id id filter)
                                           (apply #'cash/create cash-kind filter)
                                           (apply #'cash/update cash-kind :id id filter)
                                           (apply #'cash/delete cash-kind :id id filter)))
   :disabled-items disabled-items))

(defun cash-notifications ()
  (notifications
   '((company  (:company-title-unknown "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"))
     (amount (:non-positive-amount  "Το ποσό της συναλλαγής πρέπει να είναι θετικός αριθμός"
              :invalid-amount  "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
     (account-id (:acc-id-null "Δεν έχετε επιλέξει λογαριασμό")))))

;;; ------------------------------------------------------------
;;; Pages
;;; ------------------------------------------------------------

(define-regex-page cash (("financial/cash/" cash-kind) :registers (cash-kind "(expense|revenue)"))
    ((search    string)
     (start     integer)
     (id        integer chk-tx-id))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let* ((filter (parameters->plist search))
               (page-title (conc "Μετρητά » " (cash-kind-label cash-kind) " » Κατάλογος"))
               (cash-tx-table (make-instance 'cash-tx-table
                                             :id "cash-tx-table"
                                             :subpage cash-kind
                                             :op 'catalogue
                                             :filter filter)))
          (with-document ()
            (:head
             (:title (str page-title))
             (financial-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'financial)
                   (financial-navbar 'cash)
                   (:div :class "window grid_10"
                         (:div :class "title" (str page-title))
                         (cash-menu cash-kind
                                    (val id)
                                    filter
                                    (if (val id)
                                        '(catalogue)
                                        '(catalogue details update delete)))
                         (display cash-tx-table
                                  :selected-id (val* id)
                                  :selected-data nil
                                  :start (val* start)))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (cash cash-kind) (val search))
                         (cash-filters cash-kind (val search)))
                   (footer)))))
        (see-other (notfound)))))

(define-regex-page cash/create (("financial/cash/" cash-kind "/create")
                                :registers (cash-kind "(expense|revenue)"))
    ((search      string)
     (date        date)
     (company     string chk-company-title*)
     (description string)
     (amount      string  chk-amount)
     (account-id  integer chk-acc-id))
  (with-auth ("configuration")
    (no-cache)
    (let ((filter (parameters->plist search))
          (page-title (conc "Μετρητά » " (cash-kind-label cash-kind) " » Δημιουργία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'cash)
               (:div :id "cash-window" :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cash-menu cash-kind
                                nil
                                filter
                                '(create update delete))
                     (cash-notifications)
                     (with-form (actions/financial/cash/create cash-kind :search (val* search))
                       (cash-data-form 'create cash-kind
                                       :filter filter
                                       :data (parameters->plist date
                                                                company
                                                                description
                                                                amount
                                                                account-id)
                                       :styles (parameters->styles date
                                                                   company
                                                                   description
                                                                   amount))))
               (footer)))))))

(define-regex-page cash/update (("financial/cash/" cash-kind "/update")
                                :registers (cash-kind "(expense|revenue)"))
    ((search      string)
     (id          integer chk-tx-id t)
     (date        date)
     (company     string chk-company-title*)
     (description string)
     (amount      string  chk-amount)
     (account-id  integer chk-acc-id))
  (with-auth ("configuration")
    (no-cache)
    (let ((filter (parameters->plist search))
          (page-title (conc "Μετρητά » " (cash-kind-label cash-kind) " » Επεξεργασία"))
          (acc-keyword (if (string-equal cash-kind "expense") :debit-acc-id :credit-acc-id)))
      (with-document ()
        (:head
         (:title (str page-title))
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'cash)
               (:div :id "cash-window" :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cash-menu cash-kind
                                nil
                                filter
                                '(create update delete))
                     (with-form (actions/financial/cash/update cash-kind
                                                               :id (val id)
                                                               :search (val* search))
                       (cash-data-form 'update cash-kind
                                       :id (val id)
                                       :filter filter
                                       :data (plist-union (parameters->plist date
                                                                             company
                                                                             description
                                                                             amount)
                                                          (subst :account-id
                                                                 acc-keyword
                                                                 (get-tx-plist (val id))))
                                       :styles (parameters->styles date
                                                                   company
                                                                   description
                                                                   amount
                                                                   account-id))))
               (footer)))))))


(define-regex-page cash/delete (("financial/cash/" cash-kind "/delete")
                                :registers (cash-kind "(expense|revenue)"))
    ((id     integer chk-project-id t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search))
               (page-title (conc "Μετρητά » " (cash-kind-label cash-kind) " » Διαγραφή"))
               (cash-tx-table (make-instance 'cash-tx-table
                                             :op 'delete
                                             :subpage cash-kind
                                             :filter filter)))
          (with-document ()
            (:head
             (:title (str page-title))
             (financial-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'financial)
                   (financial-navbar 'cash)
                   (:div :id "cash-window" :class "window grid_10"
                         (:div :class "title" (str page-title))
                         (cash-menu cash-kind
                                    (val id)
                                    filter
                                    '(catalogue delete))
                         (with-form (actions/financial/cash/delete cash-kind
                                                                   :id (val id)
                                                                   :search (val* search))
                           (display cash-tx-table
                                    :selected-id (val id))))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (cash cash-kind) (val search))
                         (cash-filters cash-kind (val search)))
                   (footer)))))
        (see-other (error-page)))))

(defun cash-data-form (op cash-kind &key id data styles filter)
  (let ((disabledp (eql op 'details))
        (tree (apply #'make-instance 'account-radio-tree
                     (if (string-equal cash-kind "revenue")
                         (list :filter nil :root-id *revenues-root-account*)
                         (list :filter t   :root-id *expenses-root-account*)))))
    (flet ((label+textbox (name label)
             (with-html
               (label name label)
               (textbox name
                        :id (string-downcase name)
                        :value (getf data (make-keyword name))
                        :disabledp disabledp
                        :style (getf styles (make-keyword name))))))
      (with-html
        (:div :id "cash-data-form" :class "data-form"
              (:div :class "grid_6 alpha"
                    (label+textbox 'date "Ημερομηνία")
                    (label+textbox 'description "Περιγραφή")
                    (label+textbox 'company "Εταιρία")
                    (label+textbox 'amount "Ποσό")
                    (:div :class "data-form-buttons"
                          (if disabledp
                              (cancel-button (apply #'cash cash-kind :id id filter)
                                             "Επιστροφή στον Κατάλογο Συναλλαγών Μετρητών")
                              (progn
                                (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                                (cancel-button (apply #'cash cash-kind :id id filter) "Άκυρο")))))
              (:div :class "grid_6 omega"
                    (label 'account "Λογαριασμός")
                    ;; Display the tree. If needed, preselect the first account of the tree.
                    (display tree :selected-id (or (getf data :account-id)
                                                   (key (first (children (root tree))))))))))))