(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass cheque-stran-page (regex-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
   (payload-parameter-names
    :allocation :class
    :initform '(date company description amount debit-account credit-account))
   (filter-parameter-names
    :allocation :class
    :initform '())
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))
   (messages
    :allocation :class
    :reader messages
    :initform
    '((title
       (:cheque-stran-title-null
        "Η περιγραφή του συμβάντος είναι κενή."
        :cheque-stran-title-exists
        "Αυτή η περιγραφή συμβάντος έχει ήδη οριστεί."))
      (from-state
       (:cheque-stran-from/to/payable-exists
        "Έχει ήδη οριστεί συμβάν για αυτή την αρχική και τελική κατάσταση"
        :cheque-stran-from-to-equal nil))
      (to-state
       (:cheque-stran-from-to-equal
        "Η τελική κατάσταση δεν μπορεί να είναι ίδια με την αρχική κατάσταση."))
      (debit-account
       (:account-title-null
        "Άκυρος λογαριασμός χρέωσης: Το όνομα είναι κενό."
        :account-title-unknown
        "Άκυρος λογαριασμός χρέωσης: Δεν υπάρχει λογαριασμός με αυτό το όνομα."))
      (credit-account
       (:account-title-null
        "Άκυρος λογαριασμός πίστωσης: Το όνομα είναι κενό."
        :account-title-unknown
        "Άκυρος λογαριασμός πίστωσης: Δεν υπάρχει λογαριασμός με αυτό το όνομα."))))))



;;; ----------------------------------------------------------------------
;;; Checks
;;; ----------------------------------------------------------------------

(define-existence-predicate cheque-state-exists-p cheque-state id)

(define-existence-predicate cheque-stran-exists-p cheque-stran id)
(define-existence-predicate* cheque-stran-title-exists-p cheque-stran title id)

(defun chk-cheque-stran-id (id)
  (if (cheque-stran-exists-p id)
      nil
      :cheque-stran-id-unknown))

(defun chk-cheque-stran-title (title)
  (if (eql title :null)
      :cheque-stran-title-null
      nil))

(defun chk-cheque-stran-title/create (title)
  (cond ((eql :null title) :cheque-stran-title-null)
        ((cheque-stran-title-exists-p title) :cheque-stran-title-exists)
        (t nil)))

(defun chk-cheque-stran-title/update (title id)
  (cond ((eql :null title) :cheque-stran-title-null)
        ((cheque-stran-title-exists-p title id) :cheque-stran-title-exists)
        (t nil)))

(defun chk-cheque-state (state)
  (if (cheque-state-exists-p state)
      nil
      :cheque-state-invalid))


;;; post checks

(defun cheque-stran-from/to/payable-exists-p (from-state to-state kind)
  (if (or (null from-state) (null to-state))
      nil
      (with-db ()
        (query (:select 1 :from 'cheque-stran
                        :where (:and (:= 'from-state from-state)
                                     (:= 'to-state to-state)
                                     (:= 'payable-p (string= kind "payable"))))
               :plists))))

(defun chk-cheque-stran-from/to/payable (from-state to-state kind)
  (if (cheque-stran-from/to/payable-exists-p from-state to-state kind)
      :cheque-stran-from/to/payable-exists
      nil))

(defun check-cheque-stran-parameters (from-state to-state kind)
  (validate-parameters (lambda (from to)
                         (chk-cheque-stran-from/to/payable from to kind))
                       from-state to-state)
  (validate-parameters (lambda (from to)
                         (if (string= from to)
                             :cheque-stran-from-to-equal
                             nil))
                       from-state to-state)
  nil)



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun cheque-stran-menu (id kind &optional disabled)
  (menu (crud-actions-spec (config/cheque-stran kind :id id)
                           (config/cheque-stran/create kind)
                           (config/cheque-stran/update kind :id id)
                           (config/cheque-stran/delete kind :id id))
        :id "cheque-stran-actions"
        :css-class "hmenu actions"
        :disabled disabled))

(defun cheque-stran-filters (kind)
  (let ((spec `((receivable ,(config/cheque-stran "receivable") "Προς είσπραξη")
                (payable    ,(config/cheque-stran "payable")    "Προς πληρωμή"))))
    (with-html
      (:div :id "filters" :class "filters"
            (:p :class "title" "Είδος επιταγής")
            (navbar spec
                    :id "cheque-stran-filters"
                    :css-class "vnavbar"
                    :active (intern (string-upcase kind))
                    :test #'string-equal)))))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defun cheque-stran-record (id)
  (with-db ()
    (query (:select 'cheque-stran.id 'cheque-stran.title
                    (:as 'debit-account-tbl.title 'debit-account)
                    (:as 'credit-account-tbl.title 'credit-account)
                    (:as 'from-cheque-state.description 'from-description)
                    (:as 'to-cheque-state.description 'to-description)
                    :from 'cheque-stran
                    :inner-join (:as 'account 'debit-account-tbl)
                    :on (:= 'debit-acc-id 'debit-account-tbl.id)
                    :inner-join (:as 'account 'credit-account-tbl)
                    :on (:= 'credit-acc-id 'credit-account-tbl.id)
                    :inner-join (:as 'cheque-state 'from-cheque-state)
                    :on (:= 'from-cheque-state.id 'cheque-stran.from-state)
                    :inner-join (:as 'cheque-state 'to-cheque-state)
                    :on (:= 'to-cheque-state.id 'cheque-stran.to-state)
                    :where (:= 'cheque-stran.id id))
           :plist)))



;;; ------------------------------------------------------------
;;; Cheque Stran Table
;;; ------------------------------------------------------------

;;; table

(defclass cheque-stran-table (scrooge-table)
  ((kind        :accessor kind :initarg :kind)
   (header-labels  :initform '("" "<br />Περιγραφή"
                               "Αρχική<br />Κατάσταση" "Τελική<br />Κατάσταση"
                                "Πρότυπη<br />Συναλλαγή"))
   (paginator      :initform nil))
  (:default-initargs :item-class 'cheque-stran-row))

(defmethod get-records ((table cheque-stran-table))
  (with-db ()
    (query (:order-by (:select 'id 'cheque-stran.title
                               'from-state 'to-state
                               'debit-account 'credit-account
                               (:as 'temtx.title 'temtx)
                       :from 'cheque-stran
                       :inner-join (:as 'account 'debit-account-tbl)
                       :on (:= 'debit-acc-id 'debit-account-tbl.id)
                       :inner-join (:as 'account 'credit-account-tbl)
                       :on (:= 'credit-acc-id 'credit-account-tbl.id)
                       :where (:= 'payable-p (string= (kind table) "payable")))
                      'cheque-stran.title)
           :plists)))


;;; rows

(defclass cheque-stran-row (scrooge-row/plist)
  ())

(defmethod selector ((row cheque-stran-row) enabled-p)
  (let* ((id (key row))
         (table (collection row))
         (kind (kind table)))
    (html ()
      (:a :href (if enabled-p
                    (apply #'config/cheque-stran kind)
                    (apply #'config/cheque-stran kind :id id))
          (selector-img enabled-p)))))

(defmethod payload ((row cheque-stran-row) enabled-p)
  (let ((record (record row))
        (disabled (not enabled-p)))
    (list (make-instance 'textbox
                         :title 'title
                         :value (getf record :title)
                         :disabled disabled)
          (make-instance 'dropdown
                         :name 'from-state
                         :label-value-alist *cheque-states*
                         :selected (getf record :from-state)
                         :disabled disabled)
          (make-instance 'dropdown
                         :name 'to-state
                         :label-value-alist *cheque-states*
                         :selected (getf record :to-state)
                         :disabled disabled)
          (make-instance 'textbox
                         :name 'debit-account
                         :value (getf record :debit-account)
                         :css-class "ac-account"
                         :disabled disabled)
          (make-instance 'textbox
                         :name 'credit-account
                         :value (getf record :credit-account)
                         :css-class "ac-account"
                         :disabled disabled))))

(defmethod controls ((row cheque-stran-row) enabled-p)
  (let ((id (key row))
        (kind (kind (collection row))))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (config/cheque-stran kind :id id)))
        (list nil nil))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage cheque-stran-page config/cheque-stran
    (("config/cheque-stran/" (kind "(receivable|payable)")))
    ((start integer)
     (id    integer chk-cheque-stran-id))
  (with-view-page
    (let ((title "Μεταπτώσεις Επιταγών » Κατάλογος")
          (cheque-stran-table (make-instance 'cheque-stran-table
                                             :op :read
                                             :id "cheque-stran-table"
                                             :kind kind)))
      (with-document ()
        (:head
         (:title (str title))
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-stran)
               (:div :id "cheque-stran-window" :class "window grid_10"
                     (:div :class "title" (str title))
                     (cheque-stran-menu (val id)
                                        kind
                                        (if (val id)
                                            '(:read)
                                            '(:read :update :delete)))
                     (display cheque-stran-table
                              :key (val id)))
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage cheque-stran-page config/cheque-stran/create
    (("config/cheque-stran/" (kind "(receivable|payable)") "/create"))
    ((title          string  chk-cheque-stran-title/create)
     (payable-p      boolean)
     (from-state     string  chk-cheque-state)
     (to-state       string  chk-cheque-state)
     (debit-account  string  chk-acc-title)
     (credit-account string  chk-acc-title))
  (with-view-page
    (check-cheque-stran-parameters from-state to-state kind)
    (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                             :kind kind
                                             :op :create)))
      (with-document ()
        (:head
         (:title "Μεταπτώσεις Επιταγών » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque)
               (:div :class "window grid_12"
                     (:div :class "title" "Μεταπτώσεις Επιταγών » Δημιουργία")
                     (cheque-stran-menu nil
                                        kind
                                        '(:create :update :delete))
                     (notifications)
                     (with-form (actions/cheque-stran/create kind)
                       (display cheque-stran-table :payload (params->payload))))))))))

(defpage cheque-stran-page actions/cheque-stran/create
    (("actions/cheque-stran/" (kind "(receivable|payable)") "/create") :request-type :post)
    ((title          string  chk-cheque-stran-title/create)
     (payable-p      boolean)
     (from-state     string  chk-cheque-state)
     (to-state       string  chk-cheque-state)
     (debit-account  string  chk-acc-title)
     (credit-account string  chk-acc-title))
  (with-controller-page (config/cheque-stran/create kind)
    (check-cheque-stran-parameters from-state to-state kind)
    (let* ((debit-acc-id (account-id (val debit-account)))
           (credit-acc-id (account-id (val credit-account)))
           (new-cheque-stran (make-instance 'cheque-stran
                                            :title (val title)
                                            :payable-p (string= kind "payable")
                                            :from-state (val from-state)
                                            :to-state (val to-state)
                                            :debit-acc-id debit-acc-id
                                            :credit-acc-id credit-acc-id)))
      (insert-dao new-cheque-stran)
      (see-other (config/cheque-stran kind :id (cheque-stran-id new-cheque-stran))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage cheque-stran-page config/cheque-stran/update
    (("config/kind/" (kind "(receivable|payable)") "/update"))
    ((id             integer chk-cheque-stran-id           t)
     (title          string  chk-cheque-stran-title/update)
     (payable-p      boolean)
     (from-state     string  chk-cheque-state)
     (to-state       string  chk-cheque-state)
     (debit-account  string  chk-acc-title)
     (credit-account string  chk-acc-title))
  (with-view-page
    (check-cheque-stran-parameters from-state to-state kind)
    (let ((title "Μεταπτώσεις Επιταγών » Επεξεργασία")
          (cheque-stran-table (make-instance 'cheque-stran-table
                                             :kind kind
                                             :op :update)))
      (with-document ()
        (:head
         (:title (str title))
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-stran)
               (:div :id "cheque-stran-window" :class "window grid_12"
                     (:div :class "title" (str title))
                     (cheque-stran-menu (val id)
                                        kind
                                        '(:create :update))
                     (notifications)
                     (with-form (actions/cheque-stran/update kind :id (val id))
                       (display cheque-stran-table :key (val id)
                                                   :payload (params->payload))))
               (footer)))))))

(defpage cheque-stran-page actions/cheque-stran/update
    (("actions/cheque-stran/" (kind "(receivable|payable)") "/update")
     :request-type :post)
    ((id             integer chk-cheque-stran-id           t)
     (title          string  chk-cheque-stran-title/update)
     (payable-p      boolean)
     (from-state     string  chk-cheque-state)
     (to-state       string  chk-cheque-state)
     (debit-account  string  chk-acc-title)
     (credit-account string  chk-acc-title))
  (with-controller-page (config/cheque-stran/update kind)
    (check-cheque-stran-parameters from-state to-state kind)
    (let ((debit-acc-id (account-id (val debit-account)))
          (credit-acc-id (account-id (val credit-account))))
      (execute (:update 'cheque-stran :set
                        'title (val title)
                        'from-state (val from-state)
                        'to-state (val to-state)
                        'debit-acc-id debit-acc-id
                        'credit-acc-id credit-acc-id
                        :where (:= 'id (val id)))))
    (see-other (config/cheque-stran kind :id (val id)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage cheque-stran-page config/cheque-stran/delete
    (("config/cheque-stran/" (kind "(receivable|payable)") "/delete"))
    ((id integer chk-cheque-stran-id t))
  (with-view-page
    (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                             :kind kind
                                             :op :delete)))
      (with-document ()
        (:head
         (:title "Μεταπτώσεις Επιταγών » Διαγραφή")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-stran)
               (:div :id "cheque-stran-window" :class "window grid_10"
                     (:div :class "title" "Μεταπτώσεις Επιταγών » Διαγραφή")
                     (cheque-stran-menu (val id)
                                        kind
                                        '(:create :delete))
                     (with-form (actions/cheque-stran/delete kind
                                                             :id (val id))
                       (display cheque-stran-table
                                :key (val id))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (cheque-stran-filters kind))
               (footer)))))))

(defpage cheque-stran-page actions/cheque-stran/delete
    (("actions/cheque-stran/" (kind "(receivable|payable)") "/delete")
     :request-type :post)
    ((id integer chk-cheque-stran-id t))
  (with-controller-page (config/cheque-stran/delete kind)
    (delete-dao (get-dao 'cheque-stran (val id)))
    (see-other (config/cheque-stran kind))))
