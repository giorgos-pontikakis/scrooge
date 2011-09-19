(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass cheque-event-page (regex-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
   (payload-parameter-names
    :allocation :class
    :initform '(date company description amount account-id))
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
       (:cheque-event-title-null
        "Η περιγραφή του συμβάντος είναι κενή."
        :cheque-event-title-exists
        "Αυτή η περιγραφή συμβάντος έχει ήδη οριστεί."))
      (from-state
       (:cheque-event-from/to/payable-exists
        "Έχει ήδη οριστεί συμβάν για αυτή την αρχική και τελική κατάσταση"
        :cheque-event-from-to-equal nil))
      (to-state
       (:cheque-event-from-to-equal
        "Η τελική κατάσταση δεν μπορεί να είναι ίδια με την αρχική κατάσταση."))))))



;;; ----------------------------------------------------------------------
;;; Checks
;;; ----------------------------------------------------------------------

(define-existence-predicate cheque-event-exists-p cheque-event id)
(define-existence-predicate cheque-state-exists-p cheque-state id)

(defun chk-cheque-event-id (id)
  (if (cheque-event-exists-p id)
      nil
      :cheque-event-id-unknown))

(defun chk-cheque-event-title (title)
  (if (eql title :null)
      :cheque-event-title-null
      nil))

(defun chk-cheque-state (state)
  (if (cheque-state-exists-p state)
      nil
      :cheque-state-invalid))

(defun chk-cheque-event-from/to/payable (from-state to-state kind)
  (if (cheque-event-from/to/payable-exists-p from-state to-state kind)
      :cheque-event-from/to/payable-exists
      nil))

(defun cheque-event-from/to/payable-exists-p (from-state to-state kind)
  (if (or (null from-state) (null to-state))
      nil
      (with-db ()
        (query (:select 1 :from 'cheque-event
                        :where (:and (:= 'from-state from-state)
                                     (:= 'to-state to-state)
                                     (:= 'payable-p (string= kind "payable"))))
               :plists))))

(defun check-cheque-event-parameters (from-state to-state kind)
  (validate-parameters (lambda (from to)
                         (chk-cheque-event-from/to/payable from to kind))
                       from-state to-state)
  (validate-parameters (lambda (from to)
                         (if (string= from to)
                             :cheque-event-from-to-equal
                             nil))
                       from-state to-state)
  nil)



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun cheque-event-menu (id kind &optional disabled)
  (menu (crud-actions-spec (config/cheque-event kind :id id)
                           (config/cheque-event/create kind)
                           (config/cheque-event/update kind :id id)
                           (config/cheque-event/delete kind :id id))
        :id "cheque-event-actions"
        :css-class "hmenu actions"
        :disabled disabled))

(defun cheque-event-filters (kind)
  (let ((spec `((receivable ,(config/cheque-event "receivable") "Προς είσπραξη")
                (payable    ,(config/cheque-event "payable")    "Προς πληρωμή"))))
    (with-html
      (:div :id "filters" :class "filters"
            (:p :class "title" "Είδος επιταγής")
            (navbar spec
                    :id "cheque-event-filters"
                    :css-class "vnavbar"
                    :active (intern (string-upcase kind))
                    :test #'string-equal)))))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defun cheque-event-record (id)
  (with-db ()
    (query (:select 'cheque-event.id 'cheque-event.title
                    (:as 'debit-account-tbl.title 'debit-account)
                    (:as 'credit-account-tbl.title 'credit-account)
                    (:as 'from-cheque-state.description 'from-description)
                    (:as 'to-cheque-state.description 'to-description)
                    :from 'cheque-event
                    :inner-join (:as 'account 'debit-account-tbl)
                    :on (:= 'debit-acc-id 'debit-account-tbl.id)
                    :inner-join (:as 'account 'credit-account-tbl)
                    :on (:= 'credit-acc-id 'credit-account-tbl.id)
                    :inner-join (:as 'cheque-state 'from-cheque-state)
                    :on (:= 'from-cheque-state.id 'cheque-event.from-state)
                    :inner-join (:as 'cheque-state 'to-cheque-state)
                    :on (:= 'to-cheque-state.id 'cheque-event.to-state)
                    :where (:= 'cheque-event.id id))
           :plist)))



;;; ------------------------------------------------------------
;;; Cheque Event Table
;;; ------------------------------------------------------------

;;; table

(defclass cheque-event-table (scrooge-table)
  ((kind        :accessor kind :initarg :kind)
   (header-labels  :initform '("" "<br />Περιγραφή"
                               "Αρχική<br />Κατάσταση" "Τελική<br />Κατάσταση"
                                "Πρότυπη<br />Συναλλαγή"))
   (paginator      :initform nil))
  (:default-initargs :item-class 'cheque-event-row))

(defmethod get-records ((table cheque-event-table))
  (with-db ()
    (query (:order-by (:select 'id 'cheque-event.title 'from-state 'to-state
                               (:as 'temtx.title 'temtx)
                       :from 'cheque-event
                       :inner-join 'temtx
                       :on (:= 'temtx.id 'cheque-event.temtx-id)
                       :where (:= 'payable-p (string= (kind table) "payable")))
                      'cheque-event.title)
           :plists)))


;;; rows

(defclass cheque-event-row (scrooge-row/plist)
  ())

(defmethod selector ((row cheque-event-row) enabled-p)
  (let* ((id (key row))
         (table (collection row))
         (kind (kind table)))
    (html ()
      (:a :href (if enabled-p
                    (apply #'config/cheque-event kind)
                    (apply #'config/cheque-event kind :id id))
          (selector-img enabled-p)))))

(defmethod payload ((row cheque-event-row) enabled-p)
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
          (make-instance 'input-text
                         :name 'temtx
                         :value (getf record :temtx)
                         :css-class "ac-temtx"
                         :disabled (not enabled-p)))))

(defmethod controls ((row cheque-event-row) enabled-p)
  (let ((id (key row))
        (kind (kind (collection row))))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (config/cheque-event kind :id id)))
        (list nil nil))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage cheque-event-page config/cheque-event
    (("config/cheque-event/" (kind "(receivable|payable)")))
    ((id integer chk-cheque-event-id))
  (with-view-page
    (let ((cheque-event-table (make-instance 'cheque-event-table
                                             :op :read
                                             :id "cheque-event-table"
                                             :kind kind)))
      (with-document ()
        (:head
         (:title "Συμβάντα Επιταγών")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-event)
               (:div :id "cheque-event-window" :class "window grid_10"
                     (:div :class "title" "Συμβάντα Επιταγών » Κατάλογος")
                     (cheque-event-menu (val id)
                                        kind
                                        (if (val id)
                                            '(:read)
                                            '(:read :update :delete)))
                     (display cheque-event-table
                              :key (val id)))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (cheque-event-filters kind))
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage cheque-event-page config/cheque-event/create
    (("config/cheque-event/" (kind "(receivable|payable)") "/create"))
    ((title      string chk-cheque-event-title)
     (payable-p  boolean)
     (from-state string chk-cheque-state)
     (to-state   string chk-cheque-state))
  (with-view-page
    (check-cheque-event-parameters from-state to-state kind)
    (let ((cheque-event-table (make-instance 'cheque-event-table
                                             :kind kind
                                             :op :create)))
      (with-document ()
        (:head
         (:title "Συμβάντα Επιταγών » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque)
               (:div :class "window grid_12"
                     (:div :class "title" "Συμβάντα Επιταγών » Δημιουργία")
                     (cheque-event-menu nil
                                        kind
                                        '(:create :update :delete))
                     (notifications)
                     (with-form (actions/cheque-event/create kind)
                       (display cheque-event-table :payload (params->payload))))))))))

(defpage cheque-event-page actions/cheque-event/create
    (("actions/cheque-event/" (kind "(receivable|payable)") "/create") :request-type :post)
    ((title      string chk-cheque-event-title)
     (payable-p  boolean)
     (from-state string chk-cheque-state)
     (to-state   string chk-cheque-state))
  (with-controller-page (config/cheque-event/create kind)
    (check-cheque-event-parameters from-state to-state kind)
    (let ((new-cheque-event (make-instance 'cheque-event
                                           :title (val title)
                                           :payable-p (string= kind "payable")
                                           :from-state (val from-state)
                                           :to-state (val to-state))))
      (insert-dao new-cheque-event)
      (see-other (config/cheque-event kind :id (cheque-event-id new-cheque-event))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage cheque-event-page config/cheque-event/update
    (("config/kind/" (kind "(receivable|payable)") "/update"))
    ((id         integer chk-cheque-event-id    t)
     (title      string  chk-cheque-event-title)
     (payable-p  boolean)
     (from-state string  chk-cheque-state)
     (to-state   string  chk-cheque-state))
  (with-view-page
    (check-cheque-event-parameters from-state to-state kind)
    (let ((cheque-event-table (make-instance 'cheque-event-table
                                             :kind kind
                                             :op :update)))
      (with-document ()
        (:head
         (:title "Συμβάντα Επιταγών » Επεξεργασία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-event)
               (:div :id "cheque-event-window" :class "window grid_12"
                     (:div :class "title" "Συμβάντα Επιταγών » Επεξεργασία")
                     (cheque-event-menu (val id)
                                        kind
                                        '(:create :update))
                     (notifications)
                     (with-form (actions/cheque-event/update kind :id (val id))
                       (display cheque-event-table :key (val id)
                                                   :payload (params->payload))))
               (footer)))))))

(defpage cheque-event-page actions/cheque-event/update
    (("actions/cheque-event/" (kind "(receivable|payable)") "/update")
     :request-type :post)
    ((id         integer chk-cheque-event-id    t)
     (title      string  chk-cheque-event-title)
     (payable-p  boolean)
     (from-state string  chk-cheque-state)
     (to-state   string  chk-cheque-state))
  (with-controller-page (config/cheque-event/update kind)
    (check-cheque-event-parameters from-state to-state kind)
    (execute (:update 'cheque-event :set
                      'title (val title)
                      'from-state (val from-state)
                      'to-state (val to-state)
                      :where (:= 'id (val id))))
    (see-other (config/cheque-event kind :id (val id)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage cheque-event-page config/cheque-event/delete
    (("config/cheque-event/" (kind "(receivable|payable)") "/delete"))
    ((id integer chk-cheque-event-id t))
  (with-view-page
    (let ((cheque-event-table (make-instance 'cheque-event-table
                                             :kind kind
                                             :op :delete)))
      (with-document ()
        (:head
         (:title "Συμβάντα Επιταγών » Διαγραφή")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-event)
               (:div :id "cheque-event-window" :class "window grid_10"
                     (:div :class "title" "Συμβάντα Επιταγών » Διαγραφή")
                     (cheque-event-menu (val id)
                                        kind
                                        '(:create :delete))
                     (with-form (actions/cheque-event/delete kind
                                                             :id (val id))
                       (display cheque-event-table
                                :key (val id))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (cheque-event-filters kind))
               (footer)))))))

(defpage cheque-event-page actions/cheque-event/delete
    (("actions/cheque-event/" (kind "(receivable|payable)") "/delete")
     :request-type :post)
    ((id integer chk-cheque-event-id t))
  (with-controller-page (config/cheque-event/delete kind)
    (delete-dao (get-dao 'cheque-event (val id)))
    (see-other (config/cheque-event kind))))
