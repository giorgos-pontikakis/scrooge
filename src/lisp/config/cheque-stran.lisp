(in-package :scrooge)

(define-existence-predicate cheque-stran-exists-p cheque-stran id)
(define-existence-predicate cheque-status-exists-p cheque-status id)

(defun chk-cheque-stran-id (id)
  (if (cheque-stran-exists-p id)
      nil
      :cheque-stran-id-unknown))

(defun chk-cheque-status (status)
  (if (cheque-status-exists-p status)
      nil
      :cheque-status-invalid))

(defun chk-cheque-stran-title (title)
  (if (eql title :null)
      :cheque-stran-title-null
      nil))

(defun chk-cheque-stran-from/to/payable (from-status to-status cheque-kind)
  (if (cheque-stran-from/to/payable-exists-p from-status to-status cheque-kind)
      :cheque-stran-from/to/payable-exists
      nil))

(defun cheque-stran-from/to/payable-exists-p (from-status to-status cheque-kind)
  (if (or (null from-status) (null to-status))
      nil
      (with-db ()
        (query (:select 1 :from 'cheque-stran
                        :where (:and (:= 'from-status from-status)
                                     (:= 'to-status to-status)
                                     (:= 'payable-p (string= cheque-kind "payable"))))
               :plists))))

(defun post-checks (from-status to-status debit-account credit-account cheque-kind)
  (validate-parameters (lambda (from to)
                         (chk-cheque-stran-from/to/payable from to cheque-kind))
                       from-status to-status)
  (validate-parameters (lambda (from to)
                         (if (string= from to)
                             :cheque-stran-from-to-equal
                             nil))
                       from-status to-status)
  (validate-parameters (lambda (&rest accounts)
                         (if (some #'chequing-p accounts)
                             nil
                             :no-chequing-accounts-found))
                       debit-account credit-account)
  nil)



;;; ------------------------------------------------------------
;;; Cheque state transitions - actions
;;; ------------------------------------------------------------

(define-regex-page actions/config/cheque-stran/create
    (("actions/config/cheque-stran/" (cheque-kind "receivable|payable") "/create")
     :request-type :post)
    ((title          string chk-cheque-stran-title)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title)
     (from-status    string chk-cheque-status)
     (to-status      string chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (post-checks from-status to-status debit-account credit-account cheque-kind)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (let* ((debit-acc-id (account-id (val debit-account)))
                 (credit-acc-id (account-id (val credit-account)))
                 (new-cheque-stran (make-instance 'cheque-stran
                                                  :title (val title)
                                                  :debit-acc-id debit-acc-id
                                                  :credit-acc-id credit-acc-id
                                                  :payable-p (string= cheque-kind "payable")
                                                  :from-status (val from-status)
                                                  :to-status (val to-status))))
            (insert-dao new-cheque-stran)
            (see-other (config/cheque-stran cheque-kind :id (id new-cheque-stran)))))
        (see-other (config/cheque-stran/create cheque-kind
                                               :title (raw title)
                                               :debit-account (raw debit-account)
                                               :credit-account (raw credit-account)
                                               :from-status (raw from-status)
                                               :to-status (raw to-status))))))

(define-regex-page actions/config/cheque-stran/update
    (("actions/config/cheque-stran/" (cheque-kind "receivable|payable") "/update")
     :request-type :post)
    ((id             integer chk-cheque-stran-id t)
     (title          string  chk-cheque-stran-title)
     (debit-account  string  chk-acc-title)
     (credit-account string  chk-acc-title)
     (from-status    string  chk-cheque-status)
     (to-status      string  chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (post-checks from-status to-status debit-account credit-account cheque-kind)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (let* ((debit-acc-id (account-id (val debit-account)))
                 (credit-acc-id (account-id (val credit-account))))
            (execute (:update 'cheque-stran :set
                              'title (val title)
                              'debit-acc-id debit-acc-id
                              'credit-acc-id credit-acc-id
                              'from-status (val from-status)
                              'to-status (val to-status)
                              :where (:= 'id (val id)))))
          (see-other (config/cheque-stran cheque-kind)))
        (see-other (config/cheque-stran/update cheque-kind
                                               :id (val id)
                                               :title (raw title)
                                               :debit-account (raw debit-account)
                                               :credit-account (raw credit-account)
                                               :from-status (raw from-status)
                                               :to-status (raw to-status))))))

(define-regex-page actions/config/cheque-stran/delete
    (("actions/config/cheque-stran/" (cheque-kind "receivable|payable") "/delete")
     :request-type :post)
    ((id integer chk-cheque-stran-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'cheque-stran (val id)))
          (see-other (config/cheque-stran cheque-kind)))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun cheque-stran-menu (id cheque-kind &optional disabled)
  (menu (crud-actions-spec (config/cheque-stran cheque-kind :id id)
                           (config/cheque-stran/create cheque-kind)
                           (config/cheque-stran/update cheque-kind :id id)
                           (config/cheque-stran/delete cheque-kind :id id))
        :id "cheque-stran-actions"
        :style "hnavbar actions"
        :disabled disabled))

(defun cheque-stran-notifications ()
  (notifications
   '((title (:cheque-stran-title-null "Η περιγραφή της  καταστατικής μεταβολής είναι κενή."
             :cheque-stran-title-exists "Αυτή η περιγραφή καταστατικής μεταβολής έχει ήδη οριστεί."))
     (from-status (:cheque-stran-from/to/payable-exists
                   "Έχει ήδη οριστεί καταστατική μεταβολή για αυτή την αρχική και τελική κατάσταση"
                   :cheque-stran-from-to-equal nil))
     (to-status (:cheque-stran-from-to-equal
                 "Η τελική κατάσταση δεν μπορεί να είναι ίδια με την αρχική κατάσταση."))
     (debit-account (:account-title-unknown "Άγνωστος λογαριασμός χρέωσης."
                     :account-title-null "O λογαριασμός χρέωσης είναι κενός."
                     :no-chequing-accounts-found nil))
     (credit-account (:account-title-unknown "Άγνωστος λογαριασμός πίστωσης."
                      :account-title-null "O λογαριασμός πίστωσης είναι κενός."
                      :no-chequing-accounts-found "Πρέπει να ορίσετε τραπεζικό λογαριασμό είτε για το λογαριασμό χρέωσης είτε για το λογαριασμό πίστωσης.")))))

(defun cheque-stran-filters (cheque-kind)
  (let ((spec `((receivable ,(config/cheque-stran "receivable") "Προς είσπραξη")
                (payable    ,(config/cheque-stran "payable")    "Προς πληρωμή"))))
    (with-html
      (:div :id "filters" :class "filters"
            (:p :class "title" "Είδος επιταγής")
            (navbar spec
                    :id "cheque-stran-filters"
                    :style "vnavbar"
                    :active-page-name (intern (string-upcase cheque-kind)))))))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defun get-cheque-stran-plist (id)
  (with-db ()
    (query (:select 'cheque-stran.id 'cheque-stran.title
                    (:as 'debit-account-tbl.title 'debit-account)
                    (:as 'credit-account-tbl.title 'credit-account)
                    (:as 'from-cheque-status.description 'from-description)
                    (:as 'to-cheque-status.description 'to-description)
                    :from 'cheque-stran
                    :inner-join (:as 'account 'debit-account-tbl)
                    :on (:= 'debit-acc-id 'debit-account-tbl.id)
                    :inner-join (:as 'account 'credit-account-tbl)
                    :on (:= 'credit-acc-id 'credit-account-tbl.id)
                    :inner-join (:as 'cheque-status 'from-cheque-status)
                    :on (:= 'from-cheque-status.id 'cheque-stran.from-status)
                    :inner-join (:as 'cheque-status 'to-cheque-status)
                    :on (:= 'to-cheque-status.id 'cheque-stran.to-status)
                    :where (:= 'cheque-stran.id id))
           :plist)))



;;; ------------------------------------------------------------
;;; Cheque state transitions - table
;;; ------------------------------------------------------------

;;; table

(defclass cheque-stran-table (scrooge-crud-table)
  ((subpage        :accessor subpage :initarg :subpage)
   (item-key-field :initform :id)
   (header-labels  :initform '("" "<br />Περιγραφή"
                              "Αρχική<br />Κατάσταση" "Τελική<br />Κατάσταση"
                              "Λογαριασμός<br />Χρέωσης" "Λογαριασμός<br />Πίστωσης"))
   (paginator      :initform nil))
  (:default-initargs :item-class 'cheque-stran-row))

(defmethod read-records ((table cheque-stran-table))
  (let ((payable-p (string= (subpage table) "payable")))
   (with-db ()
     (query (:order-by (:select 'cheque-stran.id 'cheque-stran.title
                                (:as 'debit-account-tbl.title 'debit-account)
                                (:as 'credit-account-tbl.title 'credit-account)
                                (:as 'from-cheque-status.description 'from-description)
                                (:as 'to-cheque-status.description 'to-description)
                                :from 'cheque-stran
                                :inner-join (:as 'account 'debit-account-tbl)
                                :on (:= 'debit-acc-id 'debit-account-tbl.id)
                                :inner-join (:as 'account 'credit-account-tbl)
                                :on (:= 'credit-acc-id 'credit-account-tbl.id)
                                :inner-join (:as 'cheque-status 'from-cheque-status)
                                :on (:= 'from-cheque-status.id 'cheque-stran.from-status)
                                :inner-join (:as 'cheque-status 'to-cheque-status)
                                :on (:= 'to-cheque-status.id 'cheque-stran.to-status)
                                :where (:= 'payable_p payable-p))
                       'cheque-stran.title)
            :plists))))

;;; rows

(defclass cheque-stran-row (scrooge-crud-row)
  ())

(defmethod selector ((row cheque-stran-row) enabled-p)
  (let* ((id (key row))
         (table (collection row))
         (filter (filter table))
         (cheque-kind (subpage table))
         (start (page-start (paginator table) (index row) (start-index table))))
    (html ()
      (:a :href (if enabled-p
                    (apply #'config/cheque-stran cheque-kind :start start filter)
                    (apply #'config/cheque-stran cheque-kind :id id filter))
          (selector-img enabled-p)))))

(defmethod payload ((row cheque-stran-row) enabled-p)
  (let ((record (record row))
        (disabled (not enabled-p)))
   (list (make-instance 'textbox
                        :title 'title
                        :value (getf record :title)
                        :disabled disabled)
         (make-instance 'dropdown
                        :name 'from-status
                        :label-value-alist *cheque-statuses*
                        :selected (getf record :from-description)
                        :disabled disabled)
         (make-instance 'dropdown
                        :name 'to-status
                        :label-value-alist *cheque-statuses*
                        :selected (getf record :to-description)
                        :disabled disabled)
         (make-instance 'textbox
                        :name 'debit-account
                        :value (getf record :debit-account)
                        :disabled disabled)
         (make-instance 'textbox
                        :name 'credit-account
                        :value (getf record :credit-account)
                        :disabled disabled))))

(defmethod controls ((row cheque-stran-row) enabled-p)
  (let ((id (key row))
        (cheque-kind (subpage (collection row))))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (config/cheque-stran cheque-kind :id id)))
        (list nil nil))))


;;; ------------------------------------------------------------
;;; Cheque state transitions - pages
;;; ------------------------------------------------------------

(define-regex-page config/cheque-stran (("config/cheque-stran/" (cheque-kind "receivable|payable")))
    ((id integer chk-cheque-stran-id))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                                 :op :catalogue
                                                 :id "cheque-stran-table"
                                                 :subpage cheque-kind)))
          (with-document ()
            (:head
             (:title "Καταστατικές Μεταβολές Επιταγών")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'cheque-stran)
                   (:div :id "cheque-stran-window" :class "window grid_10"
                         (:div :class "title" "Καταστατικές Μεταβολών Επιταγών » Κατάλογος")
                         (cheque-stran-menu (val id)
                                            cheque-kind
                                            (if (val id)
                                                '(:catalogue)
                                                '(:catalogue :update :delete)))
                         (display cheque-stran-table
                                  :selected-id (val* id)))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (cheque-stran-filters cheque-kind))
                   (footer))))))))

(define-regex-page config/cheque-stran/create
    (("config/cheque-stran/" (cheque-kind "receivable|payable") "/create"))
    ((title          string chk-cheque-stran-title)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title)
     (from-status    string chk-cheque-status)
     (to-status      string chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (post-checks from-status to-status debit-account credit-account cheque-kind)
    (with-document ()
      (:head
       (:title "Καταστατικές Μεταβολές Επιταγών » Δημιουργία")
       (config-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'config)
             (config-navbar 'cheque)
             (:div :class "window grid_12"
                   (:div :class "title" "Καταστατικές Μεταβολές Επιταγών » Δημιουργία")
                   (cheque-stran-menu nil
                                      cheque-kind
                                      '(:create :update :delete))
                   (cheque-stran-notifications)
                   (with-form (actions/config/cheque-stran/create cheque-kind)
                     (cheque-stran-data-form cheque-kind
                                             'create
                                             :id nil
                                             :data (parameters->plist title
                                                                      debit-account
                                                                      credit-account
                                                                      from-status
                                                                      to-status)
                                             :styles (parameters->styles title
                                                                         debit-account
                                                                         credit-account
                                                                         from-status
                                                                         to-status)))))))))

(define-regex-page config/cheque-stran/update
    (("config/cheque-kind/" (cheque-kind "receivable|payable") "/update"))
    ((id             integer chk-cheque-stran-id t)
     (title          string chk-cheque-stran-title)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title)
     (from-status    string chk-cheque-status)
     (to-status      string chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (progn
          (post-checks from-status to-status debit-account credit-account cheque-kind)
          (with-document ()
            (:head
             (:title "Καταστατικές Μεταβολές Επιταγών » Επεξεργασία")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'cheque-stran)
                   (:div :id "cheque-stran-window" :class "window grid_12"
                         (:div :class "title" "Καταστατικές Μεταβολές Επιταγών » Επεξεργασία")
                         (cheque-stran-menu (val id)
                                            cheque-kind
                                            '(:create :update))
                         (cheque-stran-notifications)
                         (with-form (actions/config/cheque-stran/update cheque-kind :id (val id))
                           (cheque-stran-data-form cheque-kind
                                                   'update
                                                   :id (val id)
                                                   :data (plist-union
                                                          (parameters->plist title
                                                                             debit-account
                                                                             credit-account
                                                                             from-status
                                                                             to-status)
                                                          (get-cheque-stran-plist (val id)))
                                                   :styles (parameters->styles title
                                                                               debit-account
                                                                               credit-account
                                                                               from-status
                                                                               to-status))))
                   (footer)))))
        (see-other (notfound)))))

(define-regex-page config/cheque-stran/delete
    (("config/cheque-stran/" (cheque-kind "receivable|payable") "/delete"))
    ((id integer chk-cheque-stran-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                                 :id "cheque-stran-table"
                                                 :subpage cheque-kind
                                                 :op :delete)))
          (with-document ()
            (:head
             (:title "Καταστατικές Μεταβολές Επιταγών » Διαγραφή")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'cheque-stran)
                   (:div :id "cheque-stran-window" :class "window grid_10"
                         (:div :class "title" "Καταστατικές Μεταβολές Επιταγών » Διαγραφή")
                         (cheque-stran-menu (val id)
                                            cheque-kind
                                            '(:create :delete))
                         (with-form (actions/config/cheque-stran/delete cheque-kind
                                                                        :id (val id))
                           (display cheque-stran-table
                                    :selected-id (val id))))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (cheque-stran-filters cheque-kind))
                   (footer)))))
        (see-other (notfound)))))

(defun cheque-stran-data-form (cheque-kind op &key id data styles)
  (let ((disabled (eql op :details)))
    (flet ((label+textbox (name label &optional extra-styles)
             (with-html
               (label name label)
               (textbox name
                        :value (getf data (make-keyword name))
                        :disabled disabled
                        :style (conc (getf styles (make-keyword name))
                                     " " extra-styles)))))
      (with-html
        (:div :id "cheque-data-form" :class "data-form"
              (label+textbox 'title "Περιγραφή")
              ;;
              (label 'from-status "Αρχική Κατάσταση")
              (dropdown 'from-status *cheque-statuses*
                        :selected (getf data :from-status)
                        :disabled disabled
                        :style (getf styles :from-status))
              (label 'from-status "Τελική Κατάσταση")
              (dropdown 'to-status *cheque-statuses*
                        :selected (getf data :to-status)
                        :disabled disabled
                        :style (getf styles :to-status))
              ;;
              (label+textbox 'debit-account "Λογαριασμός Χρέωσης" "ac-account")
              (label+textbox 'credit-account "Λογαριασμός Πίστωσης" "ac-account"))
        (:div :class "data-form-buttons grid_9"
              (if disabled
                  (cancel-button (config/cheque-stran cheque-kind :id id)
                                 :content "Επιστροφή στον Κατάλογο Επιταγών")
                  (progn
                    (ok-button :content (if (eql op :update) "Ανανέωση" "Δημιουργία") )
                    (cancel-button (config/cheque-stran cheque-kind :id id)
                                   :content "Άκυρο"))))))))
