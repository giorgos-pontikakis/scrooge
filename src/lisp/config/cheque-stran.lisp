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


;;; ------------------------------------------------------------
;;; Cheque state transitions - actions
;;; ------------------------------------------------------------

(define-regex-page actions/config/cheque-stran/create
    (("actions/config/cheque-stran/" cheque-kind "/create")
     :registers (cheque-kind "(receivable|payable)")
     :request-type :post)
    ((title          string chk-cheque-stran-title)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title)
     (from-status    string chk-cheque-status)
     (to-status      string chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
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
    (("actions/config/cheque-stran/" cheque-kind "/update")
     :registers (cheque-kind "(receivable|payable)")
     :request-type :post)
    ((id             integer chk-cheque-stran-id t)
     (title          string  chk-cheque-stran-title)
     (debit-account  string  chk-acc-title)
     (credit-account string  chk-acc-title)
     (from-status    string  chk-cheque-status)
     (to-status      string  chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
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
    (("actions/config/cheque-stran/" cheque-kind "/delete")
     :registers (cheque-kind "(receivable|payable)")
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

(defun cheque-stran-menu (id cheque-kind &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "cheque-stran-actions"
                          :style "hnavbar actions"
                          :spec (crud-actions-spec (config/cheque-stran cheque-kind :id id)
                                                   (config/cheque-stran/create cheque-kind)
                                                   (config/cheque-stran/update cheque-kind :id id)
                                                   (config/cheque-stran/delete cheque-kind :id id)))
           :disabled-items disabled-items))

(defun cheque-stran-notifications ()
  (notifications
   '((title (:cheque-stran-title-null "Το όνομα καταστατικής μεταβολής είναι κενό."
             :cheque-stran-title-exists "Το όνομα καταστατικής μεταβολής υπάρχει ήδη.")))))



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

(defclass cheque-stran-table (crud-table)
  ((item-key-field :initform :id)
   (header-labels  :initform '("" "<br />Περιγραφή"
                              "Αρχική<br />Κατάσταση" "Τελική<br />Κατάσταση"
                              "Λογαριασμός<br />Χρέωσης" "Λογαριασμός<br />Πίστωσης"))
   (paginator      :initform nil))
  (:default-initargs :item-class 'cheque-stran-row))

(defmethod read-records ((table cheque-stran-table))
  (let ((payable-p (string= (filter table) "payable")))
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

(defclass cheque-stran-row (crud-row)
  ())

(defmethod cells ((row cheque-stran-row) &key start)
  (declare (ignore start))
  (let* ((id (key row))
         (record (record row))
         (cheque-kind (filter (collection row))))
    (list :selector (make-instance 'selector-cell
                                   :states (list
                                            :on (config/cheque-stran cheque-kind)
                                            :off (config/cheque-stran cheque-kind :id id)))
          :payload (list (make-instance 'textbox-cell
                                        :name 'title
                                        :value (getf record :title))
                         (make-instance 'dropdown-cell
                                        :name 'from-status
                                        :selected (getf record :from-description)
                                        :alist *cheque-statuses*)
                         (make-instance 'dropdown-cell
                                        :name 'to-status
                                        :selected (getf record :to-description)
                                        :alist *cheque-statuses*)
                         (make-instance 'textbox-cell
                                        :name 'debit-account
                                        :value (getf record :debit-account))
                         (make-instance 'textbox-cell
                                        :name 'credit-account
                                        :value (getf record :credit-account)))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (config/cheque-stran cheque-kind :id id))))))



;;; ------------------------------------------------------------
;;; Cheque state transitions - pages
;;; ------------------------------------------------------------

(define-regex-page config/cheque-stran (("config/cheque-stran/" cheque-kind)
                                        :registers (cheque-kind "(receivable|payable)"))
    ((id integer chk-cheque-stran-id))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                                 :op 'catalogue
                                                 :id "cheque-stran-table"
                                                 :filter cheque-kind)))
          (with-document ()
            (:head
             (:title "Καταστατικές Μεταβολές Επιταγών")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'cheque-stran)
                   (:div :id "cheque-stran-window" :class "window grid_9"
                         (:div :class "title" "Καταστατικές Μεταβολών Επιταγών » Κατάλογος")
                         (cheque-stran-menu (val id)
                                            cheque-kind
                                            (if (val id)
                                                '(catalogue)
                                                '(catalogue update delete)))
                         (display cheque-stran-table
                                  :selected-id (val* id)))
                   (footer))))))))

(define-regex-page config/cheque-stran/create (("config/cheque-stran/" cheque-kind "/create")
                                               :registers (cheque-kind "(receivable|payable)"))

    ((title          string chk-cheque-stran-title)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title)
     (from-status    string chk-cheque-status)
     (to-status      string chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (with-document ()
      (:head
       (:title "Καταστατικές Μεταβολές Επιταγών » Δημιουργία")
       (config-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'config)
             (config-navbar 'cheque)
             (:div :class "window grid_9"
                   (:div :class "title" "Καταστατικές Μεταβολές Επιταγών » Δημιουργία")
                   (cheque-stran-menu nil
                                      cheque-kind
                                      '(create update delete))
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

(define-regex-page config/cheque-stran/update (("config/cheque-kind/" cheque-kind "/update")
                                               :registers (cheque-kind "(receivable|payable)"))
    ((id     integer chk-cheque-stran-id t)
     (title          string chk-cheque-stran-title)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title)
     (from-status    string chk-cheque-status)
     (to-status      string chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-document ()
          (:head
           (:title "Καταστατικές Μεταβολές Επιταγών » Επεξεργασία")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'cheque-stran)
                 (:div :id "sidebar" :class "sidebar grid_3"
                       (:p :class "title" "Φίλτρα")
                       (cheque-stran-notifications))
                 (:div :id "cheque-stran-window" :class "window grid_9"
                       (:div :class "title" "Καταστατικές Μεταβολές Επιταγών » Επεξεργασία")
                       (cheque-stran-menu (val id)
                                          cheque-kind
                                          '(create update))
                       (with-form (actions/config/cheque-stran/update cheque-kind :id (val id))
                         (cheque-stran-data-form cheque-kind
                                                 'update
                                                 :id (val id)
                                                 :data (plist-union
                                                        (get-cheque-stran-plist (val id))
                                                        (parameters->plist title
                                                                           debit-account
                                                                           credit-account
                                                                           from-status
                                                                           to-status)))))
                 (footer))))
        (see-other (notfound)))))

(define-regex-page config/cheque-stran/delete (("config/cheque-stran/" cheque-kind "/delete")
                                        :registers (cheque-kind "(receivable|payable)"))
    ((id integer chk-cheque-stran-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                                 :id "cheque-stran-table"
                                                 :filter cheque-kind
                                                 :op 'delete)))
          (with-document ()
            (:head
             (:title "Καταστατικές Μεταβολές Επιταγών » Διαγραφή")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'cheque-stran)
                   (:div :id "cheque-stran-window" :class "window grid_9"
                         (:div :class "title" "Καταστατικές Μεταβολές Επιταγών » Διαγραφή")
                         (cheque-stran-menu (val id)
                                            cheque-kind
                                            '(create delete))
                         (with-form (actions/config/cheque-stran/delete cheque-kind
                                                                        :id (val id))
                           (display cheque-stran-table
                                    :selected-id (val id))))
                   (footer)))))
        (see-other (notfound)))))

(defun cheque-stran-data-form (cheque-kind op &key id data styles)
  (let ((disabledp (eql op 'details)))
    (flet ((label+textbox (name label)
             (with-html
               (label name label)
               (textbox name
                        :id (string-downcase name)
                        :value (getf data (make-keyword name))
                        :disabledp disabledp
                        :style (getf styles (make-keyword name))))))
      (with-html
        (:div :id "cheque-data-form" :class "data-form"
              (label+textbox 'title "Περιγραφή")
              ;;
              (label 'from-status "Αρχική Κατάσταση")
              (dropdown 'from-status *cheque-statuses*
                        :selected (getf data :from-description)
                        :disabledp disabledp
                        :style (getf styles :from-description))
              (label 'from-status "Τελική Κατάσταση")
              (dropdown 'to-status *cheque-statuses*
                        :selected (getf data :to-description)
                        :disabledp disabledp
                        :style (getf styles :to-description))
              ;;
              (label+textbox 'debit-account "Λογαριασμός Χρέωσης")
              (label+textbox 'credit-account "Λογαριασμός Πίστωσης"))
        (:div :class "data-form-buttons grid_9"
              (if disabledp
                  (cancel-button (config/cheque-stran cheque-kind :id id)
                                 "Επιστροφή στον Κατάλογο Επιταγών")
                  (progn
                    (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                    (cancel-button (config/cheque-stran cheque-kind :id id) "Άκυρο"))))))))
